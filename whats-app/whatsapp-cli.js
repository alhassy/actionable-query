#!/usr/bin/env node
// whatsapp-cli.js — headless WhatsApp bridge for Emacs (whats-app.el)
//
// Prerequisites (one-time):
//   npm install @whiskeysockets/baileys qrcode-terminal   (run from ~/.emacs.d/)
//   node whatsapp-cli.js auth                             (scan QR; session saved)
//
// Read operations (contacts, unread) read directly from the WhatsApp macOS app's
// SQLite databases — no network connection needed after auth.
//
// Subcommands:
//   auth                   — QR-scan auth; writes session to whatsapp-session/
//   contacts               — print JSON array [{jid,name,phone}]
//   unread                 — print JSON array [{jid,name,snippet,timestamp,count}]
//   send   <jid> <msg>     — send a message; prints {"ok":true}
//   reply  <jid> <msg>     — alias for send

'use strict';

const path = require('path');
const fs   = require('fs');
const os   = require('os');

// ─── paths ────────────────────────────────────────────────────────────────────

const SESSION_DIR = path.join(__dirname, 'whatsapp-session');
const WA_SHARED   = path.join(os.homedir(),
  'Library/Group Containers/group.net.whatsapp.WhatsApp.shared');
const CONTACTS_DB = path.join(WA_SHARED, 'ContactsV2.sqlite');
const CHAT_DB     = path.join(WA_SHARED, 'ChatStorage.sqlite');

// ─── sqlite helper (synchronous, using python3 — no npm sqlite3 needed) ───────

function sqliteQuery(dbPath, sql) {
  // Runs sql against dbPath via python3 and returns parsed JSON rows.
  const script = `
import sqlite3, json, sys
con = sqlite3.connect(sys.argv[1])
con.row_factory = sqlite3.Row
rows = con.execute(sys.argv[2]).fetchall()
print(json.dumps([dict(r) for r in rows]))
con.close()
`;
  const { execFileSync } = require('child_process');
  const out = execFileSync('python3', ['-c', script, dbPath, sql], { encoding: 'utf8' });
  return JSON.parse(out.trim());
}

// ─── helpers ──────────────────────────────────────────────────────────────────

function jidToPhone(jid) {
  const raw = (jid || '').split('@')[0];
  if (!raw || raw.length < 6) return raw || '';
  if (raw.length >= 10) {
    const cc  = raw.slice(0, raw.length - 10);
    const num = raw.slice(-10);
    const fmt = `${num.slice(0,3)}-${num.slice(3,6)}-${num.slice(6)}`;
    return cc ? `+${cc} ${fmt}` : fmt;
  }
  return `+${raw}`;
}

function isSkippable(jid) {
  if (!jid) return true;
  return jid.endsWith('@g.us')
    || jid.endsWith('@newsletter')
    || jid.includes('@status');
}

// Resolve a LID-format JID like "12345@lid" to a phone JID like "1905...@s.whatsapp.net"
// by reading the LID mapping files Baileys writes to the session directory.
const lidCache = {};
function resolveLid(jid) {
  if (!jid || !jid.endsWith('@lid')) return jid;
  const lid = jid.split('@')[0];
  if (lidCache[lid]) return lidCache[lid];
  const f = path.join(SESSION_DIR, `lid-mapping-${lid}_reverse.json`);
  try {
    const phone = JSON.parse(fs.readFileSync(f, 'utf8'));
    const resolved = `${phone}@s.whatsapp.net`;
    lidCache[lid] = resolved;
    return resolved;
  } catch { return jid; }
}

function die(msg) {
  process.stderr.write(msg + '\n');
  process.exit(1);
}

// ─── Baileys setup (only needed for auth and send) ───────────────────────────

// ponytail: auto-installs missing deps on first auth/send instead of requiring
// a manual one-time `npm install` ritual the user has to remember.
function requireOrInstall(pkg) {
  try {
    return require(pkg);
  } catch (err) {
    if (err.code !== 'MODULE_NOT_FOUND') throw err;
    require('child_process').execFileSync(
      'npm', ['install', '@whiskeysockets/baileys', 'qrcode-terminal', 'pino'],
      { cwd: __dirname, stdio: ['ignore', 'ignore', 'inherit'] });
    return require(pkg);
  }
}

async function makeSocket() {
  const {
    makeWASocket,
    useMultiFileAuthState,
    fetchLatestBaileysVersion,
    makeCacheableSignalKeyStore,
  } = requireOrInstall('@whiskeysockets/baileys');
  const pino   = requireOrInstall('pino');
  const logger = pino({ level: 'silent' }, pino.destination(2));

  fs.mkdirSync(SESSION_DIR, { recursive: true });
  const { state, saveCreds } = await useMultiFileAuthState(SESSION_DIR);
  const { version } = await fetchLatestBaileysVersion();
  const sock = makeWASocket({
    version, logger,
    auth: { creds: state.creds, keys: makeCacheableSignalKeyStore(state.keys, logger) },
    printQRInTerminal: false, syncFullHistory: false, markOnlineOnConnect: false,
  });
  sock.ev.on('creds.update', saveCreds);
  return sock;
}

async function connectWithRetry(onQR) {
  const { DisconnectReason } = requireOrInstall('@whiskeysockets/baileys');
  const FATAL_CODES = new Set([
    DisconnectReason.loggedOut, DisconnectReason.forbidden,
    DisconnectReason.badSession, DisconnectReason.multideviceMismatch,
  ]);
  while (true) {
    const sock = await makeSocket();
    const result = await new Promise((resolve) => {
      sock.ev.on('connection.update', ({ connection, lastDisconnect, qr }) => {
        if (qr && onQR) onQR(qr);
        if (connection === 'open') { resolve({ status: 'open', sock }); return; }
        if (connection === 'close') {
          const err  = lastDisconnect?.error;
          const code = err?.output?.statusCode;
          const msg  = err?.message || '';
          const fatal = FATAL_CODES.has(code)
            || msg.includes('Connection Failure')
            || msg.includes('Connection Closed');
          if (fatal) resolve({ status: 'fatal', code, msg });
          else { process.stderr.write(`Reconnecting (${code})…\n`); resolve({ status: 'retry' }); }
        }
      });
    });
    if (result.status === 'open')  return result.sock;
    if (result.status === 'fatal') {
      await sock.end().catch(() => {});
      throw new Error(`Session rejected (${result.code}). Run: node whatsapp-cli.js auth`);
    }
    await sock.end().catch(() => {});
  }
}

// ─── auth ─────────────────────────────────────────────────────────────────────

async function cmdAuth() {
  const qrcode = requireOrInstall('qrcode-terminal');
  // Stale creds make Baileys try (and get rejected) with the dead identity
  // before ever reaching the empty-creds path that emits a fresh QR.
  fs.rmSync(SESSION_DIR, { recursive: true, force: true });
  let qrShown = false;
  const sock = await connectWithRetry((qr) => {
    if (!qrShown) {
      process.stderr.write('\nScan this QR code with WhatsApp → Linked Devices → Link a device:\n\n');
      qrShown = true;
    }
    qrcode.generate(qr, { small: true });
    process.stderr.write('\n');
  });
  process.stderr.write('Authenticated! Session saved.\n');
  await sock.end().catch(() => {});
  process.stdout.write(JSON.stringify({ ok: true }) + '\n');
}

// ─── contacts — read directly from WhatsApp's ContactsV2.sqlite ───────────────

function cmdContacts() {
  const rows = sqliteQuery(CONTACTS_DB, `
    SELECT ZFULLNAME, ZGIVENNAME, ZPHONENUMBER, ZWHATSAPPID, ZLOCALIZEDPHONENUMBER
    FROM ZWAADDRESSBOOKCONTACT
    WHERE ZWHATSAPPID IS NOT NULL
      AND ZWHATSAPPID NOT LIKE '%@g.us'
      AND ZFULLNAME IS NOT NULL
      AND ZFULLNAME != ''
    ORDER BY ZFULLNAME COLLATE NOCASE
  `);

  // Last message per chat lives in a *separate* DB, so we can't JOIN; pull a
  // jid → snippet map and merge in JS.  ZISFROMME flags whether I sent it, so
  // the snippet can read "you: …" vs "them: …".
  const EPOCH_OFFSET = 978307200;
  const chats = sqliteQuery(CHAT_DB, `
    SELECT s.ZCONTACTJID AS jid, s.ZLASTMESSAGEDATE AS ts,
           m.ZTEXT AS snippet, m.ZISFROMME AS fromme
    FROM ZWACHATSESSION s
    LEFT JOIN ZWAMESSAGE m ON m.Z_PK = s.ZLASTMESSAGE
    WHERE s.ZCONTACTJID NOT LIKE '%@g.us'
  `);
  // Session jids may be in LID form (`…@lid`); normalise each to the phone-jid
  // (`…@s.whatsapp.net`) that contacts use, so the merge below matches.
  const lastByJid = {};
  for (const c of chats) {
    if (!c.jid) continue;
    // A textless last message (sticker/media/voice) shows as "{media}" rather
    // than an empty string, so the row still reads as a real exchange.
    const body = (c.snippet || '').replace(/\s+/g, ' ').trim() || '{media}';
    lastByJid[resolveLid(c.jid)] = {
      snippet: (c.fromme ? 'you: ' : '') + body,
      ts: c.ts ? new Date((c.ts + EPOCH_OFFSET) * 1000).toISOString() : '',
    };
  }

  const list = rows.map(r => {
    const jid = r.ZWHATSAPPID;
    const last = lastByJid[jid] || lastByJid[resolveLid(jid)] || {};
    return {
      id:    jid,   // stable identity for hearting/dismissal (see `aq--obj-id')
      jid,
      name:  r.ZFULLNAME,
      phone: r.ZLOCALIZEDPHONENUMBER || r.ZPHONENUMBER || jidToPhone(jid),
      snippet:   (last.snippet || '').slice(0, 120),
      timestamp: last.ts || '',
    };
  });

  process.stdout.write(JSON.stringify(list) + '\n');
}

// ─── unread — read from ChatStorage.sqlite + last message text ────────────────

function cmdUnread() {
  // CoreData timestamps are seconds since 2001-01-01 (Mac absolute time).
  // Convert to Unix: add 978307200.
  const EPOCH_OFFSET = 978307200;

  const rows = sqliteQuery(CHAT_DB, `
    SELECT
      s.ZCONTACTJID     AS jid,
      s.ZPARTNERNAME    AS name,
      s.ZUNREADCOUNT    AS count,
      s.ZLASTMESSAGEDATE AS ts,
      m.ZTEXT           AS snippet
    FROM ZWACHATSESSION s
    LEFT JOIN ZWAMESSAGE m ON m.Z_PK = s.ZLASTMESSAGE
    WHERE s.ZUNREADCOUNT > 0
      AND s.ZCONTACTJID NOT LIKE '%@g.us'
      AND s.ZCONTACTJID NOT LIKE '%@status'
      AND s.ZCONTACTJID NOT LIKE '%@newsletter'
    ORDER BY s.ZLASTMESSAGEDATE DESC
    LIMIT 50
  `);

  const list = rows.map(r => {
    const resolvedJid = resolveLid(r.jid);
    const ts = r.ts ? new Date((r.ts + EPOCH_OFFSET) * 1000).toISOString() : '';
    return {
      jid:       resolvedJid,
      name:      r.name || jidToPhone(resolvedJid),
      phone:     jidToPhone(resolvedJid),
      snippet:   (r.snippet || '').slice(0, 120),
      timestamp: ts,
      count:     r.count || 0,
    };
  });

  process.stdout.write(JSON.stringify(list) + '\n');
}

// ─── send / reply ─────────────────────────────────────────────────────────────

async function cmdSend(jid, msg) {
  if (!jid || !msg) die('Usage: send <jid> <message>');
  const sock = await connectWithRetry();
  await sock.sendMessage(jid, { text: msg });
  process.stdout.write(JSON.stringify({ ok: true }) + '\n');
  await sock.end().catch(() => {});
}

// ─── dispatch ─────────────────────────────────────────────────────────────────

const [,, cmd, ...args] = process.argv;

(async () => {
  switch (cmd) {
    case 'auth':     await cmdAuth();                                 break;
    case 'contacts':       cmdContacts();                             break;
    case 'unread':         cmdUnread();                               break;
    case 'send':
    case 'reply':    await cmdSend(args[0], args.slice(1).join(' ')); break;
    default:
      die(`Unknown command: ${cmd || '(none)'}\nUsage: node whatsapp-cli.js auth|contacts|unread|send|reply`);
  }
})().catch(e => die(e.message));
