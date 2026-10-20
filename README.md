# chicken-minecraft-server

A minimal Minecraft server (Java Edition 1.8.x, protocol 47) written in [CHICKEN Scheme](https://call-cc.org).

## Features
- Server list ping (MOTD, player count)
- Offline-mode login
- Flat 7×7-chunk world, creative mode
- Multiple players, shared chat, join/leave messages
- Keep-alive

Not implemented: seeing other players, block changes, mobs, encryption.

## Build & run
Requires CHICKEN 5 and the `srfi-18` egg:

```
chicken-install srfi-18
csc -O2 server.scm
./server
```

Then connect with Minecraft 1.8.9 to `localhost` (port 25565).
On Windows, keep `libchicken.dll` next to `server.exe`.
