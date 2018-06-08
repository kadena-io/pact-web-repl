# pact-web-repl
A web-based REPL for the Pact smart contract language

Building
---

1. Go to https://nixos.org/nix/, click "Get Nix", follow the instructions to install the Nix package manager
2. Edit `$NIX_CONF_DIR/nix.conf`
3. Set the `binary-caches` and `binary-cache-public-keys` lines as follows:

```
binary-caches = https://pact.cachix.org https://nixcache.reflex-frp.org https://cache.nixos.org/
binary-cache-public-keys = pact.cachix.org-1:cg1bsryGrHnQzqEp52NcHq4mBBL+R25XbR2Q/I/vQ8Y= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

4. Restart the nix daemon.

On mac:

```
sudo launchctl stop org.nixos.nix-daemon
sudo launchctl start org.nixos.nix-daemon
```

On linux:

```
sudo systemctl restart nix-daemon.service
```

5. Run `nix-build` from the project root
