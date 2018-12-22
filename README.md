Deployed [here](http://ac.rgpu.club/).

# Building

Make sure `pulp` binary is in `$PATH` or modify the `package.json` to use the one from `node_modules`.

```bash
npm install
./node_modules/purescript-spago/spago psc-package-insdhall
psc-package install
npm test # optional
npm run build
```
