Problems like:
- vscode cannot find packages
- build fails
might be caused by the new PnP used by yarn2, which packages no longed downloaded into `node_modules`.
to change this default in yarn2, add
```yaml
nodeLinker: node-modules
```
to `.yarnrc.yml`
