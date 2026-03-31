# `yesod-vite`

Yesod 💜 Vite.


This package provides an implement for the `yesod` web framework to connect with vite.
Vite is a modern build tool used across stacks.

## Quick Start
The assumption here is that your setup is similar to the default Yesod scaffold.
You will need to have installed both `yesod-static` and `yesod-vite` for this to work.
In addition, you will need the static subsite configured, as the route constructor is required.
- Run `npm init -y` or the equivalent for your build tool.
- Run `npm i -D vite` or the equivalent for your build tool.
- Create a `vite.config.js` to include the following:
```javascript
import { defineConfig } from "vite";
import * as path from "node:path";
export default defineConfig({
  plugins: [],
  base: "/static/",
  build: {
    outDir: path.resolve("./static"),
    manifest: "manifest.json",
    rollupOptions: {
      input: path.resolve("./assets/src/app.js"),
    },
  },
});
```
- Install and configure "Yesod.Static" for your application.
- Setup 'YesodVite' as the following:
```haskell
instance YesodVite App where
  viteBuildDir :: app -> IO FilePath
  vitebuildDir app = return $ appStaticDir app
  viteInDev :: app -> IO Bool
  viteinDev app = return . const True
  viteRoute :: Route Static -> Route site
  viteRoute = StaticR
```
- Add the `viteAsset \<asset-name\>` to your default layout.
