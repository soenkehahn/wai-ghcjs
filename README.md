# wai-shake

`wai-shake` provides a command-line tool `serve-ghcjs` that starts a http
server that compiles Haskell files to javascript on the fly using `ghcjs`. This
is meant to enable a workflow -- known from interpreted languages -- that
consists roughly of:

- Modifying a source file,
- saving the file,
- reloading the page in the browser.

`serve-ghcjs` will

- serve the web app (`index.html` and corresponding javascript files) created by
  `ghcjs`,
- recompile the Haskell source files when they changed on disk,
- in case of compilation errors, show the error messages both in the html page
  and on the javascript console.

Example usage:

``` shell
serve-ghcjs --port 8080 --main-is src/Main.hs -isrc
```

Then point your browser to `http://localhost:8080`.

## Status

This is an experimental project and has lots of room for improvement.

- Recompilation could be triggered through inotify to speed things up.
- `wai-shake` already contains a library which should be used to integrate
  `ghcjs` compiled javascript in bigger wai applications. I haven't used it like
  this though.
- `wai-shake` should allow to use custom shake rulesets (e.g. for purescript,
  typescript, minifying, etc.)
- `wai-shake` could have a production mode, where it performs the build steps at
  application startup once.
- travis could execute the test-suite. (Currently the test-suite depends on
  `ghcjs` in the `$PATH`, but that takes very long to install. Don't have a good
  idea how to deal with this.)
