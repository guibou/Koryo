This is an implementation of the card game Koryo.

# Development

# Direnv setup (optional)

Install `direnv` and `cd` in this repository, it will automatically load the dev environment for incremental work, with haskell language server configured.

# Start the nix-shell

You can either use:

- `nix-shell ./ -A shell` to get a light weight developement shell with `cabal`.
- `nix-shell ./ -A shell_hls` to get a huge developement shell with `cabal` and HLS.

`direnv` loads `shell_hls` by default.

`cabal repl` gives a shell with mostly everything you need for developement.

## Run the server

```
$ cabal rep/nix/store/5l35zwr49bf17lw46r72cdyjlddkyz74-Koryo-0.1.0.0l
> :l Server
> startServer ["Alice", "Bob", "Abigael", "Guillaume"]
```

Will start a websocket server with four players.

## Run the UI

Inside `cabal repl`, load the runners `:l UIReflex.Runner` and then you can do the following:

- `runUI Nothing` will run the UI on `http://localhost:3003/` with no pre-connected user
- `runUI (Just "Guillaume")` same, but with `Guillaume` user pre connected.
- `runUIDeveloper ["Guillaume", "Alice", "Bob", "Abigael"]` runs the UI in developer mode. All the listed players are displayed simultaneously.

# Release

The Javascript version is compiled with GHCJS and uses the reflex platform.

The build is slow and fetchs a lot of dependencies from the reflex-frp cache. See the documentation in https://github.com/reflex-frp/reflex-platform in order to configure the cache.

The javascript version can be built using:

```
$ nix-build -A runKoryo
/nix/store/d3zylygm9awwh4l8icqy44s3hwbzvypc-run-koryo
```

This is a script which will start http server for the UI and serve it on `localhost:3003`.

The backend server is built by `nix-build -A server` or run using `cabal repl` as depiced in the previous section.
