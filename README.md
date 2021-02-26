This is an implementation of the card game Koryo.

# Development

I use `direnv` in order to setup the reflex dependencies using nix. So install it, and move to the working directory.

`cabal repl` gives a shell with mostly everything you need for developement.

## Run the server

```
$ cabal repl
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

The javascript version can be built using:

```
$ nix-build ./deploy.nix -A runKoryo
/nix/store/d3zylygm9awwh4l8icqy44s3hwbzvypc-run-koryo
```

This is a script which will start http server for the UI and serve it on `localhost:3003`.

The backend server is built by `nix-build ./deploy.nix -A server`.
