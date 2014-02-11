Tabula is a tool for recording and analysing command line sessions. 

At a basic level, it's quite similar to the Unix 'script' tool, but rather than just recording everything that's typed or displayed, tabula tries to capture much more meaningful information about the actions that are being performed. It explicitly captures commands that are run and whether they were successful or not, the directory and environment they were run in, and the input and output consumed and produced.

Tabula reports either locally to a log file or centrally to a redis server. Using redis allows sessions to be accessed concurrently from multiple locations, and lets multiple people collaborate in a single session.

Output is produced in JSON format for easy analysis.

# Usage

To start a basic recorded shell session, simply type `tabula start`. This will record to a session called default stores on your local machine at `$HOME/.tabula/projects`. Ctrl-d will terminate the recorded session and return you to your regular terminal.

You can connect to a different data store using the `-d` option, or specify the name of a different session by adding it as an argument. For example, to work on a project called 'homework' and write to a file in the current directory, you could do:

    tabula start -d file://`pwd` homework

Currently, tabula recognises two protocols for storage: `file://`, which stores records on the local filesystem, and `redis://`, which connects to a redis server. For example, if you have a redis server running on localhost then you can connect to it using:

    tabula start -d redis://localhost:6379

At present, tabula does not support authenticated connections to redis servers.

To review an existing session, you can use `tabula cat` to print out the session. `cat` admits both the `-d` option and the project name in the same was as `start`. It also allows you to print only the commands invoked as if recorded by bash history - to do this, use the `--as-history` option.

To view all sessions available at a destination, you may use `tabula ls`.

```
tabula - a utility for recording shell sessions.

Usage: tabula COMMAND [-V|--verbosity LEVEL] [-q|--quiet]
  Open a recorded shell session for a specific project.

Available options:
  --version                Print version information
  -h,--help                Show this help text
  -V,--verbosity LEVEL     Set logging level (default ERROR)
  -q,--quiet               Disable logging

Available commands:
  start                    Start or resume a project session.
  cat                      Print a project session to stdout.
  ls                       List all projects created at a destination.
```

# Building from source

Prerequisites:
The [Glasgow Haskell Compiler](https://www.haskell.org/ghc/)
[Cabal](http://www.haskell.org/cabal/)

The easiest way to build is in a cabal sandbox. Most dependencies are available from Hackage, but Tabula requires Vinyl >= 0.2.1, which has not yet been released (as of 2014-02-11) and must be built locally.

```
git clone git@github.com:VinylRecords/Vinyl.git
git clone git@github.com:nc6/tabula.git
cd tabula
cabal sandbox init
cabal sandbox add-source ../Vinyl
cabal update
cabal install --dependencies-only
cabal configure
cabal build
```

This will generate the tabula executable in dist/build/tabula/tabula.

# Bash completions
Tabula comes (courtesy of [optparse-applicative](http://hackage.haskell.org/package/optparse-applicative)) with the option to generate automatic bash completions. The easiest way to get access to these is the following:

    source <(tabula --bash-completion-script `which tabula`)
