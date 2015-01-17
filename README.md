# xmonad-entryhelper

[![Build Status](https://travis-ci.org/Javran/xmonad-entryhelper.svg?branch=master)](https://travis-ci.org/Javran/xmonad-entryhelper)

[![Version](https://img.shields.io/hackage/v/xmonad-entryhelper.svg)](https://hackage.haskell.org/package/xmonad-entryhelper)

xmonad-entryhelper makes your compiled XMonad config a standalone binary.

It simulates the XMonad's argument handling
and supports customized compliation.

**Table of Contents**

- [Introduction](#introduction)
- [Simple Setup](#simple-setup)
- [Advanced features](#advanced-features)
- [Feedback](#feedback)

## Introduction

xmonad-entryhelper frees you from keeping a xmonad library as a system- or user- level dependency.
Instead, you can keep your XMonad configurations either as a local
[cabal project](https://www.haskell.org/cabal/) using
[cabal sandbox](https://www.fpcomplete.com/school/to-infinity-and-beyond/older-but-still-interesting/an-introduction-to-cabal-sandboxes-copy) or within
a protected environment like those created by [hsenv](https://github.com/tmhedberg/hsenv)

## Simple Setup

1. After installation, modify your `xmonad.hs` accordingly:

    Your xmonad config might look like:

        -- some imports here
        import ...

        ...

        -- your main entry point
        main :: IO ()
        main = _

    Rename your `main` to something else, import `withHelper` from `XMonad.Util.EntryHelper`
    and use it to call your old `main`:

        -- some imports here
        import ...
        import XMonad.Util.EntryHelper (withHelper)

        ...

        -- your old main entry point
        oldMain :: IO ()
        oldMain = _

        -- your new main entry point
        main :: IO ()
        main = withHelper oldMain

2. Finally you need to have a writable local `PATH` directory.

    For example you can make directory `$HOME/bin`:

        mkdir -p ~/bin

    And append the following lines in your shell profile file
    (it's usually your `~/.bash_profile` file):

        ...
        # my local executable files
        export PATH="${HOME}/bin:${PATH}"

    Create soft link to your compiled `xmonad` binary:

        # the binary name varies depending on your OS and architecture,
        # check your ~/.xmonad/ directory to find out
        $ ln -s ~/.xmonad/xmonad-x86_64-linux ~/bin/xmonad

    And verify if `xmonad` is now leading to your compiled xmonad config:

        $ source ~/.profile
        $ which xmonad
        /home/username/bin/xmonad

    If this doesn't work, check articles like
    [Zsh/Bash startup files loading order](https://shreevatsa.wordpress.com/2008/03/30/zshbash-startup-files-loading-order-bashrc-zshrc-etc/)
    for troubleshooting.

3. Now you are free to remove XMonad from your system- or user- level packages.
Because your compiled XMonad will work on its own:

        $ xmonad --help
        xmonad-entryhelper - XMonad config entry point wrapper

        Usage: xmonad [OPTION]
        Options:
          --help                       Print this message
          --version                    Print XMonad's version number
          --recompile                  Recompile XMonad
          --replace                    Replace the running window manager with XMonad
          --restart                    Request a running XMonad process to restart

## Advanced features

* Customized shell command compilation

* Customized compilation and post-compilation handling

* Parallel compilation protection

* Sending restart request to current xmonad instance

## Feedback

Feel free to open issues for either bug report, enhancement or discussion.
