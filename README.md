# A Haskell + Nix tutorial _for Haskell and Nix noobs (ALPHA)_

NOTE: I'm very, very new to Haskell. It's a feature. 

## Why Not Cabal?

Let's talk about Cabal. Cabal is great and all for a build system but as a package manager it's well.. bad, maybe because [it's not a package mannager](https://ivanmiljenovic.wordpress.com/2010/03/15/repeat-after-me-cabal-is-not-a-package-manager/). Cabal is known for :fire: **Cabal Hell** :fire:  where you get your packages in a state that's broken and very hard to fix manually. This is, to put it lightly, annoying and unbecoming of a package manager written in a language with such a central focus on _purity_ and _correctness_. 

I was quite shocked and annoyed when I went to `cabal install` _the most popular haskell frameworks and libraries_ such as http://www.yesodweb.com/ and they **don't work!**. This is mainly because versions are off and the current versions that you have previously `cabal install`'d don't play nice together (to over simplify it). While this can sort of be fixed by `cabal sandbox` this is less than ideal because it wastes space like crazy, and again doesn't always work. __Nix to the resue!__.

## Why Nix

This is not something I'm going to cover in depth but Nix is better because unlike most package managers that are based around **mutating** a global store of packages that all depend on each other it's based around a declarative file that specifies the dependencies and then builds all the things that are in it and uninstalls all the stuff that is not (cabal doesn't have a way to uninstall packages besides deleting and building again from source). And this is per project if your doing what I'm going to do in this tutorial.

Other (better) arguments:

- https://nixos.org/nix/
- https://ocharles.org.uk/blog/posts/2014-02-04-how-i-develop-with-nixos.html#what-nix-buys-me

# Getting Started
before you get started:
- Install nix -> https://nixos.org/nix/

To make a simple project cabal file:

- `cabal init` And fill out the information that you like (or just clone this repo)
- `$ nix-env -i cabal2nix`  To install the program that will convert cabal files to nix files
- `$ cabal2nix --shell ./. > shell.nix` To generate a usable shell.nix 
- `$ nix-shell --pure` This will put you into a nix-shell with GHC version you spesifed (and anything else in shell.nix) in your path and nothing else.

Let's see how this `nix-shell` thing works:

In my normal terminal I have Ruby in the path (just an example, no Ruby has nothing to do with this tutorial).
``
$ ruby -v
ruby 2.2.2p95 (2015-04-13 revision 50295) [x86_64-linux]

``

however when I run nix-shell with the pure option:

``
$ nix-shell --pure

[nix-shell:~/nix-haskell]$ ruby -v
bash: ruby: command not found...

[nix-shell:~/nix-haskell]$ ghc -v
[nix-shell:~/nix-haskell]$ ghci --version
The Glorious Glasgow Haskell Compilation System, version 7.10.1

``
Ruby isn't found because we used the `--pure` option which strips out our path. However our "Glorious" Haskell compiler is! (because it was in the `shell.nix` file)

**NOTE:** this is _not_ a sandbox. This still lets you do anything to your file system. It simply modifies your "Path" and other env variables.

# Adding a dependency to `lens`

Adding a haskell library dependency is pretty easy just edit your `whatever.cabal` file and add something to the `build-depens` part.

from:
``
  build-depends:       base >=4.8 && <4.9
``
to:
``
  build-depends:       base >=4.8 && <4.9, lens >= 1
``

Now I'm going to add this to a separate `shell.nix` to preserve the old environment.

``
$ cabal2nix --shell ./. > shell2.nix
``

Next we'll run the nix-shell with it:

``
$ nix-shell shell2.nix

... probably a lot of crazy output while it loads things ...

[nix-shell:~/code/haskell/nix-another]$ ghci
Prelude> import Control.Lens
Prelude Control.Lens> 

``

Tada! `lens` is magically in the scope now!

If you run `nix-shell` (which by default will load `shell.nix` and try the same thing you can verify that `lens` is no longer in the scope.

# To be continued...

Things I'm going to write next:

- How to build a project

# Notes:
- NEVER run `cabal install` or anything of the like, that's what Nix is for

# Resources
  Blogs are great and all but the real up to date stuff should be at: https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure

