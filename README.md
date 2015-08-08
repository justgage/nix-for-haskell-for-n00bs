# A Haskell + Nix tutorials for Haskell and Nix noobs (ALPHA)

NOTE: I'm very, very new to Haskell. It's a feature.

## Why?

Let's talk about Cabal. Cabal is great and all for a build system but as a package manager it's well.. bad. Being known for :fire **Cabal Hell** :fire:  where you get your packages in a state that's broken is to put it lightly, annoying and unbecoming of a package manager written in a language with such a central focus on . I was quite shocked and annoyed when I went to install _the most popular haskell frameworks and libraries_ such as http://www.yesodweb.com/ and others that they **wouldn't work!**. This is mainly because versions are off and the current versions that you have previously `cabal install` 'd don't play nice together. 

# Getting Started
before you get started:
- Install nix -> https://nixos.org/nix/

- `cabal init` And fill out the information that you like (or just clone this repo)
- `$ nix-env -i cabal2nix`  (if you don't already have it installed)
- `$ cabal2nix`  (if you don't already have it installed)
- `$ cabal2nix --shell ./. > shell.nix` To generate a usable shell.nix 
- `$ nix-shell --pure` This will put you into a nix-shell with GCH (and anything else in shell.nix) in your path and nothing else.

Example: 
``
$ ruby -v
ruby 2.2.2p95 (2015-04-13 revision 50295) [x86_64-linux]

``
In my normal terminal I have ruby in the path (just an example, no ruby has nothing to do with this tutorial) as you can see. But when we do:

``
$ nix-shell --pure

[nix-shell:~/nix-haskell]$ ruby -v
bash: ruby: command not found...

[nix-shell:~/nix-haskell]$ ghc -v
[nix-shell:~/nix-haskell]$ ghci --version
The Glorious Glasgow Haskell Compilation System, version 7.10.1

``

The ruby isn't found because we used the `--pure` option which strips out our path. However our "Glorious" Haskell compiler is!

**NOTE:** this is _not_ a sandbox. This still lets you do anything to your file system. It simply modifies your "Path" and other env variables.

# Adding a dependency

Adding a dependency is pretty easy just edit your `whatever.cabal` file and add something to the `build-depens`

for example: 

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

[nix-shell:~/code/haskell/nix-another]$ ghci
Prelude> import Control.Lens
Prelude Control.Lens> 

``

Tada! `lens` is magically in the scope now!

If you run `nix-shell` (which by default will load `shell.nix` and try the same thing you can verify that `lens` is no longer in the scope.

# Notes:
- NEVER run `cabal install` or anything of the like, that's what Nix is for

# Resources
  Blogs are great and all but the real up to date stuff should be at: https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure

