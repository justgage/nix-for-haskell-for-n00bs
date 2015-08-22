# A Haskell + Nix tutorial _for Haskell and Nix noobs (ALPHA)_

NOTE: I'm very, very new to Haskell. It's a feature. 

## Why Not Cabal?

__disclaimer__: I'm new, don't understand all the issues of why cabal-install doesn't work. These are just my experiences.

Let's talk about Cabal. Cabal is great for a build system but `cabal-install` as a package manager it's got issues. `cabal-install` is known for :fire: **Cabal Hell** :fire:  where you get your packages in a state that's broken and very hard to fix. This is further made worse by the fact that __there's no way to uninstall packages.__ Not to mention that you have to wait for all the dependencies to build just to see if it is even going to work at all. This is quite unbecoming of a package manager written in a language with such a central focus on _purity_ and _correctness_.

I was quite shocked and annoyed when I went to `cabal install` _the most popular haskell frameworks_ such as <http://www.yesodweb.com/> and Snap even libraries like `wreq` **don't work!**. This is mainly because their dependencies' versions are incompatable with the current versions that you have previously `cabal install`'d. This is because `cabal install` is **global** to all projects. While this can be helped by `cabal sandbox` this is less than ideal because it wastes space like crazy, takes forever because it builds everything for every new project, and again __doesn't always work.__ Stack is pretty good as well but is based around a caball install and thus has a lot of the same inherit problems.

Nix to the rescue!

## Why Nix?

This is not something I'm going to cover in depth but Nix is better because unlike most package managers that are based around **mutating** a global store of packages that all depend on each other it's based around a declarative file that specifies the dependencies and then builds all the things that are in it _and uninstalls all the stuff that is not_ (remember cabal doesn't have a way to uninstall packages besides deleting and building again from source). And this is _per project_ if you prefer *but* it will share the compiled binaries between identical dependencies.

Other (better) arguments:

- https://nixos.org/nix/
- https://ocharles.org.uk/blog/posts/2014-02-04-how-i-develop-with-nixos.html#what-nix-buys-me

# Getting Started

## 0. Nuke your cabal packages
If you already have cabal installed and have tried to do `cabal install foo` then do the next step:

Nuke your local package database:

```
rm -r ~/.ghc/
```

Nuke your cabal sandboxes (if you have one in your current project)
```
cabal sandbox delete
```

Take a moment to enjoy the feeling of _Zen._

## 1. Install Nix

- Install nix -> https://nixos.org/nix/
- Install Cabal globaly with nix (anothe version of cabal might be ok) `$ nix-env -f "<nixpkgs>" -iA haskellPackages.cabal-install`

## 2. Setting up a simple project

```bash
cabal init                      # And fill out the information that you like (or possibly just clone this repo)
cabal2nix --shell . > shell.nix # To generate a nix version of your dependencies
nix-shell                       # This will put you into a nix-shell with GHC version you spesifed in your path
```

### nix-shell in a nutshell
Let's see how this `nix-shell` thing works:

In my normal terminal I have Ruby in the path (just an example, no Ruby has nothing to do with this tutorial).
```
$ ruby -v
ruby 2.2.2p95 (2015-04-13 revision 50295) [x86_64-linux]

```

However when I run nix-shell with the pure option:

```
$ nix-shell --pure

[nix-shell:~/nix-haskell]$ ruby -v
bash: ruby: command not found...

[nix-shell:~/nix-haskell]$ ghc -v
[nix-shell:~/nix-haskell]$ ghci --version
The Glorious Glasgow Haskell Compilation System, version 7.10.1
```
Ruby isn't found because we used the `--pure` option which strips out our path. However our "Glorious" Haskell compiler is! (because it was in the `shell.nix` file)

**NOTE:** this is _not_ a sandbox. This still lets you do anything to your file system. It simply modifies your "Path" and other env variables.
**NOTE:** you can't use the pure option in building your project unless you add cabal to your `shell.nix` which I currently don't know how to do


## 3. Adding a dependencies

Adding a Haskell library dependency is pretty easy just edit your `whatever.cabal` file and add something to the `build-depends` part __which you'll probably have to add__. I usually put it somewhere after the `executable your-project-name`

from:
```cabal
executable first-haskell-nix
-- ... 
  build-depends:       base >=4.8 && <4.9
```
to:
```cabal
executable first-haskell-nix
-- ... 
  build-depends:       base >=4.8 && <4.9, lens >= 0
```
I made my a generic >= 0 just to get the newest version.

Now I'm going to add this to a `shell2.nix` to preserve the old `shell.nix` 

```
$ cabal2nix --shell ./. > shell2.nix
```

Next we'll run the nix-shell with it:

```
$ nix-shell shell2.nix

... probably a lot of crazy output while it loads things ...

[nix-shell:~/code/haskell/nix-another]$ ghci
Prelude> import Control.Lens
Prelude Control.Lens> 

```

Tada! `lens` is magically in the scope now!

If you run `nix-shell` (which by default will load `shell.nix` and try the same thing you can verify that `lens` is no longer in the scope.

## 4. Building the project

I'm going to add a dependency to `wreq` which is a nice little HTTP client for interacting with things like Twitter or any RESTfull api.

```
  build-depends:       base >=4.8 && <4.9,
                       lens >= 0,
                       wreq == 0.4.0.0
```

I'm using the latest I found on the [hackage repo](https://hackage.haskell.org/package/wreq).


We'll do the dance to regenerate the `shell.nix`, I'm going to call mine `shell-wreq.nix`.

```
cabal2nix --shell > shell-wreq.nix
```

I'm going to change `Main.hs` to:

(Don't get caught up understanding this file, it's just an example.)

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Network.Wreq
import qualified Network.Wreq.Session as S

main :: IO ()
main = S.withSession $ \sess -> do
  -- First request: tell the server to set a cookie
  S.get sess "http://httpbin.org/cookies/set?name=hi"

  -- Second request: the cookie should still be set afterwards.
  r <- S.post sess "http://httpbin.org/post" ["a" := (3 :: Int)]
  print $ r ^. responseCookie "name" . cookieValue
```

Next we'll build it.

```
$ nix-shell shell-wreq.nix

.... lots of downloading ....

[nix-shell:~/code/haskell/nix-for-haskell-for-n00bs]$ cabal configure

[nix-shell:~/code/haskell/nix-for-haskell-for-n00bs]$ cabal build
./first-haskell-nix.cabal has been changed. Re-configuring with most recently
used options. If this fails, please run configure manually.
Warning: The package list for 'hackage.haskell.org' is 19.2 days old.
Run 'cabal update' to get the latest list of available packages.
Resolving dependencies...
Configuring first-haskell-nix-0.1.0.0...
Warning: The 'license-file' field refers to the file 'LICENSE' which does not
exist.
Building first-haskell-nix-0.1.0.0...
Preprocessing executable 'first-haskell-nix' for first-haskell-nix-0.1.0.0...
[1 of 1] Compiling Main             ( Main.hs, dist/build/first-haskell-nix/first-haskell-nix-tmp/Main.o )
Linking dist/build/first-haskell-nix/first-haskell-nix ...
```

Now the binary will be deep inside of your dist folder. You can run it like so:

```
$ ./dist/build/first-haskell-nix/first-haskell-nix
"hi"
```

Tada! We got back the response we expected!

## FIN. Any Questions?

Please anything who can't get this working please make an issue on this repo and I'll do what I can to fix it.

# Pitfalls
- NEVER run `cabal install` or anything of the like, that's what Nix is going to handle that portion of it and if you do you'll get a weird path.

# Resources
  - Blog are great but they get out of date really quick (because nix is being developed so fast!) so to find the latest info look at: https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure However this is so hard to read I really couldn't get it figured out, hence the tutorial.
  - Cabal2Nix tutorial, more in depth info that this this guide offers https://github.com/NixOS/cabal2nix/blob/master/doc/user-guide.md


