# boil

This is a script that creates a new haskell project. The new project:

- Uses cabal
- Supports hpack (package.yaml)
- Most language extensions are enabled
- mtl, containers (and friends) are dependencies
- Replaces `Prelude` with a `MyPrelude` sub-project, ~~which is mostly identical to Prelude~~


Unlike stack this supports:

- A custom prelude AND ghci simultaneously
- Backpack
- Nightly/modified versions ghc


Having the latest and greatest everything at my fingertips right now
is way more important to me than having the project still compile if
I come back to it half a year down the line.


### Installation

```bash
git clone https://github.com/BlackCapCoder/boil
ln -s boil/build SOME_LOCATION_THAT_IS_IN_YOUR_PATH/boil
```

### Create a new project

```bash
boil nameofproject
cd nameofproject
```

### Build

```bash
hpack; cabal new-build --allow-newer
```


### Repl

```bash
cabal new-repl --allow-newer
```


## Feature creep

Unlike most of my projects I ended up actually using this one!

I try to keep my Prelude lightweight and tidy, yet things
that are more generally useful, or that I find myself reimplementing
often, somehow end up in my Prelude!

Things that get in the way later I throw back out, or just
not exported by default.

