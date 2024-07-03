# hsku - Haskell Haiku detection (German)

## Build and test

You need a [modern GHC and cabal][ghcup].

```
$ cabal build all
$ cabal test
```

[ghcup]: https://www.haskell.org/ghcup/

## Usage example

```
$ cabal repl
λ :m + HsKu.Load Data.Maybe
λ langs <- loadLanguages
λ (lang, haiku) = fromJust $ parseHaiku langs "Decken auf dem Gras, eine Nacht lang ohne Haus - reich nur durch den Mond."
λ name lang
"Deutsch"
λ haiku
("Decken auf dem Gras,","eine Nacht lang ohne Haus -","reich nur durch den Mond.")
```

## Author and license

(c) 2024 Mirko Westermeier

Licensed under the MIT license (see [LICENSE][license]) for details.

[license]: LICENSE
