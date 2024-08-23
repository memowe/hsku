# hsku - Haskell Haiku detection (German)

## Build and test

You need a [modern GHC (>= 9) and cabal][ghcup].

```
$ cabal build all
$ cabal test
```

[ghcup]: https://www.haskell.org/ghcup/

## Library usage example

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

## Web service

```
$ cabal run hsku-webservice
```
Haiku parsing with JSON results:
```
$ curl http://localhost:8080/haiku?input=42
{"result":null}
$ curl  --get \
        --data-urlencode "input=Decken auf dem Gras, eine Nacht lang ohne Haus - reich nur durch den Mond" \
        http://localhost:8080/haiku
{"result":["Decken auf dem Gras,","eine Nacht lang ohne Haus -","reich nur durch den Mond"]}
```

## Simple JSON CLI

```
$ cabal run hsku-cli
42
{"result":null}
$ cabal run hsku-cli
Decken auf dem Gras, eine Nacht lang ohne Haus - reich nur durch den Mond.
{"result":["Decken auf dem Gras,","eine Nacht lang ohne Haus -","reich nur durch den Mond."]}
```

## Author and license

(c) 2024 Mirko Westermeier

Licensed under the MIT license (see [LICENSE][license]) for details.

[license]: LICENSE
