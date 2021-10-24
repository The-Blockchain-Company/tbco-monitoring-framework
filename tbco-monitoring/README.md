# tbco-monitoring-framework

[![Build status](https://badge.buildkite.com/1cc7939a1fed4972c15b8f87d510e0404b0eb65d73cfd1e30b.svg?branch=master)](https://buildkite.com/The-Blockchain-Company/tbco-monitoring-framework)
[![Coverage Status](https://coveralls.io/repos/github/The-Blockchain-Company/tbco-monitoring-framework/badge.svg?branch=master)](https://coveralls.io/github/The-Blockchain-Company/tbco-monitoring-framework?branch=master)

This framework provides logging, benchmarking and monitoring.

## documentation

Documentation of the [source code and tests](docs/The Blockchain Co.-Monitoring.pdf) in PDF format. Please, download the PDF file and open in external viewer. It contains links that make it easy to navigate in the source code. Those links are not active in the online viewer.

Presentations and more documentation is available from our [docs](https://The-Blockchain-Company.github.io/tbco-monitoring-framework/) section.

## module dependencies

![Overview of modules](docs/OverviewModules.png)

## building and testing

`cabal new-build tbco-monitoring`

`cabal new-test pkg:tbco-monitoring:tests`

## examples
https://github.com/The-Blockchain-Company/tbco-monitoring-framework/edit/master/README.md
Some examples are available in the directory `examples`:
* `simple`  -  run with `cabal new-run example-simple`
* `complex`  -  run with `cabal new-run example-complex`
* `performance` - run with `cabal new-run example-performance`

These showcase the usage of this framework in an application. The *complex* example includes `EKGView` (http://localhost:12789) and the configuration editor (http://localhost:13789).

![Edit runtime configuration](docs/ConfigEditor.png)

## development

* `cabal new-build` and `cabal new-test`
* `ghcid -c "cabal new-repl"` watches for file changes and recompiles them immediately
* `liquid --ghc-option=-XOverloadedStrings --prune-unsorted src/Bcc/BM/*.lhs` verify top modules in tbco-monitoring using LiquidHaskell
