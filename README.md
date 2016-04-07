# [Blunt][]

[![Build badge][]][build]

Blunt converts between pointfree and pointful Haskell expressions. It is a web
front end to the [pointfree][] and [pointful][] libraries. While you can
install and run it locally, there's no real reason to prefer it over the
`pointfree` and `pointful` executables.

-   [Install](#install)
-   [Run](#run)
-   [Configure](#configure)

## Install

1.  Install [Stack][].

2.  `stack build`

## Run

1.  `stack exec blunt`

## Configure

Name                | Default
---                 | ---
HONEYBADGER_API_KEY | -
LOGS_PRIORITY       | `NOTICE`
METRICS_HOST        | `127.0.0.1`
METRICS_PORT        | `8081`
SERVER_HOST         | `127.0.0.1`
SERVER_PORT         | `8080`

[Blunt]: https://github.com/tfausak/blunt
[Build badge]: https://travis-ci.org/tfausak/blunt.svg?branch=master
[build]: https://travis-ci.org/tfausak/blunt
[pointfree]: https://hackage.haskell.org/package/pointfree
[pointful]: https://hackage.haskell.org/package/pointful
[Stack]: http://docs.haskellstack.org/en/stable/README/
