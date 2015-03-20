<h1 align="center">
    <a href="https://github.com/tfausak/blunt">
        Blunt
    </a>
</h1>

<p align="center">
    Blunt makes Haskell expressions pointfree.
</p>

<p align="center">
    <a href="https://hackage.haskell.org/package/blunt">
        <img alt="" src="https://img.shields.io/hackage/v/blunt.svg">
    </a>
    <a href="http://packdeps.haskellers.com/feed?needle=blunt">
        <img alt="" src="https://img.shields.io/hackage-deps/v/blunt.svg">
    </a>
</p>

<hr>

## Install

``` sh
$ cabal update
$ cabal install 'blunt ==0.0.*'
```

## Develop

``` sh
$ git clone https://github.com/tfausak/blunt
$ cd blunt
$ cabal sandbox init
$ cabal install happy
$ cabal install
```

## Deploy

``` sh
$ heroku create
$ git checkout -b deploy
$ echo 'web: ./blunt' > Procfile
$ cp dist/build/blunt/blunt .
$ touch requirements.txt
$ git add Procfile blunt requirements.txt
$ git commit -m v0.0.7
$ git push heroku deploy:master
```
