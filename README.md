<h1 align="center">
    <a href="https://github.com/tfausak/blunt">
        Blunt
    </a>
</h1>

<p align="center">
    Blunt converts between pointfree and pointful Haskell expressions.
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

Blunt is a web front end to the [pointfree][] and [pointful][] libraries. While
you can install and run it locally, there's no real reason to prefer it over
the `pointfree` and `pointful` executables. Instead, use the hosted version:
<https://evening-thicket-5270.herokuapp.com>.

## Install

``` sh
$ cabal update
$ cabal install 'blunt ==0.0.*'
```

## Use

``` sh
$ blunt
# http://localhost:8080

$ env PORT=8888 blunt
# http://localhost:8888
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
$ echo '{}' > package.json
$ echo 'web: ./blunt' > Procfile
$ cp dist/build/blunt/blunt .
$ git add package.json Procfile blunt
$ git commit --allow-empty-message --message ''
$ git push --force heroku deploy:master
```

[pointfree]: http://hackage.haskell.org/package/pointfree
[pointful]: http://hackage.haskell.org/package/pointful
