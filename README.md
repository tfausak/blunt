<h1 align="center">
    <a href="https://github.com/tfausak/blunt">
        Blunt
    </a>
</h1>

<p align="center">
    Blunt converts between pointfree and pointful Haskell expressions.
</p>

<p align="center">
    <a href="https://hackage.haskell.org/package/blunt"><img alt="Version" src="https://img.shields.io/hackage/v/blunt.svg?label=version&style=flat-square"></a>
    <a href="http://packdeps.haskellers.com/feed?needle=blunt"><img alt="Dependencies" src="https://img.shields.io/hackage-deps/v/blunt.svg?label=dependencies&style=flat-square"></a>
</p>

<hr>

Blunt is a web front end to the [pointfree][] and [pointful][] libraries. While
you can install and run it locally, there's no real reason to prefer it over
the `pointfree` and `pointful` executables. Instead, use the hosted version:
<https://blunt.herokuapp.com>.

## Install

``` sh
$ cabal update
$ cabal install 'blunt ==1.*'
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
# Create a new app on Heroku using the Haskell on Heroku buildpack.
$ heroku apps:create --buildpack https://github.com/mietek/haskell-on-heroku

# Let Halcyon know that we need happy installed.
$ heroku config:set HALCYON_SANDBOX_EXTRA_APPS='happy'

# Configure AWS S3.
$ heroku config:set HALCYON_AWS_ACCESS_KEY_ID='...'
$ heroku config:set HALCYON_AWS_SECRET_ACCESS_KEY='...'
$ heroku config:set HALCYON_S3_BUCKET='...'

# Push the code up to Heroku. Note that this build is expected to fail.
$ git push heroku master

# Build the app on a PX dyno.
$ heroku run --size PX build

# Force Heroku to rebuild the app using the cache built in the last step.
$ git commit --amend --no-edit
$ git push --force heroku master

# Scale up a web dyno to serve requests.
$ heroku ps:scale web=1
```

[pointfree]: http://hackage.haskell.org/package/pointfree
[pointful]: http://hackage.haskell.org/package/pointful
