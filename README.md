[Snap-Routes](https://ajnsit.github.io/snap-routes) [![Hackage](https://img.shields.io/badge/hackage-v0.0.1-brightgreen.svg)](https://hackage.haskell.org/package/snap-routes) [![Hackage-Deps](https://img.shields.io/hackage-deps/v/snap-routes.svg)](http://packdeps.haskellers.com/feed?needle=snap-routes) [![Build Status](https://img.shields.io/travis/ajnsit/snap-routes.svg)](https://travis-ci.org/ajnsit/snap-routes)
===========

Snap-routes provides typesafe routing for snap applications.

Features
========

Snap-routes adds the following features on top of Snap -

  - Typesafe URLs, including automatic boilerplate generation using TH. Including features such as -
    - Nested Routes
    - Subsites
    - Route Annotations
  - Seamlessly mix and match "unrouted" request handlers with typesafe routing.
  - Sitewide Master data which is passed to all handlers and can be used for persistent data (like DB connections)


Changelog
=========

* 0.0.1   : Intial release
