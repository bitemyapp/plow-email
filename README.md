[![Circle CI](https://circleci.com/gh/plow-technologies/plow-email.svg?style=shield)](https://circleci.com/gh/plow-technologies/plow-email)
## Deployment

Pushing to the master branch causes circleci to build the new binary for plowemail, if it passes, it then sends it to staging, (it currently does not restart the process, because we aren't using it at the moment) and makes a copy with the date to the release history folder on that server.

Pushing to the production branch causes circleci to build the new binary for plowemail, if it passes, it then sends it to production, restarts the process and makes a copy with the date to the release history folder on that server.

hi-hspec
=================

A template for [hi](https://github.com/fujimura/hi).

example:

```
$ hi -m Foo.Bar -p foo-bar  -r git@github.com:fujimura/hi-hspec.git
$ tree
.
├── LICENSE
├── README.md
├── foo-bar.cabal
├── src
│   └── Foo
│       ├── Bar
│       │   └── Internal.hs
│       └── Bar.hs
└── test
    ├── Foo
    │   └── BarSpec.hs
    └── Spec.hs

5 directories, 7 files
```

Copyright 2013-2014 Fujimura Daisuke, under the MIT license.
