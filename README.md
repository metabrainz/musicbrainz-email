# musicbrainz-email [![Build Status](https://travis-ci.org/metabrainz/musicbrainz-email.svg?branch=master)](https://travis-ci.org/metabrainz/musicbrainz-email)

The `musicbrainz-email` consists of a library and two executables:

- `musicbrainz-emailer` is a long running process that listens on a message
  queue for email commands, and interprets these commands in order to send an
  actual email.

- `enqueue-mail` is a command line script which allows for email to be
  enqueued for `musicbrainz-emailer` to later dispatch. This is useful for
  email that needs to be sent via human interaction, not automatically.

- The `musicbrainz-emailer` library is a little bit of commonality that both
  of these executables depend on.


## Preliminary: Install dependencies

Before you can build or install `musicbrainz-email`, you will need to
install dependencies.

    cabal install --only-dependencies .

If you will be running tests, then you should instead run

    cabal install --enable-tests --only-dependencies .


## Installing

`musicbrainz-email` is packaged using Cabal, and requires GHC 7.4 (or
higher). We recommend installing the 2012.2.0.0 version of the [Haskell
Platform](http://haskell.org/platform), as this provides a lot of dependencies
this project uses; however, with an up to date `cabal-install` and the
aforementioned GHC version satisfied you should also be able to build this
project.

Once you have met those requirements, you can now run:

    cabal install

This project also depends on some other services running:

- A PostgreSQL database, setup to run [`musicbrainz-server`](http://github.com/metabrainz/musicbrainz-server)
- [RabbitMQ](http://rabbitmq.com)
- Something providing `/usr/bin/sendmail`


## Building

To build, you first need to configure the package. If you wish to simply build,
use:

    cabal configure

Whereas if you also wish to run tests, use:

    cabal configure --enable-tests

You can now build the package with:

    cabal build

If you later change the `.cabal` file (for example, exporting more modules or
adding dependencies) then Cabal should be clever enough to reconfigure with the
same options for you. If you get stuck, you can use cabal clean and re-run these
steps.


## Running tests

To run tests you will need to have a MusicBrainz database available that you
wish to run tests on. The `script/create_test_db.sh` script in
`musicbrainz-server` will provide this for you.

You will also need a `/test/email` virtual host in your RabbitMQ server. This
can be added with:

    sudo -u rabbitmq rabbitmqctl add_vhost '/test/email'

Also, be sure to make sure your user has permissions to use this vhost! For
example,

    sudo -u rabbitmq rabbitmqctl set_permissions -p '/test/email' guest '.*' '.*' '.*'

Now copy `test.cfg.example` to `test.cfg` and edit to match your test
environment.

We use Cabal to run tests. To run all tests, run

    cabal test

Check the output of `cabal test --help` for various things that can also be done
while you run tests.


## Running the Service

The service can be ran with

    musicbrainz-emailer

You can enqueue emails by adding things to the queue manually, or by running:

    enqueue-mail

Both of these commands understand the `-h` and `--help` options, and will
provide you with more information.
