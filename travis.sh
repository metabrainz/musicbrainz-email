#!/bin/bash

git clone git://github.com/metabrainz/musicbrainz-server
pushd musicbrainz-server

psql -U postgres -c "CREATE USER musicbrainz NOCREATEDB NOCREATEUSER"
psql -U postgres -c "CREATE DATABASE musicbrainz_email WITH OWNER musicbrainz"
psql -U postgres musicbrainz_email -c "CREATE EXTENSION cube"
psql -U postgres musicbrainz_email -c "CREATE EXTENSION \"uuid-ossp\""
psql -U musicbrainz musicbrainz_email -c "CREATE SCHEMA musicbrainz"
psql -U musicbrainz musicbrainz_email < admin/sql/CreateTables.sql

sudo rabbitmqctl add_vhost /email
sudo rabbitmqctl set_permissions -p /email guest '.*' '.*' '.*'
popd

cabal update
cabal clean
cabal install --enable-tests --only-dependencies --force-reinstalls
cabal configure --enable-tests
cabal build

cat - > test.cfg <<EOF
db {
  user = "musicbrainz"
  password = "musicbrainz"
  database = "musicbrainz_email"
  host = "localhost"
  port = 5432
}
rabbitmq {
  user = "guest"
  password = "guest"
  vhost = "/email"
  host = "127.0.0.1"
}
EOF

cabal test
