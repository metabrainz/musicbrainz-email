#!/bin/bash
export PATH=/usr/local/postgres/bin:$PATH
cabal update

git clone git://github.com/metabrainz/musicbrainz-server

if ! grep -q $(md5sum musicbrainz-server/admin/sql/CreateTables.sql) .schema-version
then
  psql -U postgres -c "DROP DATABASE musicbrainz_email"
  psql -U postgres -c "CREATE DATABASE musicbrainz_email WITH OWNER musicbrainz"
  psql -U postgres musicbrainz_email < /usr/local/postgres/share/contrib/cube.sql
  psql -U postgres musicbrainz_email < /usr/local/postgres/share/contrib/uuid-ossp.sql
  psql -U postgres musicbrainz_email < /usr/local/postgres/share/contrib/musicbrainz_collate.sql
  psql -U musicbrainz musicbrainz_email -c "CREATE SCHEMA musicbrainz"
  psql -U musicbrainz musicbrainz_email < musicbrainz-server/admin/sql/CreateTables.sql
fi
md5sum musicbrainz-server/admin/sql/CreateTables.sql > .schema-version

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

cabal test --test-option='--jxml=junit.xml' --test-option '-j5'
