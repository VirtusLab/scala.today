#!/bin/bash

set -euo pipefail

echo "Fetching database dump from the database pod"

rm -rf /tmp/dbdump

mkdir -p /tmp/dbdump

kubectl exec -n db postgresml-0 -- sh -c 'mkdir -p /tmp/dbdump && pg_dump -n public --clean -Ft postgresml | gzip > /tmp/dbdump/db.sql.tar.gz'

kubectl cp db/postgresml-0:/tmp/dbdump /tmp/dbdump

echo "Restoring database dump to the local database"

gzip -d /tmp/dbdump/db.sql.tar.gz

pg_restore --clean -h localhost -p 5432 -d postgresml -U postgresml /tmp/dbdump/db.sql.tar

echo "Cleaning up the database dump from the database pod"

kubectl exec -n db postgresml-0 -- sh -c 'rm -rf /tmp/dbdump'

echo "Cleaning up the local database dump"

rm -rf /tmp/dbdump

echo "Database restored successfully"
