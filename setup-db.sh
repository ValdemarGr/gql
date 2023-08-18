#!/bin/bash
docker stop gql-postgres || true && docker rm gql-postgres || true
docker run --name gql-postgres --rm -d -p 5432:5432 -e POSTGRES_PASSWORD=1234 postgres
sleep 1
cat db.sql | docker exec -i gql-postgres psql -Upostgres
