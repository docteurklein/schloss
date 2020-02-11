

```
cabal new-run

topic=test
curl -isSL 0:8888/sse/$topic &

psql -c "select pg_notify('$topic', 'test')"
