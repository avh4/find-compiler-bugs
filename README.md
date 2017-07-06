
## Running

```sh
(cd server && ./go.sh) &
(cd ui && elm-reactor) &
open http://localhost:8000/ElmCompiler.elm
```


## Development

```sh
(cd server && ./node_modules/.bin/nodemon server.js ) &
(cd ui && elm-reactor) &
open http://localhost:8000/ElmCompiler.elm
```
