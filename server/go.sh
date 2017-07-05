#!/bin/bash

set -ex
docker build -t elm-find-compiler-bugs .
(sleep 5 && docker ps) &
docker run -p 42771:8080 elm-find-compiler-bugs
