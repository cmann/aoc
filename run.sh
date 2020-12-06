#!/usr/bin/env bash

set -eux

stack runghc "src/$1.hs" < "input/$1.txt"
