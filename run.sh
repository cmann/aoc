#!/usr/bin/env bash

set -eux

stack runghc "$1.hs" < "$1.txt"
