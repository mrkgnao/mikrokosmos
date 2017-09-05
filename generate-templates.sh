#!/usr/bin/env bash

set -o xtrace

runghc templates/default.hs > templates/default.html
