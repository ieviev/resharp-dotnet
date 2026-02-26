#!/usr/bin/env bash
set -euo pipefail
wd=$(dirname "$0")
# the set of benchmarks in the readme
# this will take a while to compile because Source Generators need to compile the patterns
dotnet run -c Release --project src/Resharp.Benchmarks -- --filter "*monster*" --join



