#!/usr/bin/env bash
set -euo pipefail
wd=$(dirname "$0")
# the set of benchmarks in the readme
dotnet run -c Release --project src/Resharp.Benchmarks -p:BuildSourceGen=true -- --filter "*monster*" --join



