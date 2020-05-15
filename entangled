#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
EXE="$( cabal exec --project-file=${DIR}/cabal.project which entangled )"
export entangled_datadir="${DIR}"
exec "${EXE}" $*

