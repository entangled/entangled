#!/usr/bin/bash

DIR="$(readlink -f "$(dirname "${BASH_SOURCE[0]}")")"
PROJECT_ROOT="$(readlink -f "${DIR}/../..")"
EXIT_CODE=0
ENTANGLED_EXEC="$(cabal exec which entangled)"

function entangled() {
        # cabal exec entangled -- $@
        # cabal run -v0 --project-file=${PROJECT_ROOT}/cabal.project entangled:entangled -- $*
        export entangled_datadir="${PROJECT_ROOT}"
        if [ -z $verbose ]; then
                ${ENTANGLED_EXEC} $@
        else
                ${ENTANGLED_EXEC} -V $@
        fi
}

function entangled-daemon() {
        export entangled_datadir="${PROJECT_ROOT}"
        if [ -z $verbose ]; then
                ${ENTANGLED_EXEC} daemon $@ &
        else
                ${ENTANGLED_EXEC} -V daemon $@ &
        fi
        daemon_pid=$!
        echo "Running 'entangled daemon' with PID=$daemon_pid"
        sleep 0.1
}

function kill-daemon() {
        echo "Killing PID $daemon_pid"
        kill $daemon_pid
}

function setup() {
        TMPDIR=$(mktemp --tmpdir -d entangled-test-XXXXXXXX)
        echo "Setting up in ${TMPDIR} ..."
        cp "${DIR}"/*.{md,diff,test,dhall} "${TMPDIR}"
        pushd "${TMPDIR}" > /dev/null
}

function teardown() {
        echo "Cleaning up ..."
        popd > /dev/null
        rm -rf "${TMPDIR}"
}

function show_help() {
        echo "usage: $0 [args]"
        echo
        echo "where [args] can be one of:"
        echo "    -h           help: show this help"
        echo "    -d           debug: run here instead of /tmp"
        echo "    -x           break on first failure"
        echo "    -u <unit>    only run unit"
        echo "    -c           clean after local run (with -d)"
        echo "    -v           verbose entangled"
        echo
        echo "Available units:"
        for t in *.test; do
                echo "    - $(basename ${t} .test)"
        done
}


while getopts "hdxcvu:" arg
do
        case ${arg} in
        h)      show_help
                exit 0
                ;;
        d)      no_setup=1
                ;;
        x)      break_on_fail=1
                ;;
        u)      test_only=$(basename ${OPTARG} .test)
                ;;
        v)      verbose=1
                ;;
        c)      rm -fv "${DIR}"/entangled.db
                rm -fv "${DIR}"/*.scm
                git checkout "${DIR}"/*.md
                exit 0
                ;;
        :)      echo "Invalid option: ${OPTARG} requires an argument"
                show_help
                exit 2
                ;;
        \?)     show_help
                exit 2
                ;;
        esac
done

function report-success() {
        echo -e "\033[32m✓\033[m $1"
}

function report-failure() {
        echo -e "\033[31m✗\033[m $2, \033[1m$1\033[m args:"
        shift ; shift
        for var in "$@"; do
                echo "    - \"${var}\""
        done

        EXIT_CODE=1
        if [ ! -z ${break_on_fail} ]; then
                exit ${EXIT_CODE}
        fi
}

function assert-streq() {
        if [ "$2" = "$3" ]; then
                report-success "$1"
        else
                report-failure assert-streq "$@"
        fi
}

function assert-arrayeq() {
        local a1=($2)
        local a2=($3)
        local n=${#a1[@]}
        for (( i=0; i<${n}; i++)); do
                if [ ! "${a1[$i]}" = "${a2[$i]}" ]; then
                        report-failure assert-arrayeq "$@"
                        break
                fi
        done
        report-success "$1"
}        

function assert-exists() {
        if [ -e "$2" ]; then
                report-success "$1"
        else
                report-failure "$@"
        fi
}

function assert-not-exists() {
        if [ ! -e "$2" ]; then
                report-success "$1"
        else
                report-failure "$@"
        fi
}

function run-test() {
        echo -e "\033[33m ~~~\033[m \033[1m$(basename $1 .test)\033[m \033[33m~~~\033[m"
        if [ -z ${no_setup} ]; then
                setup
        fi
        
        source "$(basename $1 .test).test"

        if [ -z ${no_setup} ]; then
                teardown
        fi
        echo
}

if [ -z ${test_only} ]; then
        for unit in "${DIR}"/*.test; do
                run-test "${unit}"
        done
else
        if [ -f "${test_only}.test" ]; then
                run-test "${test_only}"
        else
                echo "Could not find test: ${test_only}"
        fi
fi

exit ${EXIT_CODE}

