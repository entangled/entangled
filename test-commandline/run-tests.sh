#!/usr/bin/bash

DIR="$(readlink -f "$(dirname "${BASH_SOURCE[0]}")")"
PROJECT_ROOT="$(readlink -f "${DIR}/..")"
EXIT_CODE=0

function entangled() {
        # cabal exec entangled -- $@
        cabal run -v0 --project-file=${PROJECT_ROOT}/cabal.project entangled:entangled -- $*
        # ${ENTANGLED_EXEC} $@
}

function setup() {
        TMPDIR=$(mktemp --tmpdir -d entangled-test-XXXXXXXX)
        echo "Setting up in ${TMPDIR} ..."
        cp "${DIR}"/*.{md,diff} "${DIR}/entangled.dhall" "${TMPDIR}"
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
        echo "    -h     help: show this help"
        echo "    -d     debug: run here instead of /tmp"
        echo "    -x     break on first failure"
}

while getopts "hdx" arg
do
        case $arg in
        h)      show_help && exit 0
                ;;
        d)      no_setup=1
                ;;
        x)      break_on_fail=1
                ;;
        ?)      show_usage && exit 2
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
        if [ -f "$2" ]; then
                report-success "$1"
        else
                report-failure "$@"
        fi
}

function test-01() {
        echo "Running 'entangled insert -s ...'"
        entangled insert -s test-01.md
        assert-arrayeq "Source contains expected files" \
                "$(entangled list | sort)" "factorial.scm hello.scm"
        assert-streq "Stitching conserves content" \
                "$(entangled stitch test-01.md)" "$(cat test-01.md)"
        assert-streq "Tangling gives an correct program #1" \
                "$(guile -c "$(entangled tangle -f factorial.scm)")" "3628800"
        assert-streq "Tangling gives an correct program #2" \
                "$(guile -c "$(entangled tangle -f hello.scm)")" "Hello, World!"

        echo "Running 'entangled tangle -a -d'."
        entangled tangle -a -d
        assert-exists "'tangle -a' created 'factorial.scm'" "factorial.scm"
        assert-exists "'tangle -a' created 'hello.scm'" "hello.scm"

        echo "Modifying 'hello.scm'"
        patch -s hello.scm test-01-01.diff
        echo "Running 'entangled insert -t hello.scm'"
        entangled insert -t hello.scm 
        assert-streq "Tangling gives an correct program #3" \
                "$(guile -c "$(entangled tangle -f hello.scm)")" "Hello, Universe!"
        echo "Stitching ..."
        entangled stitch test-01.md > test-01.md
        patch -s test-01.md test-01-02.diff
        entangled insert -s test-01.md 
        assert-streq "Tangling gives an correct program #4" \
                "$(guile -c "$(entangled tangle -f fib.scm)")" \
                "(1 1 2 3 5 8 13 21 34 55 89 144)"
}

function test-02() {
        entangled insert -s test-02-a.md
        assert-streq "Tangling gives an correct program #1" \
                "$(guile -c "$(entangled tangle -f case1.scm)")" "aba"
        assert-streq "Tangling gives an correct program #2" \
                "$(guile -c "$(entangled tangle -f case2.scm)")" "aa"

        entangled insert -s test-02-b.md
        assert-streq "Tangling gives an correct program #1" \
                "$(guile -c "$(entangled tangle -f case1.scm)")" "abaB"
        assert-streq "Tangling gives an correct program #2" \
                "$(guile -c "$(entangled tangle -f case2.scm)")" "aa" 
        assert-streq "Tangling gives an correct program #3" \
                "$(guile -c "$(entangled tangle -f case3.scm)")" "abaBc"
}

if [ -z ${no_setup} ]; then
        setup
fi

test-01
rm entangled.db
test-02

if [ -z ${no_setup} ]; then
        teardown
fi

exit ${EXIT_CODE}

