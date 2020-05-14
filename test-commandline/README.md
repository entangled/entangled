# Command-line tests
These tests run in Bash. Each starts with a set of Markdown files that are sequentially patched to see if Entangled give the correct output. The different testing suites should have a `.test` extension. For each suite the contents of this folder (the `.test`, `.dhall`, `.md` and `.diff` files that is) are copied to a temporary folder, where the commands in the `.test` file are executed, after which the temporary folder is deleted. Run with:

    ./run-tests.sh

If things take a long time, this means that `cabal` is recompiling the code. If you are developing a test, you may want to run the tests locally to do a post-mortem. In that case, run with `-u` to select the test you're working on, `-d` to run locally and `-x` to quit after the first failure:

    ./run-tests.sh -xdu 01-basic

## Rationale
Entangled is a simple program, but its workings are sometimes subtle and very stateful. In stead of loading a debugger and checking the state of the memory when something goes wrong, we work with a Sqlite database. All the actions that the daemon does can be programmed from the command-line, allowing for easy inspection.

