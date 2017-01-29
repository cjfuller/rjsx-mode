#!/bin/bash

set -e

if [ -n "$CI" ]
then
    # Load the seq library first when running on emacs 24 for CI.
    emacs --batch -l ./seq.el/seq-24.el -l ./seq.el/seq.el -l ./rjsx-mode.el --eval "(progn (setq debug-on-error 1) (rjsx-mode-test))" > /dev/null
else
    emacs --batch -l ./rjsx-mode.el --eval "(progn (setq debug-on-error 1) (rjsx-mode-test))" > /dev/null
fi

set +e
exit_status=0

all_tests=`ls tests/* | grep -v "_err"`
for test in $all_tests
do
    testname=`basename $test`
    if [ -e "tests/_err.$testname" ]
    then
        echo ""
        echo -e "\e[31mFAILED\e[0m $test"
        exit_status=1
        diff $test tests/_err.$testname
    else
        echo -e "\e[32mPASSED\e[0m $test"
    fi
done
exit $exit_status
