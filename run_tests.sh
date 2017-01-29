#!/bin/bash

set -e

emacs --batch -l ./rjsx-mode.el --eval "(progn (setq debug-on-error 1) (rjsx-mode-test))" > /dev/null

set +e

all_tests=`ls tests/* | grep -v "_err"`
for test in $all_tests
do
    testname=`basename $test`
    if [ -e "tests/_err.$testname" ]
    then
        echo ""
        echo -e "\e[31mFAILED\e[0m $test"
        diff $test tests/_err.$testname
    else
        echo -e "\e[32mPASSED\e[0m $test"
    fi
done
