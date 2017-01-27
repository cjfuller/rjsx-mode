#!/bin/bash

emacs --batch -l ~/.emacs.d/init.el --eval "(rjsx-mode-test)" > /dev/null 2>&1

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
