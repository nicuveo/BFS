#! /usr/bin/env bash

set -u

cd $(dirname $0)

if [ $# -ne 0 ]; then
    echo "usage: test.sh" >&2
    exit 1
fi

stack build

for source in *.bs ; do
    testname=${source%.*}
    input=$testname.in
    output=$testname.out

    ok=1
    stack exec bfs --    $source > /tmp/$testname.1.bf
    if [ $? -ne 0 ] ; then
        printf "%-20s[\033[1;31mKO\033[0m] failed to compile default version\n" $testname
        ok=0
    fi
    stack exec bfs -- -O $source > /tmp/$testname.2.bf
    if [ $? -ne 0 ] ; then
        printf "%-20s[\033[1;31mKO\033[0m] failed to compile optimized version\n" $testname
        ok=0
    fi

    if [ $ok -eq 1 ] ; then
        bfc /tmp/$testname.1.bf 2> /dev/null; mv a.out /tmp/$testname.1
        bfc /tmp/$testname.2.bf 2> /dev/null; mv a.out /tmp/$testname.2

        cat $input | /tmp/$testname.1 > /tmp/$testname.1.out
        cat $input | /tmp/$testname.2 > /tmp/$testname.2.out

        ok=1
        if [ -n "$(diff $output /tmp/$testname.1.out)" ] ; then
            printf "%-20s[\033[1;31mKO\033[0m] output differs for default version\n" $testname
            cat /tmp/$testname.1.out
            ok=0
        fi
        if [ -n "$(diff $output /tmp/$testname.2.out)" ] ; then
            printf "%-20s[\033[1;31mKO\033[0m] output differs for optimized version\n" $testname
            cat /tmp/$testname.2.out
            ok=0
        fi
        if [ $ok -eq 1 ] ; then
            printf "%-20s[\033[1;32mOK\033[0m]\n" $testname
        fi
    fi
done
