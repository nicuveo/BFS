#! /usr/bin/env bash

set -u

cd $(dirname $0)

if [ $# -ne 0 ]; then
    echo "usage: test.sh" >&2
    exit 1
fi

stack build
if [ $? -ne 0 ]; then
    echo "failed to compile BFS"
    exit 1
fi

failed=0

function run() {
    source="$1"
    label="$2"
    flag="$3"

    test=${source%.*}
    name=$test-$label
    bffile=/tmp/$name.bf
    afile=/tmp/$name.a
    outfile=/tmp/$name.out

    stack exec bfs -- $flag $source > $bffile
    if [ $? -ne 0 ] ; then
        printf "%-32s[\033[1;31mKO\033[0m] failed to transpile\n" $name
        failed=1
        return
    fi

    bfc $bffile 2> /dev/null
    if [ $? -ne 0 ] ; then
        printf "%-32s[\033[1;31mKO\033[0m] failed to compile\n" $name
        failed=1
        return
    fi
    mv a.out $afile

    if test -f "$test.in" ; then
        elapsed=`{ time $afile < $test.in > $outfile; } 2>&1 | sed -n 's/real\t*//p'`
    else
        elapsed=`{ time $afile            > $outfile; } 2>&1 | sed -n 's/real\t*//p'`
    fi
    if [ -n "$(diff $test.out $outfile)" ] ; then
        printf "%-32s[\033[1;31mKO\033[0m] output diff:\n" $name
        diff $test.out $outfile
        failed=1
        return
    fi

    printf "%-32s[\033[1;32mOK\033[0m] (in %s) \n" $name "$elapsed"
}

for source in *.bs ; do
    run "$source" "verbose" ""
    run "$source" "dense"   "-O"
done

exit $failed
