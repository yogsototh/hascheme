#!/usr/bin/env zsh

typeset -a listfic
if (($#==0)); then
    listfic=( tests/*(.N) )
else
    for arg in $@; do
        listfic=( $listfic tests/$arg )
    done
fi

tmpfic=tests/tmp
for input in $listfic; do
    sed 's/\\/\\\\/g' $input > $tmpfic
    # set 3 as the file descriptor for the file $tmpfic
    exec 3< $tmpfic
    done=0
    num=1
    until ((done == 1)); do
        read <&3 program
        (($?!=0)) && {done=1;continue}
        read <&3 expected
        (($?!=0)) && {done=1;continue}
        result="$(runghc y.hs "$program")"
        printf "%18s (line %3d): " ${input:t} $num
        if [[ $expected == $result ]]; then
            print -- "OK"
        else
	        print -- "ERROR"
            print -- "   program: '$program'"
            print -- "  expected: '$expected'"
            print -- "       got: '$result'"
            print -- ""
        fi
        ((num+=2))
    done
done
\rm -f $tmpfic
