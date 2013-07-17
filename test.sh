#!/usr/bin/env zsh

for fic in tests/*.in; do
  input="${fic:r}"
  arg="$(cat $input.in)"
  print -n -- "$input: "
  runghc y.hs "$arg" > $input.res
  if diff $input.res $input.out > /dev/null; then
    print -- "OK"
  else
	print -- "ERROR"
    print -- "  expected: '$(cat $input.out)'"
    print -- "       got: '$(cat $input.res)'"
    print -- ""
  fi
  \rm -f $input.res
done
