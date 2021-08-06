#!/bin/sh

case $1 in
    sbcl)
        exec sbcl --noinform --non-interactive \
             --load benchmark.lisp \
             --eval '(flexi-streams-bench:run :n 4000 :count 10)'
        ;;
    ecl)
        exec ecl -q --nodebug \
             --load benchmark.lisp \
             --eval '(flexi-streams-bench:run :n 2000 :count 6)' \
             --eval "(ext:quit)"
        ;;
    clisp)
        exec clisp --quiet --silent -C \
             -x '(load "benchmark.lisp")' \
             -x '(flexi-streams-bench:run :n 1000 :count 6)'
        ;;
    *)
        echo "Unknown lisp: $1"
        exit 1
        ;;
esac
