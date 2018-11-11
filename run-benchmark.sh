set -e

EMACS=${EMACS:-emacs}

$EMACS -batch -Q -l generate-test-data.el
git clone . issue-124-testbench || true

for branch in master scratch/fix-124-mkcms scratch/fix-124-joaotavora; do
    (cd issue-124-testbench && git checkout -f $branch && make clean && make eglot.elc) 1>&2
    echo $branch `$EMACS -batch -Q --eval '(package-initialize)' -l issue-124-testbench/eglot.elc -l run-benchmark.el`s
done
