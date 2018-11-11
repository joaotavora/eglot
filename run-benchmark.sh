set -e

EMACS=${EMACS:-emacs}

$EMACS -batch -Q -l generate-test-data.el
rm -rf issue-124-testbench
git clone . issue-124-testbench 2>/dev/null|| true

POSITIONS=${1:-positions.el}
TESTFILE=${2:-file.txt}

for branch in master scratch/fix-124-joaotavora scratch/fix-124-mkcms; do
    (cd issue-124-testbench && git checkout -f $branch && make clean && make eglot.elc) >/dev/null 2>&1 
    echo $branch `$EMACS -batch -Q --eval '(package-initialize)' -l issue-124-testbench/eglot.elc -l run-benchmark.el $POSITIONS $TESTFILE`
done
