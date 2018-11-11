set -e

emacs -batch -Q -l generate-test-data.el

for branch in master scratch/fix-124-mkcms scratch/fix-124-joaotavora; do
    git checkout $branch >/dev/null 2>&1
    (make clean && make eglot.elc) >/dev/null 2>&1
    echo $branch `emacs -batch -Q -l eglot.elc -l run-benchmark.el`s
done
