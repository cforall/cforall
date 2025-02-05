# A test.py run (including what happens in the nightly build) runs the dimexpr-match test in a coarse, quick-check, fashion.
# This script helps you run the dimexpr-match test manually, in a more thorough fashion.
# test.py runs do not use this script.

# When a test.py run shows that an output has changed, use this script for verification, before running --regenerate-expected.
# Particularly with -ERRS, we don't actually care what the error message is (likely source of noise requiring --regenerate-expected), just that all the should-reject cases are rejected (which this script shows).

# Success means the script does not print "TEST FAILURE."
# Expect one large output with a mix of "done" and "skip" messages.  That is the acceptance cases.
# Then expect many small outputs all showing rc=1.  These are the rejection cases.

# The thoroughness that this script affords is
#   - verifying that _each_ rejection case is rejected, among a huge number of rejection cases
#     - particularly, counting those that reject by way of CFACC calling GCC, which issues a warning; these rejections are not reached by getting CFACC to report all errors at once
#   - observing the behaviour of a compiler other than the CFACC version in whose folder the test occurs; for example, GCC

# usage examples
#   ./dimexpr-match-detail.sh cfa
#   ./dimexpr-match-detail.sh      # same as above
#   ./dimexpr-match-detail.sh ~/cfa6/build/driver/cfa
#   ./dimexpr-match-detail.sh "$cfa -m32 -nodebug"
#   ./dimexpr-match-detail.sh 'gcc -x c'


compiler=${1:-cfa}
test=${2:-dimexpr-match-c.cfa}

# Same as first half of the auto-test: check that all the cases that should be accepted are accepted
set -x
$compiler $test
rc=$?
{ set +x; } 2> /dev/null

if [ $rc -gt 0 ]; then
    echo
    echo
    echo "TEST FAILURE: compiler rejected a case that should be accepted"
    echo
    echo

    exit 1
fi

set -x
./a.out
rc=$?
{ set +x; } 2> /dev/null

if [ $rc -gt 0 ]; then
    echo
    echo
    echo "TEST FAILURE: runtime crash on a case that should be accepted"
    echo
    echo

    exit 1
fi

# More detailed alternative to the second half of the auto-test: check that each case that the first half skipped is rejected, when run all by itself

function verifyCompilationRejection() {
    set -x
    $compiler $1 &> /dev/null
    rc=$?

    { set +x; } 2> /dev/null

    if [ $rc -eq 0 ]; then
        echo
        echo
        echo "TEST FAILURE: compiler accepted case that should be rejected:"
        echo $1
        echo
        echo

        # keep checking other cases
    fi
}

export -f verifyCompilationRejection
export compiler

./a.out -cmd4skip | sed -E -n 's/skip.*\| *//p' | xargs -n 1 -I {} bash -c 'verifyCompilationRejection "$@"' _ {}
