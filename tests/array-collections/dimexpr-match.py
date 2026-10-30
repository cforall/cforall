#!/usr/bin/env python3

# Test harness for the dimexpr-match test framework (see dimexpr-match.hfa).
# The test.py target array-collections/dimexpr-match (in the nightly build) runs this script; see its rule in Makefile.am.
# That rule runs it three times, each with --no-trace and --runner, concatenating the output, which test.py diffs against .expect/dimexpr-match.txt:
#   c runs c array:      the backend gcc that the SUT's cfa invokes (CFA_BACKEND_CC), on dimexpr-match-c.cfa
#   cfa runs c array:    the SUT's cfa, on dimexpr-match-c.cfa
#   cfa runs cfa array:  the SUT's cfa, on dimexpr-match-cfa.cfa
# All the action happens under the Makefile recipe, so test.py counts it all as compile time.

# What one run does:
#   - compile the test (preprocessing with INCLUDE_MINIMAL first, to tidy the output) and run it; it prints the acceptance cases as "done" lines
#   - for each rejection case that the run prints as "skip", compile that case all by itself, and print it as a "chkr" line, ending with "-> accept" or "-> reject"
# The "chkr" lines state outcomes without judging them; the .expect file, in which every one says "-> reject", does the judging.
# A "TEST FAILURE" line means the test itself did not compile, or crashed; the script exits non-zero, but the make rule carries on, so this output reaches the diff too.

# Compiling each rejection case by itself matters because
#   - it verifies that _each_ rejection case is rejected, among a huge number of rejection cases
#   - particularly, it counts those that reject by way of CFACC calling GCC, which issues a warning (an error, under -Werror); these rejections are not reached by getting CFACC to report all errors at once

# Binaries go in --bindir (default: current directory; under test.py, the build tree's array-collections directory), named after the test:
#   dimexpr-match-cfa-wrapper                   compiled from the whole test; prints the "done" lines; removed on success
#   dimexpr-match-cfa-wrapper.cfa               its preprocessed source; removed on success
#   dimexpr-match-cfa-PTRPARM_CALL-LIT-NE-LIT   one "chkr" case; exists only if the compiler accepted it (which we hope it doesn't)
# With --runner R, names get "-by-R" after the test name (e.g. dimexpr-match-c-by-c-wrapper), and the output starts with a "==== R runs ... array" header.

# Running it manually:
#   - Without --no-trace, it echoes its top-level commands (to stderr).
#   - Pass --detail to see, after each "chkr" line, its compiler invocation and the compiler's output.
#   - Pass any compiler, e.g. one from another build, to observe its behaviour.

# usage examples
#   ./dimexpr-match.py --compiler cfa
#   ./dimexpr-match.py      # means same as above
#   ./dimexpr-match.py --compiler ~/cfa6/build/driver/cfa
#   ./dimexpr-match.py --compiler "$cfa -m32 -nodebug"
#   ./dimexpr-match.py --compiler "$cfa" --detail
#   ./dimexpr-match.py --compiler 'gcc -x c' dimexpr-match-c.cfa
#   ./dimexpr-match.py --compiler cfa        dimexpr-match-c.cfa
#   ./dimexpr-match.py --compiler "$cfa" --bindir /tmp/dm
#   ./dimexpr-match.py --compiler 'gcc-11 -x c' --runner c dimexpr-match-c.cfa

import argparse
import os
import re
import shlex
import subprocess
import sys

parser = argparse.ArgumentParser()
parser.add_argument('test', nargs='?', default='dimexpr-match-cfa.cfa')
parser.add_argument('--compiler', default='cfa', help='compiler command (with any flags) for every compilation this script runs')
parser.add_argument('--detail', action='store_true', help='show the compiler invocation and output of each rejection case')
parser.add_argument('--no-trace', action='store_true', help='do not echo the top-level commands (for deterministic output)')
parser.add_argument('--bindir', default='.', help='directory for compiled binaries')
parser.add_argument('--runner', help='label for the compiler (e.g. c, cfa): names the binaries and heads the output')
opts = parser.parse_args()

compiler = shlex.split(opts.compiler)
test = os.path.abspath(opts.test)
stem = os.path.splitext(os.path.basename(test))[0]
name = stem + ('-by-' + opts.runner if opts.runner else '')
bindir = os.path.abspath(opts.bindir)
os.makedirs(bindir, exist_ok=True)

def trace(cmd, redirect=None):
    sys.stdout.flush()
    if opts.no_trace:
        return
    print('+ ' + shlex.join(cmd) + (' > ' + shlex.quote(redirect) if redirect else ''), file=sys.stderr, flush=True)

def failure(msg, *details):
    print('\n')
    print('TEST FAILURE: ' + msg)
    for d in details:
        print(d)
    print('\n', flush=True)

if opts.runner:
    print(f'==== {opts.runner} runs {stem.rsplit("-", 1)[1]} array')

# Same as first half of the auto-test: check that all the cases that should be accepted are accepted
# Compile in two steps, preprocessing with INCLUDE_MINIMAL first, to tidy the output.
# The test path is absolute, so the "chkr" commands, which name it via __FILE__, work from any directory.
wrapper = os.path.join(bindir, name + '-wrapper')
pp = wrapper + '.cfa'
cmd = compiler + [test, '-DINCLUDE_MINIMAL', '-E']
trace(cmd, pp)
with open(pp, 'w') as ppfile:
    if subprocess.run(cmd, stdout=ppfile).returncode != 0:
        failure('preprocessor rejected the test')
        sys.exit(1)
cmd = compiler + [pp, '-o', wrapper]
trace(cmd)
if subprocess.run(cmd).returncode != 0:
    failure('compiler rejected a case that should be accepted')
    sys.exit(1)

# Run the acceptance cases, and, interspersed, compile each case that they skipped all by itself, reporting whether it is accepted or rejected

# e.g. dimexpr-match-cfa-PTRPARM_CALL-LIT-NE-LIT; unique per case, unlike the "STA NE STA" categories
def caseName(args):
    def get(pattern):
        return re.search(pattern, args).group(1)
    arr = get(r'-DARRANGEMENT_(?!_OPTIN)(\w+)')
    kl  = get(r'-DCASE_FORMATION_L=(\w+)')
    kr  = get(r'-DCASE_FORMATION_R=(\w+)')
    eq  = get(r'-DCASE_EQUALITY=(\w+)')
    return f'{name}-{arr}-{kl}-{eq}-{kr}'

def checkCompilation(desc, args):
    out = os.path.join(bindir, caseName(args))
    if os.path.exists(out):
        os.remove(out)                  # so that a binary present afterward is from this run
    cmd = compiler + shlex.split(args) + ['-o', out]
    run = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    outcome = 'accept' if run.returncode == 0 else 'reject'
    print(f'chkr{desc} -> {outcome}')
    if opts.detail:
        print('+ ' + shlex.join(cmd))
        print(run.stdout, end='')

cmd = [wrapper, '-cmd4skip']
trace(cmd)
with subprocess.Popen(cmd, stdout=subprocess.PIPE, text=True) as run:
    for line in run.stdout:
        m = re.match(r'skip(.*?) *\| *(.*)', line)
        if m:
            checkCompilation(m.group(1), m.group(2))
        else:
            print(line, end='')
if run.returncode != 0:
    failure('runtime crash on a case that should be accepted')
    sys.exit(1)

os.remove(wrapper)
os.remove(pp)
