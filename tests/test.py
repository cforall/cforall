#!/usr/bin/python3

from pybin.tools import *
from pybin.test_run import *
from pybin import settings

import argparse
import itertools
import re
import sys
import tempfile
import time

import os
import signal

################################################################################
#               help functions
################################################################################

def find_tests():
	expected = []

	def match_test(path):
		match = re.search("^%s\/([\w\/\-_]*).expect\/([\w\-_\+]+)(\.[\w\-_]+)?\.txt$" % settings.SRCDIR, path)
		if match :
			test = Test()
			test.name = match.group(2)
			test.path = match.group(1)
			test.arch = match.group(3)[1:] if match.group(3) else None

			expected.append(test)

	path_walk( match_test )

	return expected

# reads the directory ./.expect and indentifies the tests
def list_tests( includes, excludes ):
	# tests directly in the .expect folder will always be processed
	test_list = find_tests()

	# if we have a limited number of includes, filter by them
	if includes:
		test_list = [x for x in test_list if
			x.target().startswith( tuple(includes) )
		]

	# # if we have a folders to excludes, filter by them
	if excludes:
		test_list = [x for x in test_list if not
			x.target().startswith( tuple(excludes) )
		]

	# sort the test alphabetically for convenience
	test_list.sort(key=lambda t: ('~' if t.arch else '') + t.target() + (t.arch if t.arch else ''))

	return test_list

# from the found tests, filter all the valid tests/desired tests
def valid_tests( options ):
	tests = []

	# if we are regenerating the tests we need to find the information of the
	# already existing tests and create new info for the new tests
	if options.regenerate_expected :
		for testname in options.tests :
			testname = os.path.normpath( os.path.join(settings.SRCDIR, testname) )

			# first check if this is a valid name to regenerate
			if Test.valid_name(testname):
				# this is a valid name, let's check if it already exists
				found = [test for test in all_tests if canonical_path( test.target() ) == testname]
				setup = itertools.product(settings.all_arch if options.arch else [None])
				if not found:
					# it's a new name, create it according to the name and specified architecture
					tests.extend( [Test.new_target(testname, arch) for arch in setup] )
				elif len(found) == 1 and not found[0].arch:
					# we found a single test, the user better be wanting to create a cross platform test
					if options.arch:
						print('ERROR: "%s", test has no specified architecture but --arch was specified, ignoring it' % testname, file=sys.stderr)
					else:
						tests.append( found[0] )
				else:
					# this test is already cross platform, just add a test for each platform the user asked
					tests.extend( [Test.new_target(testname, arch) for arch in setup] )

					# print a warning if it users didn't ask for a specific architecture
					found_arch = [f.arch for f in found if f.arch]
					if found_arch and not options.arch:
						print('WARNING: "%s", test has architecture specific expected files but --arch was not specified, regenerating only for current host' % testname, file=sys.stderr)

			else :
				print('ERROR: "%s", tests are not allowed to end with a C/C++/CFA extension, ignoring it' % testname, file=sys.stderr)

	else :
		# otherwise we only need to validate that all tests are present in the complete list
		for testname in options.tests:
			test = [t for t in all_tests if path_cmp( t.target(), testname )]

			if test :
				tests.extend( test )
			else :
				print('ERROR: No expected file for test %s, ignoring it' % testname, file=sys.stderr)

	return tests

# parses the option
def parse_args():
	# create a parser with the arguments for the tests script
	parser = argparse.ArgumentParser(description='Script which runs cforall tests')
	parser.add_argument('--arch', help='Test for specific architecture', type=comma_separated(str), default=None)
	parser.add_argument('--debug', help='Run all tests in debug or release', type=comma_separated(yes_no), default='yes')
	parser.add_argument('--install', help='Run all tests based on installed binaries or tree binaries', type=comma_separated(yes_no), default='no')
	parser.add_argument('--continue', help='When multiple specifications are passed (debug/install/arch), sets whether or not to continue if the last specification failed', type=yes_no, default='yes', dest='continue_')
	parser.add_argument('--timeout', help='Maximum duration in seconds after a single test is considered to have timed out', type=int, default=180)
	parser.add_argument('--global-timeout', help='Maximum cumulative duration in seconds after the ALL tests are considered to have timed out', type=int, default=7200)
	parser.add_argument('--timeout-with-gdb', help='Instead of killing the command when it times out, orphan it and print process id to allow gdb to attach', type=yes_no, default="no")
	parser.add_argument('--dry-run', help='Don\'t run the tests, only output the commands', action='store_true')
	parser.add_argument('--list', help='List all test available', action='store_true')
	parser.add_argument('--all', help='Run all test available', action='store_true')
	parser.add_argument('--regenerate-expected', help='Regenerate the .expect by running the specified tets, can be used with --all option', action='store_true')
	parser.add_argument('--archive-errors', help='If called with a valid path, on test crashes the test script will copy the core dump and the executable to the specified path.', type=str, default='')
	parser.add_argument('-j', '--jobs', help='Number of tests to run simultaneously, 0 (default) for unlimited', nargs='?', const=0, type=int)
	parser.add_argument('--list-comp', help='List all valide arguments', action='store_true')
	parser.add_argument('--list-dist', help='List all tests for distribution', action='store_true')
	parser.add_argument('-I','--include', help='Directory of test to include, can be used multiple time, All  if omitted', action='append')
	parser.add_argument('-E','--exclude', help='Directory of test to exclude, can be used multiple time, None if omitted', action='append')
	parser.add_argument('tests', metavar='test', type=str, nargs='*', help='a list of tests to run')

	try:
		options =  parser.parse_args()
	except:
		print('ERROR: invalid arguments', file=sys.stderr)
		parser.print_help(sys.stderr)
		sys.exit(1)

	# script must have at least some tests to run or be listing
	listing    = options.list or options.list_comp or options.list_dist
	all_tests  = options.all
	some_tests = len(options.tests) > 0
	some_dirs  = len(options.include) > 0 if options.include else 0

	# check that exactly one of the booleans is set to true
	if not sum( (listing, all_tests, some_tests, some_dirs) ) > 0 :
		print('''ERROR: must have option '--all', '--list', '--include', '-I' or non-empty test list''', file=sys.stderr)
		parser.print_help()
		sys.exit(1)

	return options

################################################################################
#               running test functions
################################################################################
def success(val):
	return val == 0 or settings.dry_run

def no_rule(file, target):
	return not settings.dry_run and file_contains_only(file, "make: *** No rule to make target `%s'.  Stop." % target)

# logic to run a single test and return the result (No handling of printing or other test framework logic)
def run_single_test(test):

	# find the output file based on the test name and options flag
	exe_file = test.target_executable();
	out_file = test.target_output()
	err_file = test.error_log()
	cmp_file = test.expect()
	in_file  = test.input()

	# prepare the proper directories
	test.prepare()

	# ----------
	# MAKE
	# ----------
	# build, skipping to next test on error
	with Timed() as comp_dur:
		make_ret, _, _ = make( test.target(), output_file=subprocess.DEVNULL, error=out_file, error_file = err_file )

	# ----------
	# RUN
	# ----------
	# run everything in a temp directory to make sure core file are handled properly
	run_dur = None
	with tempdir():
		# if the make command succeeds continue otherwise skip to diff
		if success(make_ret):
			with Timed() as run_dur:
				if settings.dry_run or is_exe(exe_file):
					# run test
					retcode, _, _ = sh(exe_file, output_file=out_file, input_file=in_file, timeout=True, nice=True)
				else :
					# simply cat the result into the output
					retcode = cat(exe_file, out_file)
		else:
			retcode = mv(err_file, out_file)

		if success(retcode):
			if settings.generating :
				# if we are only generating the output we still need to check that the test actually exists
				if no_rule(out_file, test.target()) :
					retcode = 1
					error = "\t\tNo make target for test %s!" % test.target()
					rm(out_file)
				else:
					error = None
			else :
				# fetch return code and error from the diff command
				retcode, error, _ = diff(cmp_file, out_file)

		else:
			if os.stat(out_file).st_size < 1048576:
				with open (out_file, "r", encoding='latin-1') as myfile:  # use latin-1 so all chars mean something.
					error = myfile.read()
			else:
				error = "Output log can't be read, file is bigger than 1MB, see {} for actual error\n".format(out_file)

			ret, info = core_info(exe_file)
			error = error + info if error else info

			if settings.archive:
				error = error + '\n' + core_archive(settings.archive, test.target(), exe_file)



	# clean the executable
	rm(exe_file)

	return retcode, error, [comp_dur.duration, run_dur.duration if run_dur else None]

# run a single test and handle the errors, outputs, printing, exception handling, etc.
def run_test_worker(t) :
	try :
		# print formated name
		name_txt = t.format_target(width=settings.output_width) + '  '

		retcode, error, duration = run_single_test(t)

		# update output based on current action
		result_key, result_txt = TestResult.toString( retcode, duration )

		#print result with error if needed
		text = '\t' + name_txt + result_txt
		out = sys.stdout
		if error :
			text = text + '\n' + error

		return retcode == TestResult.SUCCESS, result_key, text
	except KeyboardInterrupt:
		return False, 'keybrd', ""
	# except Exception as ex:
	# 	print("Unexpected error in worker thread running {}: {}".format(t.target(), ex), file=sys.stderr)
	# 	sys.stderr.flush()
	# 	return False, ""


# run the given list of tests with the given parameters
def run_tests(tests, jobs) :
	# clean the sandbox from previous commands
	make('clean', output_file=subprocess.DEVNULL, error=subprocess.DEVNULL)

	# create the executor for our jobs
	pool = multiprocessing.Pool(jobs)

	failed = False
	rescnts = {	'pass': 0, 'fail': 0, 'time': 0, 'keybrd': 0 }
	other = 0

	# for each test to run
	try :
		num = len(tests)
		fancy = sys.stdout.isatty()
		results = pool.imap_unordered(
			run_test_worker,
			tests,
			chunksize = 1
		)

		for i, (succ, code, txt) in enumerate(timed(results, timeout = settings.timeout.total), 1) :
			if code in rescnts.keys():
				rescnts[code] += 1
			else:
				other += 1

			if not succ :
				failed = True

			print("       " + txt)

			if(fancy and i != num):
				print("%d/%d" % (i, num), end='\r')
				sys.stdout.flush()

	except KeyboardInterrupt:
		print("Tests interrupted by user", file=sys.stderr)
		pool.terminate()
		pool.join()
		failed = True
	except multiprocessing.TimeoutError:
		print("ERROR: Test suite timed out", file=sys.stderr)
		pool.terminate()
		pool.join()
		failed = True
		killgroup() # needed to cleanly kill all children


	# clean the workspace
	make('clean', output_file=subprocess.DEVNULL, error=subprocess.DEVNULL)

	print("{} passes, {} failures, {} timeouts, {} cancelled, {} other".format(rescnts['pass'], rescnts['fail'], rescnts['time'], rescnts['keybrd'], other))

	return failed


################################################################################
#               main loop
################################################################################
if __name__ == "__main__":

	# parse the command line arguments
	options = parse_args()

	# init global settings
	settings.init( options )

	# --------------------------------------------------
	# list all the test for auto completion programs
	# not pretty, single line, with the command line options
	if options.list_comp :
		# fetch the liest of all valid tests
		tests = list_tests( None, None )

		# print the possible options
		print("-h --help --debug --dry-run --list --arch --all --regenerate-expected --archive-errors --install --timeout --global-timeout --timeout-with-gdb -j --jobs -I --include -E --exclude --continue ", end='')
		print(" ".join(map(lambda t: "%s" % (t.target()), tests)))

		# done
		sys.exit(0)

	# --------------------------------------------------
	# list all the test for auto completion programs
	if options.list_dist :
		# fetch the liest of all valid tests
		tests = list_tests( None, None )

		for t in tests:
			print(os.path.relpath(t.expect(), settings.SRCDIR), end=' ')
			print(os.path.relpath(t.input() , settings.SRCDIR), end=' ')
			code, out, err = make_recon(t.target())

			if code != 0:
				print('ERROR: recond failed for test {}: {} \'{}\''.format(t.target(), code, err), file=sys.stderr)
				sys.exit(1)

			print(' '.join(re.findall('([^\s]+\.cfa)', out)), end=' ')

		print('')

		# done
		sys.exit(0)


	# --------------------------------------------------
	# list all the tests for users, in a pretty format
	if options.list :
		# fetch the liest of all valid tests
		tests = list_tests( options.include, options.exclude )

		# print the available tests
		fancy_print("\n".join(map(lambda t: t.toString(), tests)))

		# done
		sys.exit(0)

	# fetch the liest of all valid tests
	all_tests = list_tests( options.include, options.exclude )

	# if user wants all tests than no other treatement of the test list is required
	if options.all or options.include :
		tests = all_tests

	#otherwise we need to validate that the test list that was entered is valid
	else :
		tests = valid_tests( options )

	# make sure we have at least some test to run
	if not tests :
		print('ERROR: No valid test to run', file=sys.stderr)
		sys.exit(1)

	# prep invariants
	settings.prep_output(tests)
	failed = 0

	# check if the expected files aren't empty
	if not options.regenerate_expected:
		for t in tests:
			if is_empty(t.expect()):
				print('WARNING: test "{}" has empty .expect file'.format(t.target()), file=sys.stderr)

	options.jobs = job_count( options )

	# for each build configurations, run the test
	with Timed() as total_dur:
		for arch, debug, install in itertools.product(settings.all_arch, settings.all_debug, settings.all_install):
			settings.arch    = arch
			settings.debug   = debug
			settings.install = install

			# filter out the tests for a different architecture
			# tests are the same across debug/install
			local_tests = settings.arch.filter( tests )

			# check the build configuration works
			settings.validate()
			jobs = min(options.jobs, len(local_tests))

			# print configuration
			print('%s %i tests on %i cores (%s - %s)' % (
				'Regenerating' if settings.generating else 'Running',
				len(local_tests),
				jobs,
				settings.arch.string,
				settings.debug.string
			))
			if not local_tests :
				print('WARNING: No tests for this configuration')
				continue

			# otherwise run all tests and make sure to return the correct error code
			failed = run_tests(local_tests, jobs)
			if failed:
				if not settings.continue_:
					break

	print('Tests took %s' % fmtDur( total_dur.duration ))
	sys.exit( failed )
