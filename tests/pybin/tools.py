import __main__
import argparse
import contextlib
import datetime
import fileinput
import multiprocessing
import os
import re
import resource
import signal
import stat
import subprocess
import sys
import tempfile
import time
import types

from pybin import settings

################################################################################
#               shell helpers
################################################################################

# helper functions to run terminal commands
def sh(*cmd, timeout = False, output_file = None, input_file = None, input_text = None, error = subprocess.STDOUT, ignore_dry_run = False, pass_fds = [], nice = False):
	try:
		cmd = list(cmd)

		if input_file and input_text:
			return 401, "Cannot use both text and file inputs"

		# if this is a dry_run, only print the commands that would be ran
		if settings.dry_run and not ignore_dry_run:
			cmd = "{} cmd: {}".format(os.getcwd(), ' '.join(cmd))
			if output_file and not isinstance(output_file, int):
				cmd += " > "
				cmd += output_file

			if error and not isinstance(error, int):
				cmd += " 2> "
				cmd += error

			if input_file and not isinstance(input_file, int) and os.path.isfile(input_file):
				cmd += " < "
				cmd += input_file

			print(cmd)
			return 0, None, None

		with contextlib.ExitStack() as onexit:
			# add input redirection if needed
			input_file = openfd(input_file, 'r', onexit, True)

			# add output redirection if needed
			output_file = openfd(output_file, 'w', onexit, False)

			# add error redirection if needed
			error = openfd(error, 'w', onexit, False)

			# prepare the parameters to the call
			popen_kwargs = {
				'stdout' : output_file,
				'stderr' : error,
				'pass_fds': pass_fds,
			}

			# depending on how we are passing inputs we need to set a different argument to popen
			if input_text:
				popen_kwargs['input'] = bytes(input_text, encoding='utf-8')
			else:
				popen_kwargs['stdin'] = input_file

			# we might want to nice this so it's not to obnixious to users
			if nice:
				popen_kwargs['preexec_fn'] = lambda: os.nice(5)

			# run the desired command
			# use with statement to make sure proc is cleaned
			# don't use subprocess.run because we want to send SIGABRT on exit
			with subprocess.Popen( cmd, **popen_kwargs ) as proc:
				try:
					out, errout = proc.communicate(
						timeout = settings.timeout.single if timeout else None
					)

					return proc.returncode, out.decode("latin-1") if out else None, errout.decode("latin-1") if errout else None
				except subprocess.TimeoutExpired:
					if settings.timeout2gdb:
						print("Process {} timeout".format(proc.pid))
						proc.communicate()
						return 124, str(None), "Subprocess Timeout 2 gdb"
					else:
						proc.send_signal(signal.SIGABRT)
						proc.communicate()
						return 124, str(None), "Subprocess Timeout 2 gdb"

	except Exception as ex:
		print ("Unexpected error: %s" % ex)
		raise

def is_empty(fname):
	if not os.path.isfile(fname):
		return True

	if os.stat(fname).st_size == 0:
		return True

	return False

def is_ascii(fname):
	if settings.dry_run:
		print("is_ascii: %s" % fname)
		return (True, "")

	if not os.path.isfile(fname):
		return (False, "No file")

	code, out, err = sh("file", fname, output_file=subprocess.PIPE)
	if code != 0:
		return (False, "'file EXPECT' failed with code {} '{}'".format(code, err))

	match = re.search(".*: (.*)", out)

	if not match:
		return (False, "Unreadable file type: '{}'".format(out))

	if "ASCII text" in match.group(1):
		return (True, "")

	return (False, "File type should be 'ASCII text', was '{}'".format(match.group(1)))

def is_exe(fname):
	return os.path.isfile(fname) and os.access(fname, os.X_OK)

def openfd(file, mode, exitstack, checkfile):
	if not file:
		return file

	if isinstance(file, int):
		return file

	if checkfile and not os.path.isfile(file):
		return None

	file = open(file, mode, encoding="latin-1") # use latin-1 so all chars mean something.
	exitstack.push(file)
	return file

# Remove 1 or more files silently
def rm( files ):
	if isinstance(files, str ): files = [ files ]
	for file in files:
		sh( 'rm', '-f', file, output_file=subprocess.DEVNULL, error=subprocess.DEVNULL )

# Create 1 or more directory
def mkdir( files ):
	if isinstance(files, str ): files = [ files ]
	for file in files:
		p = os.path.normpath( file )
		d = os.path.dirname ( p )
		sh( 'mkdir', '-p', d, output_file=subprocess.DEVNULL, error=subprocess.DEVNULL )


def chdir( dest = __main__.__file__ ):
	abspath = os.path.abspath(dest)
	dname = os.path.dirname(abspath)
	os.chdir(dname)

# diff two files
def diff( lhs, rhs ):
	# fetch return code and error from the diff command
	return sh(
		'''diff''',
		'''--text''',
		'''--old-group-format=\t\tmissing lines :\n%<''',
		'''--new-line-format=\t\t%dn\t%L''',
		'''--new-group-format=\t\tnew lines : \n%>''',
		'''--old-line-format=\t\t%dn\t%L''',
		'''--unchanged-group-format=%=''',
		'''--changed-group-format=\t\texpected :\n%<\t\tgot :\n%>''',
		'''--unchanged-line-format=''',
		lhs,
		rhs,
		output_file=subprocess.PIPE
	)

# call make
def make(target, *, flags = '', output_file = None, error = None, error_file = None, silent = False):
	test_param = """test="%s" """ % (error_file) if error_file else None
	cmd = [
		*settings.make,
		'-s' if silent else None,
		test_param,
		settings.arch.flags,
		settings.debug.flags,
		settings.install.flags,
		settings.distcc if settings.distribute else None,
		flags,
		target
	]
	cmd = [s for s in cmd if s]
	return sh(*cmd, output_file=output_file, error=error, pass_fds=settings.make_jobfds)

def make_recon(target):
	cmd = [
		*settings.make,
		'-W',
		os.path.abspath(os.path.join(settings.BUILDDIR, '../driver/cfa')),
		'--recon',
		target
	]
	cmd = [s for s in cmd if s]
	return sh(*cmd, output_file=subprocess.PIPE)

def which(program):
	fpath, fname = os.path.split(program)
	if fpath:
		if is_exe(program):
			return program
	else:
		for path in os.environ["PATH"].split(os.pathsep):
			exe_file = os.path.join(path, program)
			if is_exe(exe_file):
				return exe_file
	return None

@contextlib.contextmanager
def tempdir():
	cwd = os.getcwd()
	with tempfile.TemporaryDirectory() as temp:
		os.chdir(temp)
		try:
			yield temp
		finally:
			os.chdir(cwd)

def killgroup():
	try:
		os.killpg(os.getpgrp(), signal.SIGINT)
	except KeyboardInterrupt:
		pass # expected
	except Exception as exc:
		print("Unexpected exception", file=sys.stderr)
		print(exc, file=sys.stderr)
		sys.stderr.flush()
		sys.exit(2)

################################################################################
#               file handling
################################################################################
# move a file
def mv(source, dest):
	ret, _, _ = sh("mv", source, dest)
	return ret

# cat one file into the other
def cat(source, dest):
	ret, _, _ = sh("cat", source, output_file=dest)
	return ret

# helper function to replace patterns in a file
def file_replace(fname, pat, s_after):
	if settings.dry_run:
		print("replacing '%s' with '%s' in %s" % (pat, s_after, fname))
		return

	file = fileinput.FileInput(fname, inplace=True, backup='.bak')
	for line in file:
		print(line.replace(pat, s_after), end='')
	file.close()

# helper function to check if a files contains only a specific string
def file_contains_only(file, text) :
	with open(file, encoding="latin-1") as f: # use latin-1 so all chars mean something.
		ff = f.read().strip()
		result = ff == text.strip()

		return result

# transform path to canonical form
def canonical_path(path):
	abspath = os.path.abspath(os.path.realpath(__main__.__file__))
	dname = os.path.dirname(abspath)
	return os.path.join(dname, os.path.normpath(path) )

# compare path even if form is different
def path_cmp(lhs, rhs):
	return canonical_path( lhs ) == canonical_path( rhs )

# walk all files in a path
def path_walk( op ):
	dname = settings.SRCDIR
	for dirname, _, names in os.walk(dname):
		for name in names:
			path = os.path.join(dirname, name)
			op( path )

################################################################################
#               system
################################################################################
def jobserver_version():
	make_ret, out, err = sh('make', '.test_makeflags', '-j2', ignore_dry_run = True, output_file=subprocess.PIPE, error=subprocess.PIPE)
	if make_ret != 0:
		print("ERROR: cannot find Makefile jobserver version", file=sys.stderr)
		print("       test returned : {} '{}'".format(make_ret, err), file=sys.stderr)
		sys.exit(1)

	re_jobs = re.search("--jobserver-(auth|fds)", out)
	if not re_jobs:
		print("ERROR: cannot find Makefile jobserver version", file=sys.stderr)
		print("       MAKEFLAGS are : '{}'".format(out), file=sys.stderr)
		sys.exit(1)

	return "--jobserver-{}".format(re_jobs.group(1))

def prep_recursive_make(N):
	if N < 2:
		return []

	# create the pipe
	(r, w) = os.pipe()

	# feel it with N-1 tokens, (Why N-1 and not N, I don't know it's in the manpage for make)
	os.write(w, b'+' * (N - 1));

	# prep the flags for make
	make_flags = ["-j{}".format(N), "--jobserver-auth={},{}".format(r, w)]

	# tell make about the pipes
	os.environ["MAKEFLAGS"] = os.environ["MFLAGS"] = " ".join(make_flags)

	# make sure pass the pipes to our children
	settings.update_make_fds(r, w)

	return make_flags

def prep_unlimited_recursive_make():
	# prep the flags for make
	make_flags = ["-j"]

	# tell make about the pipes
	os.environ["MAKEFLAGS"] = os.environ["MFLAGS"] = "-j"

	return make_flags


def eval_hardware():
	# we can create as many things as we want
	# how much hardware do we have?
	if settings.distribute:
		# remote hardware is allowed
		# how much do we have?
		ret, jstr, _ = sh("distcc", "-j", output_file=subprocess.PIPE, ignore_dry_run=True)
		return int(jstr.strip()) if ret == 0 else multiprocessing.cpu_count()
	else:
		# remote isn't allowed, use local cpus
		return multiprocessing.cpu_count()

# count number of jobs to create
def job_count( options ):
	# check if the user already passed in a number of jobs for multi-threading
	make_env = os.environ.get('MAKEFLAGS')
	make_flags = make_env.split() if make_env else None
	jobstr = jobserver_version()

	if options.jobs and make_flags:
		print('WARNING: -j options should not be specified when called form Make', file=sys.stderr)

	# Top level make is calling the shots, just follow
	if make_flags:
		# do we have -j and --jobserver-...
		jobopt = None
		exists_fds = None
		for f in make_flags:
			jobopt = f if f.startswith("-j") else jobopt
			exists_fds = f if f.startswith(jobstr) else exists_fds

		# do we have limited parallelism?
		if exists_fds :
			try:
				rfd, wfd = tuple(exists_fds.split('=')[1].split(','))
			except:
				print("ERROR: jobserver has unrecoginzable format, was '{}'".format(exists_fds), file=sys.stderr)
				sys.exit(1)

			# read the token pipe to count number of available tokens and restore the pipe
			# this assumes the test suite script isn't invoked in parellel with something else
			tokens = os.read(int(rfd), 65536)
			os.write(int(wfd), tokens)

			# the number of tokens is off by one for obscure but well documented reason
			# see man make for more details
			options.jobs = len(tokens) + 1

		# do we have unlimited parallelism?
		elif jobopt and jobopt != "-j1":
			# check that this actually make sense
			if jobopt != "-j":
				print("ERROR: -j option passed by make but no {}, was '{}'".format(jobstr, jobopt), file=sys.stderr)
				sys.exit(1)

			options.jobs = eval_hardware()
			flags = prep_unlimited_recursive_make()


		# then no parallelism
		else:
			options.jobs = 1

		# keep all flags make passed along, except the weird 'w' which is about subdirectories
		flags = [f for f in make_flags if f != 'w']

	# Arguments are calling the shots, fake the top level make
	elif options.jobs :

		# make sure we have a valid number of jobs that corresponds to user input
		if options.jobs < 0 :
			print('ERROR: Invalid number of jobs', file=sys.stderr)
			sys.exit(1)

		flags = prep_recursive_make(options.jobs)

	# Arguments are calling the shots, fake the top level make, but 0 is a special case
	elif options.jobs == 0:
		options.jobs = eval_hardware()
		flags = prep_unlimited_recursive_make()

	# No one says to run in parallel, then don't
	else :
		options.jobs = 1
		flags = []

	# Make sure we call make as expected
	settings.update_make_cmd( flags )

	# return the job count
	return options.jobs

# enable core dumps for all the test children
resource.setrlimit(resource.RLIMIT_CORE, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))

################################################################################
#               misc
################################################################################

# get hash for given configuration
def config_hash():
	path = os.path.normpath(os.path.join(
		settings.SRCDIR,
	))

	distcc_hash = os.path.join(settings.SRCDIR, '../tools/build/distcc_hash')
	config = "%s-%s" % (settings.arch.target, settings.debug.path)
	_, out, _ = sh(distcc_hash, config, output_file=subprocess.PIPE, ignore_dry_run=True)
	return out.strip()

# get pretty string for time of day
def pretty_now():
	ts = time.time()
	print(ts, file=sys.stderr)
	return datetime.datetime.fromtimestamp(ts).strftime('%Y-%m-%d_%H:%M:%S')

# check if arguments is yes or no
def yes_no(string):
	if string == "yes" :
		return True
	if string == "no" :
		return False
	raise argparse.ArgumentTypeError(msg)

# Convert a function that converts a string to one that converts comma separated string.
def comma_separated(elements):
    return lambda string: [elements(part) for part in string.split(',')]

def fancy_print(text):
	column = which('column')
	if column:
		subprocess.run(column, input=bytes(text + "\n", "UTF-8"))
	else:
		print(text)


def core_info(path):
	if not os.path.isfile(path):
		return 1, "ERR Executable path is wrong"

	cmd   = os.path.join(settings.SRCDIR, "pybin/print-core.gdb")
	if not os.path.isfile(cmd):
		return 1, "ERR Printing format for core dumps not found"

	core  = os.path.join(os.getcwd(), "core" )

	if not os.path.isfile(core):
		return 1, "ERR No core dump, expected '{}' (limit soft: {} hard: {})".format(core, *resource.getrlimit(resource.RLIMIT_CORE))

	try:
		ret, out, err = sh('gdb', '-n', path, core, '-batch', '-x', cmd, output_file=subprocess.PIPE)
		if ret == 0:
			return 0, out
		else:
			return 1, err
	except:
		return 1, "ERR Could not read core with gdb"

def core_archive(dst, name, exe):
	# Get the core dump
	core = os.path.join(os.getcwd(), "core" )

	# update the path for this test
	dst  = os.path.join(dst, name)

	# make a directory for this test
	# mkdir makes the parent directory only so add a dummy
	mkdir( os.path.join(dst, "core") )

	# moves the files
	mv( core, os.path.join(dst, "core" ) )
	mv( exe , os.path.join(dst, "exe"  ) )

	# return explanatory test
	return "Archiving %s (executable and core) to %s" % (os.path.relpath(exe, settings.BUILDDIR), os.path.relpath(dst, settings.original_path))

class Timed:
	def __enter__(self):
		self.start = time.time()
		return self

	def __exit__(self, *args):
		self.end = time.time()
		self.duration = self.end - self.start

def timed(src, timeout):
	expire = time.time() + timeout
	i = iter(src)
	with contextlib.suppress(StopIteration):
		while True:
			yield i.next(max(expire - time.time(), 0))

def fmtDur( duration ):
	if duration :
		hours, rem = divmod(duration, 3600)
		minutes, rem = divmod(rem, 60)
		seconds, millis = divmod(rem, 1)
		return "%2d:%02d.%03d" % (minutes, seconds, millis * 1000)
	return " n/a"