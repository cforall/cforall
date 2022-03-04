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
def sh(*cmd, timeout = False, output_file = None, input_file = None, input_text = None, error = subprocess.STDOUT, ignore_dry_run = False):
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
			return 0, None

		with contextlib.ExitStack() as onexit:
			# add input redirection if needed
			input_file = openfd(input_file, 'r', onexit, True)

			# add output redirection if needed
			output_file = openfd(output_file, 'w', onexit, False)

			# add error redirection if needed
			error = openfd(error, 'w', onexit, False)

			# run the desired command
			# use with statement to make sure proc is cleaned
			# don't use subprocess.run because we want to send SIGABRT on exit
			with subprocess.Popen(
				cmd,
				**({'input' : bytes(input_text, encoding='utf-8')} if input_text else {'stdin' : input_file}),
				stdout  = output_file,
				stderr  = error
			) as proc:

				try:
					out, _ = proc.communicate(
						timeout = settings.timeout.single if timeout else None
					)

					return proc.returncode, out.decode("latin-1") if out else None
				except subprocess.TimeoutExpired:
					if settings.timeout2gdb:
						print("Process {} timeout".format(proc.pid))
						proc.communicate()
						return 124, str(None)
					else:
						proc.send_signal(signal.SIGABRT)
						proc.communicate()
						return 124, str(None)

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

	code, out = sh("file", fname, output_file=subprocess.PIPE)
	if code != 0:
		return (False, "'file EXPECT' failed with code {}".format(code))

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
		settings.ast.flags,
		settings.arch.flags,
		settings.debug.flags,
		settings.install.flags,
		settings.distcc if settings.distribute else None,
		flags,
		target
	]
	cmd = [s for s in cmd if s]
	return sh(*cmd, output_file=output_file, error=error)

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
	ret, _ = sh("mv", source, dest)
	return ret

# cat one file into the other
def cat(source, dest):
	ret, _ = sh("cat", source, output_file=dest)
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
# count number of jobs to create
def job_count( options ):
	# check if the user already passed in a number of jobs for multi-threading
	if not options.jobs:
		make_flags = os.environ.get('MAKEFLAGS')
		force = bool(make_flags)
		make_jobs_fds = re.search("--jobserver-(auth|fds)=\s*([0-9]+),([0-9]+)", make_flags) if make_flags else None
		if make_jobs_fds :
			tokens = os.read(int(make_jobs_fds.group(2)), 1024)
			options.jobs = len(tokens)
			os.write(int(make_jobs_fds.group(3)), tokens)
		else :
			if settings.distribute:
				ret, jstr = sh("distcc", "-j", output_file=subprocess.PIPE, ignore_dry_run=True)
				if ret == 0:
					options.jobs = int(jstr.strip())
				else :
					options.jobs = multiprocessing.cpu_count()
			else:
				options.jobs = multiprocessing.cpu_count()
	else :
		force = True

	# make sure we have a valid number of jobs that corresponds to user input
	if options.jobs <= 0 :
		print('ERROR: Invalid number of jobs', file=sys.stderr)
		sys.exit(1)

	return options.jobs, force

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
	_, out = sh(distcc_hash, config, output_file=subprocess.PIPE, ignore_dry_run=True)
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
		return 1, "ERR No core dump (limit soft: {} hard: {})".format(*resource.getrlimit(resource.RLIMIT_CORE))

	try:
		return sh('gdb', '-n', path, core, '-batch', '-x', cmd, output_file=subprocess.PIPE)
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