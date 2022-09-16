import os
import subprocess
import sys
from . import tools

global original_path

try :
	original_path = os.getcwd()
	testpath = os.path.dirname(os.path.abspath(os.path.join(os.getcwd(), sys.argv[0])))
	sys.path.append(testpath)
	import config

	SRCDIR = os.path.abspath(config.SRCDIR)
	BUILDDIR = os.path.abspath(config.BUILDDIR)
	distribute = config.DISTRIBUTE
	os.chdir(testpath)

except:
	print('ERROR: missing config.py, re-run configure script.', file=sys.stderr)
	sys.exit(1)

class Architecture:
	KnownArchitectures = {
		'x64'         : 'x64',
		'x86-64'      : 'x64',
		'x86_64'      : 'x64',
		'x86'         : 'x86',
		'aarch64'     : 'arm64',
		'arm64'       : 'arm64',
		'ARM64'       : 'arm64',
		'i386'        : 'x86',
		'i486'        : 'x86',
		'i686'        : 'x86',
		'Intel 80386' : 'x86',
		'arm'         : 'arm32',
		'ARM'         : 'arm32',
		'arm32'       : 'arm32',
		'ARM32'       : 'arm32',
	}

	CrossCompileFlags = {
		'x64'  : 'ARCH_FLAGS=-m64',
		'x86'  : 'ARCH_FLAGS=-m32',
		'arm64': 'ARCH_FLAGS=',
		'arm32': 'ARCH_FLAGS=',
	}

	def __init__(self, arch):
		try:
			canonical_host = Architecture.make_canonical( config.HOSTARCH )
		except KeyError:
			print("Unknown host architecture %s" % config.HOSTARCH, file=sys.stderr)
			sys.exit(1)

		if arch:
			try:
				arch = Architecture.make_canonical( arch )
			except KeyError:
				print("Unknown architecture %s" % arch, file=sys.stderr)
				sys.exit(1)

		if arch and arch != canonical_host:
			self.target = arch
			self.cross_compile = True
		else:
			self.target = canonical_host
			self.cross_compile = False


		try :
			self.flags = Architecture.CrossCompileFlags[self.target]
		except KeyError:
			print("Cross compilation not available for architecture %s" % self.target, file=sys.stderr)
			sys.exit(1)

		self.string = self.target

	def update(self):
		if not self.cross_compile:
			self.target = machine_default()
			self.string = self.target
			print("updated to %s" % self.target)

	def filter(self, tests):
		return [test for test in tests if not test.arch or self.target == test.arch]

	@staticmethod
	def make_canonical(arch):
		return Architecture.KnownArchitectures[arch]


class Debug:
	def __init__(self, value):
		self.string = "debug" if value else "no debug"
		self.flags  = """DEBUG_FLAGS=%s""" % ("-debug -O0" if value else "-nodebug -O2")
		self.path   = "debug" if value else "nodebug"

class Install:
	def __init__(self, value):
		if value:
			distribute = False

		self.string = "installed" if value else "in tree"
		self.flags  = """installed=%s""" % ("yes" if value else "no")

class Timeouts:
	def __init__(self, ts, tg):
		self.single = Timeouts.check(ts)
		self.total  = Timeouts.check(tg)

	@staticmethod
	def check(value):
		if value < 1:
			print("Timeouts must be at least 1 second", file=sys.stderr)
			sys.exit(1)

		return value

def init( options ):
	global all_arch
	global all_debug
	global all_install
	global arch
	global debug
	global archive
	global install

	global continue_
	global dry_run
	global generating
	global make
	global make_jobfds
	global output_width
	global timeout
	global timeout2gdb

	all_arch     = [Architecture(o) for o in list(dict.fromkeys(options.arch   ))] if options.arch else [Architecture(None)]
	all_debug    = [Debug(o)        for o in list(dict.fromkeys(options.debug  ))]
	all_install  = [Install(o)      for o in list(dict.fromkeys(options.install))]
	archive      = os.path.abspath(os.path.join(original_path, options.archive_errors)) if options.archive_errors else None
	continue_    = options.continue_
	dry_run      = options.dry_run # must be called before tools.config_hash()
	generating   = options.regenerate_expected
	make         = ['make']
	make_jobfds  = []
	output_width = 24
	timeout      = Timeouts(options.timeout, options.global_timeout)
	timeout2gdb  = options.timeout_with_gdb

	# if we distribute, distcc errors will fail tests, use log file for distcc
	# don't use "'DISTCC_LOG' not in os.environ" because it can be set to ''
	if distribute and not os.environ.get('DISTCC_LOG'):
		os.putenv('DISTCC_LOG', os.path.join(BUILDDIR, 'distcc_error.log'))

def update_make_cmd(flags):
	global make
	make = ['make', *flags]

def update_make_fds(r, w):
	global make_jobfds
	make_jobfds = (r, w)

def validate():
	"""Validate the current configuration and update globals"""

	global distcc
	distcc       = "DISTCC_CFA_PATH=~/.cfadistcc/%s/cfa" % tools.config_hash()
	make_ret, out, err = tools.make( ".validate", output_file=subprocess.PIPE, error=subprocess.PIPE )
	if make_ret != 0:
		print("ERROR: Invalid configuration %s:%s" % (arch.string, debug.string), file=sys.stderr)
		print("       verify returned : \n%s" % err, file=sys.stderr)
		sys.exit(1)

def prep_output(tests):
	global output_width
	output_width = max(map(lambda t: len(t.target()), tests))
	# 35 is the maximum width of the name field before we get line wrapping.
	output_width = min(output_width, 35)
