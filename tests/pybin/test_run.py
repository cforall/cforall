import os

from pybin.tools import *

import pybin.settings

# Test class that defines what a test is
class Test:
	def __init__(self):
		self.name = ''
		self.path = ''
		self.arch = ''
		self.astv = ''

	def toString(self):
		return "{:25s} ({:5s} arch, {:s} ast: {:s})".format( self.name, self.arch if self.arch else "Any", self.astv if self.astv else "Any", self.target() )

	def prepare(self):
		mkdir( (self.output_log(), self.error_log(), self.input()            ) )
		rm   ( (self.output_log(), self.error_log(), self.target_executable()) )

	def expect(self):
		arch = '' if not self.arch else ".%s" % self.arch
		astv = '' if not self.astv else ".nast" if self.astv == "new" else ".oast"
		return os.path.normpath( os.path.join(settings.SRCDIR  , self.path, ".expect", "%s%s%s.txt" % (self.name,astv,arch)) )

	def error_log(self):
		return os.path.normpath( os.path.join(settings.BUILDDIR, self.path, ".err"   , "%s.log" % self.name) )

	def output_log(self):
		return os.path.normpath( os.path.join(settings.BUILDDIR, self.path, ".out"   , "%s.log" % self.name) )

	def input(self):
		return os.path.normpath( os.path.join(settings.SRCDIR  , self.path, ".in"    , "%s.txt" % self.name) )

	def target_output(self):
		return self.output_log() if not settings.generating else self.expect()

	def target(self):
		return os.path.normpath( os.path.join(self.path, self.name) )

	def target_executable(self):
		return os.path.normpath( os.path.join(settings.BUILDDIR, self.path, self.name) )

	@staticmethod
	def valid_name(name):
		return not name.endswith( ('.c', '.cc', '.cpp', '.cfa') )

	@staticmethod
	def new_target(target, arch, astv):
		test = Test()
		test.name = os.path.basename(target)
		test.path = os.path.relpath (os.path.dirname(target), settings.SRCDIR)
		test.arch = arch.target if arch else ''
		test.astv = astv.target if astv else ''
		return test


class TestResult:
	SUCCESS = 0
	FAILURE = 1
	TIMEOUT = 124

	@classmethod
	def toString( cls, retcode, duration ):
		if settings.generating :
			if   retcode == TestResult.SUCCESS: 	key = 'pass'; text = "Done   "
			elif retcode == TestResult.TIMEOUT: 	key = 'time'; text = "TIMEOUT"
			else :	key = 'fail';	text = "ERROR code %d" % retcode
		else :
			if   retcode == TestResult.SUCCESS: 	key = 'pass'; text = "PASSED "
			elif retcode == TestResult.TIMEOUT: 	key = 'time'; text = "TIMEOUT"
			else :	key = 'fail';	text = "FAILED with code %d" % retcode

		text += "    C%s - R%s" % (fmtDur(duration[0]), fmtDur(duration[1]))
		return key, text
