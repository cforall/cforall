#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([cfa-cc],[1.0.0],[cforall@plg.uwaterloo.ca])
AC_CONFIG_AUX_DIR([automake])
AC_CONFIG_MACRO_DIRS([automake])
AC_CONFIG_HEADERS([config.h:src/config.h.in])
AM_SILENT_RULES([yes])

m4_include([tools/build/cfa.m4])

# don't use the default CFLAGS as they unconditonnaly add -O2
: ${CFLAGS=""}
: ${CXXFLAGS=""}

AM_INIT_AUTOMAKE([subdir-objects])

# Allow program name tansformation
# will fill program_transform_name with appropriate sed regex
AC_ARG_PROGRAM

#==============================================================================
#Trasforming cc1 will break compilation
M4CFA_PROGRAM_NAME

#==============================================================================
# New AST toggling support
AH_TEMPLATE([CFA_USE_NEW_AST],[Sets whether or not to use the new-ast, this is adefault value and can be overrided by --old-ast and --new-ast])
DEFAULT_NEW_AST="True"
AC_ARG_ENABLE(new-ast,
	[  --enable-new-ast     whether or not to use new ast as the default AST algorithm],
	[case "${enableval}" in
		yes) newast=true ; DEFAULT_NEW_AST="True"  ;;
		no)  newast=false; DEFAULT_NEW_AST="False" ;;
		*) AC_MSG_ERROR([bad value ${enableval} for --enable-new-ast]) ;;
	esac],[newast=true])
AC_DEFINE_UNQUOTED([CFA_USE_NEW_AST], $newast)
AC_SUBST(DEFAULT_NEW_AST)

#==============================================================================
# version information

rm -f version
echo ${PACKAGE_VERSION} > version		# file containing version number for other tools
chmod ugo-w version
ver_major=`cut -d '.' -f1 version`		# subdivide version number into components at periods
ver_minor=`cut -d '.' -f2 version`
ver_patch=`cut -d '.' -f3 version`
ver_build=`cut -d '.' -f4 version`

# AC_SUBST([CONFIG_STATUS_DEPENDENCIES], ['$(top_srcdir)/version'])
AC_DEFINE_UNQUOTED(CFA_VERSION_MAJOR, ${ver_major}, [Major version number.])
AC_DEFINE_UNQUOTED(CFA_VERSION_MINOR, ${ver_minor}, [Minor version number.])
AC_DEFINE_UNQUOTED(CFA_VERSION_PATCH, ${ver_patch}, [Patch version number.])
AC_DEFINE_UNQUOTED(CFA_VERSION_BUILD, ${ver_build}, [Build version number.])
AC_DEFINE_UNQUOTED(CFA_VERSION_SHORT, ["${ver_major}"], [Major])
AC_DEFINE_UNQUOTED(CFA_VERSION, ["${ver_major}.${ver_minor}"], [Major.Minor])
AC_DEFINE_UNQUOTED(CFA_VERSION_LONG, ["${ver_major}.${ver_minor}.${ver_patch}"], [Major.Minor.Patch])
AC_DEFINE_UNQUOTED(CFA_VERSION_FULL, ["${ver_major}.${ver_minor}.${ver_patch}.${ver_build}"], [Major.Minor.Patch.Build])

#==============================================================================
# HACK to be able to use conditionnals inside makefiles
DOifskipcompile='ifeq ($(skipcompile),yes)
else'
AC_SUBST([DOifskipcompile])
AM_SUBST_NOTMAKE([DOifskipcompile])

DOendif='endif'
AC_SUBST([DOendif])
AM_SUBST_NOTMAKE([DOendif])

#==============================================================================
# distcc support

AC_ARG_ENABLE(distcc,
	[  --enable-distcc     whether or not to enable distributed compilation],
	enable_distcc=$enableval, enable_distcc=no)

AC_ARG_WITH(bwlimit,
	[  --with-bwlimit=RATE     RATE the maximum rate at which rsync will be limited when using distributed builds],
	[], [])

AM_CONDITIONAL([ENABLE_DISTCC], [test x$enable_distcc = xyes])
HAS_DISTCC="False"

if test x$enable_distcc = xyes; then
	CXX="distcc ${CXX}"
	LD="distcc ${LD} -lstdc++"
	HAS_DISTCC="True"
	echo "Enabling distributed builds"
fi

AC_SUBST(CXX)
AC_SUBST(LD)
AC_SUBST(HAS_DISTCC)

#==============================================================================
# Installation paths
M4CFA_PARSE_PREFIX

#==============================================================================
# Create variables for commonly used targets

TOP_SRCDIR="$(readlink -e $ac_abs_confdir/)/"
TOP_BUILDDIR="$(readlink -e $ac_pwd/)/"

AC_DEFINE_UNQUOTED(TOP_SRCDIR, "$TOP_SRCDIR", [Top src directory])
AC_DEFINE_UNQUOTED(TOP_BUILDDIR, "$TOP_BUILDDIR", [Top build directory])

DRIVER_DIR=${TOP_BUILDDIR}driver/
CFACC=${DRIVER_DIR}cfa
CFACC_INSTALL=${CFA_BINDIR}${CFA_NAME}
CFACPP=${DRIVER_DIR}cfa-cpp
AC_SUBST(DRIVER_DIR)
AC_SUBST(CFACC)
AC_SUBST(CFACC_INSTALL)
AC_SUBST(CFACPP)

#==============================================================================
# Flag variables needed to build in tree
LIBCFA_SRC='${TOP_SRCDIR}/libcfa/src'
BUILD_IN_TREE_FLAGS="-XCFA -t -B${DRIVER_DIR}"
AC_SUBST(BUILD_IN_TREE_FLAGS)

#==============================================================================
# handle the list of hosts to build for
for var in $ac_configure_args
do
	#strip quotes surrouding values
	case $var in
		# skip cross compilation related arguments
		\'--host=*) ;; \'host_alias=*) ;; \'--build=*) ;; \'build_alias=*) ;; \'--target=*) ;; \'target_alias=*) ;;

		# skip the target hosts
		\'--with-target-hosts=*) ;;

		# skip gprofiler for libcfa
		\'--enable-gprofiler=*) ;;
		\'--disable-gprofiler) ;;

		# skip the target hosts
		\'--enable-new-ast=*) ;;
		\'--disable-new-ast) ;;

		# skip this, it only causes problems
		\'--srcdir=*) ;;

		# append all other arguments to the sub configure arguments
		*) LIBCFA_GENERAL_ARGS="${LIBCFA_GENERAL_ARGS} $var";;
	esac
done

#==============================================================================
# handle the list of hosts to build for
AC_CANONICAL_BUILD
AC_CANONICAL_HOST

if ! test "$host_cpu" = "$build_cpu"; then
	case $host_cpu in
		i386)
			HOST_FLAGS="-m32"
			;;
		i686)
			HOST_FLAGS="-m32"
			;;
		x86_64)
			HOST_FLAGS="-m64"
			;;
	esac
fi
AC_SUBST(HOST_FLAGS)

default_target="${host_cpu}:debug, ${host_cpu}:nodebug"
AC_ARG_WITH(target-hosts,
	[  --with-target-hosts=HOSTS     HOSTS comma seperated list of hosts to build for, format ARCH:[debug|nodebug|nolib]],
	target_hosts=$withval, target_hosts=${default_target})

AC_ARG_ENABLE(gprofiler,
	[  --enable-gprofiler     whether or not to enable gprofiler tools (if available)],
	enable_gprofiler=$enableval, enable_gprofiler=yes)

AC_ARG_ENABLE(demangler,
	[  --enable-demangler     whether or not to build the demangler (executable and library)],
	enable_demangler=$enableval, enable_demangler=yes)

AC_SUBST(TARGET_HOSTS, ${target_hosts})

LIBCFA_PATHS="DRIVER_DIR=${DRIVER_DIR}"

for i in $(echo $target_hosts | sed "s/,/ /g")
do
	# call your procedure/other scripts here below
	arch_name=$(echo $i | sed -r "s/:(.*)//g")
	lib_config=$(echo $i | sed -r "s/(.*)://g")

	case $lib_config in
		"nodebug") ;;
		"debug") ;;
		"nolib") ;;
		"profile") ;;
		*)
			>&2 echo "Configuration must be 'debug', 'nodebug' or 'nolib'"
			exit 1
		;;
	esac

	M4CFA_CANNON_CPU([${arch_name}])
	lib_arch=${cannon_arch_name}
	lib_dir="libcfa/${lib_arch}-${lib_config}"

	LIBCFA_TARGET_DIRS="${LIBCFA_TARGET_DIRS} ${lib_dir}"
	LIBCFA_1TARGET_DIR="${lib_dir}"
	LIBCFA_TARGET_MAKEFILES="${LIBCFA_TARGET_MAKEFILES} ${lib_dir}/Makefile"

	mkdir -p ${lib_dir}
	echo -n "${LIBCFA_GENERAL_ARGS} " > ${lib_dir}/config.data
	echo -n "${LIBCFA_PATHS} " >> ${lib_dir}/config.data
	echo -n "ARCHITECTURE=${lib_arch} " >> ${lib_dir}/config.data
	echo -n "CONFIGURATION=${lib_config} " >> ${lib_dir}/config.data
	echo -n "CFA_VERSION=${ver_major}:${ver_minor}:${ver_patch}" >> ${lib_dir}/config.data
done

AC_SUBST(LIBCFA_TARGET_DIRS)
AC_SUBST(LIBCFA_1TARGET_DIR)
AC_SUBST(LIBCFA_TARGET_MAKEFILES)

M4CFA_CANNON_CPU([${host_cpu}])
AC_DEFINE_UNQUOTED(CFA_DEFAULT_CPU, "$cannon_arch_name", [Default cpu to use if neither -m32 or -m64 are defined.])
AC_DEFINE_UNQUOTED(CFA_64_CPU, "x64", [CPU to use if the -m64 flags is given.])
AC_DEFINE_UNQUOTED(CFA_32_CPU, "x86", [CPU to use if the -m32 flags is given.])

#==============================================================================
# CAFLAGS
AC_DEFINE_UNQUOTED(CFA_FLAGS, "${CFAFLAGS}", [compilation flags for cfa libraries and test programs.])
AC_SUBST(CFA_FLAGS, ${CFAFLAGS})

#==============================================================================
# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AM_PROG_AS
# These are often not installed and people miss seeing the "no", so stop the configure.
AC_PROG_YACC
if test "${YACC}" = "yacc" ; then echo "Error: bison required." ; exit 1 ; fi
AC_PROG_LEX
if test "${LEX}" = "lex" ; then echo "Error: flex required." ; exit 1 ; fi
AC_PROG_LIBTOOL
AC_PROG_INSTALL

# Checks for libraries.
AC_CHECK_LIB([fibre], [Fibre::yield], [HAVE_LIBFIBRE=1], [HAVE_LIBFIBRE=0])
AM_CONDITIONAL([WITH_LIBFIBRE], [test "$HAVE_LIBFIBRE" -eq 1])

AC_CHECK_LIB([profiler], [ProfilingIsEnabledForAllThreads], [HAVE_LIBPROFILER=1], [HAVE_LIBPROFILER=0])
AM_CONDITIONAL([WITH_LIBPROFILER], [test "x$enable_gprofiler" = "xyes" -a "$HAVE_LIBPROFILER" -eq 1])

AC_CHECK_LIB([tcmalloc], [malloc], [HAVE_LIBTCMALLOC=1], [HAVE_LIBTCMALLOC=0])
AM_CONDITIONAL([WITH_LIBTCMALLOC], [test "x$enable_gprofiler" = "xyes" -a "$HAVE_LIBTCMALLOC" -eq 1])

# conditionnally build the demangler
if test "x$enable_demangler" == xyes; then
	LIBDEMANGLE="libdemangle.a"
	DEMANGLER="demangler"
else
	LIBDEMANGLE=""
	DEMANGLER=""
fi
AC_SUBST([LIBDEMANGLE])
AC_SUBST([DEMANGLER])

# Checks for header files.
AC_CHECK_HEADERS([libintl.h malloc.h unistd.h], [], [echo "Error: Missing required header"; exit 1])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_TYPES([_Float32], AC_DEFINE([HAVE_KEYWORDS_FLOATXX], [], [Have keywords _FloatXX.]), [], [[]])

# Checks for compiler flags.
M4CFA_CHECK_COMPILE_FLAG([-Wcast-function-type], AC_DEFINE([HAVE_CAST_FUNCTION_TYPE], [], [Have compiler warning cast-function-type.]))

#==============================================================================
# backend compiler implementation
AC_DEFINE_UNQUOTED(CFA_BACKEND_CC, "${CC}", [Backend compiler to use.])
AC_SUBST(CFA_BACKEND_CC)

#==============================================================================
AC_CONFIG_FILES([
	Makefile
	driver/Makefile
	src/Makefile
	libcfa/Makefile:libcfa/Makefile.dist.in
	tests/Makefile
	])

# Some of our makefile don't need to be distributed
AM_CONDITIONAL([CFORALL_DISTRIBUTE], [test -e $TOP_SRCDIR/autogen.sh])
AM_COND_IF([CFORALL_DISTRIBUTE], [
	AC_CONFIG_FILES([
		longrun_tests/Makefile
		benchmark/Makefile
		benchmark/io/http/Makefile
		tools/Makefile
		tools/prettyprinter/Makefile
	])

	AC_OUTPUT(benchmark/Cargo.toml)
])

AC_CONFIG_LINKS([tests/test.py:tests/test.py])

AC_OUTPUT(tests/config.py)

# Final text
AC_MSG_RESULT(Cforall configuraton completed. Type "make -j 8 install".)
