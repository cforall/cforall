
m4_define([M4CFA_PARSE_PREFIX], [
	if test "x$prefix" = "xNONE"; then
		cfa_prefix=${ac_default_prefix}
	else
		cfa_prefix=${prefix}
	fi
	cfa_prefix="$(readlink -m ${cfa_prefix})/"
	AC_DEFINE_UNQUOTED(CFA_PREFIX, "${cfa_prefix}", [Location of cfa install.])
	AC_SUBST(CFA_PREFIX, ${cfa_prefix})

	if test "$includedir" = '${prefix}/include'; then
		cfa_incdir="${cfa_prefix}include/${cfa_name}"
	else
		cfa_incdir=${includedir}
	fi
	cfa_incdir="$(readlink -m ${cfa_incdir})/"
	AC_DEFINE_UNQUOTED(CFA_INCDIR, "${cfa_incdir}", [Location of include files.])
	AC_SUBST(CFA_INCDIR, ${cfa_incdir})

	if test "$bindir" = '${exec_prefix}/bin'; then
		cfa_bindir="${cfa_prefix}bin"
	else
		cfa_bindir=${bindir}
	fi
	cfa_bindir="$(readlink -m ${cfa_bindir})/"
	AC_DEFINE_UNQUOTED(CFA_BINDIR, "${cfa_bindir}", [Location of cfa command.])
	AC_SUBST(CFA_BINDIR, ${cfa_bindir})

	if test "$libdir" = '${exec_prefix}/lib'; then
		if test "${ARCHITECTURE}" != ""; then
			cfa_libdir="${cfa_prefix}lib/${cfa_name}/${ARCHITECTURE}-${CONFIGURATION}/"
		else
			cfa_libdir="${cfa_prefix}lib/${cfa_name}/"
		fi
	else
		cfa_libdir="${libdir}/${ARCHITECTURE}${CONFIGURATION}"
	fi
	cfa_libdir="$(readlink -m ${cfa_libdir})/"
	AC_DEFINE_UNQUOTED(CFA_LIBDIR, "${cfa_libdir}", [Location of cc1 and cfa-cpp commands.])
	AC_SUBST(CFA_LIBDIR, ${cfa_libdir})
])

m4_define([M4CFA_PROGRAM_NAME], [
	if test "${program_transform_name}" = ""; then
	AC_MSG_ERROR([Program transform not supported.
			Use --with-cfa-name='[[Desired name here]]' instead])
	fi

	#Define the new name of the installed command
	AC_ARG_WITH(cfa-name,
		[  --with-cfa-name=NAME     NAME too which cfa will be installed],
		cfa_name=$withval, cfa_name="cfa")

	AC_SUBST(CFA_NAME, ${cfa_name})
])

AC_DEFUN([M4CFA_CANNON_CPU], [
	case $1 in
		"host") arch_name=${host_cpu};;
		*) arch_name=$1;;
	esac

	case $arch_name in
		"x64"        ) cannon_arch_name="x64";;
		"x86-64"     ) cannon_arch_name="x64";;
		"x86_64"     ) cannon_arch_name="x64";;
		"aarch64"    ) cannon_arch_name="arm64";;
		"arm64"      ) cannon_arch_name="arm64";;
		"ARM64"      ) cannon_arch_name="arm64";;
		"x86"        ) cannon_arch_name="x86";;
		"i386"       ) cannon_arch_name="x86";;
		"i486"       ) cannon_arch_name="x86";;
		"i686"       ) cannon_arch_name="x86";;
		"Intel 80386") cannon_arch_name="x86";;
		"arm"        ) cannon_arch_name="arm32";;
		"ARM"        ) cannon_arch_name="arm32";;
		"arm32"      ) cannon_arch_name="arm32";;
		"ARM32"      ) cannon_arch_name="arm32";;
		"armv7l"     ) cannon_arch_name="arm32";;
		*)
		>&2 echo "Unknown architecture " $arch_name;
		exit 1
		;;
	esac
])

# http://git.savannah.gnu.org/gitweb/?p=autoconf-archive.git;a=blob_plain;f=m4/ax_check_compile_flag.m4
AC_DEFUN([M4CFA_CHECK_COMPILE_FLAG],
[AC_PREREQ(2.64)dnl for _AC_LANG_PREFIX and AS_VAR_IF
AS_VAR_PUSHDEF([CACHEVAR],[m4cfa_cv_check_[]_AC_LANG_ABBREV[]flags_$4_$1])dnl
AC_CACHE_CHECK([whether _AC_LANG compiler accepts $1], CACHEVAR, [
	m4cfa_check_save_flags=$[]_AC_LANG_PREFIX[]FLAGS
	_AC_LANG_PREFIX[]FLAGS="$[]_AC_LANG_PREFIX[]FLAGS $4 $1"
	AC_COMPILE_IFELSE([m4_default([$5],[AC_LANG_PROGRAM()])],
		[AS_VAR_SET(CACHEVAR,[yes])],
		[AS_VAR_SET(CACHEVAR,[no])])
	_AC_LANG_PREFIX[]FLAGS=$m4cfa_check_save_flags])
AS_VAR_IF(CACHEVAR,yes,
	[m4_default([$2], :)],
	[m4_default([$3], :)])
AS_VAR_POPDEF([CACHEVAR])dnl
])dnl M4CFA_CHECK_COMPILE_FLAGS
