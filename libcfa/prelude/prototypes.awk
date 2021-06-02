#
# Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
#
# The contents of this file are covered under the licence agreement in the
# file "LICENCE" distributed with Cforall.
#
# prototypes.awk --
#
# Author           : Peter A. Buhr
# Created On       : Sat May 16 07:57:37 2015
# Last Modified By : Peter A. Buhr
# Last Modified On : Sat Feb  8 09:46:58 2020
# Update Count     : 36
#

# http://llvm.org/svn/llvm-project/cfe/trunk/include/clang/Basic/Builtins.def

BEGIN {
	FS = "[( )]"
	# order so string search is longest string
	i=-1
	types[i+=1] = "BOOL";						vtypes[i] = "_Bool"
	types[i+=1] = "UINTMAX";					vtypes[i] = "unsigned long int"
	types[i+=1] = "UINT16";						vtypes[i] = "short int"
	types[i+=1] = "UINT32";						vtypes[i] = "int"
	types[i+=1] = "UINT64";						vtypes[i] = "long long int"
	types[i+=1] = "UINT";						vtypes[i] = "unsigned int"
	types[i+=1] = "INTMAX";						vtypes[i] = "long int"
	types[i+=1] = "INTPTR";						vtypes[i] = "int *"
	types[i+=1] = "WINT";						vtypes[i] = "unsigned int"
	types[i+=1] = "INT";						vtypes[i] = "int"
	types[i+=1] = "ULONGLONG";					vtypes[i] = "unsigned long long"
	types[i+=1] = "ULONG";						vtypes[i] = "unsigned long"
	types[i+=1] = "UNSIGNED";					vtypes[i] = "unsigned"
	types[i+=1] = "COMPLEX_LONGDOUBLE";			vtypes[i] = "_Complex long double"
	types[i+=1] = "COMPLEX_DOUBLE";				vtypes[i] = "_Complex double"
	types[i+=1] = "COMPLEX_FLOAT";				vtypes[i] = "_Complex float"
	types[i+=1] = "LONGDOUBLEPTR";				vtypes[i] = "long double *"
	types[i+=1] = "LONGDOUBLE";					vtypes[i] = "long double"
	types[i+=1] = "LONGLONG";					vtypes[i] = "long long"
	types[i+=1] = "LONG";						vtypes[i] = "long"
	types[i+=1] = "DFLOAT32";					vtypes[i] = "__Unsupported"
	types[i+=1] = "DFLOAT64";					vtypes[i] = "__Unsupported"
	types[i+=1] = "DFLOAT128";					vtypes[i] = "__Unsupported"
	types[i+=1] = "DOUBLEPTR";					vtypes[i] = "double *"
	types[i+=1] = "DOUBLE";						vtypes[i] = "double"
	types[i+=1] = "FLOATPTR";					vtypes[i] = "float *"
	types[i+=1] = "FLOAT128X";					vtypes[i] = "__Unsupported"
	types[i+=1] = "FLOAT128";					vtypes[i] = "__Unsupported"
	types[i+=1] = "FLOAT64X";					vtypes[i] = "__Unsupported"
	types[i+=1] = "FLOAT64";					vtypes[i] = "__Unsupported"
	types[i+=1] = "FLOAT32X";					vtypes[i] = "__Unsupported"
	types[i+=1] = "FLOAT32";					vtypes[i] = "__Unsupported"
	types[i+=1] = "FLOAT16";					vtypes[i] = "__Unsupported"
	types[i+=1] = "FLOAT";						vtypes[i] = "float"
	types[i+=1] = "CONST_VPTR";					vtypes[i] = "const volatile void *"
	types[i+=1] = "CONST_PTR";					vtypes[i] = "const void *"
	types[i+=1] = "CONST_STRING";				vtypes[i] = "const char *"
	types[i+=1] = "CONST_TM_PTR";				vtypes[i] = "const struct tm *"
	types[i+=1] = "PTR_FN_VOID_VAR_PTR_SIZE";	vtypes[i] = ""
	types[i+=1] = "PTR_CONST_STRING";			vtypes[i] = "char *const"
	types[i+=1] = "PTRMODE_PTR";				vtypes[i] = ""
	types[i+=1] = "PTRPTR";						vtypes[i] = "void **"
	types[i+=1] = "VPTR";						vtypes[i] = "volatile void *"
	types[i+=1] = "PTR";						vtypes[i] = "void *"
	types[i+=1] = "VOID";						vtypes[i] = "void"
	types[i+=1] = "STRING";						vtypes[i] = "char *"
	types[i+=1] = "FILEPTR";					vtypes[i] = "struct _IO_FILE *"
	types[i+=1] = "SIZE";						vtypes[i] = "unsigned long"
	types[i+=1] = "VAR";						vtypes[i] = "..."
	types[i+=1] = "VALIST_ARG";					vtypes[i] = "__builtin_va_list"
	types[i+=1] = "VALIST_REF";					vtypes[i] = "__builtin_va_list"
	types[i+=1] = "UNWINDWORD";					vtypes[i] = "void *"
	types[i+=1] = "WORD";						vtypes[i] = ""
	types[i+=1] = "SSIZE";						vtypes[i] = "long int"
	types[i+=1] = "PID";						vtypes[i] = "int"
	types[i+=1] = "I16";						vtypes[i] = "__int128"
	types[i+=1] = "I8";							vtypes[i] = "long long int"
	types[i+=1] = "I4";							vtypes[i] = "int"
	types[i+=1] = "I2";							vtypes[i] = "short"
	types[i+=1] = "I1";							vtypes[i] = "char"
	N = i + 1
} # BEGIN

/BT_FN/ {
	for (i = 1; i <= NF; i += 1 ) {
		if ( match($i, "BT_FN") != 0 ) {
			prototypes[$i] = $i
		}
	}
}

END {
	printf( "#define DEF_BUILTIN(ENUM, NAME, CLASS, TYPE, LIBTYPE, BOTH_P, FALLBACK_P, NONANSI_P, ATTRS, IMPLICIT, COND) TYPE(NAME)\n" );
	printf( "#define FUNC_SIMPLE(RETURN, NAME, ARGS...) RETURN NAME(ARGS);\n" );
	printf( "#define BT_LAST(NAME) FUNC_SIMPLE(void, NAME)\n\n" );

	# generate C types for macros names
	for ( i = 0; i < N; i += 1 ) {
		printf( "#define BT_%s %s\n", types[i], vtypes[i] )
	} # for
	printf( "\n" )

	for ( prototype in prototypes ) {
		# printf( "//\"%s\"\n", prototype )
		if ( index( "BT_LAST", prototype ) == 1 ) {
			continue
		} # if

		printf( "#define %s(NAME) FUNC_SIMPLE(", prototype )

		if ( sub( "BT_FN_", "", prototype ) == 0 ) {
			printf( "\n********** BAD MACRO NAME \"%s\" **********\n", prototype )
			exit 0
		} # if

		# generate function return type as macro
		for ( t = 0; t < N; t += 1 ) {					# find longest match
			type = types[t];
			if ( index( prototype, type ) == 1 ) {		# found match
				printf( "BT_%s, NAME", type )
				sub( type, "", prototype )
				break;
			} # if
		} # for

		# generate function parameter types as macro
		if ( index( prototype, "VAR" ) != 2 ) {			# C-style empty parameters ?
			for ( p = 0; length( prototype ) > 0; p += 1 ) { # until all parameters types are removed
				sub( "_", "", prototype)				# remove "_"
				printf( ", ", type )
				temp = prototype
				for ( t = 0; t < N; t += 1 ) {			# find longest match
					type = types[t];
					if ( index( prototype, type ) == 1 ) { # found match
						printf( "BT_%s", type )
						sub( type, "", prototype )
						break;
					} # if
				} # for
				if ( temp == prototype ) {				# no match found for parameter in macro table
					printf( "\n********** MISSING TYPE \"%s\" **********\n", prototype )
					exit 0
				} # if
			} # for
		} # if
		printf( ")\n" )
	} # for

	# extras
	printf( "\n#include \"builtins.def\"\n\n" );
	printf( "\n#include \"sync-builtins.cf\"\n\n" );
	printf( "extern const char *__PRETTY_FUNCTION__;\n" );
	printf( "float _Complex __builtin_complex( float, float );\n" );
	printf( "double _Complex __builtin_complex( double, double );\n" );
	printf( "long double _Complex __builtin_complex( long double, long double );\n" );
} # END

# Local Variables: #
# tab-width: 4 #
# mode: awk #
# compile-command: "make install" #
# End: #
