s/; /;\n/g  				#Make multi declaration lines into multiple lines
/targetm/s/.*//				#Remove targetm declarations
/__Unsupported/s/.*//			#Remove Unsupported types declarations
s/void \(const char \*\)0\(\);//	#Remove void (const char \*)0();
s/\"//g 					#Remove extraenous quotes in declarations
/__builtin_/s/_ /_/g			#Remove extraenous spaces in declarations

#Fix gcc overloading
# various sed rules for the gcc sync builtins which are overloaded
# kept here because they generate an acceptable approximate of the correct prototypes

#/__sync_/s/_[0-9][0-9]*\(.*\)/\(\);/g	#hack since it will accept any parameters
#/__atomic_/s/_[0-9][0-9]*\(.*\)/\(\);/g	#hack since it will accept any parameters

#/_16/s/void \*/__int128 \*/g
#/_8/s/void \*/long long int \*/g
#/_4/s/void \*/int \*/g
#/_2/s/void \*/short \*/g
#/_1/s/void \*/char \*/g

#s/([a-zA-Z0-9_ ]+)\s+__sync([a-z_]+)_([0-9]+)\((.*)\);/\1 __sync\2\(\4\,...); \1 __sync\2_\3\(\4\,...);/
#s/([a-zA-Z0-9_ ]+)\s+__atomic([a-z_]+)_([0-9]+)\((.*)\);/\1 __atomic\2\(\4\); \1 __atomic\2_\3\(\4\);/
