0 constant [os]`
: `cr ."^M^J" ; `cr ' cr !^
"kernel32.dll" lib: k32
: k32.` wsparse k32 lit` #call ' call, ;
62 open' 1+ "FFROOT" 3_ k32. GetEnvironmentVariableA open' c!
"'ff.ff"
2dup openr close 0- drop 0< IF "^Fc:\\ff\\" open' place drop
2dup openr close 0- drop 0< IF 0 open' !
2dup openr close 0- drop 0< IF 2drop "[os]" THEN THEN THEN
needed ' ; `boot ' >r `exec ;
