1 constant [os]`
"libc.so.6" lib: libc
: libc.` wsparse libc lit` #call ' call, ;
"/ff/" drop "HOME" 1_ libc. getenv
open' 1+ 2 libc. strcpy 2 libc. strcat 1 libc. strlen open' c!
"'ff.ff" 
2dup openr close 0- drop 0< IF 0 open' !
2dup openr close 0- drop 0< IF 2drop "[os]" THEN THEN
needed ' ; `boot ' >r `exec ;
