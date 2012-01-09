: mmap 9 syscall6 ;
: initblocks 0 3 1 3 1000 0 mmap ;
: page 0 0 62 3 1000 0 mmap ;

: ||data 'dhere a! @ swap align !
: data create postpone dup 058d48 3, dhere rel32, postpone ;; ;
: allot 'dhere a! @ + ! ;
: d, dhere a! ! 8 allot ;
: qfill swap a! swap for dup !+ next drop ;
: >qwords 2/ 2/ 2/ ;

20 constant spacebar
7f constant backspace
