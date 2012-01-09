create : ] ] [
macro create : ] create ;

forth : call, e8 1, rel32, ;
macro : postpone '' call, ;

: swap 0e894891480e8b48 , ;
: drop ad48 2, ;
: nip 08768d48 4, ;
: over postpone dup 08468b48 4, ;
: if 74 2, here ;


: a> : a postpone dup d08948 3, ;
: >a : a! c28948 3, postpone drop ;
: ! 028948 3, postpone drop ;
: c! 0288 2, postpone drop ;
: ca+ 01528d48 4, ;
: a+ 08528d48 4, ;
: !+ postpone ! postpone a+ ;
: c!+ postpone c! postpone ca+ ;
: c@ 028a 2, c0b60f 3, ;
: c@+ 028a 2, c0b60f 3, postpone ca+ ;
: @ postpone dup 028b48 3, ;
: @t 008b48 3, ;
: c@t 008a 2, c0b60f 3, ;
: +! 020148 3, postpone drop ;

: not d0f748 3, ;
: + 060348 3, postpone nip ;
: and 062348 3, postpone nip ;
: or 060b48 3, postpone nip ;
: xor 063348 3, postpone nip ;
: test c08548 3, ;
: neg d8f748 3, ;
: 2* e0d148 3, ;
: 2/ f8d148 3, ;

: >r : push 50 1, postpone drop ;
: r> : pop postpone dup 58 1, ;
: for postpone push here ;
: next 240cff48 4, 75 1, here not + 1, 08c48348 4, ;
: i postpone dup 24048b48 4, ;

: */ c88b48 3, postpone drop 2ef748 3, f9f748 3, postpone nip ;
: * 06af0f48 4, postpone nip ;

: .s' .s ;
: >rcx c18948 3, ;
: jmp postpone >rcx postpone drop e1ff 2, ;
: ijmp postpone >rcx postpone drop 21ff 2, ;
: execute postpone >rcx postpone drop d1ff 2, ;
: min postpone >rcx postpone drop c83b48 3, c14c0f48 4, ;
: max postpone >rcx postpone drop c83b48 3, c14f0f48 4, ;

forth
: */ */ ; : * * ; : / 1 swap */ ; : rem / drop a ;
: + + ; : and and ; : or or ; : xor xor ; : neg neg ; : not not ;
: a a ; : a! a! ; : @ @ ; : ! ! ; : !+ !+ ; : @t @t ; : c@t c@t ;
: 2* 2* ; : 2/ 2/ ; : jmp jmp ; : ijmp ijmp ;
: dup dup ; : over over ; : swap swap ; : nip nip ; : drop drop ;
: min min ; : max max ;

: !t a! ! ;
: 1+ not neg ;
: 1- neg not ;
: align neg swap over not + and ;
: align\ neg and ;

: constant create lit, postpone ;; ;

: 10. 0a base. ;
: bin. 02 base. ;
: h. 10 base. ;

macro : [ascii] key lit, ;
forth : ascii key ;

: star 2a emit ;
: stars for star next ;

: times ' swap for dup execute next drop ;
