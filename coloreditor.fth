data curpos testblock d,
: curpos+ curpos >a +! ;
: curpos@+ curpos @t c@t 1 curpos+ ;

: inside dup . key drop ;

: emitw buf. 0buf ;
: space 20 emit ;

: hexnum curpos @t @t h. 8 curpos+ ;

{keymap printing default inside
0 calls bye
1 [[ drop cr red paint emitw space ]] mapkey
2 [[ drop green paint emitw space ]] mapkey
3 [[ drop yellow paint emitw space ]] mapkey
4 [[ drop blue paint emitw space ]] mapkey
5 [[ drop magenta paint emitw space ]] mapkey
6 [[ drop cyan paint emitw space ]] mapkey
8 [[ __ brush green paint hexnum plain brush space ]] mapkey
21 7f call char
}keymap

: showblock curpos@+ printing over vector showblock ;
showblock
