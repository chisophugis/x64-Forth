8 ||data buflen 0 d,
20 constant maxbuf
maxbuf ||data buf maxbuf allot
: buflen@ buflen @t ; : buflen! buflen !t ;
: bufnormed buf @t ; : 0buf 0 buflen! [ maxbuf >qwords lit, ] buf 0 qfill ;
: bufadd buflen@ dup 1+ maxbuf min buflen! buf + >a c! ;
: char bufadd ; : ws 0buf ;

{keymap console default abort
0 21 call ws
21 7f call char
ctrl P [[ clear .s drop key drop ]] mapkey
ctrl B [[ drop blue paint ]] mapkey
ctrl M [[ drop bufnormed clear 1 8 at 0buf find jmp ]] mapkey
ctrl N [[ drop buflen @t buf number if 0buf ;; fi white canvas ]] mapkey
ctrl L [[ drop clear ]] mapkey
backspace [[ drop 0buf ]] mapkey
ascii q calls bye
}keymap

: bufinfo 1 3 at bufnormed . 1 4 at bufnormed find . 1 5 at buf . ;
: buf. buflen@ test if for i not buflen@ 1+ + buf + c@t emit next ;; fi drop ;
: showbuf 1 1 at eraseline buf. ;
: ?valid bufnormed find drop if red canvas fi ;
: redraw bufinfo ?valid showbuf black canvas ;
