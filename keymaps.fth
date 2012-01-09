80 constant table
data latest 0 d, : latest@ latest @t ; : latest! latest !t ;
: >slots table + ; : xts 2* 2* 2* ;
: 0table [ table >qwords lit, ] swap 0 qfill ;
: vector over + c@t xts + table + ijmp [
: {keymap table ||data dhere dup latest! 0table table allot ;
: nothing : }keymap ; : default ' d, ;
: ?newslot dhere 8 neg + @t over xor drop if d, ;; fi drop ;
: slot# dhere latest@ >slots neg + >qwords 1- ;
: calls '
: mapkey ?newslot latest@ + slot# swap a! c! ;
: call '
: maprange >a over neg + >r 1- a> r> for over i + over mapkey next drop drop ;
: [[ here ] ; macro : ]] postpone ; ; forth
: ctrl ascii [ 40 not lit, ] and ;
