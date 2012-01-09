: color 1e + constant ;
0 color black  1 color red      2 color green  3 color yellow
4 color blue   5 color magenta  6 color cyan   7 color white
: \e[ 01b emit [ascii] [ emit ;
: canvas 0a + : paint : brush \e[ 10. [ascii] m emit ;
: at \e[ 10. [ascii] ; emit 10. [ascii] f emit ;
: clear \e[ 2 10. [ascii] J emit ;
: direction create dup lit, 1 + postpone ;; ;
ascii A  direction up     direction down
         direction right  direction left  drop
: move \e[ 10. emit ;
: eraseline \e[ [ascii] K emit ;
: attr constant ;
0 attr plain   1 attr bold     4 attr __
5 attr blink  7 attr reverse  8 attr concealed
