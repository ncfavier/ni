"Stack transformers" $

\const [ $x $y x   ] define
\swap  [ $x $y x y ] define
\dup   [ $x    x x ] define

"Logic" $

\if [ [] ifelse ] define

"Lists" $

\fold [ $f $acc $list
    list null
    \acc
    [ list uncons acc f eval f fold ]
    ifelse
] define

\sum     [ 0 \+ fold ] define
\product [ 1 \* fold ] define
