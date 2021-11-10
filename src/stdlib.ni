# Environments

# TODO isolate the stack to build lists
\isolate [ new use eval unuse $ ] define

# Stack

\const [ $_x $   _x    ] define
\swap  [ $_x $_y _x _y ] define
\dup   [ $_x     _x _x ] define

# Loops

\times [ replicate eval ] define

\while [ $action $cond
    cond eval
    [ action eval cond action while ]
    []
    ifelse
] define

\forever [ $action
    action eval
    action forever
] define

# Equality

\/= [ = not ] define

# Logic

\if [ [] ifelse ] define

# Math

\subtract [ swap - ] define
\divide   [ swap / ] define
\power    [ swap ^ ] define

\neg [ 0 - ] define
\increment [ 1 + ] define
\decrement [ 1 subtract ] define
\half [ 2 divide ] define
\square [ 2 power ] define

# Lists

\head [ uncons const ] define
\tail [ uncons $ ] define

\length [ $l
    l null?
    [ 0 ]
    [ l tail length increment ]
    ifelse
] define

\fold [ $f $z $l
    l null?
    [ z ]
    [
        l head
        l tail z f fold
        swap f eval
    ]
    ifelse
] define

\sum     [ 0 \+ fold ] define
\product [ 1 \* fold ] define

\any [ :false \or fold ] define
\all [ :true \and fold ] define

\concat [ [] \+ fold ] define

\replicate [ $n $l
    n 0 =
    [ [] ]
    [ l n decrement replicate l swap + ]
    ifelse
] define

\range [ $a $b
    a b =
    [ [] ]
    [ a b a increment range swap cons ]
    ifelse
] define
