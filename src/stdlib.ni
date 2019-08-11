"Stack manipulation" $

[ $x $y x y ] \swap define
[ $x x x ] \dup define
[ $x x x x ] \trup define

"Logic" $

[ $yes
    [] ifelse
] \if define

"Lists" $

[ $f $acc $list
    list null
    [acc]
    [
        list uncons acc f eval
        f fold
    ]
    ifelse
] \fold define

[0 [+] fold] \sum define
[1 [*] fold] \product define
