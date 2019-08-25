\fib [ $n
    0 1
    [ $x $y x y x + ]
    n times
    const
] define

0 $i [
    i fib print
    i increment $i
] 10 times
