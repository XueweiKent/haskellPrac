--my solution about Euler Problem 1
multipleThreeFive = sum [ x | x<-[1..1000], x`mod`3==0||x`mod`5==0 ]


main = print multipleThreeFive