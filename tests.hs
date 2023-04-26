primes = [x | x <- [2..], null [n | n <- [2..x-1], mod x n == 0]]

primes2 = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])
