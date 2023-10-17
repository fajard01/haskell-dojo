module Sandbox where
    ---------------
    cup f10z = \message -> message f10z
    getOz aCup = aCup (\flOz -> flOz)
    drink aCup ozDrank = if ozDiff >= 0
                            then cup ozDiff
                            else cup 0
                                where 
                                    flOz = getOz aCup
                                    ozDiff = flOz - ozDrank
    isEmpty aCup = getOz aCup == 0
    afterManySips aCup sips = foldl drink aCup sips

    ---------------
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)

    fib' 0 = 0
    fib' 1 = 1
    fib' 2 = 1
    fib' n = fastFib 1 1 n
                where
                    fastFib x y n | n == 3    = x + y
                                  | otherwise = fastFib y (x+y) (n-1)
    
    ----------------
    ackermann 0 n = n + 1
    ackermann m 0 = ackermann (m-1) 1
    ackermann m n = ackermann (m-1) (ackermann m (n-1))

    collatz 1 = 1
    collatz n = if even n
                then 1 + collatz (n `div` 2)
                else 1 + collatz (n*3 + 1)

    ----------------
    binaryPartialApplication f x = \y -> f x y

    ----------------
    counter x = let x = x + 1
                    in
                        let x = x + 1
                            in
                                x

    counter' x = (\x ->
                    (\x -> x) x + 1
                 ) x + 1

    ----------------
    overwrite = let x = 2
                    in
                        let x = 3
                            in
                                let x = 4
                                    in
                                        x

    overwrite' = (\x ->
                    (\x ->
                        (\x -> x) 4
                    ) 3
                 ) 2

    ----------------
    hello = "Hello World!"
