module Sandbox where
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
