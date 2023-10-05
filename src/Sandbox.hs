module Sandbox where

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
    