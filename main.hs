-- paskell - Pascal's simplex generator written in Haskell
--
-- Copyright (C) 2014 Wesley Merkel <ooesili@gmail.com>
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice,
-- this list of conditions and the following disclaimer in the documentation
-- and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its
-- contributors may be used to endorse or promote products derived from this
-- software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

import Data.List

-- Haskell is statically typed, so I had to come up with a way to have a list
-- whose depth can change at runtime. This "Brane" type can hold any number of
-- nested lists inside of it, perfect for the multi-dimensional nature of this
-- problem
data Brane = String [Int] | Brane [Brane]

instance Show Brane where
    show = init . indentBrane ""

-- this is the main workhorse for showing Brane structures.
indentBrane :: String -> Brane -> String
-- this will show the array of exponents of each variable in the term, which
-- are stored in [Int], then an arrow pointing to the coefficient for that term
indentBrane ind (String xs) = ind ++ show xs
                           ++ " -> " ++ show (multinomial xs) ++ "\n"
-- this one prints the word Brane, manages the brackets, and keeps track of
-- indentation
indentBrane ind (Brane  bs) = ind ++ "Brane [\n"
                           ++ concatMap (indentBrane ("  " ++ ind)) bs
                           ++ ind ++ "]\n"

-- read the dimension and size of the Pascal's simplex, then print it
main :: IO ()
main = do
    [dimension, size] <- fmap (map read . words) getLine
    let brane = hyperPascal dimension size
    print brane

-- returns a multinomial coefficient
-- cs contains exponent of each variable in the term, for example:
-- x^2*y*z is stored as [2,1,1]
-- the returned value is the coefficient of that term, assuming of course, that
-- the term is part of a multinomial expansion where the number of variables is
-- equal to length of cs, and the exponent of the original expression is equal
-- to the sum of cs
multinomial :: [Int] -> Int
multinomial cs = f (sum cs) `div` (product $ map f cs)
    where f n = product [1..n]

-- creates a Pascal's simplex based on the given dimension and size
hyperPascal :: Int -> Int -> Brane
hyperPascal d s = go br
          -- create a one dimension simplex
    where br = Brane (map (\x -> String [x]) [0..s-1])
          -- deepen it d-1 times
          go = foldl (.) id $ replicate (d-1) deepen

-- adds a dimension to the pyramid
deepen :: Brane -> Brane
-- we cannot deepen a single term,
-- this is more of a design choice than a logical issue
-- the hyperPascal function will never pass us a plain String
deepen (String _) = error "cannot deepen a plain string"
-- match each call to go with an integer representing which layer of the
-- simplex the lobe resides on
-- this integer can also be derived from the length of each lobe's list, but it
-- seemed computationally less complex to just figure out this number in one
-- place
deepen (Brane bs) = Brane . zipWith go [0..] $ lobes
          -- pack the inits of bs, excluding the empty list, into new Branes
          -- this adds another layer to the simplex, and sets up the foundation
          -- for the next step, adding another variable to each term
    where lobes = map Brane . tail $ inits bs
          -- since adding a dimension (deepening) involves adding another
          -- variable to each term, we must add another element to each
          -- String's list
          -- s is the exponent to which the original expression is raised
          -- (which is, again, also the current layer of simplex), for example:
          -- (x+y)^3
          -- as a rule, the sum of the exponents of all the variables in any
          -- term must add up to s, so we can just prepend (s - sum xs) to xs
          go s (String xs) = String $ (s - sum xs):xs
          -- to deepen a Brane, we simply call go on each sub-Branes
          go s (Brane  bs') = Brane $ map (go s) bs'
