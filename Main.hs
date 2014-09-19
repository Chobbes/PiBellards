#!/usr/bin/env runhaskell

{- Copyright (C) 2014 Calvin Beck

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction,
   including without limitation the rights to use, copy, modify, merge,
   publish, distribute, sublicense, and/or sell copies of the Software,
   and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}

import System.Environment
import Data.Ratio
import Data.Decimal

piEst :: Integer -> Decimal
piEst digits = fromRational $ sum [((-1)^n % (2^(10*n + 6))) * sum (zipWith (%) numerators denominators) | n <- [0..digits],
                    let denominators = [4*n + 1, 4*n + 3, 10*n + 1, 10*n + 3, 10*n + 5, 10*n + 7, 10*n + 9],
                    let numerators = [-2^5, -1, 2^8, -2^6, -2^2, -2^2, 1]]

main = do
  [digitStr] <- getArgs
  let digits = read digitStr
  putStrLn . take (digits + 2) . show $ piEst (fromIntegral digits)
