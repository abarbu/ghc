{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Prelude
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The Prelude: a standard module imported by default into all Haskell
-- modules.  For more documentation, see the Haskell 98 Report
-- <http://www.haskell.org/onlinereport/>.
--
-----------------------------------------------------------------------------

module Prelude (

    -- * Basic data types
    Bool(..),
    Maybe(..),
    Either(..),
    Ordering(..), 
    Char, String, Int, Integer, Float, Double, IO,
    Rational,
    -- []((:), []),	-- Not legal Haskell 98; available through built-in syntax
    
    module Data.Tuple,
        -- Includes tuple types + fst, snd, curry, uncurry
    -- ()(..),		-- Not legal Haskell 98
    -- (->),		-- ... available through built-in syntax
    
    -- * Basic type classes
    Eq(..),
    Ord(..), 
    Enum(..),
    Bounded(..), 
    Num(..),
    Real(..),
    Integral(..),
    Fractional(..),
    Floating(..),
    RealFrac(..),
    RealFloat(..),

    -- * List operations
    map, (++), filter, concat,
    head, last, tail, init, null, length, (!!), 
    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    reverse, and, or,
    any, all, elem, notElem, lookup,
    maximum, minimum, concatMap,
    zip, zip3, zipWith, zipWith3, unzip, unzip3,

    lines, words, unlines, unwords,
    sum, product,

    -- * Converting to and from @String@
    ReadS, ShowS,
    Read(readsPrec, readList),
    Show(showsPrec, showList, show),
    reads, shows, read, lex, 
    showChar, showString, readParen, showParen,
    
    -- * Simple I\/O operations
    ioError, userError, catch,
    FilePath, IOError,
    putChar,
    putStr, putStrLn, print,
    getChar,
    getLine, getContents, interact,
    readFile, writeFile, appendFile, readIO, readLn,

    -- * Monads
    Monad(..),
    Functor(..), 
    mapM, mapM_, sequence, sequence_, (=<<),

    -- * Miscellaneous functions
    maybe, either,
    (&&), (||), not, otherwise,
    subtract, even, odd, gcd, lcm, (^), (^^), 
    fromIntegral, realToFrac,
    --exported by Data.Tuple: fst, snd, curry, uncurry,
    id, const, (.), flip, ($), until,
    asTypeOf, error, undefined,
    seq, ($!)

  ) where

import Control.Monad
import System.IO
import Text.Read
import Text.Show
import Data.List
import Data.Either
import Data.Maybe
import Data.Bool
import Data.Tuple

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.IOBase
import GHC.Exception
import GHC.Read
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Float
import GHC.Show
import GHC.Conc
import GHC.Err   ( error, undefined )
#endif

infixr 0 $!


-- -----------------------------------------------------------------------------
-- Miscellaneous functions

($!)    :: (a -> b) -> a -> b
f $! x  = x `seq` f x


