module Parsing (module Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char

-- General datatypes
data Input = Input
    { inputLoc :: Input
    , inputStr :: String
    } deriving (Show, Eq)

-- Hutton parser - Modified from Graham Hutton's Programming in Haskell
newtype Parser a = P (String -> [(a, String)])

parse::Parser a -> String -> [(a, String)]
parse (P p) input = p input

item :: Parser Char
item = P (\input -> case input of
                            []      -> []
                            (x:xs)  -> [(x,xs)])

-- Hutton sequencing parsers
instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p1 = P (\input -> case parse p1 input of
                                []           -> []
                                [(x,output)] -> [(f x,output)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = P (\input -> [(x, input)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> p1 = P (\input -> case parse pf input of                 -- pf, the (a -> b), is a Parser! 
                                []              -> []
                                [(f, output)]  -> parse (fmap f p1) output)

three :: Parser (Char,Char)
three = pure g <*> item <*> item <*> item
        where g x y z = (x,z)

-- Tsoding parser - modified from github.com/tsoding/haskell-json/blob/master/Main.hs
-- newtype Parser' a = Parser' {parse' :: String -> (a, Input)} -- uses the free function created by the record syntax

-- instance Functor Parser' where 
--     fmap f (Parser' p) =
--         Parser' $ \input -> do
--             (x, input') <- p input
--             return (f x, input')



-- AUG parser