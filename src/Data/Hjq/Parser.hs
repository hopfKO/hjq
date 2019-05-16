{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq.Parser (
    JqFilter(..)
    ,parseJqFilter
    ,JqQuery(..)
    ,parseJqQuery
) where

import Control.Applicative
import Data.Text
import Data.Attoparsec.Text
import Control.Lens

path3 = (^. _3)
path23 = (^. _2 . _3)
path323 = path23 . path3
unco = (1,2,(111,(243,7,4),10))
unchi = path323 unco




data JqFilter
    = JqField Text JqFilter
    | JqIndex Int JqFilter
    | JqNil
    deriving (Show,Read,Eq)

-- フィルタ文字列のパーサ
parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter s = showParseResult $ parse (jqFilterParser <* endOfInput) s `feed` ""

showParseResult :: Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . pack  $ show r

schar :: Char -> Parser Char
schar c = skipSpace *> char c <* skipSpace

jqFilterParser :: Parser JqFilter
jqFilterParser = schar '.' >> (jqField <|> jqIndex <|> pure JqNil)
    where
        jqFilter :: Parser JqFilter
        jqFilter = (schar '.' >> jqField) <|> jqIndex <|> pure JqNil

        jqField :: Parser JqFilter
        jqField = JqField <$> word <* skipSpace <*> jqFilter

        jqIndex :: Parser JqFilter
        jqIndex = JqIndex <$> (schar '[' *> decimal <* schar ']') <*> jqFilter

word :: Parser Text
word = fmap pack $ many1 (letter <|> schar '-' <|> schar '_' <|> digit)

liftA2IgnoringCenter :: (Applicative m) => (a -> b -> c) -> m a -> m d -> m b -> m c
liftA2IgnoringCenter f x y z = f <$> x <* y <*> z

data JqQuery = JqQueryObject [(Text, JqQuery)]
            |  JqQueryArray [JqQuery]
            |  JqQueryFilter JqFilter
            deriving (Show, Read, Eq)

-- クエリ文字列のパーサ
parseJqQuery :: Text -> Either Text JqQuery
parseJqQuery s = showParseResult $ parse (jqQueryParser <* endOfInput) s `feed` ""

jqQueryParser :: Parser JqQuery
jqQueryParser = queryArray <|> queryFilter <|> queryObject
    where
        queryArray :: Parser JqQuery
        queryArray = JqQueryArray <$> (schar '[' *> jqQueryParser `sepBy` schar ',' <* schar ']')  
        
        queryFilter :: Parser JqQuery
        queryFilter = JqQueryFilter <$> jqFilterParser

        qObj :: Parser (Text, JqQuery)
        qObj = (,) <$> (schar '"' *> word <* schar '"' <* schar ':') <*> jqQueryParser

        queryObject :: Parser JqQuery
        queryObject = JqQueryObject <$> (schar '{' *> qObj `sepBy` schar ',' <* schar '}')