{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Data.Hjq.Parser
import Data.Text
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Data.Aeson


main :: IO ()
main = do
    _ <- runTestTT $ TestList
        [
            jqFilterParserTest,
            jqFilterParserSpacesTest,
            jqQueryParserTest,
            jqQueryParserSpacesTest
        ]
    return ()

-- フィルタ文字列のパーサテスト(スペースなし)
jqFilterParserTest :: Test
jqFilterParserTest = TestList
    [
        "jqFilterParser test 1" ~: parseJqFilter (pack ".") ~?= Right JqNil,
        "jqFilterParser test 2" ~: parseJqFilter (pack ".[0]") ~?= Right ( JqIndex 0 JqNil),
        "jqFilterParser test 3" ~: parseJqFilter (pack ".fieldName") ~?= Right (JqField "fieldName" JqNil),
        "jqFilterParser test 4" ~: parseJqFilter (pack ".[0].fieldName") ~?= Right ( JqIndex 0 (JqField "fieldName" JqNil) ),
        "jqFilterParser test 5" ~: parseJqFilter (pack ".fieldName[0]") ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
    ]

-- フィルタ文字列のパーサテスト(スペースあり)
jqFilterParserSpacesTest :: Test
jqFilterParserSpacesTest = TestList
    [
        "jqFilterParser test 1" ~: parseJqFilter (pack " ." ) ~?= Right JqNil,
        "jqFilterParser test 2" ~: parseJqFilter (pack " . [ 0 ] ") ~?= Right ( JqIndex 0 JqNil),
        "jqFilterParser test 3" ~: parseJqFilter (pack " . fieldName ") ~?= Right (JqField "fieldName" JqNil),
        "jqFilterParser test 4" ~: parseJqFilter (pack " . [ 0 ] . fieldName" ) ~?= Right ( JqIndex 0 (JqField "fieldName" JqNil) ),
        "jqFilterParser test 5" ~: parseJqFilter (pack " . fieldName [ 0 ] ") ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
    ]

-- クエリのパーサテスト(スペースなし)
jqQueryParserTest :: Test
jqQueryParserTest = TestList
    [
        "jqQueryParser test 1" ~: parseJqQuery (pack "[]") ~?= Right(JqQueryArray []),
        "jqQueryParser test 2" ~: parseJqQuery "[.hoge,.piyo]" ~?= Right(JqQueryArray [JqQueryFilter (JqField "hoge" JqNil), JqQueryFilter (JqField "piyo" JqNil)]),
        "jqQueryParser test 3" ~: parseJqQuery "{\"hoge\":[],\"piyo\":[]}" ~?= Right(JqQueryObject [("hoge", JqQueryArray []), ("piyo", JqQueryArray [])])
    ]

-- クエリのパーサテスト(スペースあり)
jqQueryParserSpacesTest :: Test
jqQueryParserSpacesTest = TestList
    [
        "jqQueryParser test 1" ~: parseJqQuery " [ ] " ~?= Right(JqQueryArray []),
        "jqQueryParser test 2" ~: parseJqQuery " [ . hoge , . piyo ] " ~?= Right(JqQueryArray [JqQueryFilter (JqField "hoge" JqNil), JqQueryFilter (JqField "piyo" JqNil)]),
        "jqQueryParser test 3" ~: parseJqQuery " { \"hoge\" : [ ] , \"piyo\" : [ ] } " ~?= Right(JqQueryObject [("hoge", JqQueryArray []), ("piyo", JqQueryArray [])])
    ]

-- クエリ用テストデータ
testData :: Value
testData = Object $ H.fromList
    [
        ("string-field",String "stringvalue"),
        ("nested-field", Object $ H.fromList
            [
                ("inner-string", String "inner value"),
                ("inner-number", Number 100)
            ]
        ),
        ("array-field", Array $ V.fromList
            [
                String "first field",
                String "second field",
                Object (H.fromList [("object-in-array", String "string value in object-in-array")])
            ]
        )
    ]