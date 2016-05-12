{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as H
import Data.Aeson
import Data.Aeson.Types (Parser, parse)
import Text.PrettyPrint

data Person = Person
  { age::Int
  , name::String
  } deriving (Show)

personP :: Value -> Parser Person
personP (Object v) =
  Person <$> v .: "age" <*> v .: "name"

parsePerson :: Value -> Result Person
parsePerson =
  parse personP

printPerson :: Person -> Value
printPerson Person {..} =
  object ["age" .= age, "name" .= name]

testValue :: Value
testValue =
  fromJust $ decode "{\"name\": \"Ayman\", \"age\": 42}"

print' :: a -> Doc
print' = undefined

main = do
  let Success p = parsePerson testValue
  print p
  print $ printPerson p
  putStrLn . render $ vcat [ text "hello", text "world" ]

  let x :: Result Int
      x = parse (\(Object v) -> v .: "age")
          (Object $ H.fromList [("age", toJSON (42 :: Int))])
  print x


data Iso a b
  = Iso (a -> Maybe b) (b -> Maybe a)

iso :: Iso Value Person
iso = Iso f g
  where
    f :: Value -> Maybe Person
    f = do { Success p <- parsePerson; return $ Just p}
    g :: Person -> Maybe Value
    g = Just . printPerson
