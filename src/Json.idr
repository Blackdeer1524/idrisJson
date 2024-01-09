module Json

import Data.String

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Int
               | JsonString (List Char)
               | JsonArray (List JsonValue)
               | JsonObject (List (List Char, JsonValue))


Eq JsonValue where 
  (==) JsonNull JsonNull = True
  (==) (JsonNumber i) (JsonNumber y) = True
  (==) (JsonString cs) (JsonString cs2) = True
  (==) (JsonArray xs) (JsonArray ys) = True
  (==) (JsonObject xs) (JsonObject ys) = True
  (==) _ _  = False


data Parser: (a: Type) -> Type where
  MkParser : (p : (input: List Char) -> Maybe (List Char, a)) -> Parser a

%name Parser parser

runParser : Parser a -> String -> Maybe (String, a)
runParser (MkParser p) str = 
  case p (unpack str) of
    Nothing => Nothing
    (Just x) => let (input', res) = x in Just (pack input', res)

charP : Char -> Parser Char
charP c = MkParser (\arg => 
          case arg of
               [] => Nothing
               (x :: xs) => 
                    if x == c 
                       then Just (xs, c)
                       else Nothing)

Functor Parser where
  map f (MkParser run) = 
    MkParser (\input => 
        do (remainder, res) <- run input
           Just (remainder, f res)
        )


Applicative Parser where
  pure a = MkParser (\input => Just (input, a)) 
  (<*>) (MkParser mapper) (MkParser parser) = MkParser (\input : List Char => 
        do (input', t) <- mapper input
           (remainder, res) <- parser input'
           Just (remainder, t res)
         )

Alternative Parser where
  empty = MkParser (\input : List Char => Nothing)
  (<|>) (MkParser first) secondParser = MkParser {a} (\input : List Char => 
        (first input) <|> (let (MkParser second) = secondParser in second input)
        )

stringP : String -> Parser (List Char)
stringP cs = sequence (map charP (unpack cs))

jsonNull : Parser JsonValue
jsonNull = (\_ => JsonNull) <$> (stringP "null")

jsonBool : Parser JsonValue 
jsonBool = mapBool <$> (<|>) (stringP "true") (stringP "false")
  where 
    mapBool : List Char -> JsonValue
    mapBool cs = case cs of
                      ('t' :: 'r' :: 'u' :: 'e' :: xs) => JsonBool True
                      _ => JsonBool False

spanP : (f: Char -> Bool) -> Parser (List Char)
spanP f = MkParser (\input => let (first, second) = span f input in Just (second, first))

notNull : Parser (List Char) -> Parser (List Char)
notNull (MkParser p) = MkParser (\input => 
                       do (input', res) <- p input
                          if null res
                             then Nothing
                             else Just (input', res) 
                          )

jsonNumber : Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
  where
    f : List Char -> JsonValue
    f cs = JsonNumber $ case parseInteger (pack cs) of
                           Nothing => 0
                           (Just x) => x

stringLiteral : Parser (List Char)
stringLiteral = spanP (/= '"')

jsonString : Parser JsonValue
jsonString = JsonString <$> (charP '"' *> stringLiteral <* charP '"')

many : Parser a -> Parser (List a)
many parser = MkParser (\input => let x =  parseUntilFailure parser input in Just x)
  where
    parseUntilFailure : Parser a -> (input: List Char) -> (List Char, List a)
    parseUntilFailure (MkParser f) input = 
      case f input of 
          Nothing => (input, [])
          Just (input', res) => 
             let (remainder, xs) = parseUntilFailure (MkParser f) input' in 
             (remainder, res :: xs)

sepBy : Parser a -> Parser b -> Parser (List b)
sepBy sep elem = (::) <$> elem <*> many (sep *> elem) <|> pure []

ws : Parser (List Char)
ws = spanP isSpace

jsonValue : Parser JsonValue

jsonArray : Parser JsonValue 
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where
    elements : Parser (List JsonValue)
    elements = sepBy ( ws *> charP ',' <* ws ) jsonValue


jsonObject : Parser JsonValue
jsonObject = charP '{' *> ws *> 
             JsonObject <$> sepBy (ws *> charP ',' <* ws) pair
             <* ws <* charP '}'
  where 
    pair : Parser (List Char, JsonValue)
    pair = (\a, b, c => (a, c)) <$> (charP '"' *> stringLiteral <* charP '"') <*> (ws *> charP ':' <* ws) <*> jsonValue

jsonValue = jsonNumber <|> jsonBool <|> jsonNull <|> jsonString <|> jsonArray <|> jsonObject


