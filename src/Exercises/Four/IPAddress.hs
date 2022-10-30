module Exercises.Four.IPAddress () where
import Text.Parsec.String (Parser)
import Text.Parsec

-- data IPAddress = IPAddress Int Int Int Int

-- ipAddressParser :: Parser String IPAddress
-- ipAddressParser = read <$> 

eofParser :: Parser Int
eofParser = read <$> (manyTill ".")

partsParser :: Parser [Int]
partsParser = map read <$> (many1 digit `sepBy` (char '.'))

main = eofParser "12345"

