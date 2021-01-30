module Parser where
{-# LANGUAGE OverloadedStrings #-}
import Data.Text
import Control.Applicative
import Data.Attoparsec.Text

data Command
  = GameCommand Text Command
  | CommandParameter Int Command
  | CommandNil
  deriving (Show,Eq,Read)

schar :: Char -> Parser Char
schar c = skipSpace *> char c <* skipSpace

parseCommand :: Text -> Either Text Command
parseCommand s = showParseResult $ parse(commandParser <* endOfInput) s `feed` pack ""

commandParser :: Parser Command
commandParser = schar ':' >> (gameCommand <|> commandParameter <|> pure CommandNil)
  where
    command :: Parser Command
    command =  (schar ':' >> gameCommand) <|> commandParameter <|> pure CommandNil

    gameCommand :: Parser Command
    gameCommand = GameCommand <$> word <*> command

    commandParameter :: Parser Command
    commandParameter = CommandParameter <$> decimal <*> command


word :: Parser Text
word = pack <$> many1 letter

showParseResult ::Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . pack $ show r
