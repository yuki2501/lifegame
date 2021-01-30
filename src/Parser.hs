{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Parser where
import Data.Text
import Control.Applicative
import Data.Attoparsec.Text

data Command 
  = GameCommand Text (Command )
  | CommandParameter Int (Command ) 
  | CommandNil
  deriving (Show,Eq,Read)

schar :: Char -> Parser Char
schar c =  char c <* skipSpace

parseCommand ::     Text -> Command 
parseCommand s = let result = showParseResult $ parse(commandParser <* endOfInput) s `feed` pack "" in case result of 
                                                                                                         Right x -> x 
                                                                                                         Left _ -> CommandNil

commandParser :: Parser (Command )
commandParser = char ':' >> (gameCommand <|> commandParameter <|> pure CommandNil)
  where
    command :: Parser (Command )
    command =  (char ':' >> gameCommand) <|> commandParameter <|> pure CommandNil

    gameCommand :: Parser (Command )
    gameCommand = GameCommand <$> word <* skipSpace <*> command

    commandParameter :: Parser (Command )
    commandParameter = CommandParameter <$> decimal <* skipSpace  <*> command <|> CommandParameter <$> decimal <*> command


word :: Parser Text
word = pack <$> many1 letter

showParseResult ::Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . pack $ show r

