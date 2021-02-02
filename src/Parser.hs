{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Parser where
import Data.Text
import Control.Applicative
import Data.Attoparsec.Text

data Command  where 
   GameCommand :: Text -> (Command) -> Command 
   CommandParameter :: Int -> (Command ) -> Command  
   CommandNil :: (Command )
   Error :: (Command)
  deriving (Show,Eq,Read)

parseCommand ::     Text -> Command 
parseCommand s = let result = showParseResult $ parse(commandParser <* endOfInput) s `feed` pack "" in case result of 
                                                                                                         Right x -> x 
                                                                                                         Left _ -> Error
commandParser :: Parser (Command )
commandParser = char ':' >> (gameCommand <|> commandParameter <|> pure CommandNil)
  where
    command :: Parser (Command )
    command =  (char ':' >> gameCommand) <|> commandParameter <|> pure CommandNil

    gameCommand :: Parser (Command )
    gameCommand = GameCommand <$> word <* skipSpace <*> command <|> GameCommand <$ skipSpace <*> word <* skipSpace <*> command   

    commandParameter :: Parser (Command )
    commandParameter = CommandParameter <$> decimal <* char ')' <* skipSpace <*> command <|> CommandParameter <$> decimal <* char ')' <*> command <|> CommandParameter <$> decimal <* skipSpace  <*> command <|> CommandParameter <$> decimal <*> command <|> CommandParameter <$  char '('  <*> decimal <* char ',' <*> command  

word :: Parser Text
word = pack <$> many1 letter

showParseResult ::Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . pack $ show r

