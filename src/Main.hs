{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import Prelude ()
import Protolude
import Control.Arrow
import Data.Maybe
import qualified Reddit as R
import qualified Reddit.Types as RT
import qualified Reddit.Types.Listing as RT
import qualified Reddit.Types.Post as RT
import qualified Reddit.Login as RL
import qualified Reddit.Actions as RA

data Options = Options { username :: Text, password :: Text }

usernameOpt :: Parser Text
usernameOpt = option auto
   ( long "username"
  <> short 'u'
  <> metavar "USERNAME"
  <> help "The username of the account"
    )

passwordOpt :: Parser Text
passwordOpt = option auto
   ( long "password"
  <> short 'p'
  <> metavar "PASSWORD"
  <> help "The password of the account"
   )

opts :: Parser Options
opts = Options <$> usernameOpt <*> passwordOpt

optsInfo :: ParserInfo Options
optsInfo = info (opts <**> helper)
  ( fullDesc
  <> progDesc "Print a simple message"
  <> header "meme-collector - a minimal application"
  )

main :: IO ()
main = do
  (Options usr pass) <- execParser optsInfo
  result <- R.runReddit usr pass collect
  print result

collect :: RT.Reddit ()
collect = do
  info <- RA.getPosts' (RT.Options Nothing Nothing) RT.New (Just $ RT.R "AdviceAnimals")
  liftIO . (mapM_ . mapM) print
         . traverse extractLinks
         . RT.contents $ info

extractLinks :: RT.Post -> Maybe (Text, Text)
extractLinks post =
  case RT.content post of
    (RT.Link l) -> Just (RT.title post, l)
    _ -> Nothing

