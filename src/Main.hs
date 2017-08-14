{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import Prelude ()
import Protolude hiding ((&))
import Control.Arrow
import Data.Maybe
import Control.Lens
import Control.Exception.Lens
import Data.Time.Clock (diffTimeToPicoseconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (HttpException)
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as B
import qualified Network.Wreq as W
import qualified Data.Attoparsec.Text as A
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
  liftIO . mapM_ (downloadImage "img")
         . concat . catMaybes
         . fmap extractLinks
         . RT.contents $ info

extractLinks :: RT.Post -> Maybe [Text]
extractLinks post =
  case RT.content post of
    (RT.Link l) -> parseImgSource (l <> "\n")
    _ -> Nothing


toImageSite :: Text -> Maybe Text
toImageSite "imgur.com" = Just "i.imgur.com"
toImageSite "imgflip.com" = Just "i.imgflip.com"
toImageSite _ = Nothing

endings = [ ".jpg"
          , ".png"
          ]
          
attatchEndings t = fmap (t <>) endings

parseExtention :: Text -> Maybe [Text]
parseExtention url =
  maybe Nothing (const $ Just [url])
  . A.maybeResult
  . A.parse (A.manyTill A.anyChar (A.choice (fmap A.string endings)))
  $ url


parseNoExtention = fmap attatchEndings . join . A.maybeResult . A.parse (do
  http <- A.string "https" <|> A.string "http"
  colonSlash <- "://"
  site <- A.takeTill ('/' ==)
  slash <- A.string "/"
  A.option "" (A.string "i/")
  imgHash <- A.manyTill' A.anyChar A.endOfLine
  pure $ (\s -> http <> colonSlash <> s <> slash <> toS imgHash) <$> toImageSite site)


parseImgSource url = parseExtention url <|> parseNoExtention url

nameFromTime :: IO Text
nameFromTime = toS . filter ('.' /=) . show . realToFrac <$> getPOSIXTime

contentType :: W.Response a -> Maybe Text
contentType r = r ^? W.responseHeader "Content-Type" & mapped %~ toS

downloadImage :: Text -> Text -> IO ()
downloadImage folder url = do
  r' <- try @HttpException $ W.get (filter ('\n' /=) $ toS url)
  flip (either (const $ pure ())) r' $ \r -> 
    case r ^. W.responseStatus . W.statusCode of
      200 -> do
        time <- nameFromTime
        let ext = case contentType r of
                    Just "image/jpeg" -> ".jpg"
                    Just "image/png" -> ".png"
                    _ -> ""
        B.writeFile (toS folder <> "/" <> toS time <> ext)  (r ^. W.responseBody)
      _ -> pure ()

  
