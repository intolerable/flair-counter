module Main where

import Reddit
import Reddit.Types.Flair
import Reddit.Types.Options
import Reddit.Types.Subreddit
import Reddit.Types.User

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text (Text)
import System.Environment (getArgs)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  args <- map Text.pack <$> getArgs
  case args of
    username : password : subreddit : [] -> do
      res <- runRedditWithRateLimiting username password $
        getAllFlairs (R subreddit) Nothing
      case res of
        Left err -> print err
        Right fs -> do
          Text.putStrLn $ formatFlairs $ countFlairs fs
          Text.putStrLn ""
          Text.putStrLn $ textShow (length fs) <> " users have chosen flair"
    _ -> putStrLn "Usage: flair-counter USERNAME PASSWORD SUBREDDIT"

getAllFlairs :: SubredditName -> Maybe UserID -> Reddit [Flair]
getAllFlairs sub u = do
  res <-
    nest $ getFlairList' (Options (After <$> u) (Just 1000)) sub
  case res of
    Left _ -> getAllFlairs sub u
    Right (FlairList fs n _) ->
      case n of
        Just _ -> do
          more <- getAllFlairs sub n
          return $ fs ++ more
        Nothing -> return fs

countFlairs :: [Flair] -> [(Text, Integer)]
countFlairs = Map.toList . Map.fromListWith (+) . map (Text.strip . fromMaybe "" . cssClass &&& const 1)

formatFlairs :: [(Text, Integer)] -> Text
formatFlairs =
  Text.unlines .
  map (\(a,b) -> a <> " | " <> textShow b) .
  sortBy (flip (comparing snd))

textShow :: Show a => a -> Text
textShow = Text.pack . show
