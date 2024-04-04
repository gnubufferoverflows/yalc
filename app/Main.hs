{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad.Trans.Maybe
import System.IO
import Control.Exception
import Data.Char
import System.Environment
import Data.List
import Control.Monad

eitherToMaybeTM :: Monad m => m (Either b a) -> MaybeT m a 
eitherToMaybeTM m = MaybeT $ m >>= (\e -> case e of
                                          Left _ -> return Nothing
                                          Right y -> return $ Just y)


checkSpaces :: String -> Bool
checkSpaces = liftA2 (||) null (all isSpace) 

safeRead :: FilePath -> MaybeT IO String 
safeRead f = eitherToMaybeTM $ (try $ readFile' f :: IO (Either IOException String)) 


-- most likely, to be moved to another file to keep this stuff and the pure code separate
projectCode :: String -> String
projectCode = error "not yet implemented"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    let argStr = intercalate " " args
    case checkSpaces argStr of
      True -> forever $ do
          putStr "> "
          c <- getLine
          putStrLn $ projectCode c
      False -> do
          f <- runMaybeT $ safeRead argStr
          case f of
            Nothing -> putStrLn $ "Could not read your file named: " <> argStr
            Just text -> putStrLn $ projectCode text



