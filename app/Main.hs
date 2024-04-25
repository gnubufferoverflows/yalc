{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad.Trans.Maybe
import System.IO
import Control.Exception
import Data.Char
import System.Environment
import Data.List
import Control.Monad
import Data.Maybe
import Control.Applicative

type Label = [Char]
data Lambda = Var Label | Apply Lambda Lambda | Lam Label Lambda

subst :: Label -> Lambda -> Lambda -> Lambda 
subst var what expr = case expr of 
                        Var x -> if x == var then what else Var x 
                        Apply e1 e2 -> Apply (subst var what e1) (subst var what e2)
                        Lam x e -> if x == var then Lam x e else Lam x (subst var what e) 

betaReduce :: Lambda -> Maybe Lambda
betaReduce expr = case expr of 
                    Apply (Lam x e1) e2 -> Just $ subst x e2 e1
                    Apply e1 e2 -> Apply <$> betaReduce e1 <*> pure e2 <|> Apply e1 <$> betaReduce e2
                    Lam x e -> Lam x <$> betaReduce e 
                    _ -> Nothing

-- runs beta reduction + alpha conversion
lambdaReduce :: Lambda -> Lambda
lambdaReduce expr = fromMaybe expr $ do 
    reduced <- betaReduce expr 
    return $ lambdaReduce reduced


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



