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
data Lambda = Var Label | Apply Lambda Lambda | Lam Label Lambda deriving Show

instance Semigroup Lambda where 
    (<>) = Apply

instance Monoid Lambda where 
    mempty = identityLambda

identityLambda :: Lambda 
identityLambda = Lam "x" (Var "x")

trueLambda :: Lambda
trueLambda = Lam "x" (Lam "y" (Var "x"))

falseLambda :: Lambda
falseLambda = Lam "x" (Lam "y" (Var "y"))

andLambda :: Lambda
andLambda = Lam "p" (Lam "q" (Apply (Apply (Var "p") (Var "q")) (Var "p")))

orLambda :: Lambda
orLambda = Lam "p" (Lam "q" (Apply (Apply (Var "p") (Var "p")) (Var "q")))

notLambda :: Lambda
notLambda = Lam "p" (Apply (Apply (Var "p") falseLambda) trueLambda)

newVar :: Lambda -> Label -> Label
newVar expr var = head $ filter (`notElem` freeVars expr) (fmap (var ++) (fmap show [1..]))

freeVars :: Lambda -> [Label]
freeVars (Var x)       = [x]
freeVars (Lam x e)     = freeVars e \\ [x]
freeVars (Apply e1 e2) = freeVars e1 `union` freeVars e2


alphaConvert :: Label -> Lambda -> Lambda -> Lambda
alphaConvert x what expr = case expr of
                                Lam y e | x /= y && y `elem` freeVars what ->
                                    let y' = newVar expr y
                                    in Lam y' (alphaConvert x what (subst y (Var y') e))
                                _ -> expr

subst :: Label -> Lambda -> Lambda -> Lambda
subst var what expr = case alphaConvert var what expr of
                           Var x -> if x == var then what else Var x
                           Apply e1 e2 -> Apply (subst var what e1) (subst var what e2)
                           Lam x e -> if x == var then Lam x e else Lam x (subst var what e)

betaReduce :: Lambda -> Maybe Lambda
betaReduce expr = case expr of
                       Apply (Lam x e1) e2 -> Just $ subst x e2 e1
                       Apply e1 e2 ->
                            case betaReduce e1 of
                                 Just e1' -> Just $ Apply e1' e2
                                 Nothing -> Apply e1 <$> betaReduce e2
                       Lam x e -> Lam x <$> betaReduce e
                       _ -> Nothing


lambdaReduce :: Lambda -> Lambda
lambdaReduce expr = case betaReduce expr of
                         Just expr' -> lambdaReduce expr'
                         Nothing -> expr



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



