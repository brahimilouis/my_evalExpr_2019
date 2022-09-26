module Lib
    ( funEvalExpr
    ) where

import System.Environment
import System.Exit
import System.IO

import Text.Printf
import Data.Char (isDigit, ord)
import Data.String

checkNextSignExpr :: String -> Bool
checkNextSignExpr []  = False
checkNextSignExpr ('^':expr) = True
checkNextSignExpr (l:expr) = checkNextSignExpr expr

puissance :: Float -> Float -> Float -> Float
puissance nb 0 res = res
puissance nb pui res = puissance nb (pui - 1) (res * nb)

lastSign :: String -> Int -> Int -> Int
lastSign [] last i = last
lastSign (l:rest) last i =
    if (l == ')')
      then last
    else if (l == '+' || l == '*' || l == '/' || l == '^')
      then lastSign rest i (i + 1)
      else lastSign rest last (i + 1)

testEnd :: String -> Bool
testEnd [] = True
testEnd (l:expr) =
  if (l == '*' || l == '/' || l == '+' || l == '^')
    then False
    else testEnd expr

exprReplace :: String -> String -> Int -> Int -> String
exprReplace [] res i1 i2 = res
exprReplace "erreur" res i1 i2 = "erreur"
exprReplace expr res i1 i2 = doReplace expr res i1 i2 [] 0
  where
    doReplace [] res i1 i2 final i = final
    doReplace (l:expr) res i1 i2 final i =
      if (i < i1)
        then doReplace expr res i1 i2 (final ++ [l]) (i + 1)
      else if (i == i1)
        then doReplace expr res i1 i2 (final ++ res) (i + 1)
      else if (i <= i2)
        then doReplace expr res i1 i2 final (i + 1)
      else doReplace expr res i1 i2 (final ++ [l]) (i + 1)


exprParse :: String -> Bool -> String
exprParse "erreur" bool = "erreur"
exprParse expr True =
  if (testEnd expr == True)
    then expr
    else exprParse expr False
exprParse expr nb = exprOrdre expr expr 0 ['f'] (length expr) 0 (lastSign expr 0 0)
    where
      exprOrdre original [] i1 (info:exprToCalc) i2 i last = exprParse (cleanExpr (exprReplace original (exprCalc exprToCalc) i1 i2) []) True
      exprOrdre original (l:rest) i1 (info:exprToCalc) i2 i last =
        if (l == '(')
           then exprOrdre original rest i ['f'] ((length expr) -1) (i + 1) last
        else if (l == ')')
          then exprParse (cleanExpr (exprReplace original (exprCalc exprToCalc) i1 i) []) True
        else if ((l == '^') && i < i2)
          then exprOrdre original rest i1 (['s'] ++ exprToCalc ++ [l]) i2 (i + 1) last
        else if ((l == '/' || l == '*') && info /= 's' && i < i2)
          then
          if (checkNextSignExpr rest == True)
             then exprOrdre original rest i ['f'] (i2) (i + 1) last
             else exprOrdre original rest i1 (['s'] ++ exprToCalc ++ [l]) i2 (i + 1) last
        else if (l == '+' && info == 's' && i2 < i)
         then exprOrdre original rest i1 ([info] ++ exprToCalc) i2 (i + 1) last
        else if ((l == '+' || l == '/' || l == '*' || l == '^') && info == 's')
          then
          if (checkNextSignExpr rest == True)
             then exprOrdre original rest i ['f'] (i2) (i + 1) last
             else exprOrdre original rest i1 ([info] ++ exprToCalc) (i - 1) (i + 1) last
        else if (l == '+' && i < last && info /= 's')
         then exprOrdre original rest (i + 1) ['f'] i2 (i + 1) last
        else if (l == '+' && i == last && info /= 's')
          then exprOrdre original rest i1 (['s'] ++ exprToCalc ++ [l]) i2 (i + 1) last
        else if (i2 > i)
          then exprOrdre original rest i1 ([info] ++ exprToCalc ++ [l]) i2 (i + 1) last
        else exprOrdre original rest i1 ([info] ++ exprToCalc) i2 (i + 1) last


exprCalc :: String -> String
exprCalc expr = dissocierExpr expr [] 0 [] 0
    where
        dissocierExpr [] saveExpr nb1 signe nb2 = (calc nb1 signe nb2)
        dissocierExpr (l:expr) saveExpr nb1 [] nb2 =
            if (l == '/' || l == '+' || l == '*' || l == '^')
                then dissocierExpr [] [] (read saveExpr) [l] (read expr)
                else dissocierExpr expr (saveExpr ++ [l]) nb1 [] nb2


calc :: Float -> String -> Float -> String
calc nb1 "+" nb2 = show (nb1 + nb2)
calc nb1 "-" nb2 = show (nb1 - nb2)
calc nb1 "*" nb2 = show (nb1 * nb2)
calc nb1 "/" nb2 =
  if (nb2 /= 0)
  then show (nb1 / nb2)
  else "erreur"
calc nb1 "^" nb2 =  show (puissance nb1 nb2 1)
calc nb1 expr nb2 = show nb1


cleanExpr :: String -> String -> String
cleanExpr [] res = res
cleanExpr "erreur" res = "erreur"
cleanExpr (' ':expr) res = cleanExpr expr res
cleanExpr ('-':expr) res =
  if (length res == 0)
    then cleanExpr expr (res ++ "-")
  else if ((last res) == '*' || (last res) == '+' || (last res) == '/' || (last res) == '^' || (last res) == '(')
     then cleanExpr expr (res ++ "-")
  else if (last res == '-')
     then cleanExpr expr res
     else cleanExpr expr (res ++ "+-")
cleanExpr ('(':')':expr) res = cleanExpr expr res
cleanExpr (l:expr) res = cleanExpr expr (res ++ [l])

checkErr :: String -> Bool
checkErr [] = False
checkErr expr = parsingExpr expr [] 0 0
  where
    parsingExpr [] res p1 p2 =
      if (p1 == p2)
        then True
        else False
    parsingExpr (l:expr) res p1 p2 =
      if (l == '(')
        then parsingExpr expr (res ++ [l]) (p1 + 1) (p2)
      else if (l == ')')
        then  if (last res == '(')
             then False
             else parsingExpr expr (res ++ [l]) (p1) (p2 + 1)
      else if ((length res /= 0) && ((last res) /= '^' && (last res) /= '*' && (last res) /= '/' && (last res) /= '+' && (last res) /= '-') && (l == '^' || l == '*' || l == '/' || l == '+'))
        then parsingExpr expr (res ++ [l]) (p1) (p2)
      else if ((isDigit l) == True || l == '.' || l == '-')
        then parsingExpr expr (res ++ [l]) (p1) (p2)
      else False

initExpr :: String -> String
initExpr expr = exprParse (cleanExpr expr []) True

printResult :: Double -> IO ()
printResult nb = putStrLn $ printf "%.2f" nb

funEvalExpr :: IO ()
funEvalExpr = do
    args <- getArgs
    case args of
        [] ->  exitWith (ExitFailure 84)
        [s] -> do
           let expr = cleanExpr (args!!0) []
           if (checkErr expr == False)
              then exitWith (ExitFailure 84)
              else do
              let res = (exprParse expr True) :: String
              if (res == "erreur")
                then exitWith (ExitFailure 84)
                else
                    printResult ((read res) :: Double)