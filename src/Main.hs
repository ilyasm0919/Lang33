module Main where

import Syntax hiding (Expr)
import qualified Data.Map as M
import Text.Parsec (parseTest)
import Parse
import Tc (checkProgram)
import Data.Foldable (traverse_)
import TcSyntax
import Builtins
import CodeGen (compileProgram)
import System.Environment (getArgs)
import Control.Monad (when)
import System.Exit (exitSuccess, exitFailure)
import Data.Functor (($>))
import Language.Haskell.Exts (prettyPrint)

main :: IO ()
main = do
    as <- getArgs
    when (null as) showUsing
    out <- if length as > 1
        then when (length as > 2) showUsing $> writeFile (as !! 1)
        else pure putStrLn
    s <- readFile $ head as
    case parseProgram s >>= checkProgram builtins of
        Left err -> putStrLn err >> exitFailure
        Right (is, fs) -> out . prettyPrint $ compileProgram is fs

showUsing :: IO ()
showUsing = putStrLn "Using: lang33 source [file]" >> exitSuccess
