-- *** Implicit conversion of String to T.Text.
{-# LANGUAGE OverloadedStrings #-}

-- Exports just the necessary.
module WExpression
( eval
, fname
, run
, pExpression
, pFuncDecls
) where

-- IMPORTS
import Control.Applicative
    ( (*>)
    , (<*)
    , (<$>)
    , (<|>)
    )

import Control.Monad

import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import Data.Attoparsec.Text (Parser)

import Data.Char

import qualified Data.Text as T


-- TYPES
type Id = String

data Expression = Const Int
                | Add Expression Expression
                | Sub Expression Expression
                | Let Id Expression Expression
                | ExpId Id
                | App Id [Expression]
                deriving(Show)

type Name  = String

type Parms = [Name]

type Body  = Expression

data FuncDecl = FuncDecl Name Parms Body deriving (Show)

type Env = [(Id, Expression)]

type FuncDecls = [FuncDecl]

-- HELPER FUNCTIONS
fname :: FuncDecl -> Id
fname (FuncDecl name parms body) = name

-- MAIN FUNCTIONS
run :: Expression -> FuncDecls -> Int
run e fds = snd (eval e [] fds)

eval :: Expression -> Env -> FuncDecls -> (Env, Int)
eval (Const val) env fds = (env, val)

eval (Add exp1 exp2) env fds = (env, val1 + val2)
    where
        val1 = (snd (eval exp1 env fds))
        val2 = (snd (eval exp2 env fds))

eval (Sub exp1 exp2) env fds = (env, val1 - val2)
    where
        val1 = (snd (eval exp1 env fds))
        val2 = (snd (eval exp2 env fds))

eval (Let idt exp1 exp2) env fds = (env', val)
    where
        env' =
            -- Checks whether an expression with the same ID exists in the environment.
            if idt `elem` [fst x | x <- env]
                -- If it exists, "removes" it from the environment (avoiding double definition exceptions) and inserts the last value.
                -- It allows for "nested lets" or context.
                then (idt, exp1') : filter (\x -> (fst x) /= idt) env
            -- Else, just inserts the new value.
            -- *** Inserting in the beginning of the list is faster than appending to the end.
            else (idt, exp1') : env
        val  = snd (eval exp2 env' fds)
        exp1' = Const (snd (eval exp1 env fds))

eval (ExpId idt) env fds =
    let exp = [snd x | x <- env, idt == fst x]
        in case exp of
            []     -> error (idt ++ " not in scope.")
            [x]    -> eval x env fds
            (x:xs) -> error ("multiple declarations of " ++ idt)

eval (App idt args) env fds =
    let fdecl = [x | x <- fds, idt == fname x]
        in case fdecl of
            []     ->  error ("function " ++ idt ++ " not defined.")
            [(FuncDecl n parms body)] -> (env, val)
                where
                    val  = snd (eval body env' fds)
        -- When applying functions, we first (eagerly) resolve the "args" and "update" the environment with the new value.
        --
        -- We were having trouble in the following situations:
        --
        --    The supplied implementation of env' was the following: env' = ((zip parms args) ++ env). It leads to CONFLICTING NAMES (Environment Identifiers vs Function Parameters' Identifiers);
        --
        --    Firstly, we tried solving the problem by just removing "env", as: env' = (zip parms args). It was sufficient for some situations, but not for all. Consider the following example:
        --
        --      eval (Let "x" (Const 2) (App "sum" [ExpId "x", Const 2])) [] [FuncDecl "sum" ["x","y"] (Add (ExpId "x") (ExpId "y"))]
        --
        -- When the formal parameters and identifiers in the context (used as actual parameters) have the same name, we fall into INFINITE RECURSION, that is, in the case of two expressions both holding the same identifier, if we consider the last added as the correct one (as we did previously, discarding the previous value of "x" -- which was in the environment -- and updating it to the value of the actual parameter), how could we resolve a parameter holding an expression like: ExpId "x"? (...). The context would remain something like: [("x", ExpId "x")], which is not resolvable.
                    env' = zip parms (Const <$> (snd <$> ((eval' env) <$> args))) -- Just maps over the args evaluating them.
                    eval' env exp = eval exp env []
            (f:fs) -> error ("multiple definitions of " ++ idt)

-- PARSERS
-- Helpers
pId :: Parser Id
pId = do A.skipSpace
         id@(x:xs) <- fmap T.unpack (A.takeWhile isAlphaNum)
         if isAlpha x
           then return id
         else fail $ "Invalid identifier: " ++ id ++ "."

pSymbol :: T.Text -> Parser T.Text
pSymbol s = do A.skipSpace
               A.string s


-- Parsers for Expressions
pConst :: Parser Expression
pConst = Const <$> (A.skipSpace *> A.decimal)

pLet :: Parser Expression
pLet = do pSymbol "let"
          id <- pId
          pSymbol "="
          e1 <- pExpression
          pSymbol "in"
          e2 <- pExpression
          return (Let id e1 e2)

pExpId :: Parser Expression
pExpId = do id <- pId
            return (ExpId id)

pApp :: Parser Expression
pApp = do id <- pId
          pSymbol "("
          params <- (pExpression <* A.skipSpace) `AC.sepBy` ","
          pSymbol ")"
          return (App id params)

pAddSub :: Parser Expression
pAddSub = do f <- pFactor
             do pSymbol "+"
                e <- pExpression
                return (Add f e)
                <|> do pSymbol "-"
                       e <- pExpression
                       return (Sub f e)
                <|> return f

pExpression :: Parser Expression
pExpression = pLet    <|>
              pApp    <|>
              pAddSub <|>
              pExpId

pFactor :: Parser Expression
pFactor = do pSymbol "("
             e <- pExpression
             pSymbol ")"
             return e
             <|> pConst
             <|> pExpId


-- Parsers for Functions
pFuncDecl :: Parser FuncDecl
pFuncDecl = do
            name <- pId
            pSymbol "("
            params <- (pId <* A.skipSpace) `AC.sepBy` ","
            pSymbol ")"
            pSymbol "="
            expr <- pExpression
            return (FuncDecl name params expr)

pFuncDecls :: Parser [FuncDecl]
pFuncDecls = A.many' pFuncDecl

