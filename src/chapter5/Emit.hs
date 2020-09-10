{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import LLVM.AST.Type
import LLVM.AST.AddrSpace
import LLVM.AST.Typed (typeOf)

import Data.Word
import Data.Int

import Control.Monad.Trans.State
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import JIT
import qualified Syntax as S

import Data.ByteString.Short
import Control.Monad.Trans.Except (runExceptT)

one = cons $ C.Float (F.Double 1.0)
zero = cons $ C.Float (F.Double 0.0)
false = zero
true = one

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name $ stringToShortBS x))

codegenTop :: AST.Module -> S.Expr -> LLVM ()
codegenTop mod (S.Function name args body) = do
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca double
        store var (local float (AST.Name $ stringToShortBS a))
        assign a var
      cgen mod body >>= ret

codegenTop mod (S.Extern name args) = do
  external double name fnargs
  where fnargs = toSig args

codegenTop mod exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen mod exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]

cgen :: AST.Module -> S.Expr -> Codegen AST.Operand
cgen mod (S.UnaryOp op a) = do
  cgen mod $ S.Call ("unary" ++ op) [a]
cgen mod (S.BinaryOp "=" (S.Var var) val) = do
  a <- getvar var
  cval <- cgen mod val
  store a cval
  return cval
cgen mod (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen mod a
      cb <- cgen mod b
      f ca cb
    Nothing -> error "No such operator"
cgen mod (S.Var x) = getvar x >>= load
cgen mod (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen mod (S.Call fn args) = do
  largs <- mapM (cgen mod) args
  let nm = AST.Name $ stringToShortBS fn
  let ty = evalLLVM mod (fnPtr nm) 
  call (externf ty nm) largs

cgen mod (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  -- %entry
  ------------------
  cond <- cgen mod cond
  test <- fcmp FP.ONE false cond
  cbr test ifthen ifelse -- Branch based on the condition

  -- if.then
  ------------------
  setBlock ifthen
  trval <- cgen mod tr       -- Generate code for the true branch
  br ifexit              -- Branch to the merge block
  ifthen <- getBlock

  -- if.else
  ------------------
  setBlock ifelse
  flval <- cgen mod fl       -- Generate code for the false branch
  br ifexit              -- Branch to the merge block
  ifelse <- getBlock

  -- if.exit
  ------------------
  setBlock ifexit
  phi double [(trval, ifthen), (flval, ifelse)]

cgen mod (S.For ivar start cond step body) = do
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"

  -- %entry
  ------------------
  i <- alloca double
  istart <- cgen mod start           -- Generate loop variable initial value
  stepval <- cgen mod step           -- Generate loop variable step

  store i istart                 -- Store the loop variable initial value
  assign ivar i                  -- Assign loop variable to the variable name
  br forloop                     -- Branch to the loop body block

  -- for.loop
  ------------------
  setBlock forloop
  cgen mod body                      -- Generate the loop body
  ival <- load i                 -- Load the current loop iteration
  inext <- fadd ival stepval     -- Increment loop variable
  store i inext

  cond <- cgen mod cond              -- Generate the loop condition
  test <- fcmp FP.ONE false cond -- Test if the loop condition is True ( 1.0 )
  cbr test forloop forexit       -- Generate the loop condition

  -- for.exit
  ------------------
  setBlock forexit
  return zero

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = do
  newast <- runJIT oldast
  return newast
  where
    modn    = mapM (codegenTop mod) fns
    oldast  = runLLVM mod modn

