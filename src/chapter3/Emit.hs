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
import qualified Syntax as S

import Data.ByteString.Short
import Control.Monad.Trans.Except (runExceptT)

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

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn $ byteStringToString llstr
    return newast
  where
    modn    = mapM (codegenTop mod) fns
    newast  = runLLVM mod modn
