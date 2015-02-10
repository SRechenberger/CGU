{-# LANGUAGE FlexibleContexts #-}

module LLVM.General.CGU.CodeGenState.Block
(
	Block,
	newBlock,
	addInstruction,
	addTerminator,
	lookupBlock,
	forgeBasicBlock

)
where

	import Data.Word (Word)
	import Data.List (intercalate, sortBy, find)
	import Data.Function (on)

	import LLVM.General.AST.Instruction (Named, Instruction, Terminator)
	import LLVM.General.AST.Global (BasicBlock (..))
	import LLVM.General.AST.Name (Name)

	import Control.Monad.Error.Class (throwError)

	import LLVM.General.CGU.CodeGenMonad (CodeGen)
	import LLVM.General.CGU.CodeGenException (CodeGenException, exception)

	data Block = Block
		{ blockName :: Name
		, idx 		:: Word
		, instrs 	:: [Named Instruction]
		, term 		:: Maybe (Named Terminator)
		}

	instance Show Block where
		show (Block name ix is t) = "Block #" ++ show ix ++ " " ++ show name ++ " {"
			++ "\n" ++ intercalate "  \n" (map show is) 
			++ "  \n" ++ show t
			++ "\n}\n"

	addInstruction :: Named Instruction -> Block -> Block
	addInstruction instr block = block { instrs = instrs block ++ [instr] }

	addTerminator :: Named Terminator -> Block -> Block
	addTerminator terminator block = block { term = Just terminator }

	newBlock :: Name -> Word -> Block 
	newBlock name ix = Block name ix [] Nothing

	forgeBasicBlock :: Block -> CodeGen s CodeGenException BasicBlock 
	forgeBasicBlock (Block name _ is t) = case t of
		Nothing -> throwError $ exception "forgeBasicBlock" $ "Block " ++ show name ++ " is not terminated."
		Just tr -> return $ BasicBlock name is tr 

	sortBlocks :: [Block] -> [Block]
	sortBlocks = sortBy (compare `on` idx)

	lookupBlock :: Name -> [Block] -> CodeGen s CodeGenException Block 
	lookupBlock sought blocks = case ((== sought) . blockName) `find` blocks of
		Nothing -> throwError $ exception "lookupBlock" $ "Could not find block " ++ show sought ++ "."
		Just bl -> return bl
