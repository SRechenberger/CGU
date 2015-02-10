module LLVM.General.CGU.CodeGenState.Local
(

)
where

	import LLVM.General.CGU.CodeGenState.NameSupply (NameSupply, requestNames, freshSupply)
	import LLVM.General.CGU.CodeGenState.SymbolTable (SymbolTable, freshSymbolTable, assignSymbol)
	import LLVM.General.CGU.CodeGenState.Block (Block, newBlock)

	import LLVM.General.CGU.CodeGenMonad (CodeGen)
	import LLVM.General.CGU.CodeGenException (CodeGenException)

	import LLVM.General.AST.Operand (Operand (LocalReference))
	import LLVM.General.AST.Name (Name (Name))
	import LLVM.General.AST.Type (Type)

	import Data.Word (Word)
	import Data.List (zip4)

	data Local = Local 
		{ localName 	  :: Name
		, returnType 	  :: Type 
		, params 		  :: [(Type,Name)] 
		, localNameSupply :: NameSupply
		, localSymbols 	  :: SymbolTable
		, blockCnt 		  :: Word
		, instrCnt 		  :: Word
		, currentBlock 	  :: Name 
		, blocks 		  :: [Block] 
		} deriving Show


	newLocal :: () 
		=> Name 
		-> Type 
		-> [(Type,String)]
		-> Local 
	newLocal name retty params = Local name retty (types `zip` names') supply symbols 1 0 (Name "entry") [newBlock (Name "entry") 0]	
		where
			(types, names)  = unzip params 
			(names',supply) = requestNames names freshSupply			
			symbols = foldr (\(n,n',o, t) tab -> assignSymbol n n' (LocalReference t n') tab) freshSymbolTable (zip4 names names' names' types) 
