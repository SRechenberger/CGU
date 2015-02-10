module LLVM.General.CGU.CodeGenState.NameSupply
(
	NameSupply,
	freshSupply,
	requestName,
	requestNames
)
where

	import Prelude hiding (lookup)

	import Data.Map (Map, adjust, insert, lookup, empty)
	import Data.Word (Word)

	import LLVM.General.AST.Name (Name (Name))

	data NameSupply = NameSupply 
		{ table :: Map String Word } deriving Show

	freshSupply :: NameSupply
	freshSupply = NameSupply empty

	requestName :: String -> NameSupply -> (Name, NameSupply)
	requestName name supply = case name `lookup` table supply of 
		Nothing -> (Name name, supply { table = insert name 0 (table supply) })
		Just n  -> (Name $ name ++ show n, supply { table = adjust (+1) name (table supply) }) 

	requestNames :: [String] -> NameSupply -> ([Name], NameSupply)
	requestNames [] supply 	   = ([], supply)
	requestNames (n:ns) supply = (n':ns', supply'')
		where
			(n',supply') = requestName n supply
			(ns',supply'') = requestNames ns supply'

