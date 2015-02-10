module LLVM.General.CGU.CodeGenState.SymbolTable
(
	SymbolTable,
	Symbol (original, real, operand),
	findBy,
	ascend,
	descend,
	assignSymbol,
	freshSymbolTable
)
where 
	
	import Data.List (intercalate, find)
	import Data.Word (Word)

	import LLVM.General.AST.Operand (Operand)
	import LLVM.General.AST.Name (Name)
	
	import Control.Monad.Error.Class (throwError)

	import LLVM.General.CGU.CodeGenMonad (CodeGen)
	import LLVM.General.CGU.CodeGenException (CodeGenException, exception)

	data Symbol = Symbol 
		{ original :: String
		, real 	   :: Name
		, operand  :: Operand 
		}

	data SymbolTable = SymbolTable 
		{ levels :: [Word] 
		, table  :: [Symbol] 
		}

	instance Show Symbol where
		show (Symbol o r op) = "Symbol " ++ o ++ " known as " ++ show r ++ " with operand " ++ show op

	instance Show SymbolTable where
		show = intercalate "\n" . map show . table

	freshSymbolTable :: SymbolTable 
	freshSymbolTable = SymbolTable [] []

	findBy :: (Eq a, Eq b, Show a) 
		=> (Symbol -> a) 	-- ^ Function to get symbol identifier; e.g. @original@, @real@ or @operand@.
		-> a  				-- ^ Value, to which the symbol identifier is to be compared.
		-> (Symbol -> b) 	-- ^ Function to get the value of the symbol; e.g. @original@, @real@ or @operand@.
		-> SymbolTable  	-- ^ Symbol table in which to search.
		-> CodeGen s CodeGenException b 
	findBy toFind sought toGet symtab = case ((== sought) . toFind) `find` table symtab of 
		Nothing -> throwError $ exception "findBy" $ "Could not find " ++ show sought ++ "."
		Just s  -> return $ toGet s

	findScopedBy :: (Eq a, Eq b, Show a) 
		=> (Symbol -> a) 	-- ^ Function to get symbol identifier; e.g. @original@, @real@ or @operand@.
		-> a  				-- ^ Value, to which the symbol identifier is to be compared.
		-> (Symbol -> b) 	-- ^ Function to get the value of the symbol; e.g. @original@, @real@ or @operand@.
		-> SymbolTable  	-- ^ Symbol table in which to search.
		-> CodeGen s CodeGenException b 
	findScopedBy toFind sought toGet symtab = do 
		limit <- case levels symtab of 
			[] -> throwError $ exception "findScopedBy" $ "There are no scopes marked."
			x:_ -> return . fromEnum $ x
		case ((== sought) . toFind) `find` (take limit $ table symtab) of 
			Nothing -> throwError $ exception "findScopedBy" $ "Could not find " ++ show sought ++ "."
			Just s  -> return $ toGet s

	assignSymbol :: String -> Name -> Operand -> SymbolTable -> SymbolTable
	assignSymbol o r op s = s { levels = (l+1):ls, table = Symbol o r op : table s }
		where 
			l:ls = levels s


	ascend :: SymbolTable -> CodeGen s CodeGenException SymbolTable
	ascend s = case levels s of
		[]   -> throwError $ exception "ascend" $ "Could not ascend further."
		x:xs -> return $ s { levels = xs, table = drop (fromEnum x) (table s) }

	descend :: SymbolTable -> CodeGen s CodeGenException SymbolTable
	descend s = return s {levels = 0 : levels s}
