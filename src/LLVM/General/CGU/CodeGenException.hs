module LLVM.General.CGU.CodeGenException
(
	CodeGenException (thrownBy, message),
	exception
)
where
	
	data CodeGenException = CodeGenException 
		{ thrownBy :: String 
		, message  :: String
		}

	instance Show CodeGenException where
		show (CodeGenException t m) = "Exception: " ++ t ++ " threw '" ++ m ++ "'."

	exception :: String -> String -> CodeGenException
	exception = CodeGenException