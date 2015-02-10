{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module LLVM.General.CGU.CodeGenMonad
(
	CodeGen, runCodeGen, liftError
)
where

	import Control.Applicative (Applicative (..), Alternative (..))
	import Control.Monad.State.Class (MonadState (..))
	import Control.Monad.IO.Class (MonadIO (..))
	import Control.Monad.Error.Class (MonadError (..))
	import Control.Monad.Except (ExceptT (..), runExceptT)

	data CodeGen state err a = CodeGen { runCodeGen :: state -> IO (Either err (a, state))}

	instance Monad (CodeGen s err) where
		return x = CodeGen $ \s -> return $ Right (x, s)
		ma >>= f = CodeGen $ \s -> do 
			result <- runCodeGen ma s 
			case result of 
				Right (x, s') -> runCodeGen (f x) s'
				Left msg     -> return $ Left msg

	instance Functor (CodeGen s err) where
		fmap f ma = CodeGen $ \s -> do 
			result <- runCodeGen ma s 
			return $ case result of 
				Right (x, s') -> Right (f x, s')
				Left msg    -> Left msg


	instance Applicative (CodeGen s err) where
		pure = return 
		mf <*> ma = do 
			f <- mf 
			a <- ma 
			return $ f a

	instance Alternative (CodeGen s err) where
		empty = fail "EMPTY"
		ma <|> mb = CodeGen $ \s -> do
			ra <- runCodeGen ma s 
			case ra of
				Right (x, s') -> return $ Right (x, s')
				Left msga 	  -> do 
					rb <- runCodeGen mb s 
					case rb of 
						Right (x, s') -> return $ Right (x, s')
						Left _        -> return $ Left msga 

	instance MonadState s (CodeGen s err) where
		get = CodeGen $ \s -> return $ Right (s,s)
		put s = CodeGen $ \_ -> return $ Right ((), s)

	instance MonadIO (CodeGen err s) where
		liftIO ma = CodeGen $ \s -> do 
			a <- ma 
			return $ Right (a, s)

	instance MonadError err (CodeGen s err) where
		throwError e = CodeGen $ \_ -> return $ Left e 
		ma `catchError` handler = CodeGen $ \s -> do 
			r <- runCodeGen ma s 
			case r of 
				Right x -> return $ Right x 
				Left  e -> runCodeGen (handler e) s

	liftError :: ExceptT err IO a -> CodeGen state err a
	liftError ma = CodeGen $ \s -> do
		r <- runExceptT ma 
		case r of 
			Right a -> return $ Right (a,s)
			Left  e -> return $ Left e