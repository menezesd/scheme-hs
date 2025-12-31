module Env
    ( isBound
    , getVar
    , setVar
    , defineVar
    , bindVars
    , extendEnv
    ) where

import Types
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.Map.Strict as Map

-- | Check if a variable is bound in the environment
isBound :: Env -> String -> IO Bool
isBound envRef var = do
    env <- readIORef envRef
    return $ Map.member var env

-- | Get a variable's value
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    case Map.lookup var env of
        Just ref -> liftIO $ readIORef ref
        Nothing  -> throwError $ UnboundVar "Getting an unbound variable" var

-- | Set a variable's value (must already exist)
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    case Map.lookup var env of
        Just ref -> liftIO $ writeIORef ref value >> return value
        Nothing  -> throwError $ UnboundVar "Setting an unbound variable" var

-- | Define a variable (create or update)
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef (Map.insert var valueRef env)
            return value

-- | Bind multiple variables at once
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do
    env <- readIORef envRef
    newBindings <- mapM addBinding bindings
    newIORef (Map.union (Map.fromList newBindings) env)
  where
    addBinding (var, value) = do
        ref <- newIORef value
        return (var, ref)

-- | Create a new environment extending the given one
extendEnv :: Env -> [(String, LispVal)] -> IO Env
extendEnv = bindVars
