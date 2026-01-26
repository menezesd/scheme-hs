{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Bytecode Virtual Machine for Scheme
--
-- This module implements the bytecode interpreter. The VM uses:
--
-- * A value stack for operands and results
-- * A call stack for function frames
-- * De Bruijn indexed environments for O(1) variable access
--
-- == Execution Model
--
-- The VM executes bytecode in a loop, dispatching on each opcode.
-- Tail calls are implemented by reusing the current frame.
module VM
    ( -- * VM types
      VM(..)
    , VMFrame(..)
    , VMResult(..)
      -- * Execution
    , runVM
    , runVMWithArgs
    , vmInit
    , vmPushArg
      -- * Conversion
    , lispValToConst
    , constToLispVal
    ) where

import Types
import Bytecode
import Env (getVar, setVar, defineVar)
import JIT (getCompiledCode)

import Control.Monad (when, unless, replicateM)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Complex (Complex(..))
import Data.Ratio (Ratio, (%), numerator, denominator)

-- | VM state
data VM = VM
    { vmStack  :: IORef (MV.IOVector LispVal)  -- Value stack
    , vmSP     :: IORef Int                     -- Stack pointer
    , vmFrames :: IORef [VMFrame]               -- Call stack (return addresses)
    , vmLocals :: IORef [IORef LispVal]         -- Current frame's locals
    , vmCode   :: IORef CodeObject              -- Current code
    , vmIP     :: IORef Int                     -- Instruction pointer
    , vmEnv    :: IORef Env                     -- Global environment
    }

-- | Call frame
data VMFrame = VMFrame
    { vfCode   :: CodeObject       -- Code being executed
    , vfIP     :: Int              -- Return address
    , vfBP     :: Int              -- Base pointer (stack frame start)
    , vfLocals :: [IORef LispVal]  -- Local variables
    }

-- | VM execution result
data VMResult
    = VMDone LispVal
    | VMError LispError
    deriving (Show)

-- | Stack size
stackSize :: Int
stackSize = 10000

-- | Initialize the VM
vmInit :: Env -> IO VM
vmInit env = do
    stack <- MV.replicate stackSize Void
    stackRef <- newIORef stack
    spRef <- newIORef 0
    framesRef <- newIORef []
    localsRef <- newIORef []
    codeRef <- newIORef emptyCodeObject
    ipRef <- newIORef 0
    envRef <- newIORef env
    return VM
        { vmStack = stackRef
        , vmSP = spRef
        , vmFrames = framesRef
        , vmLocals = localsRef
        , vmCode = codeRef
        , vmIP = ipRef
        , vmEnv = envRef
        }

-- | Run bytecode on the VM
runVM :: VM -> CodeObject -> IO VMResult
runVM vm code = do
    writeIORef (vmCode vm) code
    writeIORef (vmIP vm) 0
    writeIORef (vmSP vm) 0
    writeIORef (vmFrames vm) []
    vmLoop vm

-- | Run bytecode with arguments (for calling compiled functions)
runVMWithArgs :: VM -> CodeObject -> [LispVal] -> IO VMResult
runVMWithArgs vm code args = do
    writeIORef (vmCode vm) code
    writeIORef (vmIP vm) 0
    writeIORef (vmSP vm) 0
    writeIORef (vmFrames vm) []
    -- Set current frame's locals to the arguments
    locals <- mapM newIORef args
    writeIORef (vmLocals vm) locals
    vmLoop vm

-- | Push an argument onto the VM stack (for setup)
vmPushArg :: VM -> LispVal -> IO ()
vmPushArg = vmPush

-- | Main VM loop
vmLoop :: VM -> IO VMResult
vmLoop vm = do
    ip <- readIORef (vmIP vm)
    code <- readIORef (vmCode vm)
    let ops = coCode code
    if ip >= V.length ops
        then do
            sp <- readIORef (vmSP vm)
            if sp > 0
                then vmPop vm >>= return . VMDone
                else return $ VMDone Void
        else do
            let op = ops V.! ip
            modifyIORef' (vmIP vm) (+1)
            result <- vmStep vm op
            case result of
                Nothing -> vmLoop vm
                Just r -> return r

-- | Execute one instruction
vmStep :: VM -> Opcode -> IO (Maybe VMResult)
vmStep vm op = case op of
    OP_CONST idx -> do
        code <- readIORef (vmCode vm)
        let c = coConstants code V.! idx
        vmPush vm (constToLispVal c)
        return Nothing

    OP_POP -> do
        _ <- vmPop vm
        return Nothing

    OP_DUP -> do
        val <- vmPeek vm
        vmPush vm val
        return Nothing

    OP_LOOKUP depth offset -> do
        if depth == 0
            then do
                -- Current frame's locals
                locals <- readIORef (vmLocals vm)
                if offset < length locals
                    then do
                        val <- readIORef (locals !! offset)
                        vmPush vm val
                    else vmPush vm Void
            else do
                -- Enclosing frame's locals (on call stack)
                frames <- readIORef (vmFrames vm)
                if depth <= length frames
                    then do
                        let frame = frames !! (depth - 1)
                            locals = vfLocals frame
                        if offset < length locals
                            then do
                                val <- readIORef (locals !! offset)
                                vmPush vm val
                            else vmPush vm Void
                    else vmPush vm Void
        return Nothing

    OP_SET depth offset -> do
        val <- vmPop vm
        if depth == 0
            then do
                -- Current frame's locals
                locals <- readIORef (vmLocals vm)
                when (offset < length locals) $
                    writeIORef (locals !! offset) val
            else do
                -- Enclosing frame's locals
                frames <- readIORef (vmFrames vm)
                when (depth <= length frames) $ do
                    let frame = frames !! (depth - 1)
                        locals = vfLocals frame
                    when (offset < length locals) $
                        writeIORef (locals !! offset) val
        return Nothing

    OP_LOOKUP_GLOBAL idx -> do
        code <- readIORef (vmCode vm)
        let CSymbol name = coConstants code V.! idx
        env <- readIORef (vmEnv vm)
        result <- runExceptT $ getVar env name
        case result of
            Left _ -> vmPush vm Void
            Right val -> vmPush vm val
        return Nothing

    OP_SET_GLOBAL idx -> do
        val <- vmPop vm
        code <- readIORef (vmCode vm)
        let CSymbol name = coConstants code V.! idx
        env <- readIORef (vmEnv vm)
        _ <- runExceptT $ setVar env name val
        return Nothing

    OP_DEFINE -> do
        nameVal <- vmPop vm
        val <- vmPop vm
        case nameVal of
            Atom name -> do
                env <- readIORef (vmEnv vm)
                _ <- runExceptT $ defineVar env name val
                return ()
            _ -> return ()
        return Nothing

    OP_CLOSURE idx -> do
        code <- readIORef (vmCode vm)
        let childCode = coChildren code V.! idx
        -- Create a closure (simplified - full implementation would capture env)
        env <- readIORef (vmEnv vm)
        let func = CompiledFunc childCode env
        vmPush vm func
        return Nothing

    OP_CALL argc -> do
        func <- vmPop vm
        args <- vmPopN vm argc
        callFunc vm func args False

    OP_TAILCALL argc -> do
        func <- vmPop vm
        args <- vmPopN vm argc
        callFunc vm func args True

    OP_RETURN -> do
        frames <- readIORef (vmFrames vm)
        if null frames
            then do
                val <- vmPop vm
                return $ Just $ VMDone val
            else do
                result <- vmPop vm
                let frame = head frames
                writeIORef (vmFrames vm) (tail frames)
                writeIORef (vmCode vm) (vfCode frame)
                writeIORef (vmIP vm) (vfIP frame)
                -- Restore stack to base pointer
                writeIORef (vmSP vm) (vfBP frame)
                -- Restore locals from the return frame
                writeIORef (vmLocals vm) (vfLocals frame)
                vmPush vm result
                return Nothing

    OP_JUMP target -> do
        writeIORef (vmIP vm) target
        return Nothing

    OP_JUMPIF target -> do
        val <- vmPop vm
        case val of
            Bool False -> return Nothing
            _ -> do
                writeIORef (vmIP vm) target
                return Nothing

    OP_JUMPIFNOT target -> do
        val <- vmPop vm
        case val of
            Bool False -> do
                writeIORef (vmIP vm) target
                return Nothing
            _ -> return Nothing

    -- Specialized operations
    OP_CAR -> do
        val <- vmPop vm
        case val of
            List (x:_) -> vmPush vm x
            DottedList (x:_) _ -> vmPush vm x
            _ -> vmPush vm Void
        return Nothing

    OP_CDR -> do
        val <- vmPop vm
        case val of
            List (_:xs) -> vmPush vm (List xs)
            DottedList [_] t -> vmPush vm t
            DottedList (_:xs) t -> vmPush vm (DottedList xs t)
            _ -> vmPush vm Void
        return Nothing

    OP_CONS -> do
        cdr' <- vmPop vm
        car' <- vmPop vm
        case cdr' of
            List xs -> vmPush vm (List (car' : xs))
            _ -> vmPush vm (DottedList [car'] cdr')
        return Nothing

    OP_NULLP -> do
        val <- vmPop vm
        case val of
            List [] -> vmPush vm (Bool True)
            _ -> vmPush vm (Bool False)
        return Nothing

    OP_PAIRP -> do
        val <- vmPop vm
        case val of
            List (_:_) -> vmPush vm (Bool True)
            DottedList _ _ -> vmPush vm (Bool True)
            _ -> vmPush vm (Bool False)
        return Nothing

    OP_NOT -> do
        val <- vmPop vm
        case val of
            Bool False -> vmPush vm (Bool True)
            _ -> vmPush vm (Bool False)
        return Nothing

    OP_ADD -> vmBinaryNumOp vm (+)
    OP_SUB -> vmBinaryNumOp vm (-)
    OP_MUL -> vmBinaryNumOp vm (*)
    OP_DIV -> vmBinaryNumOp vm div'
    OP_NUMEQ -> vmNumCompare vm (==)
    OP_LT -> vmNumCompare vm (<)
    OP_GT -> vmNumCompare vm (>)
    OP_LE -> vmNumCompare vm (<=)
    OP_GE -> vmNumCompare vm (>=)

    OP_ADD1 -> do
        val <- vmPop vm
        case val of
            Number (SInteger n) -> vmPush vm (mkInteger (n + 1))
            Number (SReal d) -> vmPush vm (Number (SReal (d + 1)))
            _ -> vmPush vm val
        return Nothing

    OP_SUB1 -> do
        val <- vmPop vm
        case val of
            Number (SInteger n) -> vmPush vm (mkInteger (n - 1))
            Number (SReal d) -> vmPush vm (Number (SReal (d - 1)))
            _ -> vmPush vm val
        return Nothing

    OP_ZEROP -> do
        val <- vmPop vm
        case val of
            Number (SInteger 0) -> vmPush vm (Bool True)
            Number (SReal 0.0) -> vmPush vm (Bool True)
            _ -> vmPush vm (Bool False)
        return Nothing

    OP_EQ -> do
        b <- vmPop vm
        a <- vmPop vm
        -- Simple eq? (pointer equality for primitives)
        vmPush vm (Bool (a == b))
        return Nothing

    OP_LIST argc -> do
        args <- vmPopN vm argc
        vmPush vm (List args)
        return Nothing

    OP_VECTOR argc -> do
        args <- vmPopN vm argc
        -- Create vector from args
        vmPush vm (List args)  -- Simplified
        return Nothing

    OP_VOID -> do
        vmPush vm Void
        return Nothing

    OP_HALT -> do
        sp <- readIORef (vmSP vm)
        if sp > 0
            then vmPop vm >>= return . Just . VMDone
            else return $ Just $ VMDone Void

    OP_CALLCC -> do
        -- Simplified call/cc - full implementation needs continuation capture
        return $ Just $ VMError $ Default "call/cc not fully implemented in VM"

    OP_PRIM _ _ -> do
        -- Generic primitive dispatch
        return $ Just $ VMError $ Default "Generic primitive not implemented"

-- | Call a function
callFunc :: VM -> LispVal -> [LispVal] -> Bool -> IO (Maybe VMResult)
callFunc vm func args isTail = case func of
    CompiledFunc childCode _ -> do
        -- Set up new frame
        ip <- readIORef (vmIP vm)
        code <- readIORef (vmCode vm)
        sp <- readIORef (vmSP vm)
        currentLocals <- readIORef (vmLocals vm)

        -- Create new locals from arguments
        newLocals <- mapM newIORef args

        unless isTail $ do
            -- Save current frame (with current locals) for return
            let frame = VMFrame code ip sp currentLocals
            modifyIORef' (vmFrames vm) (frame :)

        -- Switch to new code and set new locals
        writeIORef (vmCode vm) childCode
        writeIORef (vmIP vm) 0
        writeIORef (vmLocals vm) newLocals

        return Nothing

    PrimitiveFunc f -> do
        result <- runExceptT $ liftThrows $ f args
        case result of
            Left err -> return $ Just $ VMError err
            Right val -> do
                vmPush vm val
                return Nothing

    IOFunc f -> do
        result <- runExceptT $ f args
        case result of
            Left err -> return $ Just $ VMError err
            Right val -> do
                vmPush vm val
                return Nothing

    Func _ _ _ funcClosure maybeFid -> do
        -- Check if this function has been JIT compiled
        case maybeFid of
            Just fid -> do
                maybeCode <- getCompiledCode fid
                case maybeCode of
                    Just code ->
                        -- Use the cached compiled code
                        callFunc vm (CompiledFunc code funcClosure) args isTail
                    Nothing ->
                        -- Not compiled yet, can't handle in VM
                        return $ Just $ VMError $ Default "Non-compiled function in VM"
            Nothing ->
                return $ Just $ VMError $ Default "Non-compiled function in VM"

    _ -> return $ Just $ VMError $ NotFunction "Not a function" (show func)

-- | Stack operations
vmPush :: VM -> LispVal -> IO ()
vmPush vm val = do
    sp <- readIORef (vmSP vm)
    stack <- readIORef (vmStack vm)
    MV.write stack sp val
    writeIORef (vmSP vm) (sp + 1)

vmPop :: VM -> IO LispVal
vmPop vm = do
    sp <- readIORef (vmSP vm)
    stack <- readIORef (vmStack vm)
    let sp' = sp - 1
    val <- MV.read stack sp'
    writeIORef (vmSP vm) sp'
    return val

vmPopN :: VM -> Int -> IO [LispVal]
vmPopN vm n = reverse <$> replicateM n (vmPop vm)

vmPeek :: VM -> IO LispVal
vmPeek vm = do
    sp <- readIORef (vmSP vm)
    stack <- readIORef (vmStack vm)
    MV.read stack (sp - 1)

-- | Binary numeric operations (optimized with small int cache)
vmBinaryNumOp :: VM -> (Integer -> Integer -> Integer) -> IO (Maybe VMResult)
vmBinaryNumOp vm op = do
    b <- vmPop vm
    a <- vmPop vm
    case (a, b) of
        (Number (SInteger x), Number (SInteger y)) ->
            vmPush vm (mkInteger (op x y))
        _ -> vmPush vm Void
    return Nothing

-- | Numeric comparison
vmNumCompare :: VM -> (Integer -> Integer -> Bool) -> IO (Maybe VMResult)
vmNumCompare vm op = do
    b <- vmPop vm
    a <- vmPop vm
    case (a, b) of
        (Number (SInteger x), Number (SInteger y)) ->
            vmPush vm (Bool (op x y))
        _ -> vmPush vm (Bool False)
    return Nothing

div' :: Integer -> Integer -> Integer
div' _ 0 = 0
div' a b = a `div` b

-- | Convert LispVal to ConstVal
lispValToConst :: LispVal -> Maybe ConstVal
lispValToConst (Number (SInteger n)) = Just $ CNumber n
lispValToConst (Number (SRational r)) = Just $ CRational (toRational r)
lispValToConst (Number (SReal d)) = Just $ CReal d
lispValToConst (Number (SComplex (r :+ i))) = Just $ CComplex r i
lispValToConst (String s) = Just $ CString s
lispValToConst (Char c) = Just $ CChar c
lispValToConst (Bool b) = Just $ CBool b
lispValToConst (Atom s) = Just $ CSymbol s
lispValToConst (List []) = Just CNull
lispValToConst Void = Just CVoid
lispValToConst _ = Nothing

-- | Convert ConstVal to LispVal
constToLispVal :: ConstVal -> LispVal
constToLispVal (CNumber n) = Number (SInteger n)
constToLispVal (CRational r) = Number (SRational r)
constToLispVal (CReal d) = Number (SReal d)
constToLispVal (CComplex r i) = Number (SComplex (r :+ i))
constToLispVal (CString s) = String s
constToLispVal (CChar c) = Char c
constToLispVal (CBool b) = Bool b
constToLispVal (CSymbol s) = Atom s
constToLispVal CNull = List []
constToLispVal CVoid = Void
