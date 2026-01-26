{-# LANGUAGE DeriveGeneric #-}

-- | Bytecode Definitions for the Scheme VM
--
-- This module defines the instruction set and code objects for the bytecode VM.
-- The bytecode is a stack-based format designed for efficient Scheme execution.
--
-- == Instruction Set Design
--
-- The VM uses a stack-based architecture with these instruction categories:
--
-- * __Stack operations__: Push constants, pop values
-- * __Variable access__: Using De Bruijn indices for O(1) lookup
-- * __Control flow__: Jumps, conditionals
-- * __Function calls__: Regular calls and tail calls for TCO
-- * __Primitive operations__: Fast paths for common operations
--
-- == Tail Call Optimization
--
-- The 'OP_TAILCALL' instruction implements proper tail calls by reusing
-- the current stack frame, avoiding stack growth for recursive functions.
module Bytecode
    ( -- * Opcodes
      Opcode(..)
      -- * Code objects
    , CodeObject(..)
    , emptyCodeObject
      -- * Constants
    , ConstVal(..)
      -- * Primitive IDs
    , PrimId(..)
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | Primitive function identifiers for fast dispatch
data PrimId
    = PRIM_CAR
    | PRIM_CDR
    | PRIM_CONS
    | PRIM_NULLP
    | PRIM_PAIRP
    | PRIM_NOT
    | PRIM_EQ
    | PRIM_EQV
    | PRIM_ADD
    | PRIM_SUB
    | PRIM_MUL
    | PRIM_DIV
    | PRIM_LT
    | PRIM_GT
    | PRIM_LE
    | PRIM_GE
    | PRIM_NUMEQ
    | PRIM_ZEROP
    | PRIM_POSITIVEP
    | PRIM_NEGATIVEP
    | PRIM_ADD1
    | PRIM_SUB1
    | PRIM_LIST
    | PRIM_LENGTH
    | PRIM_APPEND
    | PRIM_REVERSE
    | PRIM_DISPLAY
    | PRIM_NEWLINE
    | PRIM_GENERIC Int  -- Generic primitive by index
    deriving (Eq, Show, Generic)

instance NFData PrimId

-- | Virtual machine opcodes
data Opcode
    -- Stack manipulation
    = OP_CONST Int           -- Push constants[idx] onto stack
    | OP_POP                 -- Pop and discard top of stack
    | OP_DUP                 -- Duplicate top of stack

    -- Variable access (De Bruijn indices)
    | OP_LOOKUP Int Int      -- Push variable at (depth, offset)
    | OP_SET Int Int         -- Set variable at (depth, offset) to TOS
    | OP_DEFINE              -- Define TOS-1 as name TOS in global env
    | OP_LOOKUP_GLOBAL Int   -- Push global variable by constant name index
    | OP_SET_GLOBAL Int      -- Set global variable by constant name index

    -- Closures and functions
    | OP_CLOSURE Int         -- Create closure from children[idx]
    | OP_CALL Int            -- Call function with argc arguments
    | OP_TAILCALL Int        -- Tail call (reuse current frame)
    | OP_RETURN              -- Return from function

    -- Control flow
    | OP_JUMP Int            -- Unconditional jump to offset
    | OP_JUMPIF Int          -- Jump if TOS is truthy
    | OP_JUMPIFNOT Int       -- Jump if TOS is falsy

    -- Primitive operations (fast paths)
    | OP_PRIM PrimId Int     -- Call primitive with argc args

    -- Specialized fast operations
    | OP_CAR                 -- (car TOS)
    | OP_CDR                 -- (cdr TOS)
    | OP_CONS                -- (cons TOS-1 TOS)
    | OP_NULLP               -- (null? TOS)
    | OP_PAIRP               -- (pair? TOS)
    | OP_NOT                 -- (not TOS)
    | OP_ADD                 -- (+ TOS-1 TOS)
    | OP_SUB                 -- (- TOS-1 TOS)
    | OP_MUL                 -- (* TOS-1 TOS)
    | OP_DIV                 -- (/ TOS-1 TOS)
    | OP_NUMEQ               -- (= TOS-1 TOS)
    | OP_LT                  -- (< TOS-1 TOS)
    | OP_GT                  -- (> TOS-1 TOS)
    | OP_LE                  -- (<= TOS-1 TOS)
    | OP_GE                  -- (>= TOS-1 TOS)
    | OP_ADD1                -- (+ TOS 1)
    | OP_SUB1                -- (- TOS 1)
    | OP_ZEROP               -- (zero? TOS)
    | OP_EQ                  -- (eq? TOS-1 TOS)

    -- Continuations
    | OP_CALLCC              -- call/cc

    -- End of program
    | OP_HALT                -- Stop execution

    -- List construction
    | OP_LIST Int            -- Create list from argc stack values
    | OP_VECTOR Int          -- Create vector from argc stack values

    -- Void
    | OP_VOID                -- Push void value

    deriving (Eq, Show, Generic)

instance NFData Opcode

-- | Compiled code object
--
-- A code object represents a compiled procedure or top-level expression.
-- It contains the bytecode instructions, constant pool, and nested closures.
data CodeObject = CodeObject
    { coCode      :: Vector Opcode    -- Bytecode instructions
    , coConstants :: Vector ConstVal  -- Constant pool
    , coChildren  :: Vector CodeObject -- Nested closures
    , coArity     :: Int              -- Number of required arguments
    , coHasRest   :: Bool             -- Has rest argument?
    , coName      :: Maybe String     -- Optional name (for debugging)
    , coLocals    :: Int              -- Number of local variables
    } deriving (Show, Generic)

instance NFData CodeObject

-- | Constant values in the constant pool
--
-- We use a separate type to avoid cyclic dependencies with LispVal
data ConstVal
    = CNumber Integer
    | CRational Rational
    | CReal Double
    | CComplex Double Double
    | CString String
    | CChar Char
    | CBool Bool
    | CSymbol String
    | CNull
    | CVoid
    deriving (Eq, Show, Generic)

instance NFData ConstVal

-- | Empty code object for initialization
emptyCodeObject :: CodeObject
emptyCodeObject = CodeObject
    { coCode = V.empty
    , coConstants = V.empty
    , coChildren = V.empty
    , coArity = 0
    , coHasRest = False
    , coName = Nothing
    , coLocals = 0
    }
