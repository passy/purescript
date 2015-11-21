-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Common
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Common code generation utility functions
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.JS.Common where

import Data.Char
import Data.List (intercalate)

import Language.PureScript.Names

moduleNameToJs :: ModuleName -> String
moduleNameToJs (ModuleName pns) =
  let name = intercalate "_" (runProperName `map` pns)
  in if nameIsJsBuiltIn name then "$$" ++ name else name

-- |
-- Convert an Ident into a valid Javascript identifier:
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved javascript identifiers are prefixed with '$$'.
--
--  * Symbols are prefixed with '$' followed by a symbol name or their ordinal value.
--
identToJs :: Ident -> String
identToJs (Ident name)
  | nameIsJsReserved name || nameIsJsBuiltIn name = "$$" ++ name
  | otherwise = concatMap identCharToString name
identToJs (Op op) = concatMap identCharToString op

-- |
-- Test if a string is a valid JS identifier without escaping.
--
identNeedsEscaping :: String -> Bool
identNeedsEscaping s = s /= identToJs (Ident s)

-- |
-- Attempts to find a human-readable name for a symbol, if none has been specified returns the
-- ordinal value.
--
identCharToString :: Char -> String
identCharToString c | isAlphaNum c = [c]
identCharToString '_' = "_"
identCharToString '.' = "$dot"
identCharToString '$' = "$dollar"
identCharToString '~' = "$tilde"
identCharToString '=' = "$eq"
identCharToString '<' = "$less"
identCharToString '>' = "$greater"
identCharToString '!' = "$bang"
identCharToString '#' = "$hash"
identCharToString '%' = "$percent"
identCharToString '^' = "$up"
identCharToString '&' = "$amp"
identCharToString '|' = "$bar"
identCharToString '*' = "$times"
identCharToString '/' = "$div"
identCharToString '+' = "$plus"
identCharToString '-' = "$minus"
identCharToString ':' = "$colon"
identCharToString '\\' = "$bslash"
identCharToString '?' = "$qmark"
identCharToString '@' = "$at"
identCharToString '\'' = "$prime"
identCharToString c = '$' : show (ord c)

-- |
-- Checks whether an identifier name is reserved in Javascript.
--
nameIsJsReserved :: String -> Bool
nameIsJsReserved name =
  name `elem` jsAnyReserved

-- |
-- Checks whether a name matches a built-in value in Javascript.
--
nameIsJsBuiltIn :: String -> Bool
nameIsJsBuiltIn name =
  elem name
    [ "Infinity"
    , "NaN"
    , "undefined"
    , "eval"
    , "uneval"
    , "isFinite"
    , "isNaN"
    , "parseFloat"
    , "parseInt"
    , "decodeURI"
    , "decodeURIComponent"
    , "encodeURI"
    , "encodeURIComponent"
    , "escape"
    , "unescape"
    , "Object"
    , "Function"
    , "Boolean"
    , "Symbol"
    , "Error"
    , "EvalError"
    , "InternalError"
    , "RangeError"
    , "ReferenceError"
    , "SyntaxError"
    , "TypeError"
    , "URIError"
    , "Number"
    , "Math"
    , "Date"
    , "String"
    , "RegExp"
    , "Array"
    , "Int8Array"
    , "Uint8Array"
    , "Uint8ClampedArray"
    , "Int16Array"
    , "Uint16Array"
    , "Int32Array"
    , "Uint32Array"
    , "Float32Array"
    , "Float64Array"
    , "Map"
    , "Set"
    , "WeakMap"
    , "WeakSet"
    , "SIMD"
    , "ArrayBuffer"
    , "DataView"
    , "JSON"
    , "Promise"
    , "Generator"
    , "GeneratorFunction"
    , "Reflect"
    , "Proxy"
    , "Intl"
    , "arguments"
    ]

jsAnyReserved :: [String]
jsAnyReserved =
  concat
    [ jsReserved
    , jsFutureReserved
    , jsFutureReservedStrict
    , jsOldReserved
    , jsLiterals
    ]

jsReserved :: [String]
jsReserved =
  [ "break"
  , "case"
  , "catch"
  , "class"
  , "const"
  , "continue"
  , "debugger"
  , "default"
  , "delete"
  , "do"
  , "else"
  , "export"
  , "extends"
  , "finally"
  , "for"
  , "function"
  , "if"
  , "import"
  , "in"
  , "instanceof"
  , "let"
  , "new"
  , "return"
  , "super"
  , "switch"
  , "this"
  , "throw"
  , "try"
  , "typeof"
  , "var"
  , "void"
  , "while"
  , "with"
  , "yield"
  ]

jsFutureReserved :: [String]
jsFutureReserved =
  [ "enum"
  , "await"
  ]

jsFutureReservedStrict :: [String]
jsFutureReservedStrict =
  [ "implements"
  , "package"
  , "protected"
  , "static"
  , "interface"
  , "private"
  , "public"
  ]

jsOldReserved :: [String]
jsOldReserved =
  [ "abstract"
  , "boolean"
  , "byte"
  , "char"
  , "double"
  , "final"
  , "float"
  , "goto"
  , "int"
  , "long"
  , "native"
  , "short"
  , "synchronized"
  , "transient"
  , "volatile"
  ]

jsLiterals :: [String]
jsLiterals =
  [ "null"
  , "true"
  , "false"
  ]
