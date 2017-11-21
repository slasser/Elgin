{-# LANGUAGE TemplateHaskell #-}

module ParserGen where

import Control.Monad
import Language.Haskell.TH

data Symbol = T String | NT String deriving (Eq, Show)

data Ast = Leaf String | Node String [Ast] deriving (Eq, Show) 

-- To do : add wildcard match in case expression
mkDec :: (Symbol, [(Symbol, [Symbol])]) -> Q Dec
mkDec (T _, _)        = error ""
mkDec (NT ntName, pairs) = funD funName [nilClause, consClause]
  where
    funName       = mkName $ "parse" ++ ntName
    tokens        = mkName $ "tokens"
    first         = mkName $ "first"
    rest          = mkName $ "rest"
    subtrees      = mkName $ "subtrees"
    parseSubtrees = mkName $ "parseSubtrees"
    tokens'       = mkName $ "tokens'"

    -- Example clause:
    -- parseS [] = error ("out of tokens while parsing " ++ "S")
    nilClause  = let nilPatt = [p| [] |]
                     body    = normalB [e| error ("out of tokens while parsing " ++ ntName) |]
                 in  clause [nilPatt] body []
                     
    -- Example clause:
    -- parseS tokens@(first : rest) =
    --   let (subtrees, tokens') = case first of
    --                               T "Begin" -> parseSubtrees tokens [eat (T "Begin"), parseS, parseL]
    --                               T "Print" -> parseSubtrees tokens [eat (T "Print"), parseE]
    --   in (Node "S" subtrees, tokens')
    consClause = let consPatt = asP tokens [p| $(varP first) : $(varP rest) |]
                     caseExpr = caseE [e| $(varE first) |] (map mkMatch pairs)
                     letExpr = [e| let ($(varP subtrees), $(varP tokens')) = $(caseExpr)
                                   in  (Node ntName $(varE subtrees), $(varE tokens')) |]
                 in  clause [consPatt] (normalB letExpr) []

    mkMatch :: (Symbol, [Symbol]) -> Q Match
    mkMatch (NT _, _) = error ""
    mkMatch (T tName, ex) = match [p| T $(litP (stringL tName)) |]
                                   (normalB [e| $(varE parseSubtrees)
                                                $(varE tokens)
                                                $(listE (map symbolE ex)) |])
                                   []
                                  
    symbolE (T tName) = [e| eat (T tName) |]
    symbolE (NT ntName) = varE (mkName $ "parse" ++ ntName)

eat :: Symbol -> [Symbol] -> (Ast, [Symbol])
eat (NT _) _    = error "expected a terminal symbol"
eat (T _) []    = error "no tokens to eat"
eat t@(T tName) (t2 : ts) = if t == t2 then (Leaf tName, ts) else error "token mismatch"

parseSubtrees :: [Symbol] -> [([Symbol] -> (Ast, [Symbol]))] -> ([Ast], [Symbol])
parseSubtrees tokens parseFuncs =
  foldl (\(subtrees, toks) f ->
            let (subtree, toks') = f toks
            in  (subtrees ++ [subtree], toks'))
        ([], tokens)
        parseFuncs


mkDecs :: [(Symbol, [(Symbol, [Symbol])])] -> Q [Dec]
mkDecs grammar = forM grammar mkDec

-- Grammar 3.11 from Appel's "Modern Compiler Implementation in ML"
g311 = [(NT "S", [(T "If",    [T "If", NT "E", T "Then", NT "S", T "Else", NT "S"]),
                  (T "Begin", [T "Begin", NT "S", NT "L"]),
                  (T "Print", [T "Print", NT "E"])]),
         
        (NT "L", [(T "End",   [T "End"]),
                  (T ";",     [T ";", NT "S", NT "L"])]),
         
        (NT "E", [(T "Num",   [T "Num", T "==", T "Num"])])]
