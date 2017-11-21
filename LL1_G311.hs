-- Handwritten parser code for Grammar 3.11 from Appel's "Modern Compiler Implementation in ML"
-- Goal : Use Template Haskell to generate this source code programmatically

data Token = If | Then | Else | Begin | Print | End | Semi | Num | Eq deriving (Eq, Show)

data Ast = Leaf Token | Node String [Ast] deriving (Eq, Show)

parseS :: [Token] -> (Ast, [Token])
parseS [] = error "out of tokens while parsing S"
parseS ts@(first : _) =
  let (subtrees, ts') = case first of
                          If    -> parseSubtrees ts [eat If, parseE, eat Then, parseS, eat Else, parseS]
                          Begin -> parseSubtrees ts [eat Begin, parseS, parseL]
                          Print -> parseSubtrees ts [eat Print, parseE]
                          _     -> error "no valid first token for S"
  in  (Node "S" subtrees, ts')

parseL :: [Token] -> (Ast, [Token])
parseL [] = error "out of tokens while parsing L"
parseL ts@(first : _) =
  let (subtrees, ts') = case first of
                          End  -> parseSubtrees ts [eat End]
                          Semi -> parseSubtrees ts [eat Semi, parseS, parseL]
                          _    -> error "no valid first token for L"
  in  (Node "L" subtrees, ts')

parseE :: [Token] -> (Ast, [Token])
parseE [] = error "out of tokens while parsing E"
parseE ts@(first : _) =
  let (subtrees, ts') = case first of
                          Num -> parseSubtrees ts [eat Num, eat Eq, eat Num]
                          _   -> error "no valid first token for E"
  in  (Node "E" subtrees, ts')
    

eat :: Token -> [Token] -> (Ast, [Token])
eat _ []        = error "no tokens to eat"
eat t (t2 : ts) = if t == t2 then (Leaf t, ts) else error "token mismatch"

-- parseSubtrees :: [Token] -> [([Token] -> (Ast, [Token]))] -> ([Ast], [Token])
parseSubtrees tokens parseFuncs = foldl (\(sts, ts) f ->
                                            let (st, ts') = f ts
                                            in  (sts ++ [st], ts'))
                                        ([], tokens)
                                        parseFuncs

-- Simple input/output example
input1 = [If, Num, Eq, Num, Then, Print, Num, Eq, Num, Else, Print, Num, Eq, Num]
parse1 = parseS input1
                                        
