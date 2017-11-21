{-# LANGUAGE TemplateHaskell #-}

import ParserGen

$(mkDecs g311)

tokenize :: String -> [Symbol]
tokenize s = map T (words s)

input1 = tokenize "If Num == Num Then Print Num == Num Else Print Num == Num"
parse1 = parseS input1

input2 = tokenize "If Num == Num Then Print Num == Num Else Begin Print Num == Num End"
parse2 = parseS input2
