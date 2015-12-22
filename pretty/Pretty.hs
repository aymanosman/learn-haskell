main =
  putStrLn $ layout . showTree
  $ Node "aaa" [Node "bbb" [], Node "ccc"[]]



data Tree = Node String [Tree]


showTree (Node s ts) =
  text s
  <> text " "
  <> nest (length s + 1) (showBracket ts)


showBracket [] = nil
showBracket ts =
  text "[ "
  <> showTrees ts
  <> line
  <> text "]"


showTrees [t] = showTree t
showTrees (t:ts) =
  showTree t
  <> line
  <> text ", "
  <> showTrees ts


data Doc
  = Nil
  | String `Text` Doc
  | Int `Line` Doc
  deriving (Show)


(<>) :: Doc -> Doc -> Doc
nil :: Doc
text :: String -> Doc
line :: Doc
nest :: Int -> Doc -> Doc
layout :: Doc -> String

nil = Nil
text s = s `Text` Nil
line = 0 `Line` Nil
(s `Text` x) <> y = s `Text` (x <> y)
(i `Line` x) <> y = i `Line` (x <> y)
Nil <> y = y
nest i (s `Text` x) = s `Text` nest i x
nest i (j `Line` x) = (i+j) `Line` nest i x
nest i Nil = Nil
layout (s `Text` x) = s ++ layout x
layout (i `Line` x) = '\n' : copy i ' ' ++ layout x
layout Nil = ""
copy = replicate


-- TODO
-- showTree (Node s ts) =
--   group (text s <> nest (length s) (showBracket ts))

-- group :: Doc -> Doc
-- pretty :: Int -> Doc -> String
-- (<|>) :: Doc -> Doc -> Doc
-- flatten :: Doc -> Doc
