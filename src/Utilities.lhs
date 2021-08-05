\section{Utilities}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2017

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{verbatim}
\begin{code}
module Utilities (
  utilities
, fst3, snd3, thd3
, everyNth
, ttail
, unlines'
, issubset
, clearIt, clear
, readBool, readInt
, trim
, zip1, zip2, zip2'
, nlookup, alookup
, extract, keyListDiff
, splitFstSnd
, numberList, numberListReturnsList, numberList', pairNumberList
, putPP, putShow, pp
, YesBut(..)
, hasdup
, disjoint, overlaps
, peel
, getJust
, pulledFrom, getitem, choose
, injMap
, spaced, intcalNN
, pad
, splitLast, splitAround
, brkspn, brkspnBy, splice
, args2str, args2int, userPrompt, userPause
, cleanANSI
)
where

import Data.List
import Data.List.Split
import Data.Char
import Data.Set(Set)
import qualified Data.Set as S
import Data.Map(Map)
import qualified Data.Map as M
import System.IO
import Control.Applicative
import Control.Monad

import NiceSymbols

--import Debug.Trace -- disable when not used, as this module is 'open'
--dbg msg x = trace (msg++show x) x
\end{code}

Here we provide odds and ends not found elsewhere.

\begin{code}
utilities
 = do putStrLn "Useful interactive Utilities"
      putStrLn " putShow :: Show t =>      t -> IO ()"
      putStrLn " putPP   ::           String -> IO ()"
\end{code}

\newpage
\subsubsection{Maybe and related}

A version of \texttt{fromJust}
that gives a more helpful error message.
\begin{code}
getJust :: String -> Maybe t -> t
getJust _   (Just x)  =  x
getJust msg Nothing   =  error msg
\end{code}

More than pairs:
\begin{code}
fst3 :: (a,b,c) -> a ; fst3(x,_,_) = x
snd3 :: (a,b,c) -> b ; snd3(_,y,_) = y
thd3 :: (a,b,c) -> c ; thd3(_,_,z) = z
\end{code}

\newpage
\subsection{List Functions}

\subsubsection{Total Tail}
\begin{code}
ttail :: [a] -> [a]
ttail []      =  []
ttail (_:xs)  =  xs
\end{code}

\subsubsection{Every Nth Item}
\begin{code}
everyNth :: Int -> [a] -> [a]
everyNth n xs =
  case drop (n-1) xs of
    y : ys -> y : everyNth n ys
    [] -> []
\end{code}

\subsubsection{Predicate: has duplicates}
\begin{code}
hasdup :: Eq a => [a] -> Bool
hasdup xs = xs /= nub xs
\end{code}

\subsubsection{Pulling Prefix from a List}
\begin{code}
pulledFrom :: Eq a => [a] -> [a] -> (Bool, [a])
[]     `pulledFrom` ys  =  (True, ys)
xs     `pulledFrom` []  =  (False,[])
(x:xs) `pulledFrom` (y:ys)
 | x == y     =  xs `pulledFrom` ys
 | otherwise  =  (False,ys)
\end{code}

\subsubsection{Un-lining without a trailing newline}
\begin{code}
unlines' [] = ""
unlines' [s] = s
unlines' (s:ss) = s ++ '\n':unlines' ss
\end{code}

\subsubsection{Get item from list, or fail trying}
\begin{code}
getitem :: (Eq a, Monad m) => a -> [a] -> m [a]
getitem _ [] = fail "getitem: item not present"
getitem a (x:xs)
 | a == x     =  return xs
 | otherwise  =  do xs' <- getitem a xs
                    return (x:xs')
\end{code}

\subsubsection{List lookup by number}
\begin{code}
nlookup :: Monad m => Int -> [a] -> m a
nlookup i things
 | i < 1 || null things  =  fail "nlookup: not found"
nlookup 1 (thing:rest)   =  return thing
nlookup i (thing:rest)   =  nlookup (i-1) rest
\end{code}

\subsubsection{Association-list lookup}
\begin{code}
alookup :: (Eq k, Monad m) => k -> [(k,d)] -> m d
alookup k []   =  fail "alookup: not found"
alookup k ((n,v):rest)
  | k == n     =  return v
  | otherwise  =  alookup k rest
\end{code}

\subsubsection{Intercalation, dropping nulls}
\begin{code}
intcalNN sep = intercalate sep . filter (not . null)
\end{code}

\subsubsection{Splitting Lists}

Pulling an item out of a list, satisfying a predicate:
\begin{code}
extract :: (a -> Bool) -> [a] -> ([a],[a],[a])
extract p xs = (before,it,after)
 where
   (before,rest) = span (not . p) xs
   it = take 1 rest
   after = drop 1 rest
\end{code}

List difference, assuming any key value occurs only
once in each list.
\begin{code}
keyListDiff :: ( Ord key, Eq rec)
            => (rec -> key)  --  key function
            -> [rec]         --  1st list
            -> [rec]         --  2nd list
            -> ( [rec]       --  in 1st list, but not in 2nd
               , [rec]       --  same in both lists
               , [rec]       --  in 2nd list, and also in 1st, but different
               , [rec] )     --  in 2nd list, not in 1st
keyListDiff keyf recs1 recs2
  =  diff ([],[],[],[]) (sortOn keyf recs1) (sortOn keyf recs2)
  where
   rev = reverse
   -- mr (remove) qe (equal) fd (diff) wn (new)
   diff (mr,qe,fd,wn) [] []   =  (rev mr, rev qe, rev fd, rev wn)
   diff (mr,qe,fd,wn) [] rs2  =  (rev mr, rev qe, rev fd, rev wn++rs2)
   diff (mr,qe,fd,wn) rs1 []  =  (rev mr++rs1, rev qe, rev fd, rev wn)
   diff (mr,qe,fd,wn) (r1:rs1) (r2:rs2)
    | keyf r1 < keyf r2  =  diff (r1:mr,qe,fd,wn) rs1      (r2:rs2)
    | keyf r1 > keyf r2  =  diff (mr,qe,fd,r2:wn) (r1:rs1) rs2
    | r1 == r2           =  diff (mr,r1:qe,fd,wn) rs1      rs2
    | otherwise          =  diff (mr,qe,r2:fd,wn) rs1      rs2
\end{code}

Drop the last element of every tuple in a list
\begin{code}
splitFstSnd :: [(a, b, [c])] -> [(a, b)]
splitFstSnd []            = []
splitFstSnd [(a,b,_)]     = [(a,b)]
splitFstSnd ((a,b,_):xs)  = (a,b) : splitFstSnd xs
\end{code}

Unsure what these are about!
\begin{code}
listsplit ts = listsplit' [] [] ts
listsplit' splits before [] = splits
listsplit' splits before (t:after)
 = listsplit' ((reverse before',after):splits) before' after
 where before' = t:before
\end{code}

\begin{code}
splitLast [x] = ([],x)
splitLast (x:xs) = (x:xs',y) where (xs',y) = splitLast xs
\end{code}

\begin{code}
splitAround :: (Eq a,Monad m) => a -> [a] -> m ([a],[a])
splitAround s xs
  = splitA s [] xs
  where
    splitA s _ []  =  fail "splitAround: element not found"
    splitA s [] (x:xs)
      | s == x     =  fail "splitAround: empty left sequence"
    splitA s sx [x]
      | s == x     =  fail "splitAround: empty right sequence"
    splitA s sx (x:xs)
      | s == x     =  return (reverse sx,xs)
      | otherwise  =  splitA s (x:sx) xs
\end{code}

Not sure what the above are all about!

The following are good for processing lists ordered in a custom way:
\begin{code}
brkspn :: (a -> Bool) -> [a] -> ([a], [a], [a])
brkspn p xs = let
                (before,rest) = break p xs
                (found,after) = span  p rest
              in (before,found,after)

brkspnBy :: (a -> Ordering) -> [a] -> ([a], [a], [a])
brkspnBy cmp xs = let
                gt x           =  cmp x == GT
                eq x           =  cmp x == EQ
                (before,rest)  =  span gt xs
                (found,after)  =  span eq rest
              in (before,found,after)

splice :: Monad m => ([a] -> m [a]) -> ([a],[a],[a]) -> m [a]
splice mrg (before,found,after)
  = do found' <- mrg found
       return (before++found'++after)
\end{code}

\subsubsection{`Peeling' a list}

We use a number $i$ to extract the $i$th element of a list
peeling off all the elements before it into a reversed list.
We return a triple, of the before-list (reversed), the chosen element,
and the after list.
This fails if the index does not correspond to a list position.
\begin{code}
peel :: Monad m => Int -> [a] -> m ([a],a,[a])
peel n xs = ent [] n xs
 where
   ent _ _ [] = fail ""
   ent bef 1 (x:xs) = return (bef,x,xs)
   ent bef n (x:xs)
    | n < 2  =  fail ""
    | otherwise  =  ent (x:bef) (n-1) xs
\end{code}

\subsubsection{Trimming Strings}

\begin{code}
trim = ltrim . reverse . ltrim . reverse

ltrim "" = ""
ltrim str@(c:cs)
 | isSpace c  =  ltrim cs
 | otherwise  =  str
\end{code}

\subsection{Specialised Zips}

\begin{code}
zip1 :: a -> [b] -> [(a,b)]
zip1 a = map (\b -> (a,b))

zip2 :: [a] -> b -> [(a,b)]
zip2 as b = map (\a->(a,b)) as

zip2' :: b -> [a] -> [(a,b)]
zip2' b = map (\a->(a,b))
\end{code}

\subsubsection{Number List Display}

A common idiom is to show a list of items as a numbered list
to make selecting them easier:
\begin{code}
numberListReturnsList showItem list
  = map (numberItem showItem) $ zip [1..] list
numberList showItem list
  = unlines' $ map (numberItem showItem) $ zip [1..] list
numberItem showItem (i,item)
  = pad 4 istr ++ istr ++ ". " ++ showItem item
  where istr = show i

pad w str
  | ext > 0    =  replicate ext ' '
  | otherwise  =  ""
  where ext = w - length str
\end{code}

Sometimes, we want the number afterwards:
\begin{code}
numberList' showItem list
  = let
     lstrings = map showItem' list
     showItem' item = (istr,length istr) where istr = showItem item
     maxw = maximum $ map snd lstrings
    in unlines' $ map (numberItem' (maxw+2)) $ zip [1..] lstrings
numberItem' maxw (i,(str,strlen))
  = str ++ replicate (maxw-strlen) ' ' ++ pad 2 istr ++ istr
  where istr = show i
\end{code}

The GUI does not need to attach numbers in front of items,
it only needs them attached to items themselves.
\begin{code}
pairNumberList [] _ = []
pairNumberList (x:xs) y = (x,k) : pairNumberList xs (y+1)
  where k = show y
\end{code}

\subsubsection{Argument String Handling}

\begin{code}
args2int args = if null args then 0 else readInt $ head args

args2str args = if null args then "" else head args
\end{code}

\subsection{Set Functions}

\subsubsection{Subsets}

\begin{code}
issubset :: Eq a => [a] -> [a] -> Bool
xs `issubset` ys  =  null (xs \\ ys)
\end{code}

\subsubsection{Set disjointness}

\begin{code}
disjoint :: Ord a => Set a -> Set a -> Bool
s1 `disjoint` s2 = S.null (s1 `S.intersection` s2)
\end{code}

\subsubsection{Set overlap}

\begin{code}
overlaps :: Ord a => Set a -> Set a -> Bool
s1 `overlaps` s2 = not (s1 `disjoint` s2)
\end{code}

\subsubsection{Choosing element from a set}

\begin{code}
choose s
 | S.null s  =  error "choose: empty set."
 | otherwise  = (x,s')
 where
   x = S.elemAt 0 s
   s' = S.delete x s
\end{code}

\subsection{Map Functions}

\subsubsection{Building injective Maps}

Here is code for converting \texttt{[(a,b)]} to an injective \texttt{Map a b},
failing if there are duplicate \texttt{b}s.
\begin{code}
injMap :: (Monad m, Ord a, Ord b) => [(a,b)] -> m (Map a b)
injMap abs
 | uniqueList (map snd abs)  =  return $ M.fromList abs
 | otherwise                 =  fail "injMap: range has duplicates"

uniqueList :: Ord b => [b] -> Bool
uniqueList  =  all isSingle . group . sort

isSingle [_]  =  True
isSingle _    =  False
\end{code}

\newpage
\subsection{Smart Readers}

\subsubsection{Read Boolean}
\begin{code}
readBool :: String -> Bool
readBool str
  | map toLower str == "t"     =  True
  | map toLower str == "true"  =  True
  | map toLower str == "yes"   =  True
  | int >= 0                   =  int > 0
  where
    int = readInt str
readBool _                     =  False
\end{code}

\subsubsection{Read Integer}
\begin{code}
readInt :: String -> Int
readInt str
 | null str         =   -1
 | all isDigit str  =   read str
 | otherwise        =   -1
\end{code}

\newpage
\subsection{Control-Flow Functions}

\subsubsection{Repeat Until Equal}

\begin{code}
untilEq :: Eq a => (a -> a) -> a -> a
untilEq f x
 | x' == x  =  x
 | otherwise  =  untilEq f x'
 where x' = f x
\end{code}

\newpage
\subsection{Possible Failure Monad}

\subsubsection{Datatype: Yes, But \dots}

\begin{code}
data YesBut t
 = Yes t
 | But [String]
 deriving (Eq,Show)
\end{code}

\subsubsection{Instances: Functor, Applicative, Monad, MonadPlus}

\begin{code}
instance Functor YesBut where
  fmap f (Yes x)    =  Yes $ f x
  fmap f (But msgs)  =  But msgs

instance Applicative YesBut where
  pure x                   =  Yes x
  Yes f <*> Yes x          =  Yes $ f x
  Yes f <*> But msgs       =  But msgs
  But msgs <*> Yes x       =  But msgs
  But msgs1 <*> But msgs2  =  But (msgs1++msgs2)

instance Monad YesBut where
  return x        =  Yes x
  Yes x   >>= f   =  f x
  But msgs >>= f  =  But msgs
  fail msg        =  But $ lines msg

instance Alternative YesBut where
  empty = But []
  But msgs1 <|> But msgs2  =  But (msgs1 ++ msgs2)
  But _     <|> yes2       =  yes2
  yes1      <|> _          =  yes1

instance MonadPlus YesBut where
  mzero = But []
  But msgs1 `mplus` But msgs2  =  But (msgs1 ++ msgs2)
  But _     `mplus` yes2       =  yes2
  yes1      `mplus` _          =  yes1
\end{code}

\newpage
\subsection{Pretty-printing Derived Show}

A utility that parses the output of \texttt{derived} instances of \texttt{show}
to make debugging easier.

\begin{code}
putShow :: Show t => t -> IO ()
putShow = putPP . show

putPP :: String -> IO ()
putPP = putStrLn . pp

pp :: String -> String
--pp = display0 . pShowTree . lexify
--pp = display1 . showP
pp = display2 . showP

showP :: String -> ShowTree
showP = pShowTree . lexify
\end{code}

Basically we look for brackets (\texttt{[]()}) and punctuation (\texttt{,})
and build a tree.

\subsubsection{Pretty-printing Tokens}

Tokens are the five bracketing and punctuation symbols above,
plus any remaining contiguous runs of non-whitespace characters.
\begin{code}
data ShowTreeTok
 = LSqr | RSqr | LPar | RPar | Comma | Run String
 deriving (Eq, Show)

lexify :: String -> [ShowTreeTok]
lexify "" = []
lexify (c:cs)
 | c == '['   =  LSqr  : lexify cs
 | c == ']'   =  RSqr  : lexify cs
 | c == '('   =  LPar  : lexify cs
 | c == ')'   =  RPar  : lexify cs
 | c == ','   =  Comma : lexify cs
 | isSpace c  =  lexify cs
 | otherwise  =  lex' [c] cs

lex' nekot "" = [rrun nekot]
lex' nekot (c:cs)
 | c == '['   =  rrun nekot : LSqr  : lexify cs
 | c == ']'   =  rrun nekot : RSqr  : lexify cs
 | c == '('   =  rrun nekot : LPar  : lexify cs
 | c == ')'   =  rrun nekot : RPar  : lexify cs
 | c == ','   =  rrun nekot : Comma : lexify cs
 | isSpace c  =  rrun nekot         : lexify cs
 | otherwise  =  lex' (c:nekot) cs

rrun nekot = Run $ reverse nekot

spaced s = ' ':s ++ " "
\end{code}

\subsubsection{Useful IO bits and pieces}

Prompting:
\begin{code}
userPrompt :: String -> IO String
userPrompt str = putStr str >> hFlush stdout >> getLine
\end{code}

Screen clearing:
\begin{code}
clear = "\ESC[2J\ESC[1;1H"
clearIt str = clear ++ str
\end{code}

Pausing (before \textrm{clearIt}, usually)
\begin{code}
userPause = userPrompt "hit <enter> to continue"
\end{code}

\newpage
\subsubsection{Parsing Tokens}

We parse into a ``Show-Tree''
\begin{code}
data ShowTree
 = STtext String     -- e.g.,  D "m" 5.3 1e-99
 | STapp [ShowTree]  -- e.g., Id "x"
 | STlist [ShowTree]
 | STpair [ShowTree]
 deriving (Eq, Show)
\end{code}

The parser
\begin{code}
pShowTree :: [ShowTreeTok] -> ShowTree
pShowTree toks
 = case pContents [] [] toks of
     Nothing  ->  STtext "?"
     Just (contents,[])  ->  wrapContents stapp contents
     Just (contents,_ )  ->  STpair [wrapContents stapp contents,STtext "??"]
\end{code}

Here we accumulate lists within lists.
Internal list contents are sperated by (imaginary) whitespace,
while external lists have internal lists as components,
separated by commas.
\begin{code}
pContents :: Monad m
          => [[ShowTree]] -- completed internal lists
          -> [ShowTree]   -- internal list currently under construction
          -> [ShowTreeTok] -> m ([[ShowTree]], [ShowTreeTok])

-- no tokens left
pContents pairs []  [] = return (reverse pairs, [])
pContents pairs app [] = return (reverse (reverse app:pairs), [])

-- ',' starts a new internal list
pContents pairs app (Comma:toks)  =  pContents (reverse app:pairs) [] toks

-- a run is just added onto internal list being built.
pContents pairs app (Run s:toks)  =  pContents pairs (STtext s:app) toks

-- '[' triggers a deep dive, to be terminated by a ']'
pContents pairs app (LSqr:toks)
 =  do (st,toks') <- pContainer STlist RSqr toks
       pContents pairs (st:app) toks'
pContents pairs app toks@(RSqr:_)
 | null app   =  return (reverse pairs, toks)
 | otherwise  =  return (reverse (reverse app:pairs), toks)

-- '(' triggers a deep dive, to be terminated by a ')'
pContents pairs app (LPar:toks)
 =  do (st,toks') <- pContainer STpair RPar toks
       pContents pairs (st:app) toks'
pContents pairs app toks@(RPar:_)
 | null app   =  return (reverse pairs, toks)
 | otherwise  =  return (reverse (reverse app:pairs), toks)
\end{code}

\newpage
A recursive dive for a bracketed construct:
\begin{code}
pContainer :: Monad m
           => ([ShowTree] -> ShowTree) -- STapp, STlist, or STpair
           -> ShowTreeTok              -- terminator, RSqr, or RPar
           -> [ShowTreeTok] -> m (ShowTree, [ShowTreeTok])

pContainer cons term toks
 = do (contents,toks') <- pContents [] [] toks
      if null toks' then fail "container end missing"
      else if head toks' == term
           then return (wrapContents cons contents, tail toks')
           else tfail toks' "bad container end"
\end{code}

Building the final result:
\begin{code}
--wrapContents cons [[st]] = st
wrapContents cons contents = cons $ map stapp contents
\end{code}

Avoiding too many nested uses of \texttt{STapp},
and complain if any are empty.
A consequence of this is that all \texttt{STapp}
will have length greater than one.
\begin{code}
stapp [] = error "stapp: empty application!"
stapp [STapp sts] = stapp sts
stapp [st] = st
stapp sts = STapp sts
\end{code}

Informative error:
\begin{code}
tfail toks str = fail $ unlines [str,"Remaining tokens = " ++ show toks]
\end{code}

\subsubsection{Displaying Show-Trees}

Heuristic Zero: all on one line:
\begin{code}
display0 :: ShowTree -> String
display0 (STtext s)    =  s
display0 (STapp sts)   =  intercalate " " $ map display0 sts
display0 (STlist sts)  =  "[" ++ (intercalate ", " $ map display0 sts) ++"]"
display0 (STpair sts)  =  "(" ++ (intercalate ", " $ map display0 sts) ++")"
\end{code}

Heuristic One: Each run on a new line, with indentation.
\begin{code}
display1 :: ShowTree -> String
display1 st = disp1 0 st

disp1 _ (STtext s) = s
disp1 i (STapp (st:sts)) -- length always >=2, see stapp above,
  = disp1 i st ++  '\n' : (unlines' $ map ((ind i ++) . disp1 i) sts)
disp1 i (STlist []) = "[]"
disp1 i (STlist (st:sts)) = "[ "++ disp1 (i+2) st ++ disp1c i sts ++ " ]"
disp1 i (STpair (st:sts)) = "( "++ disp1 (i+2) st ++ disp1c i sts ++ " )"

disp1c i [] = ""
disp1c i (st:sts) = "\n" ++ ind i ++ ", " ++  disp1 (i+2) st ++ disp1c i sts

ind i = replicate i ' '
\end{code}

\newpage
Heuristic 2:  designated text values at start of \texttt{STapp}
mean it is inlined as per Heuristic Zero.
Also, used of \texttt{fromList} are rendered as sets.
\begin{code}
display2 :: ShowTree -> String
display2 st = disp2 0 st

inlineKeys = map STtext
  ["BV","BT","E","K","VB","VI","VT","V","GL","GV","LV","VR","Id","WD","BI"]

inlineSets = map STtext ["fromList","BS"]

disp2 _ (STtext s) = s
disp2 i app@(STapp (st:sts)) -- length always >=2, see stapp above,
 | st `elem` inlineSets  =  disp2set i sts
 | st `elem` inlineKeys  =  display0 app
 | otherwise = disp2 i st ++  '\n' : (unlines' $ map ((ind i ++) . disp2 i) sts)
disp2 i (STlist []) = "[]"
disp2 i (STlist (st:sts)) = "[ "++ disp2 (i+2) st ++ disp2c i sts ++ " ]"
disp2 i (STpair []) = "()"
disp2 i tuple@(STpair (STapp (st:_):_))
 | st `elem` inlineKeys   =  display0 tuple
disp2 i (STpair (st:sts)) = "( "++ disp2 (i+2) st ++ disp2c i sts ++ " )"

disp2c i [] = ""
disp2c i (st:sts) = "\n" ++ ind i ++ ", " ++  disp2 (i+2) st ++ disp2c i sts

disp2set i [] = "{}"
--disp2set i [STlist []] = "{}"
disp2set i [STlist sts] = disp2set i sts
disp2set i (st:sts) = "{ "++ disp2 (i+2) st ++ disp2c i sts ++ " }"
\end{code}

\subsubsection{Removing ANSI Colour-tags}

\begin{code}
colourList = [("[30m","#£000000#")
             ,("[31m","#£ff0000#")
             ,("[32m","#£00ff00#")
             ,("[33m","#£ffff00#")
             ,("[34m","#£00ccff#")
             ,("[35m","#£9b59b6#")
             ,("[36m","#£00ffcc#")
             ,("[37m","#£ffffff#")
             ,("[0m","#£col#")]
otherList = ["[4m","[3m","[1m"]

-- we don't just strip the ANSI because we want to add our own colours
cleanANSI :: [String] -> [String]
cleanANSI input = map stripANSI $ replaceANSI input

replaceANSI :: [String] -> [String]
replaceANSI []      = []
replaceANSI (x:xs)  = if "[0m" `isInfixOf` x
                      then map (filter (/='\ESC'))
                        $ swapColours y colourList : ys
                      else x : ys
                        where ys  = replaceANSI xs
                              y   = removeOtherANSI x

-- we also have to remove the 'bold', and 'underline' tags
removeOtherANSI :: String -> String
removeOtherANSI text = removeOtherANSIr otherList text

removeOtherANSIr :: [String] -> String -> String
removeOtherANSIr [] text      = text
removeOtherANSIr (x:xs) text  = if x `isInfixOf` text
                                then removeOtherANSIr xs fixed_text 
                                else removeOtherANSIr xs text
  where fixed_text = fixOther $ splitOn x text

-- remove matching instance of ANSI end code
fixOther :: [String] -> String
fixOther (x:y:xs) = concat [x, removeNextEndTag y, fixOther xs]
fixOther [x]      = x
fixOther []       = ""

removeNextEndTag ('[':'0':'m':xs) = xs
removeNextEndTag (x:xs)           = x : removeNextEndTag xs
removeNextEndTag ""               = ""

-- finally we interchange the terminal tags with the almost HTML colour codes
swapColours :: String -> [(String, String)] -> String
swapColours text []        = text
swapColours text [tag]     = swapColTag text tag
swapColours text (tag:xs)  = swapColours (swapColTag text tag) xs

swapColTag :: String -> (String, String) -> String
swapColTag text (x,y) = concat $ intersperse y $ splitOn x text
\end{code}