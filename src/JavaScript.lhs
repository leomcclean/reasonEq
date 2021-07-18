\section{Graphical User Interface}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2017--2021

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{verbatim}
\begin{code}
module JavaScript
( JavaScript
, modifyDOM, createJS, reduceToJS, colourJS
, htmlBr, autoScroll, setPlaceholder
, showOneEle, displayJS, hideJS
, clearOutput
)
where

import Data.List
import Data.List.Split

import Utilities
\end{code}

\subsection{All the ugly JS functions}

Ideally this is all replaced with a nice usage of IORef,
but for the moment an ugly solution is better than no solution.

\begin{code}
-- simple types to make code more clear
type JavaScript = String
type Colour = String

-- top level function that creates a series of JS statements to manipulate the HTML DOM
createJS :: Bool -> [String] ->JavaScript
createJS br s = if br
                then code ++ htmlBr
                else code
                  where code = (reduceToJS $ eliminateSChar $ unlines s) ++ autoScroll

-- delimit a string on newline (\n) and tab (\t) characters
-- also clean the array of empty ("") entries
-- can clean up specific arrays on a case by case basis later
eliminateSChar :: String -> [String]
eliminateSChar s = filter (not . null) $ splitOn "\n" s

-- recursive function to create a long string of JavaScript from an array of output text
reduceToJS :: [String] -> JavaScript
reduceToJS [] = []
reduceToJS [x] = modifyDOM x
reduceToJS (x:xs) = modifyDOM x ++ reduceToJS xs

-- the most worrying function to ever be written in Haskell
-- here we commit a sin, by being an accessory to causing a side-effect
-- by modifying the HTML DOM in this way there are a lot of headaches saved
-- however, the javascript exec() function is inherently insecure
-- we will have to sanitise all user input to avoid javascript injection (!?!?!)
modifyDOM :: String -> JavaScript
modifyDOM t = (createEle "para" "p")
              ++ (setClass "para" "output")
              ++ (setText "para" t)
              ++ ((getElement "outputBox") `appendChild` "para")

-- JS code to automatically scroll downwards upon new output
autoScroll :: JavaScript
autoScroll =  "var box = " ++ (getElement "outputBox") ++ ";\
              \box.scrollTop = box.scrollHeight;"

-- JS code to insert a line break in the outputBox
htmlBr :: JavaScript
htmlBr = (createEle "br" "p")
         ++ (setClass "br" "output")
         ++ (setText "br" "  .  ")
         ++ (style "br" "color" "transparent")
         ++ ((getElement "outputBox") `appendChild` "br")

-- JS code to clear all output
clearOutput :: [String] -> JavaScript
clearOutput boxes = "var elements = document.getElementsByClassName(\"output\");\
                    \while(elements.length > 0)\
                    \{elements[0].parentNode.removeChild(elements[0]);};"
                    ++ hideJS boxes ""

-- show one element, while hiding the rest
showOneEle :: [String] -> String -> JavaScript
showOneEle others ele = (hideJS others "") ++ (displayJS ele True)

-- display or hide a HTML element
displayJS :: String -> Bool -> JavaScript
displayJS s x = if x
                then style (getElement s) "display" "flex"
                else style (getElement s) "display" "none"

-- set the placehodler text of an input box
setPlaceholder :: String -> String -> JavaScript
setPlaceholder ele p = (getElement ele) ++ ".placeholder = \"" ++ p ++ "\";"
                       ++ (getElement ele)
                       ++ ".style.setProperty(\"--c\", \"rgb(40, 40, 40)\");"
                       ++ (getElement ele) ++ ".value = \"\";"

-- hide all 'hidable' divs
hideJS :: [String] -> JavaScript -> JavaScript
hideJS [] js     = js
hideJS [x] js    = js ++ (style (getElement x) "display" "none")
hideJS (x:xs) js = hideJS xs $ js ++ (style (getElement x) "display" "none")

-- generate a block of JS with certain text having the colour style
colourJS :: [String] -> String -> Colour -> JavaScript -> JavaScript
colourJS [] y c js     = js
colourJS [x] y c js    = if y `isInfixOf` x
                         then js ++ (colouredHTML x y c)
                         else js ++ modifyDOM x
colourJS (x:xs) y c js = if y `isInfixOf` x
                         then colourJS xs y c $ js ++ (colouredHTML x y c)
                         else colourJS xs y c $ js ++ modifyDOM x

-- JS code that adds coloured text
-- (xs) here will only ever be a single-entry array of Strings
colouredHTML :: String -> String -> Colour -> JavaScript
colouredHTML s cut colour = js
  where (x:xs) = splitOn cut s
        js     = (createEle "para" "p")
               ++ (setClass "para" "output")
               ++ (style "para" "color" "white")
               ++ (setText "para" $ x ++ cut)
               ++ (createEle "span" "span")
               ++ (style "span" "color" colour)
               ++ (setText "span" $ concat xs)
               ++ ("para" `appendChild` "span")
               ++ ((getElement "outputBox") `appendChild` "para")

-- these are various JS statements for easier recreation
-- all inputs are strings, all outputs are a single string (of JavaScript)
createEle x y   = "var " ++ x ++ " = document.createElement(\"" ++ y ++ "\");"
setClass x y    = x ++ ".className = \"" ++ y ++ "\";"
setText x y     = x ++ ".innerText = \"" ++ y ++ "\";"
style x y z     = x ++ ".style." ++ y ++ "= \"" ++ z ++ "\";"
getElement x    = "document.getElementById(\"" ++ x ++ "\")"
appendChild x y = x ++ ".appendChild(" ++ y ++ ");"
\end{code}