\section{Graphical User Interface}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2017--2021

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{verbatim}
\begin{code}
module JavaScript
( JavaScript
, modifyDOM, createJS, colourJS
, eliminateSChar
, htmlBr, autoScroll, setPlaceholder
, showOneEle, displayJS, hideJS
, clearOutput
)
where

import Data.List
import Data.List.Split
\end{code}

\subsection{All the ugly JS functions}

Ideally this is all replaced with a nice usage of IORef,
but for the moment an ugly solution is better than no solution.

\begin{code}
-- simple types to make code more clear
type JavaScript = String
type Colour = String
colFlag = "#col#"

-- top level function that creates a series of JS statements to manipulate the HTML DOM
createJS :: Bool -> [String] ->JavaScript
createJS br s = if br
                then code ++ autoScroll ++ htmlBr
                else code ++ autoScroll
                  where code = concat $ map modifyDOM $ eliminateSChar $ unlines s 

-- delimit a string on newline (\n) and tab (\t) characters
-- also clean the array of empty ("") entries
-- can clean up specific arrays on a case by case basis later
eliminateSChar :: String -> [String]
eliminateSChar s  = filter (not . null)
                  $ splitOn "\n"
                  $ filter (/='\t') s

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
colourJS :: [String] -> Colour -> JavaScript -> JavaScript
colourJS [] _ js     = js
colourJS [x] c js    = if colFlag `isInfixOf` x
                        then js ++ (colourHTML x c)
                        else js ++ modifyDOM x
colourJS (x:xs) c js = if colFlag `isInfixOf` x
                        then colourHTML x c ++ (colourJS xs c js)
                        else modifyDOM x    ++ (colourJS xs c js) 

-- JS code that adds coloured text
-- the "∨" character does not get rendered in electron, so it is substituted with "v"
colourHTML :: String -> Colour -> JavaScript
colourHTML _text c = (createEle "para" "p")
                      ++ (setClass "para" "output")
                      ++ fst_span
                      ++ snd_span
                      ++ thd_span
                      ++ ((getElement "outputBox") `appendChild` "para")
  where (x:y:xs)  = splitOn colFlag _text
        z         = head xs
        fixed_y   = map (\k -> if k=='∨' then 'v'; else k) y
        fst_span  = if x=="" then "" else (createEle "span1" "span")
                                          ++ (setText "span1" x)
                                          ++ ("para" `appendChild` "span1")
        snd_span  = if y=="" then "" else (createEle "span2" "span")
                                          ++ (style "span2" "color" c)
                                          ++ (setText "span2" fixed_y)
                                          ++ ("para" `appendChild` "span2")
        thd_span  = if z=="" then "" else (createEle "span3" "span")
                                          ++ (style "span3" "color" "white")
                                          ++ (setText "span3" z)
                                          ++ ("para" `appendChild` "span3")

-- these are various JS statements for easier recreation
-- all inputs are strings, all outputs are a single string (of JavaScript)
createEle x y   = "var " ++ x ++ " = document.createElement(\"" ++ y ++ "\");"
setClass x y    = x ++ ".className = \"" ++ y ++ "\";"
setName x y       = x ++ ".setAttribute(\"name\",\"" ++ y ++ "\");"
setText x y     = x ++ ".innerText = \"" ++ y ++ "\";"
style x y z     = x ++ ".style." ++ y ++ "= \"" ++ z ++ "\";"
getElement x    = "document.getElementById(\"" ++ x ++ "\")"
appendChild x y = x ++ ".appendChild(" ++ y ++ ");"
\end{code}