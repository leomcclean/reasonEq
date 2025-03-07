\section{Graphical User Interface}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2017--2021

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{verbatim}
\begin{code}
module JavaScript
( JavaScript
, createJS
, htmlBr, autoScroll, setPlaceholder
, showOneEle, displayJS, hideJS
, clearOutput
, jsToText
)
where

import Data.List
import Data.List.Split
\end{code}

\subsection{Building JS functions}

\begin{code}
-- simple types to make code more clear
type JavaScript = String
type Colour = String

-- top level function that creates a series of JS statements to manipulate the HTML DOM
createJS :: Bool -> [String] ->JavaScript
createJS br s = if "#£col#" `isInfixOf` initial
                then case br of
                  True  -> addbr $ result $ colourJS _split
                  False -> result $ colourJS _split
                else case br of
                  True  -> addbr $ result $ concat $ map appendOutput _split
                  False -> result $ concat $ map appendOutput _split
  where initial   = eliminateSChar $ unlines s
        _split    = splitOn "\n" initial
        result x  = x ++ autoScroll
        addbr x   = x ++ htmlBr

-- delimit a string on newline (\n) and tab (\t) characters
-- also clean the array of empty ("") entries
-- can clean up specific arrays on a case by case basis later
eliminateSChar :: String -> String
eliminateSChar s  = filterQuotes $ filter (/='\t') s

-- filters the three kinds of quotes that ruin JavaScript
filterQuotes :: String -> String
filterQuotes text = map (\x -> if or [x=='"',x=='”',x=='“'] then '\'' else x) text

appendOutput :: String -> JavaScript
appendOutput t  = (createEle "para" "p")
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

-- colour tagged text, regularly build non-tagged text
colourJS list = colourJSr list ""
colourJSr :: [String] -> JavaScript -> JavaScript
colourJSr [] js     = js
colourJSr [x] js    = if "#£col#" `isInfixOf` x
                      then js ++ (colourHTML x)
                      else js ++ appendOutput x
colourJSr (x:xs) js = if "#£col#" `isInfixOf` x
                      then colourHTML x  ++ (colourJSr xs js)
                      else appendOutput x    ++ (colourJSr xs js)

-- recursively path over a block of text to colour tagged text
colourHTML _text = colourHTMLr ts $ (createEle "para" "p") ++ (setClass "para" "output")
  where ts = splitOn "#£col#" _text
colourHTMLr :: [String] -> JavaScript -> JavaScript
colourHTMLr [] js     = js ++ ((getElement "outputBox") `appendChild` "para")
colourHTMLr [t] js    = colourHTMLr [] $ js ++ findColours [t] ""
colourHTMLr (t:ts) js = if "£ignore" `elem` _text
                        then colourHTMLr ts js
                        else colourHTMLr ts $ js ++ findColours _text ""
  where _text   = splitOn "#" t

-- uses the interwoven colours within a string to appropriately generate 'span' coloured text
findColours :: [String] -> JavaScript -> JavaScript
findColours [] js     = js ++ ((getElement "outputBox") `appendChild` "para")
findColours [x] js    = if x/="£col" then js ++ (addColour x "white") else js
findColours (x:y:xs) js
                      | x == "£col"   = findColours (y:xs) js
                      | '£' `elem` x  = findColours xs $ js ++ (addColour y $ r_col x)
                      | otherwise     = findColours (y:xs) $ js ++ (addColour x "white")
  where r_col col = "#" ++ (tail col)

-- the "∨" character does not get rendered by JS sometimes (for unknown reasons)
-- more investigation needed to figure out why it is sometimes not encoded properly
addColour :: String -> Colour -> JavaScript
addColour t c = (createEle "span" "span")
              ++ (style "span" "color" c)
              ++ (setText "span" fixed_t)
              ++ ("para" `appendChild` "span")
  where fixed_t = map (\x -> if x=='∨' then 'v'; else x) t

-- for usage with appendHTML to test JavaScript
jsToText :: JavaScript -> [String]
jsToText js = [map (\x -> if x=='"' then '`' else x) js]

-- these are various JS statements for easier recreation
-- all inputs are strings, all outputs are a single string (of JavaScript)
createEle x y   = "var " ++ x ++ " = document.createElement(\"" ++ y ++ "\");"
setClass x y    = x ++ ".className = \"" ++ y ++ "\";"
setName x y     = x ++ ".setAttribute(\"name\",\"" ++ y ++ "\");"
setText x y     = x ++ ".innerText = \"" ++ y ++ "\";"
style x y z     = x ++ ".style." ++ y ++ "= \"" ++ z ++ "\";"
getElement x    = "document.getElementById(\"" ++ x ++ "\")"
appendChild x y = x ++ ".appendChild(" ++ y ++ ");"
\end{code}