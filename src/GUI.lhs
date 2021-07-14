\section{Graphical User Interface}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2017--2021

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{verbatim}
\begin{code}
module GUI
( runServ
)
where

import Graphics.UI.Threepenny hiding (version, span, map, empty)
import qualified Graphics.UI.Threepenny as UI

import AbstractUI
import Assertions
import Dev
import JavaScript
import Persistence
import REqState
import SideCond
import TestParsing
import Utilities

--TODO:
-- Main.lhs       - Prover state recreated without reliance on REPL to parse inputs
--                - Change Proof/Conjecture/Law menus to be created from a list of options
-- GUI.lhs        - Delimit Proof/Conjecture/Law options and create appropriate UI lists
--                - Re-create the final functionality of the prover state, now to use the input box when necessary
--                - Finish implementing the rest of the basic control buttons

\end{code}

\subsection{Introduction}

The GUI utilises both the AbstractUI and REPL to achieve a functional user interface.
The most simple way to introduce a GUI is to interrupt the existing REPL, and modify
the 'print' subsection. If 'print' could instead be considered 'pass action or print',
we can utilise the existing REPL framework. This has numerous advantages.

\subsection{State}

\begin{code}
type GUIArguments = String
type GUICmd state = GUIArguments -> state -> UI state
type GUICmdDescr state = (String, GUICmd state)
type GUIExit state = GUIArguments -> state -> IO (Bool,state)
type GUICommands state = [GUICmdDescr state]
\end{code}

We have a configuration that defines the REPL behaviour
that does not change during its lifetime:
\begin{code}
data GUIConfig state
  = GUIC {
      guiEOFReplacement :: [String]
    , guiQuitCmds :: [String]
    , guiQuit :: GUIExit state
    , guiCommands :: GUICommands state
    , guiEndCondition :: state -> Bool
    , guiEndTidy :: GUICmd state
    }
\end{code}

A series of textt{state} transformations.

\begin{code}

\end{code}

Various variables that are here to de-clutter the rest of the codebase
\begin{code}
showBoxes = [ "showBtnBox", "setTheoryBox", "newConjBox", "newProofBox", "returnProofBox"
            , "saveBox", "loadBox", "loadConjBox", "assumeBox", "demoteBox", "builtinBox"]

htmlPurple = "#9b59b6"

-- cleaning up startServ by moving this outside, however it is mostly unnecessary
customConfig :: Int -> Config
customConfig port = defaultConfig {jsPort = Just port, jsStatic = Just "./static"}
\end{code}

\subsection{Main Interface Code}
The core of the user interface is a local webserver built in HTML, with
a seperate CSS file used to style it appropriately.

\begin{code}
-- the base command for creating our webpage
runServ :: Int -> REqState -> [String] -> [String] -> IO ()
runServ port reqs0 wlcmt workspace = do
  startGUI (customConfig port) $ \win -> do
    let
      -- reloading the interface after changing the REqState to refresh the button's stored REqState
      reloadInterface :: REqState -> UI ()
      reloadInterface reqs = do
        output <- getElementsByClassName win "output"
        interface <- mkInterface reqs output
        getBody win # set children [interface]
        runFunction $ ffi $ (displayJS "modifiedFlag" $ modified reqs) ++ autoScroll ++ htmlBr

      -- this creates the interface which we further manipulate directly through the DOM
      mkInterface :: REqState -> [Element] -> UI Element
      mkInterface reqs prevOutput = do

        workspaceButton <- mkJSButton "overlayBtn" "Show Workspaces" $
          (createJS True workspace) ++ (displayJS "showBtnBox" False)

        -- This has to expand to setting changes

        setTheoryInput <- UI.input #. "overlayInput" # set id_ "setTheoryInput"
        setTheoryButton <- modStateButton "overlayBtn" "Set Theory" setTheoryInput guiSetTheory reloadInterface reqs

        conjNameInput <- UI.input #. "overlayInput" # set id_ "conjNameInput"
        conjTermInput <- UI.input #. "overlayInput" # set id_ "conjTermsInput"
        newConjButton <- UI.button #. "button" # set text "Make New Conjecture" # set id_ "overlayBtn"
        on UI.click newConjButton $ \_ -> do
          value1 <- conjNameInput # get value
          value2 <- conjTermInput # get value
          reqs1 <- guiNewConj value1 value2 reqs
          reloadInterface reqs1 



        saveButton <- mkJSButton "overlayBtn" "Save All Prover States" $ hideJS showBoxes ""
        on UI.click saveButton $ \_ -> do
          writeAllStateUI reqs
          appendHTML ["REQ-STATE written to '"++projectDir reqs++"'."]
          reloadInterface reqs{ modified = False} 

        demoteInput <- UI.input #. "overlayInput" # set id_ "demoteInput"
        demoteButton <- modStateButton "overlayBtn" "Demote Specific Law" demoteInput guiDemote reloadInterface reqs

        let
          -- blank button for spacing in 'hidden' submenus
          blankButton = UI.button #. "blank" # set text ""
          cancelButton = [mkJSButton "overlayBtn" "Cancel" $ hideJS showBoxes ""]
          -- list of 'Show' related buttons
          showBtnList :: [UI Element]
          showBtnList = [ (element workspaceButton)
                        , blankButton
                        , (mkJSButton "overlayBtn" "Show Current Theory" $ abstractUIjs reqs observeCurrTheory)
                        , (mkJSButton "overlayBtn" "Show Theory Names" $ abstractUIjs reqs observeTheoryNames)
                        , (mkJSButton "overlayBtn" "Show Theory Relations" $ abstractUIjs reqs observeTheories)
                        , (mkJSButton "overlayBtn" "Show All Theories" allTheories)
                        , blankButton          
                        , (mkJSButton "overlayBtn" "Show Logic Signature" $ abstractUIjs reqs observeSig) 
                        , (mkJSButton "overlayBtn" "Show Live Proofs" $ liveProofButton reqs)
                        , (mkJSButton "overlayBtn" "Show Laws" $ abstractUIjs2 [""] reqs observeLaws)
                        , (mkJSButton "overlayBtn" "Show Laws (Uniqueness)" $ abstractUIjs2 [""] reqs observeLaws)
                        , (mkJSButton "overlayBtn" "Show Known Names" $ abstractUIjs2 [""] reqs observeKnowns)
                        , (mkJSButton "overlayBtn" "Show Current Conjectures" $ abstractUIjs2 [""] reqs observeCurrConj)
                        , (mkJSButton "overlayBtn" "Hide" $ displayJS "showBtnBox" False) ]
                        where allTheories = (createJS True [devListAllBuiltins, devBIRemind]) ++ (hideJS showBoxes "")
          -- various sub-menus that become available
          setTheoryList = map element [setTheoryInput, setTheoryButton]
          newConjList   = map element [conjNameInput, conjTermInput, newConjButton]
          newProofList = [ UI.input #. "overlayInput" # set id_ "newProofInput"
                         , (mkJSButton "overlayBtn" "Create Proof" $ hideJS showBoxes "") ]
          returnProofList = [ UI.input #. "overlayInput" # set id_ "returnProofInput"
                            , (mkJSButton "overlayBtn" "Return to Proof" $ hideJS showBoxes "")
                            , (mkJSButton "overlayBtn" "Cancel" $ hideJS showBoxes "")]
          saveList = [ element saveButton
                     , (mkJSButton "overlayBtn" "Save Current Theory" $ hideJS showBoxes "")
                     , blankButton
                     , UI.input #. "overlayInput" # set id_ "saveInput"
                     , (mkJSButton "overlayBtn" "Save Specific Theory" $ hideJS showBoxes "")
                     , (mkJSButton "overlayBtn" "Cancel" $ hideJS showBoxes "")]
          loadList = [ (mkJSButton "overlayBtn" "Load Prover States" $ hideJS showBoxes "")
                     , blankButton
                     , UI.input #. "overlayInput" # set id_ "loadInput"
                     , (mkJSButton "overlayBtn" "Load Existing Theory" $ hideJS showBoxes "")
                     , (mkJSButton "overlayBtn" "Load New Theory" $ hideJS showBoxes "")
                     , (mkJSButton "overlayBtn" "Cancel" $ hideJS showBoxes "")]
          loadConjList = [ UI.input #. "overlayInput" # set id_ "loadConjInput"
                         , (mkJSButton "overlayBtn" "Load Conjecture" $ hideJS showBoxes "")
                         , (mkJSButton "overlayBtn" "Cancel" $ hideJS showBoxes "")]
          assumeList = [ (mkJSButton "overlayBtn" "Assume All Current Conjectures" $ hideJS showBoxes "")
                       , (mkJSButton "overlayBtn" "Assume All Dependency Conjecture" $ hideJS showBoxes "")
                       , blankButton
                       , UI.input #. "overlayInput" # set id_ "assumeInput"
                       , (mkJSButton "overlayBtn" "Assume Specific Conjecture" $ hideJS showBoxes "")
                       , (mkJSButton "overlayBtn" "Cancel" $ hideJS showBoxes "")]
          demoteList = [ (mkJSButton "overlayBtn" "Demote All Current Proven Laws" $ hideJS showBoxes "")
                       , (mkJSButton "overlayBtn" "Demote All Current Assumed Laws" $ hideJS showBoxes "")
                       , blankButton, element demoteInput, element demoteButton
                       , (mkJSButton "overlayBtn" "Cancel" $ hideJS showBoxes "")]
          builtinList = [ UI.input #. "overlayInput" # set id_ "builtinInput"
                        , (mkJSButton "overlayBtn" "Install Specified Theory" $ hideJS showBoxes "")
                        , (mkJSButton "overlayBtn" "Reset Specified Theory" $ hideJS showBoxes "")
                        , (mkJSButton "overlayBtn" "Update Specified Theory" $ hideJS showBoxes "")
                        , (mkJSButton "overlayBtn" "Force Update Specified Theory" $ hideJS showBoxes "")
                        , (mkJSButton "overlayBtn" "Cancel" $ hideJS showBoxes "")]

        modifiedFlag <- UI.div #. "flag" # set text "Modified" # set id_ "modifiedFlag"
        devFlag <- UI.div #. "flag" # set text "Dev Mode" # set id_ "devFlag"
        quitBtn <- mkJSButton "button" "Quit" "window.close();"

        -- container divs for the buttons
        buttonBox <- UI.div #. "buttonBox" #+ (ctrlBtnList ++ [element quitBtn])
        showBtnBox <- UI.div #. "overlayBox" # set id_ "showBtnBox" #+ showBtnList
        setTheoryBox <- UI.div #. "overlayBox" # set id_ "setTheoryBox" #+ (setTheoryList ++ cancelButton)
        newConjBox <- UI.div #. "overlayBox" # set id_ "newConjBox" #+ (newConjList ++ cancelButton)
        newProofBox <- UI.div #. "overlayBox" # set id_ "newProofBox" #+ (newProofList ++ cancelButton)
        returnProofBox <- UI.div #. "overlayBox" # set id_ "returnProofBox" #+ (returnProofList ++ cancelButton)
        saveBox <- UI.div #. "overlayBox" # set id_ "saveBox" #+ (saveList ++ cancelButton)
        loadBox <- UI.div #. "overlayBox" # set id_ "loadBox" #+ (loadList ++ cancelButton)
        loadConjBox <- UI.div #. "overlayBox" # set id_ "loadConjBox" #+ (loadConjList ++ cancelButton)
        assumeBox <- UI.div #. "overlayBox" # set id_ "assumeBox" #+ (assumeList ++ cancelButton)
        demoteBox <- UI.div #. "overlayBox" # set id_ "demoteBox" #+ (demoteList ++ cancelButton)
        builtinBox <- UI.div #. "overlayBox" # set id_ "builtinBox" #+ (builtinList ++ cancelButton)

        -- main output and input
        initOutput <- UI.p   #. "initOutput" # set id_ "initOutput" # set text ""
        outputBox <- UI.div #. "outputBox" # set id_ "outputBox"
          #+ (map element $ [initOutput, modifiedFlag, devFlag] ++ prevOutput)
        controlBox <- UI.div #. "controlBox" # set id_ "controlBox"
          #+ (map element 
          [ showBtnBox, setTheoryBox, newConjBox, newProofBox, returnProofBox
          , saveBox, loadBox, loadConjBox, assumeBox, demoteBox, builtinBox, outputBox ])

        -- container holding everything together
        mainContainer <- UI.div #. "mainContainer"
          #+ [element buttonBox]
          #+ [element controlBox]
        return mainContainer

    -- code of runServ that creates the webpage
    return win # set title "reasonEq"
    UI.addStyleSheet win "reasonStyle.css"
    UI.getBody win #+ [mkInterface reqs0 []]
    runFunction $ ffi $ createJS True $ wlcmt ++ ["Welcome to the reasonEq GUI"]
    return ()
\end{code}

\subsection{Serving Text}
The core of the REPL is inherently incompatible with the GUI due to the way
that input is taken. In a REPL the next input is always being waited for, but
in a GUI many actions can happen and terminate independently of each other.

To make the best use of some REPL code to avoid needless duplication, we can
modify the usage \texttt{pustStrLn} within \texttt{Main} to check whether the 
program is running in GUI mode or otherwise, and produce an output accordingly.
\begin{code}
-- executing a string in JS that was put together in Haskell to modify the HTML DOM
appendHTML :: [String] -> UI ()
appendHTML output = runFunction $ ffi $ createJS False output
\end{code}

\subsection{Buttons}
The hallmark of any good GUI is plenty of buttons. Sensibly placed, and suitably
showBoxes sometimes, but none the less many buttons that produce a variety of outputs 
or other functionality (such as asking for more user input, or showing more buttons).
\begin{code}
-- create a button that performs some JavaScript Action
mkJSButton :: String -> String -> JavaScript -> UI Element
mkJSButton identity btnLabel _code = do
  btn <- UI.button #. "button" # set text btnLabel # set id_ identity
  on UI.click btn $ \_ -> do
    _ <- runFunction $ ffi _code
    UI.p # set text ""
  return btn

-- creates a button that modifys the REqState in some way
modStateButton :: String -> String -> Element -- button id, label, corresponding 'input' element
  -> (String -> REqState -> UI REqState) -- function to interface with AbstractUI
  -> (REqState -> UI ()) -- the reloadInterface function (to refresh REqState)
  -> REqState -> UI Element
modStateButton identity btnLabel inputBox auiFunc reloadFunc reqs = do
  btn <- UI.button #. "button" # set text btnLabel # set id_ identity
  on UI.click btn $ \_ -> do
    ivalue <- inputBox # get value
    reqs1 <- auiFunc ivalue reqs
    reloadFunc reqs1
  return btn

-- creates JavaScript from the result of some AbstractUI functions
abstractUIjs :: REqState -> (REqState -> String) -> JavaScript
abstractUIjs reqs f = (createJS True [f reqs])
  ++ (displayJS "showBtnBox" False)

-- creates JavaScript from the result of some AbstractUI functions
abstractUIjs2 :: [String] -> REqState
  -> (REqState -> [String] -> String) -> JavaScript
abstractUIjs2 args reqs f = (createJS True [f reqs args])
  ++ (displayJS "showBtnBox" False)

-- removes (and to replace) the terminal colour codes of Live Proofs
liveProofButton :: REqState -> JavaScript
liveProofButton reqs = 
  (colourJS (lines $ removeTermColours $ observeLiveProofs reqs) "@" htmlPurple "")
  ++ autoScroll ++ htmlBr ++ (displayJS "showBtnBox" False)
\end{code}

\subsection{Read-made Buttons}

The below buttons are used as the primary controls for the application.
\begin{code}
setCode         = showOneEle showBoxes "setTheoryBox"
                  ++ setPlaceholder "setTheoryInput" "Theory..."
newConjCode     = showOneEle showBoxes "newConjBox"
                  ++ setPlaceholder "conjNameInput" "Conjecture Name..."
                  ++ setPlaceholder "conjTermsInput" "Conjecture Terms..."
newProofCode    = showOneEle showBoxes "newProofBox"
                  ++ setPlaceholder "newProofInput" "Proof Number..."
returnProofCode = showOneEle showBoxes "returnProofBox"
                  ++ setPlaceholder "returnProofInput" "Proof Number..."
saveCode        = showOneEle showBoxes "saveBox"
                  ++ setPlaceholder "saveInput" "Theory..."
loadCode        = showOneEle showBoxes "loadBox"
                  ++ setPlaceholder "loadInput" "Theory..."
loadConjCode    = showOneEle showBoxes "loadConjBox"
                  ++ setPlaceholder "loadConjInput" "Conjecture..."
assumeCode      = showOneEle showBoxes "assumeBox"
                  ++ setPlaceholder "assumeInput" "Conjecture..."
demoteCode      = showOneEle showBoxes "demoteBox"
                  ++ setPlaceholder "demoteInput" "Law..."
builtinCode     = showOneEle showBoxes "builtinBox"
                  ++ setPlaceholder "builtinInput" "Theory..."

-- left hand primary buttons
ctrlBtnList :: [UI Element]
ctrlBtnList = [ (mkJSButton "button" "Show Options" $ showOneEle showBoxes "showBtnBox")
              , (mkJSButton "button" "Set Theory" setCode)
              , (mkJSButton "button" "New Conjecture" newConjCode)
              , (mkJSButton "button" "New Proof" newProofCode)
              , (mkJSButton "button" "Return to Proof" returnProofCode)
              , (mkJSButton "button" "Save State" saveCode)
              , (mkJSButton "button" "Load State" loadCode)
              , (mkJSButton "button" "Save Conjectures" "return();")
              , (mkJSButton "button" "Load Conjectures" loadConjCode)
              , (mkJSButton "button" "Assume Conjecture" assumeCode)
              , (mkJSButton "button" "Demote Law" demoteCode)
              , (mkJSButton "button" "Modify Theories" builtinCode)
              , (mkJSButton "button" "Clear Output" clearOutput) ]
\end{code}

\subsection{Interfacing with Abstract UI}

\begin{code}
guiShow :: REqState -> [String] -> UI REqState
guiShow reqs output = appendHTML output >> return reqs

guiSetTheory ::  String -> REqState -> UI REqState
guiSetTheory theory reqs 
    =  case setCurrentTheory theory reqs of
         Nothing     ->  guiShow reqs  ["No such theory: '"    ++ theory ++ "'"]
         Just reqs'  ->  guiShow reqs' ["Current Theory now '" ++ theory ++ "'"]

guiNewConj :: String -> String -> REqState -> UI REqState
guiNewConj name content reqs = do 
       case sPredParse content of
         But msgs  -> guiShow reqs ("Bad Term, ":msgs)
         Yes (term,_) ->
           do asn' <- mkAsn term scTrue
              case newConjecture (currTheory reqs) (name,asn') reqs of
                But msgs  -> guiShow reqs msgs
                Yes reqs' -> guiShow reqs' ["Conjecture '"++name++"' installed"]

guiDemote :: String -> REqState -> UI REqState
guiDemote law reqs
  = case demoteLaw (currTheory reqs) law reqs of
         But lns    ->  guiShow reqs  lns
         Yes reqs'  ->  guiShow reqs' ["Demoted " ++ law]
\end{code}

mkNewProof :: [String] -> REqState -> UI REqState
mkNewProof args reqs
  = case newProof1 (args2int args) reqs of
     Nothing -> do putStrLn "invalid conjecture number"
                   return reqs
     Just (nconj,strats)
      -> do putStrLn $ numberList presentSeq $ strats
            putStr "Select sequent by number: "
            hFlush stdout
            choice <- getLine
            case newProof2 nconj strats (readInt choice) reqs of
             Nothing -> doshow reqs "Invalid strategy no"
             Just liveProof -> proofREPL reqs liveProof