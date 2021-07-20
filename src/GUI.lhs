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

import Data.List.Split
import Graphics.UI.Threepenny hiding (version, span, map, empty)
import qualified Graphics.UI.Threepenny as UI

import AbstractUI
import Assertions
import Dev
import JavaScript
import LiveProofs
import Persistence
import REqState
import Sequents
import SideCond
import TestParsing
import Utilities

--TODO:
-- Main.lhs       - Prover state recreated without reliance on REPL to parse inputs
-- GUI.lhs        - Re-create the final functionality of the prover state

\end{code}

\subsection{Introduction}

\subsection{Main Interface Code}
The core of the user interface is a local webserver built in HTML, with
a seperate CSS file used to style it appropriately.

\begin{code}
-- the base command for creating our webpage
runServ :: Int -> REqState -> [String] -> [String] -> IO ()
runServ port reqs0 wlcmt workspace = do
  startGUI (customConfig port) $ \win -> do
    return win # set title "reasonEq"
    UI.addStyleSheet win "reasonStyle.css"
    UI.getBody win #+ [mkMainInterface win reqs0 workspace []]
    UI.runFunction $ UI.ffi $ createJS True $ wlcmt ++ ["Welcome to the reasonEq GUI"]
    return ()

-- this creates the interface which we further manipulate directly through the DOM
mkMainInterface ::  Window -> REqState -> [String] -> [Element] -> UI Element
mkMainInterface win reqs workspace prevOutput = do
  -- reloading the interface after changing the REqState to refresh the button's stored REqState
  let
    reloadInterface :: REqState -> UI ()
    reloadInterface reqs = do
      output <- getElementsByClassName win "output"
      interface <- mkMainInterface win reqs workspace output
      getBody win # set children [interface]
      execJS $
        (displayJS "modifiedFlag" $ modified reqs)
        ++ (displayJS "devFlag" $ inDevMode reqs)
        ++ autoScroll ++ htmlBr

  -- relevant information loaded from REqState
  -- bad double quotes used '“' and '”'
  allTheories         <- return $ map trim $ splitOn ";" devListAllBuiltins
  loadedTheories      <- return $ map trim $ splitOn ";" $ observeTheoryNames reqs
  notLoadedTheories   <- return $ filter (\x -> not (x `elem` loadedTheories)) allTheories
  currentConjectures  <- return $ parseConj $ observeCurrConj reqs [""] 
  currentLaws         <- return $ parseLaws (currTheory reqs) $ observeLaws reqs [""]
  currentProofs       <- return $ parseProofs $ observeLiveProofs reqs
  
  -- input boxes used
  conjNameInput <- UI.input #. "overlayInput" # set id_ "conjNameInput"
  conjTermInput <- UI.input #. "overlayInput" # set id_ "conjTermsInput"
  loadInput <- UI.input #. "overlayInput" # set id_ "loadInput"
  saveInput <- UI.input #. "overlayInput" # set id_ "saveInput"
  demoteInput <- UI.input #. "overlayInput" # set id_ "demoteInput"
  demoteButton <- getInputButton "Demote Specific Law" demoteInput guiDemote reloadInterface reqs

  -- certain unique buttons that would bloat the lists they are placed into
  newConjButton <- UI.button #. "button" # set text "Make New Conjecture" # set id_ "overlayBtn"
  saveConjButton <- UI.button #. "button" # set text "Save Conjectures"
  quitButton <- mkJSButton "button" "Quit" "window.close();"
  
  -- functionality for the above buttons
  on UI.click newConjButton $ \_ -> do
    value1 <- conjNameInput # get value
    value2 <- conjTermInput # get value
    reqs1 <- guiNewConj value1 value2 reqs
    reloadInterface reqs1 
  on UI.click saveConjButton $ \_ -> do
    output <- guiSaveConj reqs
    let _fix  = map (\x -> if x=='"' then '\''; else x)
        fixed = map _fix output
    guiShow reqs $ fixed ++ ["Saved Conjectures."]
  on UI.click quitButton $ \_ -> guiQuit reqs

  -- aggregation of buttons/inputs to more easily create the DOM structure
  let
    -- list of primary control buttons
    -- left hand primary buttons
    ctrlBtnList :: [UI Element]
    ctrlBtnList = [ mkJSButton "button" "Show Context" $ showOneEle reqBoxes "showBtnBox"
                  , mkJSButton "button" "Set Theory" setCode
                  , mkJSButton "button" "New Conjecture" newConjCode
                  , mkJSButton "button" "New Proof" $ newProofCode ++ printConjs
                  , mkJSButton "button" "Return to Proof" returnProofCode
                  , mkJSButton "button" "Save State" saveCode
                  , mkJSButton "button" "Load State" loadCode
                  , element saveConjButton
                  , mkJSButton "button" "Load Conjectures" loadConjCode
                  , mkJSButton "button" "Assume Conjecture" assumeCode
                  , mkJSButton "button" "Demote Law" demoteCode
                  , mkJSButton "button" "Modify Theories" builtinCode
                  , mkJSButton "button" "Change Settings" $ createJS True ["To be implemented."] 
                  , mkJSButton "button" "Clear Output" $ clearOutput reqBoxes
                  , element quitButton]
                  where printConjs = createJS True ("Conjectures:" : [observeCurrConj reqs [""]])
    -- list of 'Show' related buttons
    showBtnList :: [UI Element]
    showBtnList = [ mkJSButton "overlayBtn" "Show Current Theory" $ _show [observeCurrTheory reqs]
                  , mkJSButton "overlayBtn" "Show Theory Names" $ _show [observeTheoryNames reqs]
                  , mkJSButton "overlayBtn" "Show Theory Relations" $ _show [observeTheories reqs]
                  , mkJSButton "overlayBtn" "Show Builtin Theories" $ _show [devListAllBuiltins, devBIRemind]
                  , blankButton   
                  , mkJSButton "overlayBtn" "Show Logic Signature" $ _show [observeSig reqs]
                  , mkJSButton "overlayBtn" "Show Live Proofs" _liveProofs
                  , mkJSButton "overlayBtn" "Show Laws" $ _show [observeLaws reqs [""]]
                  , mkJSButton "overlayBtn" "Show Laws (Uniqueness)" $ _show [observeLaws reqs [""]]
                  , mkJSButton "overlayBtn" "Show Known Names" $ _show [observeKnowns reqs [""]]
                  , mkJSButton "overlayBtn" "Show Current Conjectures" $ _show [observeCurrConj reqs [""]]
                  , mkJSButton "overlayBtn" "Show Settings" $ _show [observeSettings reqs]
                  , mkJSButton "overlayBtn" "Show Workspaces" $ (createJS True workspace) ++ (displayJS "showBtnBox" False)]
                  where _show output  = (createJS True output) ++ (hideJS reqBoxes "")
                        _liveProofs   = (colourJS (removeTermColours $ observeLiveProofs reqs) htmlPurple "")
                                        ++ autoScroll ++ htmlBr ++ (displayJS "showBtnBox" False) ++ fixWeirdBox

    -- various sub-menus that become available for user input
    setTheoryList = map (\x -> hasInputButton ("Set '" ++ x ++ "'")
                        x guiSetTheory reloadInterface reqs) loadedTheories
    newConjList   = map element [conjNameInput, conjTermInput, newConjButton]
    newProofList  = map (\(x,y) -> newProofButton win ("Prove '" ++ x ++ "'")
                        y reqs) $ pairNumberList currentConjectures 1 
    returnList    = map (\(x,y) -> resumeProofButton win ("Prove '" ++ x ++ "'")
                        y reqs) $ pairNumberList currentProofs 1         
    saveList      = [ hasInputButton "Save Prover State" "" guiSave reloadInterface reqs
                    , hasInputButton "Save Current Theory" "." guiSave reloadInterface reqs
                    , blankButton] ++  map (\x -> hasInputButton ("Save '" ++ x ++ "'")
                                        x guiSave reloadInterface reqs) loadedTheories
    loadList      = [ hasInputButton "Load Prover State" "" guiLoad reloadInterface reqs
                    , blankButton
                    , mkJSButton "overlayBtn" "Load Theory" loadTCode
                    , mkJSButton "overlayBtn" "Load New Theory" fLoadTCode ]
    -- the only unfinished part -> do we get a relevant list of file names?
    loadConjList  = [ UI.input #. "overlayInput" # set id_ "loadConjInput"
                    , mkJSButton "overlayBtn" "Load Conjecture" $ hideJS reqBoxes ""]
    assumeList    = [ hasInputButton "Assume All Current Theory Conjectures" "." guiAssume reloadInterface reqs
                    , hasInputButton "Assume All Dependency Theory Conjectures" "*" guiAssume reloadInterface reqs
                    , blankButton] ++ map (\x -> hasInputButton ("Assume '" ++ x ++ "'")
                                        x guiAssume reloadInterface reqs) currentConjectures
    demoteList    = [ hasInputButton "Demote All Current Theory-Proven Laws" "[]" guiDemote reloadInterface reqs
                    , hasInputButton "Demote All Current Assume Laws" "*" guiAssume reloadInterface reqs]
                    ++ [blankButton] ++ map (\x -> hasInputButton ("Demote '" ++ x ++ "'")
                                          x guiDemote reloadInterface reqs) currentLaws
    builtinList   = [ mkJSButton "overlayBtn" "Install Theory" installTCode
                    , mkJSButton "overlayBtn" "Reset Theory" resetTCode
                    , mkJSButton "overlayBtn" "Update Theory" updateTCode
                    , mkJSButton "overlayBtn" "Force Update Theory" fUpdateTCode]
    installTList  = map (\x -> hasInputButton ("Install '" ++ x ++ "'")
                      x guiInstall reloadInterface reqs) notLoadedTheories
    resetTList    = map (\x -> hasInputButton ("Reset '" ++ x ++ "'")
                      x guiReset reloadInterface reqs) loadedTheories
    updateTList   = map (\x -> hasInputButton ("Reset '" ++ x ++ "'")
                      x guiUpdate reloadInterface reqs) loadedTheories
    fUpdateTList  = map (\x -> hasInputButton ("Reset '" ++ x ++ "'")
                      x guiForceUpdate reloadInterface reqs) loadedTheories
    loadTList     = map (\x -> hasInputButton ("Load '" ++ x ++ "'")
                      x guiLoad reloadInterface reqs) loadedTheories
    loadNewTList  = map (\x -> hasInputButton ("Load '" ++ x ++ "'")
                      x guiForceLoad reloadInterface reqs) notLoadedTheories

  -- final aggregation of all elements into parent, returning a nested structure

  -- container divs for the buttons
  buttonBox     <- UI.div #. "buttonBox" #+ ctrlBtnList
  showBtnBox    <- mkOverlayDiv "showBtnBox" "Show Context" showBtnList
  setTheoryBox  <- mkOverlayDiv "setTheoryBox" "Set Theory" setTheoryList 
  newConjBox    <- mkOverlayDiv "newConjBox" "New Conjecture" newConjList
  newProofBox   <- mkOverlayDiv "newProofBox" "New Proof" newProofList
  returnBox     <- mkOverlayDiv "returnBox" "Return to Proof" returnList
  saveBox       <- mkOverlayDiv "saveBox" "Save" saveList
  loadBox       <- mkOverlayDiv "loadBox" "Load" loadList
  loadTBox      <- mkOverlayDiv "loadTBox" "Load Theory" loadTList
  loadNewTBox   <- mkOverlayDiv "loadNewTBox" "Load New Theory" loadNewTList
  loadConjBox   <- mkOverlayDiv "loadConjBox" "Load Conjecture" loadConjList
  assumeBox     <- mkOverlayDiv "assumeBox" "Assume Conjecture" assumeList
  demoteBox     <- mkOverlayDiv "demoteBox" "Demote Law" demoteList
  builtinBox    <- mkOverlayDiv "builtinBox" "Modify Theories" builtinList
  installTBox   <- mkOverlayDiv "installTBox" "Install Theory" installTList
  resetTBox     <- mkOverlayDiv "resetTBox" "Reset Theory" resetTList
  updateTBox    <- mkOverlayDiv "updateTBox" "Update Theory" updateTList
  fUpdateTBox   <- mkOverlayDiv "fUpdateTBox" "Force Update Theory" fUpdateTList
  sequentBox    <- UI.div #. "overlayBox" # set id_ "sequentBox" # set name "sequentBox"

  controlBox <- mkOutput reqs prevOutput
    #+ (map element [ showBtnBox, setTheoryBox, newConjBox
                    , newProofBox, sequentBox, returnBox, saveBox
                    , loadBox, loadTBox, loadNewTBox, loadConjBox
                    , assumeBox, demoteBox, builtinBox, installTBox
                    , resetTBox, updateTBox, fUpdateTBox ])

  -- container holding everything together
  mainContainer <- UI.div #. "mainContainer"
    #+ [element buttonBox]
    #+ [element controlBox]
  return mainContainer

mkProofInterface :: Window -> (REqState, LiveProof) -> UI Element
mkProofInterface win (reqs,lp) = do
  let
    reloadInterface :: (REqState, LiveProof) -> UI ()
    reloadInterface (reqs,lp) = do
      output <- getElementsByClassName win "output"
      interface <- mkProofInterface win (reqs,lp)
      getBody win # set children [interface]
      execJS $
        (displayJS "modifiedFlag" $ modified reqs)
        ++ (displayJS "devFlag" $ inDevMode reqs)
        ++ autoScroll ++ htmlBr

  controlBox <- mkOutput reqs []

  -- container holding everything together
  proverContainer <- UI.div #. "mainContainer"
    #+ [element controlBox]

  return proverContainer

mkOutput :: REqState -> [Element] -> UI Element
mkOutput reqs prevOutput = do
  let
    -- 'flags' used to visually indicate some part of REqState
    modifiedFlag    = UI.div #. "flag" # set text "Modified" # set id_ "modifiedFlag"
    devFlag         = UI.div #. "flag" # set text "Dev Mode" # set id_ "devFlag"
    currTheoryFlag  = UI.div #. "flag" # set text ("Current Theory '" ++ currTheory reqs ++ "'")
      # set id_ "currTheoryFlag"
    initOutput      = UI.p   #. "initOutput" # set id_ "initOutput" # set text ""

  outputBox <- UI.div #. "outputBox" # set id_ "outputBox"
    #+ [initOutput, modifiedFlag, devFlag, currTheoryFlag]
    #+ map element prevOutput
  controlBox <- UI.div #. "controlBox" # set id_ "controlBox"
    #+ [element outputBox]

  return controlBox
\end{code}

\subsection{Serving Text}
Since we have created a source from which all output comes, specific output
can be arbitrarily appended at any time. This is the most basic point of entry
for generic text output.
\begin{code}
-- executing a string in JS that was put together in Haskell to modify the HTML DOM
appendHTML :: [String] -> UI ()
appendHTML output = execJS $ createJS False output
\end{code}

\subsection{Buttons and a Div}
The hallmark of any good GUI is plenty of buttons. The following functions create
the variety of types of buttons used to a) Display more input options,
b) Modify the REqState, or c) Show some part of REqState.
\begin{code}
-- create a button that performs some JavaScript Action
mkJSButton :: String -> String -> JavaScript -> UI Element
mkJSButton identity btnLabel _code = do
  btn <- UI.button #. "button" # set text btnLabel # set id_ identity
  on UI.click btn $ \_ -> do
    execJS _code
  return btn

-- creates a button that modifys the REqState based on some gathered input
getInputButton :: String -> Element
  -> (String -> REqState -> UI REqState)
  -> (REqState -> UI ())
  -> REqState -> UI Element
getInputButton _label _input _func reload reqs = do
  btn <- UI.button #. "button" # set text _label # set id_ "overlayBtn"
  on UI.click btn $ \_ -> do
    _value  <- _input # get value
    reqs1   <- _func _value reqs
    reload  reqs1
  return btn

-- creates a button that modifies the REqState in some fixed way
hasInputButton :: String -> String
  -> (String -> REqState -> UI REqState)
  -> (REqState -> UI ())
  -> REqState -> UI Element
hasInputButton _label arg _func reload reqs = do
  btn <- UI.button #. "button" # set text _label # set id_ "overlayBtn"
  on UI.click btn $ \_ -> do
    reqs1   <- _func arg reqs
    reload  reqs1
  return btn

-- creates a 'Prove' button that generates the relevant list of 'Sequent' buttons
newProofButton :: Window -> String -> String -> REqState -> UI Element
newProofButton win _label _conj reqs = do
  btn <- UI.button #. "button" # set text _label # set id_ "overlayBtn"
  on UI.click btn $ \_ -> do
    (nmda,plist)  <-  guiNewProof1 num reqs
    sequentList   <-  return $ map fst plist
    let btnList   =   map (\(x,y) -> sequentButton win ("Use '" ++ x ++ "'")
                        y (nmda,plist) reqs) $ pairNumberList sequentList 1
    maybeEle      <-  getElementById win "sequentBox"
    _             <-  case maybeEle of
                        Just _div ->  (return _div) # set children []
                                        #+ btnList #+ [cancelButton]
    execJS $ createJS True $ "Strategies:" : map presentSeq plist
    execJS $ showOneEle reqBoxes "sequentBox"
  return btn
  where num = read _conj :: Int

-- creates 'Sequent' buttons that move the GUI into the 'Prover' state
sequentButton :: Window -> String -> String
  -> (NmdAssertion, [(String, Sequent)])
  -> REqState -> UI Element
sequentButton win _label _seq _data reqs = do
  btn <- UI.button #. "button" # set text _label # set id_ "overlayBtn"
  on UI.click btn $ \_ -> do
    (reqs1,lp)  <- guiNewProof2 _data num reqs
    interface   <- mkProofInterface win (reqs1,lp)
    getBody win # set children [interface]
    appendHTML ["Prover Starting...", dispLiveProof lp, "proof:"]
  return btn
  where num = read _seq :: Int

-- creates a button to return to a LiveProof
resumeProofButton :: Window -> String -> String
  -> REqState -> UI Element
resumeProofButton win _label proof reqs = do
  btn <- UI.button #. "button" # set text _label # set id_ "overlayBtn"
  on UI.click btn $ \_ -> do
    (reqs1,lp)  <- guiResumeProof num reqs
    interface   <- mkProofInterface win (reqs1,lp)
    getBody win # set children [interface]
    appendHTML ["Prover Starting...", dispLiveProof lp, "proof:"]
  return btn
  where num = read proof :: Int 

-- useful buttons with specific, limited functionality
blankButton = UI.button #. "blank" # set text ""
cancelButton = mkJSButton "overlayBtn" "Cancel" $ hideJS reqBoxes ""
testButton = do
  btn <- UI.button #. "button" # set text "Test Something" # set id_ "overlayBtn"
  on UI.click btn $ \_ -> appendHTML ["test"]
  return btn

-- create an 'overlay' div
mkOverlayDiv :: String -> String -> [UI Element] -> UI Element
mkOverlayDiv identity _title btns = UI.div #. "overlayBox" # set id_ identity
  #+ [UI.p #. "divTitle" # set text _title] #+ btns #+ [cancelButton]
\end{code}

\subsection{Interfacing with Abstract UI}

These functions are remarkably similar to the REPL functions, the key
difference being the indentation and the use of the UI monad and the
changes made to accomodate it.
\begin{code}
guiShow :: REqState -> [String] -> UI REqState
guiShow reqs output = appendHTML output >> return reqs

guiSetTheory ::  String -> REqState -> UI REqState
guiSetTheory theory reqs =
  case setCurrentTheory theory reqs of
    Nothing     ->  guiShow reqs  ["No such theory: '"    ++ theory ++ "'"]
    Just reqs'  ->  guiShow reqs' ["Current Theory now '" ++ theory ++ "'"]

guiNewConj :: String -> String -> REqState -> UI REqState
guiNewConj _name terms reqs = do 
  case sPredParse terms of
    But msgs  -> guiShow reqs ("Bad Term, ":msgs)
    Yes (term,_) -> do
      asn' <- mkAsn term scTrue
      case newConjecture (currTheory reqs) (_name,asn') reqs of
        But msgs  -> guiShow reqs msgs
        Yes reqs' -> guiShow reqs' ["Conjecture '"++_name++"' installed"]

guiNewProof1 :: Int -> REqState -> UI (NmdAssertion, [(String, Sequent)])
guiNewProof1 arg reqs = do
  (nconj,strats) <- newProof1 arg reqs
  return (nconj,strats)

guiNewProof2 :: (NmdAssertion, [(String, Sequent)])
  -> Int -> REqState -> UI (REqState, LiveProof)
guiNewProof2 (nconj,strats) chosen reqs = do
  liveProof <- newProof2 nconj strats chosen reqs
  return (reqs, liveProof)

guiResumeProof :: Int -> REqState -> UI (REqState, LiveProof)
guiResumeProof num reqs = do
  liveProof <- resumeProof num reqs
  return (reqs, liveProof)

guiSave :: String -> REqState -> UI REqState
guiSave "" reqs = do
  liftIO $ writeAllState reqs
  guiShow reqs1 ["REQ-STATE written to '" ++ projectDir reqs ++ "'."]
    where reqs1 = reqs{ modified = False }
guiSave nm reqs =
  let nm' = if nm == "." then (currTheory reqs) else nm
  in
  case getTheory nm' $ theories reqs of
    Nothing
      -> guiShow reqs ["No such theory: '" ++ nm' ++ "'"]
    Just thry
      -> do liftIO $ writeNamedTheoryTxt reqs (nm',writeTheory thry)
            guiShow reqs ["Theory '" ++ nm' ++ "' written to '" ++ projectDir reqs ++ "'."]

guiLoad :: String -> REqState -> UI REqState
guiLoad "" reqs = do
  let dirfp = projectDir reqs
  (reqs1, output) <- liftIO $ readAllState dirfp 
  guiShow reqs1{ inDevMode = inDevMode reqs}
    $ ["Reading all prover state from " ++ dirfp ++ "..."]
    ++ output ++ ["...done."]
guiLoad theory reqs = do
  let dirfp = projectDir reqs
  (nm,thry,output) <- liftIO $ readNamedTheory dirfp theory
  reqs1 <- return $ changed $ theories__ (replaceTheory' thry) reqs
  guiShow reqs1 $ output
    ++ ["Theory '" ++ nm ++ "'read from  '" ++ dirfp ++ "'."]

guiForceLoad :: String -> REqState -> UI REqState
guiForceLoad theory reqs = do
  let dirfp = projectDir reqs
  (nm,thry,output) <- liftIO $ readNamedTheory dirfp theory
  _ <- guiShow reqs $ output
    ++ ["Theory '" ++ nm ++ "'read from  '" ++ dirfp ++ "'."]
  case addTheory thry $ theories reqs of
    Yes thrys'  -> return $ changed $ theories_ thrys' reqs
    But msgs    -> guiShow reqs $ ["Add theory failed:"] ++ msgs

guiSaveConj :: REqState -> UI [String]
guiSaveConj reqs =
  case getTheory (currTheory reqs) $ theories reqs of
    Nothing   -> return ["Can't find current theory!!!"]
    Just thry -> do
      let lawConjs = map lawNamedAssn (laws thry)
          allConjs = lawConjs ++ conjs thry
      liftIO $ writeConjectures reqs (thName thry) allConjs
      return $ map show allConjs

guiAssume :: String -> REqState -> UI REqState
guiAssume conj reqs =
  case assumeConjecture (currTheory reqs) conj reqs of
    But lns   ->  guiShow reqs  lns
    Yes reqs' ->  guiShow reqs' ["Assumed " ++ conj]

guiDemote :: String -> REqState -> UI REqState
guiDemote law reqs =
  case demoteLaw (currTheory reqs) law reqs of
    But lns    ->  guiShow reqs  lns
    Yes reqs'  ->  guiShow reqs' ["Demoted " ++ law]

guiInstall :: String -> REqState -> UI REqState
guiInstall theory reqs =
  case biLkp theory devKnownBuiltins of
    Nothing
      -> guiShow reqs ["devInstallBuiltin: no builtin theory '"++theory++"'"]
    Just thry
      -> case addTheory thry $ theories reqs of
            But msgs   -> guiShow reqs msgs
            Yes thrys' -> guiShow reqs1 ["Installed theory '"++theory++"'"]
              where reqs1 = reqs{theories=thrys', modified=True}

guiReset :: String -> REqState -> UI REqState
guiReset theory reqs =
  case biLkp theory devKnownBuiltins of
    Nothing
      -> guiShow reqs ["devResetBuiltin: no builtin theory '"++theory++"'"]
    Just thry0
      -> case replaceTheory theory (const thry0) (theories reqs) of
            But msgs   ->  guiShow reqs msgs
            Yes thrys' ->  guiShow reqs1 ["Reset theory '"++theory++"'"]
              where reqs1 = reqs{theories=thrys', modified=True}

guiUpdate :: String -> REqState -> UI REqState
guiUpdate theory reqs =
  case biLkp theory devKnownBuiltins of
    Nothing
      -> guiShow reqs ["devUpdateBuiltin: no builtin theory '"++theory++"'"]
    Just thry0
      ->  case updateTheory theory thry0 False (theories reqs) of
            But msgs   -> guiShow reqs msgs
            Yes thrys' -> guiShow reqs1 ["Updated theory '"++theory++"'"]
              where reqs1 = reqs{theories=thrys', modified=True}

guiForceUpdate :: String -> REqState -> UI REqState
guiForceUpdate theory reqs =
  case biLkp theory devKnownBuiltins of
    Nothing
      -> guiShow reqs ["devUpdateBuiltin: no builtin theory '"++theory++"'"]
    Just thry0
      ->  case updateTheory theory thry0 True (theories reqs) of
            But msgs   -> guiShow reqs msgs
            Yes thrys' -> guiShow reqs1 ["Updated theory '"++theory++"'"]
              where reqs1 = reqs{theories=thrys', modified=True}

guiQuit :: REqState -> UI ()
guiQuit reqs
 | inDevMode reqs  =  close
 | modified  reqs  =  saveAndGo reqs
 | otherwise       =  close
  where
    saveAndGo reqs0 = do 
      liftIO $ writeAllState reqs0
      close
    close = execJS "window.close();"
\end{code}

\subsection{AbstractUI Prover Interfacing}
\begin{code}

\end{code}

\subsection{Various Variables}

Various variables that are here to de-clutter
\begin{code}
execJS _code = runFunction $ ffi _code

reqBoxes = [ "showBtnBox", "setTheoryBox", "newConjBox", "newProofBox"
            , "returnBox", "saveBox", "loadBox","loadTBox"
            , "loadNewTBox", "loadConjBox", "assumeBox", "demoteBox"
            , "builtinBox", "installTBox", "resetTBox", "updateTBox"
            , "fUpdateTBox", "sequentBox" ]

proveBoxes =  [""]

htmlPurple = "#9b59b6"

customConfig port = UI.defaultConfig {jsPort = Just port, jsStatic = Just "./static"}

setCode         = showOneEle reqBoxes "setTheoryBox"
newConjCode     = showOneEle reqBoxes "newConjBox"
                  ++ setPlaceholder "conjNameInput" "Conjecture Name..."
                  ++ setPlaceholder "conjTermsInput" "Conjecture Terms..."
newProofCode    = showOneEle reqBoxes "newProofBox"
returnProofCode = showOneEle reqBoxes "returnBox"
saveCode        = showOneEle reqBoxes "saveBox"
loadCode        = showOneEle reqBoxes "loadBox"
loadTCode       = showOneEle reqBoxes "loadTBox"
fLoadTCode      = showOneEle reqBoxes "loadNewTBox"
loadConjCode    = showOneEle reqBoxes "loadConjBox"
                  ++ setPlaceholder "loadConjInput" "Conjecture..."
assumeCode      = showOneEle reqBoxes "assumeBox"
demoteCode      = showOneEle reqBoxes "demoteBox"
builtinCode     = showOneEle reqBoxes "builtinBox"
installTCode    = showOneEle reqBoxes "installTBox"
resetTCode      = showOneEle reqBoxes "resetTBox"
updateTCode     = showOneEle reqBoxes "updateTBox"
fUpdateTCode    = showOneEle reqBoxes "fUpdateTBox"
\end{code}

\subsection{AbstractUI Helper Functions}

Hesitancy to change some of AbstractUI's functions, and also a
not insignificant portion of REPL code, has led to these functions.
They are somewhat janky, but could be replaced in the future.
\begin{code}
-- parses current conjectures using 'bad' quotes
parseConj :: String -> [String]
parseConj _conj = badQuoteSeparator $ concat $ splitOn "\n" _conj

-- parses current laws using the '---' delimiter and 'bad' quotes
parseLaws :: String -> String -> [String]
parseLaws thry _laws  = badQuoteSeparator $ concat $ init $ cutTop
                        $ reverse $ dropWhile (/="Conjectures:")
                        $ reverse $ relevantList thrysLists
  where thrysLists    = map (\x -> splitOn "\n" x) $ tail $ splitOn "---" _laws
        cutTop (_:_:_:_:_:xs) = xs
        search = "Theory '"++ thry ++ "'"
        -- recursively search lists for relevant theory/laws combination
        relevantList []     = ["This should never happen"]
        relevantList [x]    = x
        relevantList (x:xs) = if (search==head x)
                              then x
                              else relevantList xs

badQuoteSeparator :: String -> [String]
badQuoteSeparator _input  = map (\x -> head $ splitOn "”" x)
                            $ tail $ splitOn "“" _input

-- parses live proofs using the standard indentation and inclusion of '@'
parseProofs :: [String] -> [String]
parseProofs proofs  = map (\(_:_:xs) -> trim $ head $ splitOn "@" xs)
                      $ map trim $ tail proofs
\end{code}