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

Various variables that are here to de-clutter the rest of the codebase
\begin{code}
showBoxes = [ "showBtnBox", "setTheoryBox", "newConjBox", "newProofBox"
            , "returnBox", "saveBox", "loadBox","loadTBox"
            , "fLoadTBox", "loadConjBox", "assumeBox", "demoteBox"
            , "builtinBox", "installTBox", "resetTBox", "updateTBox"
            , "fUpdateTBox" ]

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
        runFunction $ ffi $
          (displayJS "modifiedFlag" $ modified reqs)
          ++ (displayJS "devFlag" $ inDevMode reqs)
          ++ autoScroll ++ htmlBr

      -- this creates the interface which we further manipulate directly through the DOM
      mkInterface :: REqState -> [Element] -> UI Element
      mkInterface reqs prevOutput = do

        -- relevant information loaded from REqState
        allTheories <- return $ map trim $ splitOn ";" devListAllBuiltins
        loadedTheories <- return $ map trim $ splitOn ";" $ observeTheoryNames reqs
        notLoadedTheories <- return $ filter (\x -> not (x `elem` loadedTheories)) allTheories
        currentConjectures <- return $ map (\x -> head $ splitOn "”" x) $ tail $ splitOn "“"
          $ concat $ splitOn "\n" $ observeCurrConj reqs [""] -- bad double quotes used '“' and '”'
        currentProofs <- return $ map (\(_:_:xs) -> trim $ head $ splitOn "@" xs)
          $ map trim $ tail $ observeLiveProofs reqs

        -- 'flags' used to visually indicate some part of REqState
        modifiedFlag <- UI.div #. "flag" # set text "Modified" # set id_ "modifiedFlag"
        devFlag <- UI.div #. "flag" # set text "Dev Mode" # set id_ "devFlag"
        currTheoryFlag <- UI.div #. "flag" # set text ("Current Theory '" ++ currTheory reqs ++ "'") # set id_ "currTheoryFlag"
        
        -- input boxes used
        conjNameInput <- UI.input #. "overlayInput" # set id_ "conjNameInput"
        conjTermInput <- UI.input #. "overlayInput" # set id_ "conjTermsInput"
        loadInput <- UI.input #. "overlayInput" # set id_ "loadInput"
        saveInput <- UI.input #. "overlayInput" # set id_ "saveInput"
        demoteInput <- UI.input #. "overlayInput" # set id_ "demoteInput"
        demoteButton <- getInputButton "overlayBtn" "Demote Specific Law" demoteInput guiDemote reloadInterface reqs

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

        -- aggregation of buttons/inputs to more easily create DOM structure
        let
          -- list of primary control buttons
          -- left hand primary buttons
          ctrlBtnList :: [UI Element]
          ctrlBtnList = [ (mkJSButton "button" "Show Context" $ showOneEle showBoxes "showBtnBox")
                        , (mkJSButton "button" "Set Theory" setCode)
                        , (mkJSButton "button" "New Conjecture" newConjCode)
                        , (mkJSButton "button" "New Proof" newProofCode)
                        , (mkJSButton "button" "Return to Proof" returnProofCode)
                        , (mkJSButton "button" "Save State" saveCode)
                        , (mkJSButton "button" "Load State" loadCode)
                        , (element saveConjButton)
                        , (mkJSButton "button" "Load Conjectures" loadConjCode)
                        , (mkJSButton "button" "Assume Conjecture" assumeCode)
                        , (mkJSButton "button" "Demote Law" demoteCode)
                        , (mkJSButton "button" "Modify Theories" builtinCode)
                        , (mkJSButton "button" "Clear Output" $ clearOutput showBoxes) ]
          -- list of 'Show' related buttons
          showBtnList :: [UI Element]
          showBtnList = [ mkJSButton "overlayBtn" "Show Current Theory" $ _show [observeCurrTheory reqs]
                        , mkJSButton "overlayBtn" "Show Theory Names" $ _show [observeTheoryNames reqs]
                        , mkJSButton "overlayBtn" "Show Theory Relations" $ _show [observeTheories reqs]
                        , mkJSButton "overlayBtn" "Show All Theories" $ _show [devListAllBuiltins, devBIRemind]
                        , blankButton   
                        , mkJSButton "overlayBtn" "Show Logic Signature" $ _show [observeSig reqs]
                        , mkJSButton "overlayBtn" "Show Live Proofs" _liveProofs
                        , mkJSButton "overlayBtn" "Show Laws" $ _show [observeLaws reqs [""]]
                        , mkJSButton "overlayBtn" "Show Laws (Uniqueness)" $ _show [observeLaws reqs [""]]
                        , mkJSButton "overlayBtn" "Show Known Names" $ _show [observeKnowns reqs [""]]
                        , mkJSButton "overlayBtn" "Show Current Conjectures" $ _show [observeCurrConj reqs [""]]
                        , mkJSButton "overlayBtn" "Show Settings" $ _show [observeSettings reqs]
                        , mkJSButton "overlayBtn" "Show Workspaces" $ (createJS True workspace) ++ (displayJS "showBtnBox" False)]
                        where _show output  = (createJS True output) ++ (hideJS showBoxes "")
                              _liveProofs   = (colourJS (removeTermColours $ observeLiveProofs reqs) "@" htmlPurple "")
                                              ++ autoScroll ++ htmlBr ++ (displayJS "showBtnBox" False)

          -- various sub-menus that become available for user input
          setTheoryList = map (\x -> hasInputButton "overlayBtn" ("Set '" ++ x ++ "'")
                              x guiSetTheory reloadInterface reqs) loadedTheories
          newConjList   = map element [conjNameInput, conjTermInput, newConjButton]
          newProofList  = map (\(x,y) -> hasInputButton "overlayBtn" ("Prove '" ++ x ++ "'")
                              y guiNewProof reloadInterface reqs) $ appendCountToList currentConjectures 1 
          returnList    = map (\(x,y) -> hasInputButton "overlayBtn" ("Prove '" ++ x ++ "'")
                              y guiResumeProof reloadInterface reqs) $ appendCountToList currentProofs 1         
          saveList      = [ hasInputButton "overlayBtn" "Save Prover State" "" guiSave reloadInterface reqs
                          , hasInputButton "overlayBtn" "Save Current Theory" "." guiSave reloadInterface reqs
                          , blankButton] ++  map (\x -> hasInputButton "overlayBtn" ("Save '" ++ x ++ "'")
                                              x guiSave reloadInterface reqs) loadedTheories
          loadList      = [ hasInputButton "overlayBtn" "Load Prover State" "" guiLoad reloadInterface reqs
                          , blankButton
                          , mkJSButton "overlayBtn" "Load Theory" loadTCode
                          , mkJSButton "overlayBtn" "Force Load Theory" fLoadTCode ]
          loadConjList  = [ UI.input #. "overlayInput" # set id_ "loadConjInput"
                          , mkJSButton "overlayBtn" "Load Conjecture" $ hideJS showBoxes ""]
          assumeList    = [ mkJSButton "overlayBtn" "Assume All Current Conjectures" $ hideJS showBoxes ""
                          , mkJSButton "overlayBtn" "Assume All Dependency Conjecture" $ hideJS showBoxes ""
                          , blankButton
                          , UI.input #. "overlayInput" # set id_ "assumeInput"
                          , mkJSButton "overlayBtn" "Assume Specific Conjecture" $ hideJS showBoxes ""]
          demoteList    = [ mkJSButton "overlayBtn" "Demote All Current Proven Laws" $ hideJS showBoxes ""
                          , mkJSButton "overlayBtn" "Demote All Current Assumed Laws" $ hideJS showBoxes ""]
                          ++ [blankButton] ++ map element [demoteInput, demoteButton]
          builtinList   = [ mkJSButton "overlayBtn" "Install Theory" installTCode
                          , mkJSButton "overlayBtn" "Reset Theory" resetTCode
                          , mkJSButton "overlayBtn" "Update Theory" updateTCode
                          , mkJSButton "overlayBtn" "Force Update Theory" fUpdateTCode]
          installTList  = map (\x -> hasInputButton "overlayBtn" ("Install '" ++ x ++ "'")
                            x guiInstall reloadInterface reqs) notLoadedTheories
          resetTList    = map (\x -> hasInputButton "overlayBtn" ("Reset '" ++ x ++ "'")
                            x guiReset reloadInterface reqs) loadedTheories
          updateTList   = map (\x -> hasInputButton "overlayBtn" ("Reset '" ++ x ++ "'")
                            x guiUpdate reloadInterface reqs) loadedTheories
          fUpdateTList  = map (\x -> hasInputButton "overlayBtn" ("Reset '" ++ x ++ "'")
                            x guiForceUpdate reloadInterface reqs) loadedTheories
          loadTList     = map (\x -> hasInputButton "overlayBtn" ("Load '" ++ x ++ "'")
                            x guiLoad reloadInterface reqs) loadedTheories
          fLoadTList    = map (\x -> hasInputButton "overlayBtn" ("Force Load '" ++ x ++ "'")
                            x guiForceLoad reloadInterface reqs) loadedTheories

        -- final aggregation of all elements into parent, returning a nested structure

        -- container divs for the buttons
        buttonBox     <- UI.div #. "buttonBox" #+ (ctrlBtnList ++ [element quitButton])
        showBtnBox    <- mkOverlayDiv "showBtnBox" "Show" (showBtnList ++ [testButton])
        setTheoryBox  <- mkOverlayDiv "setTheoryBox" "Set Theory" setTheoryList 
        newConjBox    <- mkOverlayDiv "newConjBox" "New Conjecture" newConjList
        newProofBox   <- mkOverlayDiv "newProofBox" "New Proof" newProofList
        returnBox     <- mkOverlayDiv "returnBox" "Return to Proof" returnList
        saveBox       <- mkOverlayDiv "saveBox" "Save" saveList
        loadBox       <- mkOverlayDiv "loadBox" "Load" loadList
        loadTBox      <- mkOverlayDiv "loadTBox" "Load Theory" loadTList
        fLoadTBox     <- mkOverlayDiv "fLoadTBox" "Force Load Theory" fLoadTList
        loadConjBox   <- mkOverlayDiv "loadConjBox" "Load Conjecture" loadConjList
        assumeBox     <- mkOverlayDiv "assumeBox" "Assume Conjecture" assumeList
        demoteBox     <- mkOverlayDiv "demoteBox" "Demote Law" demoteList
        builtinBox    <- mkOverlayDiv "builtinBox" "Modify Theories" builtinList
        installTBox   <- mkOverlayDiv "installTBox" "Install Theory" installTList
        resetTBox     <- mkOverlayDiv "resetTBox" "Reset Theory" resetTList
        updateTBox    <- mkOverlayDiv "updateTBox" "Update Theory" updateTList
        fUpdateTBox   <- mkOverlayDiv "fUpdateTBox" "Force Update Theory" fUpdateTList

        -- main output and input
        initOutput <- UI.p   #. "initOutput" # set id_ "initOutput" # set text ""
        outputBox <- UI.div #. "outputBox" # set id_ "outputBox"
          #+ (map element $ [initOutput, modifiedFlag, devFlag, currTheoryFlag] ++ prevOutput)
        controlBox <- UI.div #. "controlBox" # set id_ "controlBox"
          #+ (map element 
          [ showBtnBox, setTheoryBox, newConjBox, newProofBox, returnBox
          , saveBox, loadBox, loadTBox, fLoadTBox, loadConjBox 
          , assumeBox, demoteBox, builtinBox, installTBox, resetTBox
          , updateTBox, fUpdateTBox, outputBox ])

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
Since we have created a source from which all output comes, specific output
can be arbitrarily appended at any time. This is the most basic point of entry
for generic text output.
\begin{code}
-- executing a string in JS that was put together in Haskell to modify the HTML DOM
appendHTML :: [String] -> UI ()
appendHTML output = runFunction $ ffi $ createJS False output
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
    _ <- runFunction $ ffi _code
    UI.p # set text ""
  return btn

-- creates a button that modifys the REqState based on some gathered input
getInputButton :: String -> String -> Element
  -> (String -> REqState -> UI REqState)
  -> (REqState -> UI ())
  -> REqState -> UI Element
getInputButton identity _label _input _func reload reqs = do
  btn <- UI.button #. "button" # set text _label # set id_ identity
  on UI.click btn $ \_ -> do
    _value <- _input # get value
    reqs1 <- _func _value reqs
    reload reqs1
  return btn

-- creates a button that modifies the REqState in some fixed way
hasInputButton :: String -> String -> String
  -> (String -> REqState -> UI REqState)
  -> (REqState -> UI ())
  -> REqState -> UI Element
hasInputButton identity _label arg _func reload reqs = do
  btn <- UI.button #. "button" # set text _label # set id_ identity
  on UI.click btn $ \_ -> do
    reqs1 <- _func arg reqs
    reload reqs1
  return btn

-- useful buttons with specific, limited functionality
blankButton = UI.button #. "blank" # set text ""
cancelButton = mkJSButton "overlayBtn" "Cancel" $ hideJS showBoxes ""
testButton    = do
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

guiNewProof :: String -> REqState -> UI REqState
guiNewProof arg reqs =
  case newProof1 num reqs of
    Nothing -> guiShow reqs ["Invalid conjecture number"]
    Just (nconj,strats)
      -> case newProof2 nconj strats 1 reqs of
          Nothing -> guiShow reqs ["Invalid strategy number"]
          Just liveProof -> guiShow reqs ["valid options"] -- liveProof
    where num = read arg :: Int

guiResumeProof :: String -> REqState -> UI REqState
guiResumeProof arg reqs =
  case resumeProof num reqs of
      Nothing -> do guiShow reqs ["Can't find requested live proof: " ++ arg]
      Just liveProof -> guiShow reqs ["valid options"]
  where num = read arg :: Int

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
    close = runFunction $ ffi $ "window.close();"
\end{code}

\subsection{AbstractUI Prover Interfacing}
\begin{code}

\end{code}

\subsection{Dual-Functionality Buttons}

The below buttons are some of those used as the primary controls for the application.
To reduce bloat their JavaScript is kept here.
\begin{code}
setCode         = showOneEle showBoxes "setTheoryBox"
newConjCode     = showOneEle showBoxes "newConjBox"
                  ++ setPlaceholder "conjNameInput" "Conjecture Name..."
                  ++ setPlaceholder "conjTermsInput" "Conjecture Terms..."
newProofCode    = showOneEle showBoxes "newProofBox"
returnProofCode = showOneEle showBoxes "returnBox"
saveCode        = showOneEle showBoxes "saveBox"
loadCode        = showOneEle showBoxes "loadBox"
loadTCode       = showOneEle showBoxes "loadTBox"
fLoadTCode      = showOneEle showBoxes "fLoadTBox"
loadConjCode    = showOneEle showBoxes "loadConjBox"
                  ++ setPlaceholder "loadConjInput" "Conjecture..."
assumeCode      = showOneEle showBoxes "assumeBox"
demoteCode      = showOneEle showBoxes "demoteBox"
builtinCode     = showOneEle showBoxes "builtinBox"
installTCode    = showOneEle showBoxes "installTBox"
resetTCode      = showOneEle showBoxes "resetTBox"
updateTCode     = showOneEle showBoxes "updateTBox"
fUpdateTCode    = showOneEle showBoxes "fUpdateTBox"
\end{code}