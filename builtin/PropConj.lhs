\section{Propositional Theorems (Conjunction)}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2018

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{verbatim}
\begin{code}
{-# LANGUAGE PatternSynonyms #-}
module PropConj (
  propConjName
, propConjTheory
) where

import Data.Maybe

import NiceSymbols

import Utilities
import LexBase
import Variables
import AST
import SideCond
import VarData
import Laws
import Proofs
import Theories

import Propositions
import PropEquiv
import PropNot
import PropDisj
\end{code}

\subsection{Conjunction Conjectures}

We supply conjectures here for each theorem in \cite{gries.93}
in the \textsc{Conjunction} section.

$$
\CONJPROPCONJ
$$

\begin{code}
p = fromJust $ pVar $ Vbl (fromJust $ ident "P") PredV Static
q = fromJust $ pVar $ Vbl (fromJust $ ident "Q") PredV Static
r = fromJust $ pVar $ Vbl (fromJust $ ident "R") PredV Static
\end{code}


$$
  \begin{array}{ll}
       \CJandSymm  & \CJandSymmN
  \end{array}
$$

\vspace{-8pt}
\begin{code}
cjandSym
 = ( _land++"_symm"
   , ( (p /\ q) === (q /\ p)
     , scTrue ) )
\end{code}




\begin{code}
propConjConjs :: [NmdAssertion]
propConjConjs
  = [ cjandSym
    ]
\end{code}

\subsection{The Conjunction Theory}

\begin{code}
propConjName :: String
propConjName = "PropConj"
propConjTheory :: Theory
propConjTheory
  =  Theory { thName  =  propConjName
            , thDeps  =  [ propDisjName, propNotName
                         , propEquivName, propAxiomName ]
            , known   =  newVarTable
            , laws    =  []
            , proofs  =  []
            , conjs   =  propConjConjs
            }
\end{code}
