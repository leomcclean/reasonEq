module VarDataTest ( tst_VarData )
{-
Copyright  Andrew Buttefield (c) 2017-18

LICENSE: BSD3, see file LICENSE at reasonEq root
-}
where

import Data.Maybe(fromJust)
import Data.Map as M (fromList, lookup, empty)
import Data.Set as S (fromList, singleton, empty)

import Test.HUnit
--import Test.Framework as TF (testGroup, Test)
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
--import Test.Framework.Providers.QuickCheck2 (testProperty)

import Utilities
import LexBase
import Variables
import AST
import VarData
import TestRendering

i = ObsVar  (fromJust $ ident "i") Static; ti = fromJust $ eVar ArbType i
j = ObsVar  (fromJust $ ident "j") Static; tj = fromJust $ eVar ArbType j
k = ObsVar  (fromJust $ ident "k") Static; tk = fromJust $ eVar ArbType k
u = ObsVar  (fromJust $ ident "u") Before; tu = fromJust $ eVar ArbType u
v = ObsVar  (fromJust $ ident "v") Before; tv = fromJust $ eVar ArbType v
v' = ObsVar  (fromJust $ ident "v") Before; tv' = fromJust $ eVar ArbType v'
e = ExprVar (fromJust $ ident "e") Before; te = fromJust $ eVar ArbType e
len = ExprVar (fromJust $ ident "len") Static; tlen = fromJust $ eVar ArbType len
p = PredVar (fromJust $ ident "P") Before; tp = fromJust $ pVar p
pT = PredVar (fromJust $ ident "T") Static; tT = fromJust $ pVar pT

iu = fromJust $ ident "lu" ; lu = PreVars  iu ; glu  = LstVar lu
iv = fromJust $ ident "lv" ; lv = PreVars  iv ; glv  = LstVar lv
lw  = PreVars  $ fromJust $ ident "lw"     ; glw  = LstVar lw
lv' = PostVars $ fromJust $ ident "lv"     ; glv' = LstVar lv'
lvm = MidVars  (fromJust $ ident "lv") "m" ; glvm = LstVar lvm
le  = PreExprs $ fromJust $ ident "le"     ; gle  = LstVar le
lP  = PrePreds $ fromJust $ ident "lP"     ; glP  = LstVar lP

x = ObsVar (fromJust $ ident "x") Static; lx = LVbl x [] []; glx = LstVar lx
ll  = PreVars  $ fromJust $ ident "ll"     ; gll  = LstVar ll
ls  = PreVars  $ fromJust $ ident "ls"     ; gls  = LstVar ls
f = ExprVar (fromJust $ ident "f") Static ; lf = LVbl f [] []

gi = StdVar i; gj = StdVar j
gv = StdVar v; ge = StdVar e; gp = StdVar p

aKC v t    =  fromJust . addKnownConst   v  t
aKV v t    =  fromJust . addKnownVar     v  t
aKL lv vl  =  fromJust . addKnownVarList lv vl
aKS lv vs  =  fromJust . addKnownVarSet  lv vs

tst_vardata_inserts -- not run as standard regression
-- because some tests are meant to fail
 = testGroup "Check VarData insertion shorthands"
     [ testCase "aKC: i ^= j  fails"
         ( vtList (aKC i tj newVarTable)
           @?= [(i,KnownConst tj)] )
     , testCase "aKV: i : tau succeeds"
         ( vtList (aKV i ArbType newVarTable)
           @?= [(i,KnownVar ArbType)] )
     , testCase "aKC.aKV: i : tau, then j ^=i succeeds"
         ( vtList (aKC j ti $ aKV i ArbType newVarTable)
           @?= [(i,KnownVar ArbType),(j,KnownConst ti)] )
     , testCase "aKL: lu ^= [glu] fails"
         ( dtList (aKL lu [glu] newVarTable)
           @?= [(lu,KnownVarList [glu])] )
     , testCase "aKL: lu ^= [gi] fails"
         ( dtList (aKL lu [gi] newVarTable)
           @?= [(lu,KnownVarList [gi])] )
     , testCase "aKL: lu ^= [gv] fails"
         ( dtList (aKL lu [gv] newVarTable)
           @?= [(lu,KnownVarList [gv])] )
     , testCase "aKL.aKV: i : tau, then lu ^= [gi] fails"
         ( dtList (aKL lu [gi] $ aKV i ArbType newVarTable)
           @?= [(lu,KnownVarList [gi])] )
     , testCase "dtList.aKL.aKV: v : tau, then lu ^= [gv] succeeds"
         ( dtList (aKL lu [gv] $ aKV v ArbType newVarTable)
           @?= [(lu,KnownVarList [gv])] )
     , testCase "vtList.aKL.aKV: v : tau, then lu ^= [gv] succeeds"
         ( vtList (aKL lu [gv] $ aKV v ArbType newVarTable)
           @?= [(v,KnownVar ArbType)] )
     , testCase "aKV.aKV: i : tau, then v : tau succeeds"
         ( vtList (aKV v ArbType $ aKV i ArbType newVarTable)
           @?= [(i,KnownVar ArbType),(v,KnownVar ArbType)] )
     , testCase "aKL.aKV.aKV: i : tau, then v : tau, then lu ^= [gv,gi] fails"
         ( vtList (aKL lu [gv,gi] $ aKV v ArbType $ aKV i ArbType newVarTable)
           @?= [(v,KnownVar ArbType)] )
     ]

-- -----------------------------------------------------------------------------
tst_addKnownConst :: TF.Test

k42 = EVal ArbType $ Integer 42
k99 = EVal ArbType $ Integer 99
pTrue = PVal $ Boolean True
ki = fromJust $ eVar ArbType i
kj = fromJust $ eVar ArbType j
kk = fromJust $ eVar ArbType k

tst_addKnownConst
 = testGroup "addKnownConst"
     [ testCase "k known as 42"
       ( vtList (fromJust (addKnownConst k k42 newVarTable))
         @?= [(k,KnownConst k42)] )
     , testCase "v known as 99"
       ( addKnownConst v k99 newVarTable @?= Nothing )
     , testCase "e known as 99"
       ( addKnownConst e k99 newVarTable @?= Nothing )
     , testCase "len known as 99"
       ( vtList (fromJust (addKnownConst len k99 newVarTable))
         @?= [(len,KnownConst k99)] )
     , testCase "P known as True"
       ( addKnownConst p pTrue newVarTable @?= Nothing )
     , testCase "T known as True"
       ( vtList (fromJust (addKnownConst pT pTrue newVarTable))
         @?= [(pT,KnownConst pTrue)] )
     , testCase "k |-> 99 after k |-> 42 should fail"
       ( addKnownConst k k99 (fromJust (addKnownConst k k42 newVarTable))
         @?= Nothing )
     ]

-- -----------------------------------------------------------------------------
tst_addKnownVar :: TF.Test

tBool = GivenType $ fromJust $ ident "Bool"
tInt = GivenType $ fromJust $ ident "Int"

tst_addKnownVar
 = testGroup "addKnownVar"
     [ testCase "k : Bool"
       ( vtList (fromJust (addKnownVar k tBool newVarTable))
         @?= [(k,KnownVar tBool)] )
     , testCase "v : Int "
       ( vtList (fromJust (addKnownVar v tInt newVarTable))
         @?= [(v,KnownVar tInt)] )
     , testCase "e : Int "
       ( vtList (fromJust (addKnownVar e tInt newVarTable))
        @?= [(e,KnownVar tInt)] )
     , testCase "T : Bool"
       ( vtList (fromJust (addKnownVar pT tBool newVarTable))
        @?= [(pT,KnownVar tBool)] )
     ]

-- -----------------------------------------------------------------------------
tst_lookupVarTable :: TF.Test

kvepTable
  = fromJust $ addKnownConst pT pTrue
  $ fromJust $ addKnownConst len k99
  $ fromJust $ addKnownVar v tInt
  $ fromJust $ addKnownVar k tBool newVarTable

z = ObsVar (fromJust $ ident "z") Static

tst_lookupVarTable
 = testGroup "lookupVarTable"
     [ testCase "k not in empty table"
       ( lookupVarTable newVarTable k @?= UnknownVar)
     , testCase "k in  complete table"
       ( lookupVarTable kvepTable k @?= KnownVar tBool)
     , testCase "v not in empty table"
       ( lookupVarTable newVarTable v @?= UnknownVar)
     , testCase "v in  complete table"
       ( lookupVarTable kvepTable v @?= KnownVar tInt)
     , testCase "len not in empty table"
       ( lookupVarTable newVarTable e @?= UnknownVar)
     , testCase "len in  complete table"
       ( lookupVarTable kvepTable len @?= KnownConst k99)
     , testCase "T not in empty table"
       ( lookupVarTable newVarTable pT @?= UnknownVar)
     , testCase "T in  complete table"
       ( lookupVarTable kvepTable pT @?= KnownConst pTrue)
     , testCase "z not in complete table"
       ( lookupVarTable kvepTable z @?= UnknownVar )
     ]

-- -----------------------------------------------------------------------------
tst_addKnownListVar :: TF.Test

sngl = S.singleton

-- setup vardata where i,j,u,len and T are known
-- and ll and ls map to empty lists and sets respectively
-- and lf maps to [len]
iltVarData
  = aKL lf [StdVar len] $
    aKV i ArbType $ aKV j ArbType $ aKV u ArbType $
    aKV len ArbType $ aKV pT ArbType $
    aKL ll [] $ aKS ls S.empty  newVarTable

tst_addKnownListVar
 = testGroup "addKnownVarList/Set"
     [ -- inconsistent classifications
       testCase "lu |-> <lv'>, inconsistent!"
       ( addKnownVarList lu [glv'] newVarTable @?= Nothing )
     , testCase "lu |-> <le>, inconsistent!"
       ( addKnownVarList lu [gle]  newVarTable @?= Nothing )
     , testCase "le |-> <lu>, inconsistent!"
        ( addKnownVarList le [glu] newVarTable @?= Nothing )
     , testCase "lP |-> <le>, inconsistent!"
        ( addKnownVarList lP [gle] newVarTable @?= Nothing )
     -- some map to ... the other
     , testCase "lv |-> <ls>, set and list!"
        ( addKnownVarList lv [gls] iltVarData @?= Nothing )
     , testCase "lv |-> {ll}, set and list!"
        ( addKnownVarSet lv (sngl gll) iltVarData @?= Nothing )
     , testCase "lv |-> <ll,ls>, set and list!"
        ( addKnownVarList lv [gll,gls] iltVarData @?= Nothing )
     -- list-variable cycle
     , testCase "ll |-> <ll>, cycle!"
        ( addKnownVarList ll [gll] iltVarData @?= Nothing )
     , testCase "ls |-> {ls}, cycle!"
        ( addKnownVarSet ls (sngl gls) iltVarData @?= Nothing )
     -- range list contains unknown variables
     , testCase "lv |-> [lw], unknowns!"
        ( addKnownVarList lv [glw] iltVarData @?= Nothing )
     -- trying to update entry
     , testCase "lf |-> [len,len], update!"
        ( addKnownVarList lf [StdVar len,StdVar len] iltVarData @?= Nothing )
     -- successful entries
     , testCase "lu |-> [], succeeds"
        ( dtList (aKL lu [] iltVarData)
         @?= [(ll,KnownVarList [])
             ,(ls,KnownVarSet S.empty)
             ,(lu,KnownVarList [])] )
     , testCase "lx |-> [], succeeds"
        ( stList (aKL lx [] iltVarData)
         @?= [(lf,KnownVarList [StdVar len]),(lx,KnownVarList [])] )
     , testCase "lx |-> [i,j], succeeds"
        ( stList (aKL lx [gi,gj] iltVarData)
         @?= [(lf,KnownVarList [StdVar len]),(lx,KnownVarList [gi,gj])] )
     , testCase "lx |-> {i,j}, succeeds"
        ( stList (aKS lx (S.fromList [gi,gj]) iltVarData)
         @?= [(lf,KnownVarList [StdVar len])
             ,(lx,KnownVarSet (S.fromList [gi,gj]))] )
     ]

lulvTable = fromJust $ addKnownVarList lu [glv]             newVarTable
lulwTable = fromJust $ addKnownVarSet  lu (S.singleton glw) newVarTable

tst_lookupLVarTable
 = testGroup "lookupLVarTable"
     [ testCase "lu in empty table, should be UnknownListVar"
       ( lookupLVarTable newVarTable lu @?= UnknownListVar )
     , testCase "ll in iltVarData, should be []"
       ( lookupLVarTable iltVarData ll @?= KnownVarList [] )
     , testCase "ls in iltVarData, should be {}"
       ( lookupLVarTable iltVarData ls @?= KnownVarSet S.empty )
     ]



-- -----------------------------------------------------------------------------
tst_VarData :: [TF.Test]
tst_VarData
  = [ testGroup "\nVarData"
      [ tst_addKnownConst
      , tst_addKnownVar
      , tst_lookupVarTable
      , tst_addKnownListVar
      , tst_lookupLVarTable
      ]
    ]
