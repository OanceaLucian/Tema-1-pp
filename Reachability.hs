module Reachability where

import NetworkFunction
import NetworkData
import Data.List(union,(\\))
import Utils
import Flow
import Elements

p=rewrite Port (StringAtom "c") mostGeneralF
-- Computes reachability from a source to a destination.
reachability :: Network -> String -> String -> [Flow]
reachability network source destination = filter (\x -> intersectNotEmpty x (rewrite Port (StringAtom destination ) mostGeneralF))
											(helper network [(rewrite Port (StringAtom source) mostGeneralF)] [])
--functie ajutatoare care imi returneaza toate flow-urile posibile Echivalent All din Enunt general
helper network flowlist allflowlist = if length(testApplyForAll network flowlist) == 0 then (allflowlist `union` flowlist)
										else 
											if  ( (applyForAll network flowlist) \\ (allflowlist `union` flowlist)) == [] 
												then (allflowlist `union` flowlist)
											  else 
											  	helper network  ((applyForAll network flowlist) \\ (allflowlist `union` flowlist)) (allflowlist `union` flowlist)