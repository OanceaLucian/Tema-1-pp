module Utils where

import NetworkData
import NetworkFunction
import CompactFlow
import Flow
--Make every field Any - the most general CompactFlow
mostGeneralCF = CompactFlow $ map (\h -> (h, Any)) allHeaders
--Just the most general Flow - every packet is memeber of this Flow
mostGeneralF = Flow [mostGeneralCF]

--Functii ajutatoare pentru rechability si elements

makerestrictions :: (Header,[String]) ->Flow

makerestrictions  conditions = Flow (map (\x -> rewrite  (fst conditions)  (StringAtom x) mostGeneralCF)   $snd  conditions )   

makeall::[(Header,[String])] ->Flow
makeall conditions = if length(conditions) == 1 then makerestrictions (head conditions) 
						else intersect (makerestrictions (head conditions)) (makerestrictions (last conditions)) 

makejustone ::(Header,String) -> Flow->Flow
makejustone rewritings x = (rewrite (fst rewritings) (StringAtom $snd rewritings) x)

--For reachability
testApplyForOne :: [NetworkElement] -> Flow -> [NetworkElement]
testApplyForOne [] flow = []
testApplyForOne (l:network) flow = if ((fst l) flow) ==True then (l : (testApplyForOne network flow )) else testApplyForOne network flow

testApplyForAll :: [NetworkElement] ->[Flow] ->[NetworkElement]
testApplyForAll network [] = []
testApplyForAll network (l:flows) = (testApplyForOne network l) ++ (testApplyForAll network flows) 

applyForOne :: [NetworkElement] -> Flow -> [Flow]
applyForOne [] flow =[]
applyForOne (l:network) flow = if ((fst l) flow ) == True then ((snd l) flow):(applyForOne network flow) else applyForOne network flow 

applyForAll :: [NetworkElement] ->[Flow] -> [Flow]

applyForAll network [] = []
applyForAll network (f:flows) = (applyForOne network f) ++ (applyForAll network flows)