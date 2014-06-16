module NetworkFunction where

import NetworkData
import CompactFlow

-- THE CONTENTS OF THIS FILE SHOULD NOT BE CHANGED ! (except for comments :-) )

--Match function type
--Should its pair Modify function be applied on this flow ?
type Match = Flow -> Bool
--Modifies the flow into a new one.
type Modify = Flow -> Flow
-- A network host (element) is described by this pair of functions.
-- Match dictates when the element should run its logic and the Modify dictates
-- what this logic is.
type NetworkElement = (Match, Modify)

-- The fuse function takes two network elements and generates a compound element
-- that implements the logic of both
fuse :: NetworkElement -> NetworkElement -> NetworkElement
fuse (m1, a1) (m2, a2) = ((\x -> m1 x && m2 x), a2 . a1)

-- A network is a list of network elements.
type Network = [NetworkElement]

-- Every type belonging to this class should be able to produce a pair of
-- Match and apply functions, to express its logic.
class Element a where
	getMatchAndModify :: a -> NetworkElement