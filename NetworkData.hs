module NetworkData where
-- THE CONTENTS OF THIS FILE SHOULD NOT BE CHANGED ! (except for comments :-) )

-- The possible values for a header field.
-- Null represents an impossible value (i.e. due to a constraint not being satisfied)
-- It is not mandatory to use Null

-- DO NOT CHANGE HOW Eq, Show and Read are implemented for this data type.
data Value = Any | StringAtom String | Null deriving (Eq, Show, Read)

-- The possible headers. Every header will be present in every compact flow, bound to a
-- value (see above).

-- DO NOT CHANGE HOW Eq, Show and Read are implemented for this data type.
data Header = Src | Dst | Port deriving (Eq, Show, Read)

-- A list of all the possible headers.
allHeaders = [Src, Dst, Port]

-- A compact flow field.
-- A header bound to a value.
type Field = (Header, Value)

-- A compact flow is a list of fields or a Void flow
-- (i.e. impossible - used for error cases or constraint violation cases).

-- DO NOT CHANGE HOW Show and Read are implemented for this data type.
data CompactFlow = CompactFlow [Field] | Void deriving (Show, Read)

--The flow is a collection of CompactFlows or the impossible flow (as above).
-- DO NOT CHANGE HOW Show and Read are implemented for this data type.
data Flow = Flow [CompactFlow] | VoidFlow deriving (Show, Read)

-- Type class to which Flow and CompactFlow must be enrolled.
-- It defines the three major operations that one must be able to perform on flows.
class FlowLike a where
	intersect :: a -> a -> a
	subset :: a -> a -> Bool
	rewrite :: Header -> Value -> a -> a