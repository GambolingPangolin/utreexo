{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module: Data.HashTree
--
-- This module contains a hybrid Merkle tree for storing a subset of values in
-- such a way that their inclusion proofs can be reconstructed.  The design is
-- motivated by the need to frequently reconfigure the trees in a Utreexo
-- accumulator, and construct inclusion proofs of a specific subset of the
-- values in any configuration.
module Data.HashTree
    ( HashTree
    , leaf
    , hashNode
    , branch

    , Hash (..)
    , combine

    , NodeLocator
    , getLeaves
    , replaceSubtree
    , extractPathNeighbors
    ) where


-- | Identifies one type as the type of hash digests for another type
class Hash h a where
    hash :: a -> h


-- | This data structure represents a partially expanded Merkle tree.  Maximal
-- subtrees which do not contain an explicit value are represented by a hash
-- value.
data HashTree h a
    = Leaf a
    | HashNode !h
    | Branch !h (HashTree h a) (HashTree h a)
    deriving (Eq, Show)


instance Hash h a => Hash h (HashTree h a) where
    hash = \case
        Leaf x       -> hash x
        HashNode h   -> h
        Branch h _ _ -> h


-- | Embed a value in a leaf
leaf :: a -> HashTree h a
leaf = Leaf


-- | Embed the hash of a value as a leaf
hashNode :: Hash h a => a -> HashTree h a
hashNode = HashNode . hash


-- | Join two 'HashTree' values into a new 'HashTree' value.  If both the left
-- and right subtrees are hash nodes, create a hash node that merges them using
-- the semigroup operation.
branch :: (Hash h a, Semigroup h) => HashTree h a -> HashTree h a -> HashTree h a
branch (HashNode hl) (HashNode hr) = HashNode $ hl <> hr
branch x y                         = Branch (hash x <> hash y) x y


-- | Describe a node in a 'HashTree' by listing the branch counterparts at each
-- branch node from root to leaf.
type NodeLocator h = [Either h h]


-- | Collect all explicit-value leaves, together with their locators
getLeaves :: Hash h a => HashTree h a -> [(a, NodeLocator h)]
getLeaves = inner []
    where
    inner path = \case
        Leaf x -> [(x, reverse path)]
        HashNode _ -> []
        Branch _ l r -> inner (Right (hash r) : path) l <> inner (Left (hash l) : path) r


-- | Get the nodes adjacent to nodes along the path to a subtree
extractPathNeighbors :: NodeLocator h -> HashTree h a -> [HashTree h a]
extractPathNeighbors (p:ps) (Branch _ l r)
    | Left  _ <- p = l : extractPathNeighbors ps r
    | Right _ <- p = r : extractPathNeighbors ps l
extractPathNeighbors [] _ = []
extractPathNeighbors _  _ = error "Unable to extract a node"


-- | Substitute a tree for a node in another tree, returning the updated tree and the node that was replaced
replaceSubtree
    :: (Hash h a, Semigroup h)
    => NodeLocator h
    -- ^ path in the source tree to the surgery point
    -> HashTree h a
    -- ^ replacement tree
    -> HashTree h a
    -- ^ host tree
    -> (HashTree h a, HashTree h a)
    -- ^ (extracted subtree, modified tree)
replaceSubtree [] graft t = (t, graft)
replaceSubtree (p:ps) graft (Branch _ l r)
    | Left _  <- p
    , (t, r') <- replaceSubtree ps graft r
    = (t, branch l r')

    | Right _ <- p
    , (t, l') <- replaceSubtree ps graft l
    = (t, branch l' r)
replaceSubtree _ _ _ = error "Unable to reach replacement point"


-- | Merge hashes according to a sidedness
combine :: Semigroup h => h -> Either h h -> h
combine h = either (<> h) (h <>)
