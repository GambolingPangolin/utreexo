{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- |
-- Module: Crypto.Utreexo
--
-- An implementation of https://eprint.iacr.org/2019/611.pdf
module Crypto.Utreexo
    ( -- * Utreexo accumulator

      Utreexo
    , count
    , forestSize
    , hashes
    , emptyUtreexo

      -- * Inclusion proofs

    , Proof (..)
    , verify
    , getProofs

      -- * Utreexo API

    , member
    , append
    , accumulate
    , delete
    ) where

import           Control.Monad  (foldM)
import           Data.Bifunctor (first, second)
import qualified Data.Bits      as Bits
import           Data.Maybe     (catMaybes)
import           Data.Word      (Word64)

import           Data.HashTree  (Hash (..), HashTree (..), NodeLocator)
import qualified Data.HashTree  as HT


-- | A proof consists of a value and a path to the leaf containing that value.
-- Depth decreases into the tails of the list.
newtype Proof h a = Proof { unProof :: (a, NodeLocator h) }
    deriving (Eq, Show)


instance (Hash h a, Semigroup h) => Hash h (Proof h a) where
    hash (Proof (x, hs)) = foldr (flip HT.combine) (hash x) hs


-- | Verify that the given proof proves inclusion in the Merkle Tree with the given root.
verify :: (Eq h, Hash h a, Semigroup h) => h -> Proof h a -> Bool
verify h0 pf = h0 == hash pf


-- | Compute the (zero-based) index of the tree containing the value for the given proof
itemSubtreeIndex :: Proof h a -> Int
itemSubtreeIndex = length . snd . unProof


-- | A Utreexo accumulator
data Utreexo h a
    = Utreexo
    { count :: Word64
    -- ^ The number of items in the accumulator
    , trees :: [Maybe (HashTree h a)]
    } deriving (Eq, Show)


-- | Retrieve the list of Merkle roots representing the current accumulator state
hashes :: (Hash h a, Semigroup h) => Utreexo h a -> [Maybe h]
hashes = fmap (fmap hash) . trees


-- | An accumulator with no values
emptyUtreexo :: Utreexo h a
emptyUtreexo = Utreexo 0 []


firstSubtreeIndex :: Utreexo h a -> Int
firstSubtreeIndex = Bits.countTrailingZeros . count


-- | The number of entries in the list of Merkle roots
forestSize :: Utreexo h a -> Int
forestSize (Utreexo n _) = (+ 1) . floor . logBase 2 $ fromIntegral n


-- | Construct proofs for all value nodes in the accumulator
getProofs :: Hash h a => Utreexo h a -> [Proof h a]
getProofs (Utreexo _ ts) = fmap Proof $ catMaybes ts >>= HT.getLeaves


-- | Test for membership in an accumulator
member :: (Eq h, Hash h a, Semigroup h) => Utreexo h a -> Proof h a -> Bool
member (Utreexo _ ts) p
    | Just t <- ts !! fromIntegral (itemSubtreeIndex p)
    = verify (hash t) p

    | otherwise = False


-- | Add a value node to the accumulator
append :: (Hash h a, Semigroup h) => Utreexo h a -> a -> Utreexo h a
append (Utreexo n ts) x = Utreexo (n + 1) $ insertNode (HT.leaf x) ts


-- | Add a value to the accumulator without creating a value node.  (It will
-- not be possible to update inclusion proofs for this value.)
accumulate :: (Hash h a, Semigroup h) => Utreexo h a -> a -> Utreexo h a
accumulate (Utreexo n ts) x = Utreexo (n + 1) $ insertNode (HT.hashNode x) ts


insertNode
    :: (Hash h a, Semigroup h)
    => HashTree h a
    -> [Maybe (HashTree h a)]
    -> [Maybe (HashTree h a)]
insertNode t []              = [Just t]
insertNode t (Nothing : ts') = Just t : ts'
insertNode t (Just t' : ts') =
    let ts'' = insertNode (HT.branch t t') ts'
    in Nothing : ts''


-- | Delete a value from the accumulator
delete :: forall h a. (Eq h, Hash h a, Semigroup h) => Proof h a -> Utreexo h a -> Maybe (Utreexo h a)
delete p@(Proof (_, path)) u@(Utreexo n ts)
    | n > 0
    , i1 <= forestSize u
    , Just host <- ts !! fromIntegral i1
    , verify (hash host) p
    = Just . Utreexo (n-1) $ updatedFront host graftNode <> back

    | otherwise = Nothing

    where
    i0 = firstSubtreeIndex u
    i1 = itemSubtreeIndex p

    (graftPath, leafPath) = splitAt (i1 - i0) path
    graftNode = if i0 < i1 then ts !! fromIntegral i0 else Nothing

    middle     = take (i1 - i0 - 1) middleTail
    back       = drop (i1 - i0)     middleTail
    middleTail = drop (i0 + 1)      ts

    extractFront = reverse . HT.extractPathNeighbors leafPath

    updatedFront host (Just graft) =
        let (front, updatedNode) = first extractFront $ HT.replaceSubtree graftPath graft host
        in (Just <$> front) <> [Nothing] <> middle <> [Just updatedNode]

    updatedFront host Nothing = (Just <$> extractFront host) <> [Nothing]
