{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import qualified Data.List      as L
import           Test.Hspec

import           Crypto.Utreexo
import           Data.HashTree  (Hash (..))


instance Hash [a] a where
    hash = pure


insertManyImplicit :: (Hash h a, Semigroup h) => Utreexo h a -> [a] -> Utreexo h a
insertManyImplicit = L.foldl' insertValueImplicit


insertMany :: (Hash h a, Semigroup h) => Utreexo h a -> [a] -> Utreexo h a
insertMany = L.foldl' insertValue


getValues :: (Hash h a, Ord a) => Utreexo h a -> [a]
getValues = L.sort . fmap (fst . unProof) . getProofs


rangeDec :: Int -> Int -> [Int]
rangeDec x y = reverse $ enumFromTo y x


u0, u1 :: Utreexo [Int] Int
u0 = insertMany emptyUtreexo [1 .. 99]
u1 = insertManyImplicit u0 [100 .. 120]


hs0, hs1 :: [Maybe [Int]]
hs0 = [Just [99], Just [98, 97], Nothing, Nothing, Nothing, Just (rangeDec 96 65), Just (rangeDec 64 1)]
hs1 = [ Nothing
      , Nothing
      , Nothing
      , Just (rangeDec 120 113)
      , Just (rangeDec 112 97)
      , Just (rangeDec 96 65)
      , Just (rangeDec 64 1)
      ]


main :: IO ()
main = hspec $ do

    let ps  = getProofs u0
        p10 = ps !! 89

    describe "contents" $ do
        it "should have 99 elements"                $ count u0     `shouldBe` 99
        it "should have 120 elements"               $ count u1     `shouldBe` 120
        it "should have the correct contents"       $ getValues u0 `shouldBe` [1 .. 99]
        it "should contain '10'"                    $ member u0 p10
        it "should have the correct contents after additions" $
            getValues u1 `shouldBe` [1 .. 99]

    describe "structure" $ do
        it "should have the correct hashes"       $ hashes u0 `shouldBe` hs0
        it "should still have the correct hashes" $ hashes u1 `shouldBe` hs1

    describe "deletion" $ do

        let p1  = last ps
            p99 = head ps

        describe "99 values" $ do
            it "should delete the first element" $ (getValues <$> delete p99 u0) `shouldBe` Just [1 .. 98]
            it "should delete the last element"  $ (getValues <$> delete p1  u0) `shouldBe` Just [2 .. 99]
            it "should delete an intermediate element" $
                (getValues <$> delete p10 u0) `shouldBe` Just ([1 .. 9] <> [11 .. 99])

        let u2       = delete p99' u1
            p99' : _ = getProofs u1

            u3              = delete p61 =<< u2
            Just (p61 : _)  = filter ((== 61) . fst . unProof) . getProofs <$> u2

        describe "120 elements" $ do
            it "should have the correct count"              $ (count <$> u2)     `shouldBe` Just 119
            it "should delete an intermediate element"      $ (getValues <$> u2) `shouldBe` Just [1 .. 98]
            it "should delete another intermediate element" $
                (getValues <$> u3) `shouldBe` Just ([1 .. 60] <> [62 .. 98])
            it "should not contain '61'"                    $ (member <$> u3 <*> pure p61) `shouldBe` Just False
