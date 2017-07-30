module Main (main, spec) where

import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Test.Hspec (Spec, hspec, context, describe, it, shouldBe)

import           MicroKanren


main :: IO ()
main =
    hspec spec


spec :: Spec
spec = do
    describe "MicroKanren" $ do
        context "running a goal" $ do
            it "generates a list of states, where a state = ( substitution, counter )" $
                shouldBe
                    (callFresh (equiv (str "5")) emptyState)
                    [ ( Map.singleton 0 (str "5"), 1 ) ]

            it "can generate multiple states for disjoint goals" $
                shouldBe
                    (conj
                        (callFresh (\a -> a <=> str "7"))
                        (callFresh (\b -> disj (b <=> str "5") (b <=> str "6")))
                        emptyState
                    )
                    [ ( Map.fromList [ ( 0, str "7" ), ( 1, str "5" ) ], 2 )
                    , ( Map.fromList [ ( 0, str "7" ), ( 1, str "6" ) ], 2 )
                    ]

            it "allows recursive goals to generate infinite results" $
                shouldBe
                    (let
                        fives x =
                            disj (x <=> str "5") (fives x)
                     in
                        callFresh fives emptyState
                            |> take 10 |> map (Map.lookup 0 . fst) |> catMaybes
                    )
                    (replicate 10 (str "5"))

            it "interleaves results from disjoint goals" $
                shouldBe
                    (let
                        fives x =
                            disj (x <=> str "5") (fives x)

                        sixes x =
                            disj (x <=> str "6") (sixes x)
                     in
                        callFresh (\x -> disj (fives x) (sixes x)) emptyState
                            |> take 10 |> map (Map.lookup 0 . fst) |> catMaybes
                    )
                    (concat (replicate 5 [ str "5", str "6" ] ))
