module Zebra
    where

import           MicroKanren
import           MiniKanren


zebra :: Goal
zebra =
    callFresh (\hs ->
        callFresh5 (\h1 h2 h3 h4 h5 ->
            conjAll
                -- note: goals are manually ordered for a faster search
                [ hs <=> list [ h1, h2, h3, h4, h5 ]
                , h1 |> person (str "norwegian")
                , h3 |> drink (str "milk")
                , callFresh2 (\a b -> (hs |> adjacento a b) <.> (a |> person (str "norwegian")) <.> (b |> color (str "blue")))
                , callFresh2 (\a b -> (hs |> adjacento a b) <.> (a |> animal (str "horse")) <.> (b |> smoke (str "kool")))
                , callFresh (\a -> (hs |> membero a) <.> (a |> smoke (str "kool")) <.> (a |> color (str "yellow")))
                , callFresh2 (\a b -> (hs |> aftero a b) <.> (a |> color (str "ivory")) <.> (b |> color (str "green")))
                , callFresh (\a -> (hs |> membero a) <.> (a |> person (str "englishman")) <.> (a |> color (str "red")))
                , callFresh (\a -> (hs |> membero a) <.> (a |> person (str "japanese")) <.> (a |> smoke (str "parliament")))
                , callFresh2 (\a b -> (hs |> adjacento a b) <.> (a |> animal (str "fox")) <.> (b |> smoke (str "chesterfield")))
                , callFresh (\a -> (hs |> membero a) <.> (a |> person (str "ukrainian")) <.> (a |> drink (str "tea")))
                , callFresh (\a -> (hs |> membero a) <.> (a |> person (str "spaniard")) <.> (a |> animal (str "dog")))
                , callFresh (\a -> (hs |> membero a) <.> (a |> smoke (str "oldgold")) <.> (a |> animal (str "snail")))
                , callFresh (\a -> (hs |> membero a) <.> (a |> smoke (str "luckystrike")) <.> (a |> drink (str "orangejuice")))
                , callFresh (\a -> (hs |> membero a) <.> (a |> drink (str "coffee")) <.> (a |> color (str "green")))
                --
                , callFresh (\a -> (hs |> membero a) <.> (a |> drink (str "water")))
                , callFresh (\a -> (hs |> membero a) <.> (a |> animal (str "zebra")))
                ]
        )
    )


z :: Maybe Term
z =
    zebra |> run1


--

person :: Term -> Term -> Goal
person x h =
    callFresh4 (\a b c d -> h <=> (list [ x, a, b, c, d ]))


smoke :: Term -> Term -> Goal
smoke x h =
    callFresh4 (\a b c d -> h <=> (list [ a, x, b, c, d ]))


drink :: Term -> Term -> Goal
drink x h =
    callFresh4 (\a b c d -> h <=> (list [ a, b, x, c, d ]))


animal :: Term -> Term -> Goal
animal x h =
    callFresh4 (\a b c d -> h <=> (list [ a, b, c, x, d ]))


color :: Term -> Term -> Goal
color x h =
    callFresh4 (\a b c d -> h <=> (list [ a, b, c, d, x ]))


--

aftero :: Term -> Term -> Term -> Goal
aftero x y list =
    callFresh4
        (\x2 rest y2 rest2 ->
            conj
                (conj (list <=> Pair x2 rest) (rest <=> Pair y2 rest2))
                (disj
                    (conj (x <=> x2) (y <=> y2))
                    (aftero x y rest)
                )
        )


adjacento :: Term -> Term -> Term -> Goal
adjacento x y list =
    disj
        (aftero x y list)
        (aftero y x list)
