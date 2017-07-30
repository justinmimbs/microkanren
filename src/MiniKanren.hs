module MiniKanren
    where

import qualified Data.Map as Map
import           Data.Maybe (maybe, listToMaybe)

import           MicroKanren


run1 :: Goal -> Maybe Term
run1 =
    run .> listToMaybe


runN :: Int -> Goal -> [ Term ]
runN n =
    run .> take n


run :: Goal -> [ Term ]
run g =
    g emptyState |> fmap (fst .> reifyVar0)


-- goals

-- identity for conj
succeeds :: Goal
succeeds x =
    [ x ]


-- identity for disj
fails :: Goal
fails _ =
    []


conjAll :: [ Goal ] -> Goal
conjAll =
    foldl conj succeeds


disjAll :: [ Goal ] -> Goal
disjAll =
    foldl disj fails


-- callFreshN

callFresh2 :: (Term -> Term -> Goal) -> Goal
callFresh2 f2 =
    callFresh (\x -> callFresh (\y -> f2 x y))


callFresh3 :: (Term -> Term -> Term -> Goal) -> Goal
callFresh3 f3 =
    callFresh (\x -> callFresh (\y -> callFresh (\z -> f3 x y z)))


callFresh4 :: (Term -> Term -> Term -> Term -> Goal) -> Goal
callFresh4 f4 =
    callFresh (\a -> callFresh (\b -> callFresh (\c -> callFresh (\d -> f4 a b c d))))


callFresh5 :: (Term -> Term -> Term -> Term -> Term -> Goal) -> Goal
callFresh5 f5 =
    callFresh (\a -> callFresh (\b -> callFresh (\c -> callFresh (\d -> callFresh (\e -> f5 a b c d e)))))


-- reification

reifyVar0 :: Subst -> Term
reifyVar0 subst =
    let
        term =
            substitute subst (Var 0)
    in
        term |> (reifySubst term Map.empty |> maybe id substitute)


-- `walk` recurses to the end of a Var chain but doesn't descend into
-- nested terms; `substitute` descends into nested terms.

substitute :: Subst -> Term -> Term
substitute subst term =
    case walk subst term of
        Pair x y ->
            Pair (substitute subst x) (substitute subst y)

        atom ->
            atom


reifySubst :: Term -> Subst -> Maybe Subst
reifySubst term subst =
    case walk subst term of
        Var x ->
            subst |> extend x (Atom ("_." ++ show (Map.size subst))) -- extend with reified name

        Pair x y ->
            subst |> reifySubst x >>= reifySubst y

        _ ->
            Just subst


-- lists

conso :: Term -> Term -> Term -> Goal
conso x y pair =
    pair <=> Pair x y


heado :: Term -> Term -> Goal
heado x list =
    callFresh (\xs -> conso x xs list)


tailo :: Term -> Term -> Goal
tailo xs list =
    callFresh (\x -> conso x xs list)


appendo :: Term -> Term -> Term -> Goal
appendo a b list =
    disj
        (conj (a <=> Unit) (b <=> list))
        (callFresh3
            (\x y rest ->
                conjAll
                    [ a <=> Pair x y
                    , appendo y b rest
                    , list <=> Pair x rest
                    ]
            )
        )


membero :: Term -> Term -> Goal
membero x list =
    callFresh2
        (\y rest ->
            conj
                (list <=> Pair y rest)
                (disj (x <=> y) (membero x rest))
        )


reverso :: Term -> Term -> Goal
reverso =
    reversoHelp Unit


reversoHelp :: Term -> Term -> Term -> Goal
reversoHelp result a b =
    disj
        (conj (a <=> Unit) (b <=> result))
        (callFresh3
            (\x y result2 ->
                conjAll
                    [ a <=> Pair x y
                    , result2 <=> Pair x result
                    , reversoHelp result2 y b
                    ]
            )
        )


-- helpers

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) =
    flip (.)
