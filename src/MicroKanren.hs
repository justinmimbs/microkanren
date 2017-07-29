module MicroKanren
    where

import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (maybe)


type Var = Int


data Term
    = Var Var
    | Atom String
    | Pair Term Term
    | Unit
    deriving (Eq)


type Subst =
    Map Int Term


walk :: Subst -> Term -> Term
walk subst term =
    case term of
        Var x ->
            Map.lookup x subst |> maybe term (walk subst)
        _ ->
            term


extend :: Var -> Term -> Subst -> Maybe Subst
extend x term subst =
    if occurs subst x term then
        Nothing
    else
        Just $ Map.insert x term subst


-- Given this Subst, does Var x occur in Term?

occurs :: Subst -> Var -> Term -> Bool
occurs subst x term =
    case walk subst term of
        Var y ->
            x == y

        Pair a b ->
            occurs subst x a || occurs subst x b

        _ ->
            False


unify :: Term -> Term -> Subst -> Maybe Subst
unify a b subst =
    case ( walk subst a, walk subst b ) of
        ( Atom x, Atom y ) ->
            if x == y then
                Just subst
            else
                Nothing

        ( Var x, Var y ) ->
            if x == y then
                Just subst
            else
                subst |> extend x (Var y)

        ( Var x, term ) ->
            subst |> extend x term

        ( term, Var y ) ->
            subst |> extend y term

        ( Pair a1 a2, Pair b1 b2 ) ->
            subst |> unify a1 b1 >>= unify a2 b2

        ( Unit, Unit ) ->
            Just subst

        _ ->
            -- empty and non-empty list
            Nothing


-- microKanren primitives

type State =
    ( Subst, Int )


type Goal =
    (State -> [ State ])


equiv :: Term -> Term -> Goal
equiv a b =
    \( subst, n ) ->
        unify a b subst |> maybe [] (\subst' -> [ ( subst', n ) ])


conj :: Goal -> Goal -> Goal
conj g1 g2 =
    \state ->
        g1 state >>= g2


disj :: Goal -> Goal -> Goal
disj g1 g2 =
    \state ->
        interleave (g1 state) (g2 state)


callFresh :: (Term -> Goal) -> Goal
callFresh f =
    \( subst, n ) ->
        f (Var n) ( subst, n + 1 )


-- Search strategy depends on the structure used to collect states.
-- Structure must be a MonadPlus, as it needs (bind, return, mzero, mplus).
-- Here we use a lazy list and interleave rather than append two disjoint lists
-- (i.e. interleave is our mplus).

interleave :: [ a ] -> [ a ] -> [ a ]
interleave a b =
    case ( a, b ) of
        ( [], y ) ->
            y

        ( x : xs, y ) ->
            x : interleave y xs


-- helpers

emptyState :: State
emptyState =
    ( Map.empty, 0 )


infixl 4 <=>
(<=>) :: Term -> Term -> Goal
(<=>) =
    equiv


infixl 3 <.>
(<.>) :: Goal -> Goal -> Goal
(<.>) =
    conj


infixl 2 <+>
(<+>) :: Goal -> Goal -> Goal
(<+>) =
    disj


-- expressions

instance Show Term where
    show term =
        case term of
            Var n ->
                "_." ++ show n

            Atom s ->
                "'" ++ s ++ "'"

            Pair _ _ ->
                "[ " ++ (term |> toList |> map show |> intercalate ", ") ++ " ]"

            Unit ->
                "[]"


toList :: Term -> [ Term ]
toList term =
    case term of
        Pair a b@(Pair _ _) ->
            a : toList b

        Pair a Unit ->
            [ a ]

        Pair a b ->
            [ a, b ]

        x ->
            [ x ]


list :: [ Term ] -> Term
list =
    foldr Pair Unit


str :: String -> Term
str =
    Atom


--

(|>) :: a -> (a -> b) -> b
(|>) =
    flip ($)
