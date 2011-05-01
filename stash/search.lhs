> module Search where
>
> import Data.List
> import Control.Monad.Omega

We're trying take a set of types and recombine them in every possible way. To
start with, we need a set of base types. These are defined in the data
structure 'Type'.

> data Type = A
>    deriving (Show, Eq)

Now that we have a set of base types, we need to provide a way of representing
functional types. We'll do this with the 'Sig' data type.

> data Sig = Sig {
>     name :: String,
>     sret  :: Type,
>     sargs :: [Type]
> } deriving (Show)

The whole point of this project is to produce a set of functions that match
some goal type. We need to define a way to specify this goal. The goal type is
like the signature type, but it doesn't have a name.

> data Goal = Goal {
>     gret  :: Type,
>     gargs :: [Type]
> } deriving (Show)

Now that we have signatures and goals, we need to have a way of describing all
the possible inputs we want to use to try and produce the goal. We'll call the
set of all these inputs the universe.

> type Universe = [Sig]

We've defined the signatures, the goals, and the shape of the universe, now we
need to define what our output looks like. Our output is a simple abstract
syntax tree that represents a function applied to some arguments. This
function may not have any arguments in which case, it's just an identifier
name. This identifier name is taken from the signature type.

> data Expr = Apply String [Expr]
>
> instance Show Expr where
>     show (Apply s []) = s
>     show (Apply s es) = s ++ "(" ++ (intercalate "," (map show es)) ++ ")"

We can now completely define our inputs and out outputs. Now we just need to
deal with the hard part. To start with, we need a way of looking at a goal and
determining which pieces of the universe could possibly satisfy the goal. This
can easily be discovered by filtering the universe based on the return type of
the goal. Note that a goal's arguments don't have to be part of the final AST
for it to match the goal's return type. Lets define a helper function that
takes a type and returns a new universe that only has signatures with return
types that match the provided type.

> filterverse :: Universe -> Type -> Universe
> filterverse u t = filter (\s -> sret s == t) u

Once we've identified the possible base body functions for our goal, we need a
way to start converting them into the expression trees that will represent our
output, but first, we need a way to expand out any arguments the signatures in
our universe may have.

Depending on the universe, this can be an infinite operation. Consider a
universe made up of the function 'zero' (0) of type Int and the function 'add
one' (represented as '+') of type (Int -> Int). If we intentionally pick the
siplest possible goal, Int, we have an infinite possible set of satisfactory
functions, but as long as the desired goal is a function that evaluates to a
natural number, we will eventually find a satisfactory solution. This is not
necessarily the case if we add one more function to the universe: 'subtract
one' (represented as '-') of type (Int -> Int).  Consider how we would arrive
at a goal wich when evaluated would be equal to two given all possible
combinations of these three functions. The obvious solution is ((0 + 1) + 1).
We can visualize the (simplified) tree in which we can discover this solution:

       0
     /   \
     +   -
    / \ / \
    + - + -

If we traverse the tree always selecting the left branch, then we'll quickly
arrive at our desired solution. Suppose we decided to move right first. It is
clear that a always-right strategy would produce an infinite list of
expression trees of which none would ever satisfy our goal even though the
tree does contain a valid solution. Further inspection of this tree
demonstrates that there are in fact an infinite number of possible expression
trees that evaluate to the value of '2'. How do we adequately traverse this
tree such that we avoid sinking to an infinite depth? Lets begin by defining a
function which takes a universe and a goal as inputs and returns the list of
expression trees that can be generated from the portion of the universe that
matches the goal's return type.

< goal2exprs :: Universe -> Goal -> [Expr]
< goal2exprs u g = map sig2expr $ filterverse u' (gret g)
<   where
<       argsigs = zipWith (\n a -> Sig n a []) names (gargs g)
<       names = map ("__a" ++) postfixes
<       u' = argsigs ++ u

This function assumes the existence of the function sig2expr which
presumptively converts a signature to an expression tree. Lets try and define
that function.

< sig2expr :: Sig -> Expr
< sig2expr (Sig n _ []) = Apply n []
< sig2expr (Sig n _ as) = ???

Oh dear, we've run into a problem. We don't know what to do with these
arguments! But, if we think about this again, we have a mechanism for filling
in this sort of thing. If we convert our signature to a goal, we can start the
process over again. Lets define a function that converts a signature to a
goal.

> sig2goal :: Sig -> Goal
> sig2goal (Sig _ r as) = Goal r as

Now we need to back up a bit and consider our situation. sig2expr is itself
going to return a list of expressions. We need to redefine goal2exprs to
accomodate this change. Changing our map to concatMap and adding the universe
as a parameter to sig2expr should do.

> goal2exprs :: Universe -> Goal -> [Expr]
> goal2exprs u g = diagonal $ map (sig2expr u') $ filterverse u' (gret g) -- !!!
>   where
>       argsigs = zipWith (\n a -> Sig n a []) names (gargs g)
>       names = map (\n -> "__a" ++ (show n)) ([0..] :: [Int])
>       u' = argsigs ++ u

Now that we've modified goal2exprs to play nicely with sig2expr, lets try to
redefine it. We'll need a few helpers first. Lets start by defining a function
that can figure out the valid universe for each of the arguments in the
signature.

> argverses :: Universe -> [Type] -> [Universe]
> argverses u ts = map (filterverse u) ts

argverses gives us a list of universes that correspond to the list of types.
We can use any of the signatures in these universes as stand-ins for the
arguments of the signature. Now, we just need to find a way to combine them in
all possible ways.

To do that, we need something that will take a list of lists and make a new
list of lists where each item from each list shows up with each item from each
other list. Luckily, this is what 'sequence' does.

> combinations :: [[a]] -> [[a]]
> combinations = sequence

> arglists :: Universe -> [Type] -> [[Sig]]
> arglists u ts = combinations (argverses u ts)

==============================================================================
RESUME DOCUMENTATION HERE
- use a version of type2expr without diagonal before doing the right one.
  explain the problem.
- redefine goal2exprs to use diagonal instead of concat
==============================================================================

> type2expr :: Universe -> Type -> [Expr]
> type2expr u t = diagonal $ map (sig2expr u) u'
>   where
>     u' = filterverse u t

> sig2expr :: Universe -> Sig -> [Expr]
> sig2expr _ (Sig n _ []) = [Apply n []]
> sig2expr u (Sig n _ as) = map (Apply n) (combinations as')
>   where
>     as' = map (type2expr u) as

> a0 :: Sig
> a0 = Sig "a0" A []

> a1 :: Sig
> a1 = Sig "a1" A [A]

> a2 :: Sig
> a2 = Sig "a2" A [A, A]

> universe :: Universe
> universe = [a0, a1, a2]

