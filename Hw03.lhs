Homework 3.0: The "While" programming language
Due 2017-02-19

> {-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

> {-# OPTIONS_GHC -W #-}
> module Hw03 where
>
> import qualified Data.Map as Map
> import Data.Map (Map)
> import qualified Data.Set as Set
> import Data.Set (Set)

Throughout this homework, we'll be experimenting with our first
interpreter for what really is a programming language. We'll need two
concepts throughout: variable names (which will just be strings) and
stores (a/k/a heaps, where we keep the contents of the variables of
our language). All of our variables will store integers.

> type VarName = String
>
> type Store = Map VarName Int

<h3>Problem 1: Interpreting While</h3>

We'll define an interpreter for a language that goes beyond the simple
WhileNZ language we saw in class.

> data AExp =
>     Var VarName
>   | Num Int
>   | Plus AExp AExp
>   | Times AExp AExp
>   | Neg AExp
>   deriving (Show, Eq, Ord)

Write an interpreter for these arithmetic expressions. When evaluating
variables, you should return 0 if they're not in the store (such
variables are called *unbound* or *undefined*).

> unJustify :: Num a => Maybe a -> a
> unJustify (Just a) = a
> unJustify (Nothing) = 0
>
> evalA :: Store -> AExp -> Int
> evalA st (Var name) = unJustify (Map.lookup name st)
> evalA _ (Num n) = n
> evalA st (Plus exp1 exp2) = (evalA st exp1) + (evalA st exp2)
> evalA st (Times exp1 exp2) = (evalA st exp1) * (evalA st exp2)
> evalA st (Neg exp) = negate (evalA st exp)



We can define boolean expressions similarly. Rather than concretely
specifying which arithmetic expressions they're defined over, we just
take in a parameter.

> data BExp a =
>     Bool Bool
>   | Equal a a
>   | Lt a a
>   | Not (BExp a)
>   | Or (BExp a) (BExp a)
>   | And (BExp a) (BExp a)
>   deriving (Show, Eq, Ord)

Write an interpreter for boolean expressions over our prior arithmetic expressions.

> evalB :: Store -> BExp AExp -> Bool


> evalB _ (Bool b) = b
> evalB st (Equal exp1 exp2) = (evalA st exp1) == (evalA st exp2)
> evalB st (Lt exp1 exp2) = (evalA st exp1) < (evalA st exp2)
> evalB st (Not bexp) = not (evalB st bexp)
> evalB st (Or bexp1 bexp2) = (evalB st bexp1) || (evalB st bexp2)
> evalB st (And bexp1 bexp2) = (evalB st bexp1) && (evalB st bexp2)


Finally, we'll define a simple programming language. Its abstract
syntax tree (AST) takes two type parameters: one identifying the
arithmetic expressions we'll use, one identifying the boolean
expressions we'll use.

> data Stmt a b =
>     Skip
>   | Assign VarName a
>   | Seq (Stmt a b) (Stmt a b)
>   | If (b a) (Stmt a b) (Stmt a b)
>   | While (b a) (Stmt a b)
>   deriving (Show, Eq, Ord)

Write an interpreter for this language.

> eval :: Store -> Stmt AExp BExp -> Store


> eval st Skip = st
> eval st (Assign name val) = Map.insert name (evalA st val) st
> eval st (Seq exp1 exp2) = eval (eval st exp1) exp2
> eval st (If exp exp1 exp2) = if (evalB st exp)
>                                      then (eval st exp1)
>                                      else (eval st exp2)
> eval st (While exp exp1) = if (evalB st exp)
>                                   then (eval (eval st exp1) (While exp exp1))
>                                   else st


<h3>Problem 2: While, with failures</h3>

Here's a new definition for arithmetic expressions, adding division.

> data AExp' =
>     Var' VarName
>   | Num' Int
>   | Plus' AExp' AExp'
>   | Times' AExp' AExp'
>   | Neg' AExp'
>   | Div' AExp' AExp'
>   deriving (Show, Eq)

Note that division is an operation that can fail. Write another
interpreter (defining whatever functions you need). Do not use the
`error` function.

In the interpreter above, variables not in the store were given the
default value of 0. In this version of the interpreter, make it so
that unbound variables in arithmetic expressions cause errors, just
like division. Here are the two errors that can happen:

> data Error = NoSuchVariable VarName | DivideByZero AExp'

When you encounter an unbound variable, the error has a slot for
identifying the culpable variable. Similarly, when you try to divide
by zero, you should record the entire division expression responsible,
not just the divisor. (In a more serious AST, we might keep track of
the source file and line number each expression came from, in order to
better indicate the source of the problem.)


>
> instance Show Error where
>    show (NoSuchVariable a) = "NoSuchVariable " ++ (show a)
>    show (DivideByZero a) = "DivideByZero " ++ (show a)
>
> unEitherify :: VarName -> Store -> Either Error Int
> unEitherify name st = if look == Nothing
>                       then (Left (NoSuchVariable name))
>                       else Right (unJustify look)
>                           where look = (Map.lookup name st)
>
> unRightify :: Either Error a -> a
> unRightify (Right a) = a
>
> isError :: Either Error Int -> Bool
> isError (Left _) = True
> isError (Right _) = False
>
> evalA' :: Store -> AExp' -> Either Error Int
> evalA' st (Var' name) = unEitherify name st
> evalA' _ (Num' n) = Right n
> evalA' st (Plus' exp1 exp2) = if (isError e1)
>                               then e1
>                               else if (isError e2)
>                                    then e2
>                                    else Right ((unRightify e1) + (unRightify e2))
>                                        where e1 = (evalA' st exp1)
>                                              e2 = (evalA' st exp2)
> evalA' st (Times' exp1 exp2) = if (isError e1)
>                               then e1
>                               else if (isError e2)
>                                    then e2
>                                    else Right ((unRightify e1) * (unRightify e2))
>                                        where e1 = (evalA' st exp1)
>                                              e2 = (evalA' st exp2)
> evalA' st (Neg' exp) = if (isError e)
>                        then e
>                        else Right (negate (unRightify e))
>                            where e = (evalA' st exp)
> evalA' st (Div' exp1 exp2) = if (isError e1)
>                               then e1
>                               else if (isError e2)
>                                    then e2
>                                    else if (unRightify e2 == 0)
>                                         then (Left (DivideByZero exp2))
>                                         else Right ((unRightify e1) `quot` (unRightify e2))
>                                             where e1 = (evalA' st exp1)
>                                                   e2 = (evalA' st exp2)
>
> evalB' :: Store -> BExp AExp' -> Either Error Bool
> evalB' _ (Bool b) = Right b
> evalB' st (Equal exp1 exp2) = case ((evalA' st exp1), (evalA' st exp2)) of
>                                    ((Right e1), (Right e2)) -> (Right (e1 == e2))
>                                    ((Left e1), _) -> (Left e1)
>                                    (_, (Left e2)) -> (Left e2)
> evalB' st (Lt exp1 exp2) = case ((evalA' st exp1), (evalA' st exp2)) of
>                                    ((Right e1), (Right e2)) -> (Right (e1 < e2))
>                                    ((Left e1), _) -> (Left e1)
>                                    (_, (Left e2)) -> (Left e2)
> evalB' st (Not exp) = case (evalB' st exp) of
>                            (Right e1) -> (Right (not e1))
>                            (Left e1) -> (Left e1)
> evalB' st (Or exp1 exp2) = case ((evalB' st exp1), (evalB' st exp2)) of
>                                    ((Right e1), (Right e2)) -> (Right (e1 || e2))
>                                    ((Left e1), _) -> (Left e1)
>                                    (_, (Left e2)) -> (Left e2)
> evalB' st (And exp1 exp2) = case ((evalB' st exp1), (evalB' st exp2)) of
>                                    ((Right e1), (Right e2)) -> (Right (e1 && e2))
>                                    ((Left e1), _) -> (Left e1)
>                                    (_, (Left e2)) -> (Left e2)
>

> eval' :: Store -> Stmt AExp' BExp -> Either Error Store
> eval' st Skip = Right st
> eval' st (Assign name val) = case evalA' st val of
>                                  (Left e1) -> Left e1
>                                  (Right e1) -> Right (Map.insert name {} st)
> eval' st (Seq exp1 exp2) = case eval' st exp1 of
>                                 (Left e1) -> Left e1
>                                 (Right e1) -> case eval' e1 exp2 of
>                                               (Left e2) -> Left e2
>                                               (Right e2) -> Right e2
> eval' st (If exp exp1 exp2) = case evalB' st exp of
>                                   (Left e1) -> Left e1
>                                   (Right True) -> eval' st exp1
>                                   (Right False) -> eval' st exp2
> eval' st (While exp exp1) = case evalB' st exp of
>                                   (Left e1) -> Left e1
>                                   (Right False) -> eval' st Skip
>                                   (Right True) -> case eval' st exp1 of
>                                                       (Left e2) -> Left e2
>                                                       (Right e2) -> eval' e2 (While exp exp1)

 eval' :: Store -> Stmt AExp' BExp -> Either Error Store
 eval' st Skip = Right st
 eval' st (Assign name val) = if (isError exp)
                              then exp
                              else (Right (Map.insert name exp st))
                                  where exp = (evalA' st val)
 eval' st (Seq exp1 exp2) = if (isError e1)
                            then e1
                            else if (isError e2)
                               then e2
                                 else Right st
                                     where e1 = (eval' st exp1)
                                           e2 = (eval' e1 exp2)
 eval' st (If exp exp1 exp2) = if (evalB' st exp)
                                    then (eval' st exp1)
                                      else (eval' st exp2)
 eval' st (While exp exp1) = if (evalB' st exp)
                                   then (eval' (eval' st exp1) (While exp exp1))
                                   else (Right st)


 data Stmt a b =
     Skip
   | Assign VarName a
   | Seq (Stmt a b) (Stmt a b)
   | If (b a) (Stmt a b) (Stmt a b)
   | While (b a) (Stmt a b)


<h3>Problem 3: Static analysis</h3>

Can we determine in advance whether a given program will try to use an
unbound variable if they're run in an initially empty store? This kind
of analysis is called "def/use analysis", and it's a common early step
in compilation. More generally, this is "static analysis", becuase we
inspect our programs before we run them. (*Static* and *dynamic* are
opposites; you can read them as "at compile time" and "at run time",
respectively.)

In some programs, it's easy:

> unboundY = Assign "x" (Var' "y")

The program `unboundY` will always fail in an unbound store. It can be
more ambiguous, though, as in:

> ambiguous b = Seq (If b (Assign "y" (Num' 0)) Skip) unboundY

Depending on what we know about `b`, we may or may not have a problem
on our hands. Absent any information about `b`, it *could* happen that
`ambiguous b` will try to read from `y` before it's defined.

In PL, we tend to stay on the safe side: the general philosophy is
that's better to have a false positive (saying a program is unsafe
when it's actually fine) than to have a false negative (saying a
program is safe when it isn't!). That is, PL prioritizes *soundness*
(if we say X, then X is really true) over *completeness* (if X is
really true, then we say X). As a side note, observe that it's easy to
write a trivial sound analysis (everything's unsafe, please wear a
helmet) as it is a trivial complete analysis (everything's safe, take
it easy).

To get started, write functions that collect all of the variables that
appear in given arithmetic and boolean expressions.

> varsA :: AExp' -> Set VarName


> varsA aexp = case aexp of
>                  (Var' x) -> Set.fromList [x]
>                  (Num' _) -> Set.empty
>                  (Plus' x1 x2) -> Set.union (varsA x1) (varsA x2)
>                  (Times' x1 x2) -> Set.union (varsA x1) (varsA x2)
>                  (Div' x1 x2) -> Set.union (varsA x1) (varsA x2)
>                  (Neg' x) -> varsA x


For example, `varsA (Times (Plus' (Var' "x") (Var' "y")) (Num 3)) ==
Set.fromList ["x", "y"]`.

> varsB :: BExp AExp' -> Set VarName


> varsB bexp = case bexp of
>                  (Bool bool) -> Set.empty
>                  (Equal x1 x2) -> Set.union (varsA x1) (varsA x2)
>                  (Lt x1 x2) -> Set.union (varsA x1) (varsA x2)
>                  (Not x1) -> varsB x1
>                  (Or bexp1 bexp2) -> Set.union (varsB bexp1) (varsB bexp2)
>                  (And bexp1 bexp2) -> Set.union (varsB bexp1) (varsB bexp2)


For example, `varsB (Or (Not (Equal (Var' "foo") (Var' "bar"))) (Bool
True)) == Set.fromList ["bar", "foo"]`.

Now let's write our analysis: we'll take in a set of variables that we
know to be defined, a statement in our language, and we'll return a
pair of sets: the set of variables that have been defined and the set
of variables that have been used *but not defined*.

> useBeforeDef :: Set VarName -> Stmt AExp' BExp -> (Set VarName, Set VarName)
> useBeforeDef defs Skip = (defs, Set.empty)
> useBeforeDef defs (Assign x a) = (Set.insert x defs, varsA a `Set.difference` defs)


What should the other cases do? Remember, you have to be *sound*: the
variable in the first part of the pair (the defined variables) must
*always* be defined; if it's at all possible for a variable to
undefined, it must not appear in the first part. Similarly, if it's at
all possible for variable to *ever* be used before it's defined, it
must appear in the second part.

With these guiding principles, what should we do for `Seq s1 s2`?
Everything `s1` defines will be defined for `s2`. The final set of
definitions will also include what `s2` defines. What about the the
variables that are used before they're defined? If `x` is used in `s1`
before it's defined, it doesn't matter if it's later defined in
`s2`---it's too late.

What about `If b s1 s2`? It's too hard to know anything about the
condition `b`. But if we can be certain that both branches define a
variable, then we can be certain that it'll be defined at the
end. Conversely, if either branch could use a given variable before
it's defined, then that variable could potentially be used before
being defined.

Once you know how `If` and `Seq` works, you should have the general
principle for `While`. Sketch it out on the board!

> useBeforeDef defs (Seq stmt1 stmt2) = case (useBeforeDef defs stmt1) of
>                                           (a,b) -> case (useBeforeDef (Set.union defs a) stmt2) of
>                                                        (c,d) -> (Set.union a c, Set.union b d)
> useBeforeDef defs (If exp stmt1 stmt2) = case (useBeforeDef defs stmt1) of
>                                              (a,b) -> case (useBeforeDef defs stmt2) of
>                                                           (c,d) -> (Set.union defs (Set.intersection a c),(Set.union (Set.union b d) (Set.difference (varsB exp) defs)))
> useBeforeDef defs (While exp stmt) = case (useBeforeDef defs stmt) of
>                                          (a,b) -> (Set.union a defs, Set.union b (Set.difference (varsB exp) defs))



Be very careful testing your function. Strive for soundness.  The
tests below show the results for my `useBeforeDef`---don't feel
obligated to do better, but don't do worse. You can modify or delete
these tests---my grader ignores them.

> testUnbound, testAmbiguous :: Bool
> testUnbound = useBeforeDef Set.empty unboundY ==
>               (Set.singleton "x", Set.singleton "y")
>
> testAmbiguous = useBeforeDef Set.empty (ambiguous (Bool True)) ==
>                 (Set.singleton "x", Set.singleton "y")

<h3>Problem 4: Mission Impossible</h3>

Your final task is to solve the halting problem. We'll start by
writing a function that runs a program a little bit---just one
"step". Then we'll look at the *trace* of steps the program takes. If
we ever end up in a state we've seen before, then the program
diverges. This is a dynamic analysis, since we'll be running our
programs.

First, fill in the step function below.

> type Config = (Store, Stmt AExp BExp)
>
> step :: Config -> Maybe Config
> step (_,Skip) = Nothing
> step (st,Assign x a) = Just (Map.insert x (evalA st a) st,Skip)
> step (st,Seq Skip s2) = Just (st,s2)
> step (st,Seq s1 _) = Just (st, s1)
> step (st,If b s1 s2) = case (evalB st b) of
>                            False -> step ((eval st s2), s2)
>                            True -> step ((eval st s1), s1)
> step (st,While b s) = case (evalB st b) of
>                           True -> Just ((eval st s), (While b s))
>                           False -> Nothing

Given a step function, we can compute a trace, i.e., the possibly
infinite list of `Config`s that the program will step through. Such a
program is safe to write in Haskell because Haskell is *lazy*, i.e.,
it will only compute things on demand.

> trace :: (a -> Maybe a) -> a -> [a]
> trace f v =
>   case f v of
>     Nothing -> [v]
>     Just v' -> v:trace f v'

I may have gotten excited earlier when I said we'd "solve" the halting
problem. We can *try* to solve it, but sometimes we'll have to throw
up our hands and say "Who knows?". To facilitate that, we'll use
*three-valued logic*, which extends the booleans with a notion of
"don't know".

> data TVL = No | Maybe | Yes deriving (Show, Eq, Ord)

Write a function `diverges` that checks for loops in a list of
configurations. (Note that I've written a much more general type.) The
integer paramter should serve as a timeout---a limit as to how far
we're willing to look.

What counts as a loop? Each element in the list will represent a
`Config`, i.e., a pair of a store and a statement currently being
executed. If we ever see the same pair twice, we know the program
diverges because our programs are *deterministic*, i.e., they do the
same thing every time. So your job is to check for duplicate
configurations, i.e., elements that appear more than once in the
loop. A wise choice of data structure here will make your life easier
(and speed up your program).

> diverges :: Ord a => Int -> [a] -> TVL
> diverges limit = divergesAcc limit []
>
> divergesAcc :: Ord a => Int -> [a] -> [a] -> TVL
> divergesAcc _ _ [] = No
> divergesAcc limit acc (x:xs) | limit <= 0 = Maybe
>                              | otherwise  = if (elem x acc) then Yes else (divergesAcc (limit-1) (x:acc) xs)


Write a function `haltsIn` that takes a starting configuration and a
limit and tries to determine whether that configuration ever halts
(within the specified limit, from the empty store).

> haltsIn :: Stmt AExp BExp -> Int -> TVL
> haltsIn s limit = case diverges limit (trace step (Map.empty, s)) of
>                       Yes -> No
>                       No -> Yes
>                       Maybe -> Maybe


Now we have our analysis... let's see what it can do. Write a While
program `loop` that diverges and:

```
loop `haltsIn` 1000 == No
```

> loop :: Stmt AExp BExp
> loop = While (Bool True) (Assign "a" (Num 1))

Write a While program `long` that converges and:

```
long `haltsIn` 1000 == Maybe
long `haltsIn` 5000 == Yes
```

> long :: Stmt AExp BExp
> long = While (Lt (Var "counter") (Num 2500)) (Assign "counter" (Plus (Num 1) (Var "counter")))

Write a While program `tricky` that diverges but for all `n`:

```
tricky `haltsIn` n == Maybe
```

> tricky :: Stmt AExp BExp
> tricky = While (Bool True) (Assign "counter" (Plus (Num 1) (Var "counter")))

Explain why your `haltsIn` gives an imprecise answer.


Do you think you can write a program where `haltsIn` gives a wrong
answer? If so, explain your idea---or write it! If not, explain (or
prove!) why not.

We think it always gives the correct answer. Let;s prove this claim by contradiction.
For `haltsIn` to give a wrong answer, it much either say Yes when it does not halt or
say No when it does halt.
First, let's assume it says Yes when it does not halt. The only time it says Yes is
when the trace file has ended before the limit. Thus, the program has halted so that's
why our function said it halts.
Therefore, there are no false positives.
Now, let's assume it says No when it does halt. The only time it says that it does not
halt is when it sees a state and statement pair that it has seen earlier. Since our
statements are deterministic, this means we have cycled to a previous state and will
thus be stuck in a vicious cycle. Thus, the program does not halt and so we do not
have a false negative.
Since there are no false positives or false negatives, our function `haltsIn` always
returns correct values.
