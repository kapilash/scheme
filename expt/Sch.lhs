>module Main where
>
>import Control.Monad.Cont
>import Control.Monad.State
>import qualified Data.Map as M
>import qualified Data.List as L

A simple interpreter with variables, ints, strings, native functions, lets and functions

>data SVal = SInt Int
>            | SStr String
>            | SLet [(String, SVal)] SVal
>            | SVar String
>            | SFuncApply [String] SVal
>            | SNative (([SVal] -> Exec) -> Exec)
>            | SLambda [String] SVal
>            | SErr String
>           

Let's say, we store the values of variables in a Map. Every time, we enter a new scope, we will have to create a new Map on top
of the old map. So our state is going to be a List of Maps.

>newtype ProgStack = PStack [M.Map String SVal]
>
>push :: ProgStack -> ProgStack
>push (PStack m) = PStack (M.empty : m)
>
>pop :: ProgStack -> ProgStack
>pop (PStack (m:[])) = PStack [m]
>pop (PStack r)      = PStack (tail r)

So every time, we enter a new scope, we push the stack. And every time, we leave the scope, we pop a stack.
Getting the value of a variable is to get it from the top.

>getVal :: ProgStack -> String -> Maybe SVal
>getVal (PStack maps) str = case L.dropWhile (M.notMember str) maps of
>                            []   -> Nothing
>                            (m:_) -> M.lookup str m

Evaluation state is a State Monad of our Stack.

>type EvalState = State ProgStack

Now, Let us transform this with the Continuation Transformer. That is, let us add continuation handling to the above monad.

>type Exec = ContT SVal EvalState SVal
> 

Evaluation happens with type SVal -> Exec -> Exec

