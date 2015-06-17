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
>            | SNative (([SVal] -> Exec SVal) -> Exec SVal)
>            | SLambda [String] SVal
>            | FuncCall [SVal]
>            | SErr String
>            | SNil
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

>getVal :: String -> ProgStack -> SVal
>getVal str (PStack maps) = case L.dropWhile (M.notMember str) maps of
>                            []   -> SErr ("undefined " ++ str)
>                            (m:_) -> m M.! str
>
>putVal :: (String, SVal) -> ProgStack -> ProgStack
>putVal (s,v) (PStack (m:maps)) = PStack ((M.insert s v m):maps)
>
>valueOf :: String -> Exec SVal
>valueOf v = do
>  val <- gets (getVal v)
>  return val

Evaluation state is a State Monad of our Stack.

>type EvalState = State ProgStack

Now, Let us transform this with the Continuation Transformer. That is, let us add continuation handling to the above monad.

>type Exec a = ContT SVal EvalState a
>
>type EvalCont = Exec SVal

How about Eval as SVal -> EvalState SVal

>setValues :: [(s,vs)] -> EvalState ()
>setValues [] = return ()
>setValues ((h,hval):rest) = undefined

>insertValues :: [(String,SVal)] -> Exec ()
>insertValues [] = return ()
>insertValues ((var,val):rest) = do 
>   evaled <- undefined -- eval val
>   modify $ putVal (var, evaled)
>   insertValues rest


A useful fun to popout the stack

>sillyPop :: (SVal -> EvalState SVal) -> SVal -> EvalState SVal
>sillyPop f s = do
>   modify pop
>   f s
>   

>eval :: (SVal -> EvalCont) ->SVal -> ContT SVal EvalState SVal
>eval next (SInt i) = next (SInt i)
>eval next (SStr s) = next ( SStr s)
>eval next (SLet defs v) = do 
>   modify push
>   insertValues defs
>   withContT sillyPop (eval next v)  -- Need to validate sillyPop
>eval next (SVar v) = do
>   val <- valueOf v
>   next val                         -- this seems fine. next will know what to do with SErr
>    


