----------------------------------------------------------------------
-- An interpreter for an imperative programming language with integers
----------------------------------------------------------------------
-------------------- DECLARATIONS --------------------
data Expr = Val Int | Var Name | App Op Expr Expr

type Name = Char

data Op = Add | Sub | Mul | Div
  deriving (Eq, Show)

-- 0 represents false, any other int represents true
-- If = if then else
data Prog
  = Assign Name Expr
  | If Expr Prog Prog
  | While Expr Prog
  | Seqn [Prog]

type Stack = [Int]

type Mem = [(Name, Int)]

type Code = [Inst]

-- PUSH: push to top of stack
-- PUSHV: push value of variable to top of stack
-- POP: pop top of stack and assign it to variable
-- DO: remove top 2 values from stack, operate on it, push result to stack
-- JUMP: Jump to label
-- JUMPZ: Conditional jump, pops top of stack, if value is 0, then jump, else don't
-- LABEL: Int labelling a place in code
data Inst
  = PUSH Int
  | PUSHV Name
  | POP Name
  | DO Op
  | JUMP Label
  | JUMPZ Label
  | LABEL Label
  deriving (Eq, Show)

type Label = Int

facProg :: Int -> Prog
facProg n =
  Seqn
    [ Assign 'A' (Val 1),
      Assign 'B' (Val n),
      While
        (Var 'B')
        ( Seqn
            [ Assign 'A' (App Mul (Var 'A') (Var 'B')),
              Assign 'B' (App Sub (Var 'B') (Val 1))
            ]
        )
    ]

fac10 :: Prog
fac10 = facProg 10

ifProg :: Int -> Prog
ifProg n = Seqn [If (Val n) (Assign 'A' (Val 10)) (Assign 'A' (Val (-5)))]

-- comp (facProg 10) should compile to this
compFac10 :: Code
compFac10 =
  [ PUSH 1,
    POP 'A',
    PUSH 10,
    POP 'B',
    LABEL 0,
    PUSHV 'B',
    JUMPZ 1,
    PUSHV 'A',
    PUSHV 'B',
    DO Mul,
    POP 'A',
    PUSHV 'B',
    PUSH 1,
    DO Sub,
    POP 'B',
    JUMP 0,
    LABEL 1
  ]

newtype ST a = S (Label -> (a, Label))

-- app :: ST a -> ST a
app :: ST a -> Label -> (a, Label)
app (S st) = st

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap f (S x) = S (\s -> let (x', s') = x s in (f x', s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\y -> (x, y))

  -- x: State -> (a->b, State)
  -- sta: S(State -> (a, State))
  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  -- ST b = State -> (b, State)
  (<*>) (S x) sta = S (\s -> let (x', s') = x s in app (fmap x' sta) s')

instance Monad ST where
  -- return :: a -> ST a
  return x = (S (\s -> (x, s)))

  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  (>>=) st f =
    S
      ( \s ->
          let (x, s') = app st s
           in app (f x) s'
      )

-------------------- COMPILER --------------------
comp :: Prog -> Code
comp prog = fst (app (compBeforeLabel prog) 0)

compBeforeLabel :: Prog -> ST Code
compBeforeLabel (Assign name expr) = return (exprToCode expr ++ [POP name])
compBeforeLabel (If expr x y) = do
  x' <- compBeforeLabel x
  y' <- compBeforeLabel y
  S (\s -> (exprToCode expr ++ [JUMPZ s] ++ x' ++ [JUMP (s + 1)] ++ [LABEL s] ++ y' ++ [LABEL (s + 1)], s + 2))
compBeforeLabel (While expr x) = do
  x' <- compBeforeLabel x
  S (\s -> ([LABEL s] ++ exprToCode expr ++ [JUMPZ (s + 1)] ++ x' ++ [JUMP s, LABEL (s + 1)], s + 2))
compBeforeLabel (Seqn []) = return []
compBeforeLabel (Seqn (current : rest)) = do
  current' <- compBeforeLabel current
  rest' <- compBeforeLabel (Seqn rest)
  return (current' ++ rest')

exprToCode :: Expr -> Code
exprToCode (Val x) = [PUSH x]
exprToCode (Var x) = [PUSHV x]
exprToCode (App op x y) = exprToCode x ++ exprToCode y ++ opToCode op

opToCode :: Op -> Code
opToCode op = [DO op]

-------------------- INTERPRETER --------------------
exec :: Code -> Mem
exec code = execStack code [] code []

-- First Mem is working memory
-- First Code is original code
execStack :: Code -> Stack -> Code -> Mem -> Mem
execStack code stack (current : rest) mem = case current of
  PUSH val -> execStack code (val : stack) rest mem
  PUSHV name -> let val = findMem mem name in execStack code (val : stack) rest mem
  POP name -> let (stack', mem') = pop stack mem name in execStack code stack' rest mem'
  DO op -> let stack' = doOp stack op in execStack code stack' rest mem
  JUMP label -> execStack code stack (jumpToLabel code label) mem
  JUMPZ label -> let (val : stack') = stack in if val == 0 then execStack code stack' (jumpToLabel code label) mem else execStack code stack' rest mem
  LABEL label -> execStack code stack rest mem
execStack code stack [] mem = mem

findMem :: Mem -> Name -> Int
findMem mem name = head [i | (n, i) <- mem, n == name]

-- Commented out code in pop and doOp for completeness, can uncomment to remove warnings
-- But it's better to keep them commented out, so that the program will throw an error if something unexpected happens
pop :: Stack -> Mem -> Name -> (Stack, Mem)
-- pop [] mem name = ([], mem)
pop (val : stack) mem name = (stack, [(n, i) | (n, i) <- mem, n /= name] ++ [(name, val)])

doOp :: Stack -> Op -> Stack
-- doOp [] op = []
-- doOp [x] op = [x]
doOp (val1 : val2 : rest) op = case op of
  Add -> val2 + val1 : rest
  Sub -> val2 - val1 : rest
  Mul -> val2 * val1 : rest
  Div -> val2 `div` val1 : rest

-- Returns the code starting from the label
jumpToLabel :: Code -> Label -> Code
jumpToLabel code label = [inst | (ln, inst) <- numberedLinesCode, ln > labelLineNumber]
  where
    numberedLinesCode = zip (take (length code) [0 ..]) code
    labelLineNumber = head [ln | (ln, inst) <- numberedLinesCode, inst == LABEL label]