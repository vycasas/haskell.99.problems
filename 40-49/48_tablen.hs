import Control.Monad

data Binary = Zero | One deriving (Show, Eq)

infixl 9 `not'`
infixl 8 `and'`
infixl 7 `xor'`
infixl 6 `or'`

eq' :: Binary -> Binary -> Binary
eq' x y = if x /= y then Zero else One

not' :: Binary -> Binary
not' Zero = One
not' One = Zero

and' :: Binary -> Binary -> Binary
and' Zero _ = Zero
and' _ x = x

or' :: Binary -> Binary -> Binary
or' One _ = One
or' _ x = x

nand' :: Binary -> Binary -> Binary
nand' x y = not' $ and' x y

nor' :: Binary -> Binary -> Binary
nor' x y = not' $ or' x y

xor' :: Binary -> Binary -> Binary
xor' x y = not' $ eq' x y

impl' :: Binary -> Binary -> Binary
impl' x y = if y == Zero then not' x else One

tablen :: Int -> ([Binary] -> Binary) -> [[Binary]]
tablen n f = map (\i -> i ++ [f i]) $ replicateM n [One, Zero]
