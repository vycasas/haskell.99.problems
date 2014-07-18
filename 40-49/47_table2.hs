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

tableInput01 :: [(Binary, Binary)]
tableInput01 = [(Zero, Zero), (Zero, One), (One, Zero), (One, One)]

table2 :: (Binary -> Binary -> Binary) -> [(Binary, Binary, Binary)]
table2 f = [(x, y, f x y) | (x, y) <- tableInput01]
