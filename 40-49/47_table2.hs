import Control.Monad
import System.IO

data Binary = Zero | One deriving (Show, Eq)

-- show that is padded correctly so that Zero and One aligns when printed
show' :: Binary -> String
show' One = " One"
show' Zero = "Zero"

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

-- Same as above, but outputs using putStrLn
table2p :: (Binary -> Binary -> Binary) -> IO ()
table2p f = do
    mapM_ (\(x, y, z) -> putStrLn $ (show' x) ++ " " ++ (show' y) ++ " " ++ (show' z)) $ table2 f
