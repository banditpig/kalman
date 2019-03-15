{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE FlexibleInstances   #-}


import Data.Proxy
import GHC.TypeNats
import Data.Typeable
import Data.List as L

newtype Matrix (r :: Nat) (c :: Nat) a = Matrix [[a]] deriving Show

instance (Num a ) => Semigroup ( Matrix (n :: Nat) (n :: Nat) a) where 
    (<>) = mul

instance (Num a ) => Monoid  ( Matrix (n :: Nat) (n :: Nat) a) where
    mempty = Matrix [[]]

instance Functor ( Matrix (n :: Nat) (m :: Nat) ) where
    fmap f (Matrix xs ) = Matrix ((fmap . fmap) f xs)


-- You can multiply two matrices if, and only if,
-- the number of columns in the first matrix 
-- equals the number of rows in the second matrix. 
-- Otherwise, the product of two matrices is undefined.

mul :: Num a => Matrix n m a -> Matrix m p a  -> Matrix n p a
mul (Matrix xs) (Matrix ys ) = Matrix $ with xs $ \rowi -> 
                                         with ys' $ \coli ->
                                          sum $ zipWith (*) rowi coli
                                        where ys'= L.transpose ys

with ::  [a] -> (a -> b) -> [b]
with = flip fmap
     
mul' :: Num a => Matrix n m a -> Matrix m p a  -> Matrix n p a
mul' (Matrix xs) (Matrix ys ) = Matrix $ L.transpose . map (applyRow xs) . L.transpose $ ys 
  
applyRow :: Num a => [[a]] -> [a] -> [a]
applyRow [ys]     xs = [sum $ zipWith (*) xs ys]
applyRow (ys:yss) xs = sum (zipWith (*) xs ys) : applyRow yss xs


scalarMul :: (Num a) => a -> Matrix n m a -> Matrix n m a 
scalarMul x  = fmap (* x)

transpose ::  Matrix n m a -> Matrix m n a 
transpose (Matrix xs) = Matrix (L.transpose xs)

add :: Num a => Matrix n m a -> Matrix n m a  -> Matrix n m a
add (Matrix xs) (Matrix ys ) = Matrix $ zipWith (zipWith (+)) xs ys

sub :: Num a => Matrix n m a -> Matrix n m a  -> Matrix n m a
sub (Matrix xs) (Matrix ys ) = Matrix $ zipWith (zipWith (-)) xs ys
                                                                                       
sqOp :: Num a => (a -> a -> a) -> Matrix n m a -> Matrix n m a -> Matrix n m a   
sqOp f (Matrix xs) (Matrix ys ) = Matrix $ zipWith (zipWith f) xs ys                             

add' :: Num a => Matrix n m a -> Matrix n m a  -> Matrix n m a
add' = sqOp (+)

sub' :: Num a => Matrix n m a -> Matrix n m a  -> Matrix n m a
sub' = sqOp (-)

mu :: Matrix 3 3 Integer
mu = Matrix u

mv :: Matrix 3 3 Integer
mv = Matrix  v

u = [[1,2,2],[1,1,2], [4,5,6]]
v = [[1,2,1],[3,4,1], [1,1,1]]
-- u * v = [[9,12,5],[6,8,4],[25,34,15]]
-- λ-> transpose v
-- [[1,3,1],[2,4,1],[1,1,1]]
-- fmap (myF [(+1),:r(+3)])  [[4,6],[10,11]]
--  = [14,25]
