{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE FlexibleInstances   #-}

import Data.Proxy
import GHC.TypeNats
import Data.Typeable
import Data.List as L

class DivSupported a where
  divOp :: a -> a -> a
  
instance DivSupported Int     where divOp = div
instance DivSupported Integer where divOp = div
instance DivSupported Double  where divOp = (/)
instance DivSupported Float   where divOp = (/)   

type R = Int
type C = Int
data Matrix (r :: Nat) (c :: Nat) a = Matrix R C [[a]] deriving Show

instance (Num a ) => Semigroup ( Matrix (n :: Nat) (n :: Nat) a) where 
    (<>) = mul
 
instance (Num a ) => Monoid  ( Matrix (n :: Nat) (n :: Nat) a) where
    mempty  =  [[]] 

--     
idMatrix :: (Num a) => Int -> Matrix a 
idMatrix n = Matrix n n (identity n) 

identity :: (Num a ) => Int -> [[a]]
identity n | n <= 0 = [[]]
           | otherwise = take n (fmap (take n) rc) where 
                           rc = (1 : repeat 0) : fmap (0 :) rc
 


instance Functor ( Matrix (n :: Nat) (m :: Nat) ) where
    fmap f (Matrix r c xs ) = Matrix r c ((fmap . fmap) f xs)


-- You can multiply two matrices if, and only if,
-- the number of columns in the first matrix 
-- equals the number of rows in the second matrix. 
-- Otherwise, the product of two matrices is undefined.

mul :: Num a => Matrix n m a -> Matrix m p a  -> Matrix n p a
mul (Matrix r c xs) (Matrix r' c' ys ) = Matrix r c' $ with xs $ \rowi -> 
                                         with ys' $ \coli ->
                                          sum $ zipWith (*) rowi coli
                                        where ys'= L.transpose ys

with ::  [a] -> (a -> b) -> [b] 
with = flip fmap
     
mul' :: Num a => Matrix n m a -> Matrix m p a  -> Matrix n p a
mul' (Matrix r c xs) (Matrix r' c' ys ) = Matrix r c' $ L.transpose . map (applyRow xs) . L.transpose $ ys 
  
applyRow :: Num a => [[a]] -> [a] -> [a]
applyRow [ys]     xs = [sum $ zipWith (*) xs ys]
applyRow (ys:yss) xs = sum (zipWith (*) xs ys) : applyRow yss xs


scalarMul :: (Num a) => a -> Matrix n m a -> Matrix n m a 
scalarMul x  = fmap (* x)


transposeM ::  Matrix n m a -> Matrix m n a 
transposeM (Matrix r c xs) = Matrix c r (L.transpose xs)

add :: Num a => Matrix n m a -> Matrix n m a  -> Matrix n m a
add (Matrix r c xs) (Matrix _ _   ys ) = Matrix r c $ zipWith (zipWith (+)) xs  ys

sub :: Num a => Matrix n m a -> Matrix n m a  -> Matrix n m a
sub (Matrix r c xs) (Matrix _ _ ys ) = Matrix r c $ zipWith (zipWith (-)) xs ys
                                                                                       
sqOp :: Num a => (a -> a -> a) -> Matrix n m a -> Matrix n m a -> Matrix n m a   
sqOp f (Matrix r c xs) (Matrix _ _ ys ) = Matrix r c $ zipWith (zipWith f) xs ys                             

add' :: Num a => Matrix n m a -> Matrix n m a  -> Matrix n m a
add' = sqOp (+)

sub' :: Num a => Matrix n m a -> Matrix n m a  -> Matrix n m a
sub' = sqOp (-)

divEls :: (DivSupported a, Num a) => Matrix n m a -> Matrix n m a  -> Matrix n m a
divEls = sqOp divOp


mu :: Matrix 3 3 Int
mu = Matrix 3 3 [[1,2,2],[1,1,2], [4,5,6]]

mv :: Matrix 4 2 Int
mv = Matrix 4 2  [[1,2],[3,4], [1,1], [4,4]]

ma :: Matrix 5 3 Double
ma = Matrix 5 3 [[90,80,40],
                [90,60,80],
                [60,50,70],
                [30,40,70],
                [30,20,90]]

mOnes :: Matrix 5 5 Double
mOnes = Matrix 5 5 [[1,1,1,1,1],
                    [1,1,1,1,1],
                    [1,1,1,1,1],
                    [1,1,1,1,1],
                    [1,1,1,1,1]]

devM = ma `sub` ( 0.2 `scalarMul` (mOnes `mul` ma))
covM = transposeM devM `mul` devM


           
            