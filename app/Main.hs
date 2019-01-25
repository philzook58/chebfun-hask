{-# LANGUAGE DeriveFunctor #-}
module Main where

import Lib
import Data.Complex
import Data.Fixed (mod')
import Linear.Vector

data ChebFun a = ChebFun {pow2 :: Int, coefficients :: [a] } deriving (Show, Functor)

-- data SampleFun a = 
-- interpolate :: Vector a => SampleFun a -> (Circ -> a) -- need to use linear inteperpolation
{-
instance Functor ChebFun where
	fmap f (ChebFun i cs) = ChebFun i (fmap f cs)
-}

{-
zipWith' f dx dy (x : xs) (y : ys) = (f x y) : (zipWith' f dx dy xs ys)
zipWith' f dx _ [] ys = map (f dx) ys
zipWith' f _ dy xs [] = map (flip f dy) xs
-}

unionWith' f (x : xs) (y : ys) = (f x y) : (unionWith' f xs ys)
unionWith' f [] ys = ys
unionWith' f xs [] = xs

-- Linear package makes scalar multiplication a piece of being a functor.
-- I'm not sure I can make ChebFun a Functor? Well, it can be for the purposes of scalar multiply.
-- But usually we need to FFT it to real space before applying pointwise stuff

instance Additive ChebFun where
	zero = ChebFun 0 []
	liftI2 f (ChebFun n xs) (ChebFun n' ys) = ChebFun (min n n') (zipWith f xs ys) 
	liftU2 f (ChebFun n xs) (ChebFun n' ys) = ChebFun (max n n') (unionWith' f xs ys)

-- intance Additive ChebFun where 
-- possible conventions: incorrect sized lists are error, everything past nil is 0, everything past last element is a repeat of last constant
-- Last convention allows for efficient representation of constant values. A pure nil is represented as 0. case on (a:[])
-- TrigFun is better name
-- ChebFun b a where ChebFun (b ~ Circ, Num a) => 

-- compose
-- Possible optimization
data Consty f a = NonConsty (f a) | Consty a 

{-
we can lift pointwise functions?
cheblift


almost applicative, except that the object inside needs to be foureir transformable.


instance Applicative ChebFun where
	Num a b c => (a -> b -> c) -> ChebFun a -> ChebFun b -> ChebFun c



-}
-- chebcompose :: ChebFun Circ -> ChebFun Circ -> ChebFun Circ

{-
instance Num a => Num (ChubFun a) where
	(ChebFun n xs) + (ChebFun n' ys) = ChebFun (max n n') (zipWithDefault 0 (+) xs ys) -- not right. We want to add with default 0 is [] and then re-shorten.  also the min needs to be a max.
	(ChebFun n xs) * (ChebFun n' ys) = chebify $ (funcify x) * (funcify y) -- all pointwise stuff tends to be in the real domain
	abs x = chebify $ abs . (funicfy x)




instance Vector a => Vector (ChebFun a) where
	smul s = fmap (smul s) coefficients
	vadd x y = x + y

-}

newtype Circ = Circ Double
circ :: Double -> Circ -- smart constructor
circ x = Circ $ mod' (abs x) (2 * pi)

-- https://ro-che.info/articles/2015-12-04-fft
split :: [a] -> ([a], [a])
split = foldr f ([], [])
  where
    f a (r1, r2) = (a : r2, r1)

-- interleave (The opposite of split). Not super sure I got the order right.

-- liquid haskell: interleave split = split interleave
interleave (x : xs, ys) = x : (interleave (ys, xs))


-- only takes 2^n sized lists, or we can assume is zero. Split is incorrect possible then. If not an even number it will swap
fourier :: [Complex Double] -> [Complex Double]
fourier [] = []
fourier [a] = [a]
fourier xs =  (zipWith (+) ffte ffto') <> (zipWith (-) ffte ffto')  where
	        (evens, odds) = split xs
	        ffte = fourier evens
	        n = length ffte
	        ffto = fourier odds
	        ffto' = zipWith (*) ffto (twiddle n) 

-- we may want to seperate out the real and imag part
data Fourier = Fourier {k0 :: Double, kcos :: [Double] , ksin :: [Double] }

-- could we get liquid haskell to verify this?

-- I can fourier any sequence Vector a => [a] -> [a]
-- Need scalar multiplication via fourier coefficents.
-- need vector add for + and - of 
-- I can make ChebFun a functor. Which can be useful, but perhaps suspicious.

-- instance Num a => Vector a a


fourier' :: [Double] -> [Complex Double]
fourier' [] = []
fourier' [a] = [a :+ 0]
fourier' xs =  (zipWith (+) ffte ffto') <> (zipWith (-) ffte ffto')  where
	        (evens, odds) = split xs
	        ffte = fourier' evens
	        n = length ffte
	        ffto = fourier' odds
	        ffto' = zipWith (*) ffto (twiddle n) 

twiddle :: Int -> [Complex Double]
twiddle n = map cis $ iterate (+ (pi / n')) 0 where n' = fromInteger $ toInteger n





-- only takes 2^n sized lists, or we can assume is zero. Split is incorrect possible then. If not an even number it will swap
ifourier :: [Complex Double] -> [Complex Double]
ifourier [] = []
ifourier [a] = [a]
ifourier xs =  (zipWith (+) ffte ffto') <> (zipWith (-) ffte ffto')  where
	        (evens, odds) = split xs
	        ffte = ifourier evens
	        n = length ffte
	        ffto = ifourier odds
	        ffto' = zipWith (*) ffto (map conjugate (twiddle n)) 
{-
{-# Rewrite (ifourier (fourier x) = x #-}
{-# Rewrite (fourier (ifourier x) = x #-}
	Do I need fourier . ifourier = id ? couldn't hurt I guess.
		These laws aren't strictly true, but pretty close. A nightmare for consistency, but will always make accuracy better.

-}
-- his is some bull.
ifft :: [Complex Double] -> [Complex Double]
ifft (x : xs) = fourier (x : (reverse xs))

fourierGood :: [Complex Double] -> Bool
fourierGood _ = True -- implement the envelope test

envelope :: (Ord a, Num a) => [a] -> [a] -- an envelope that replaces the list with the maximum of the list form then on
envelope = (scanr1 max) . (map abs)  -- (\x acc -> max x acc) 


-- plateaujtest j env = env ! j >= env ! j2 * (1 - )   where j2 = 1.25*j + 5
-- hmm a constant 0 may be unhappy with this. probably shouldn't use it.
-- The maximum will be the head of the envelope by the way
-- normalize xs = map (/ m) xs where m = maximum xs

--normalize . envelope


-- The stupidest test. WHat if we are accidentally in a zero of the spectrum
-- maybe lsightly less stupid would be to take the maximum of the last 1/4.
-- These really are garbage. We should actually follow the chebfun prescirption.

simpleTest :: (Fractional a, Ord a) => [a] -> Bool
simpleTest xs = if (last absxs) >= (m * (1e-12)) then False else True where
	    absxs = fmap abs xs
	    m = maximum absxs


-- If we consider iterated ChebFuns, we may want different complexity depending on whether we are at low rank pieces or not.
-- so part of our chebify should recurse inside and chebify for every element.
-- I guess i roughly expect the needed coefficients to form a polytope ?
-- It will be this blob on nenzero coefficients. In higher dimension, surfacc is large, so cutting down on that is helpful.
-- Hmm. This isn't really hierarchical. Maybe some even odd splitting combined with left right splitting in real space?
-- I get a faint sensation I might want to look at a maxi-min? Maybe not. 
{-
The new expansion test should decide on both whether to expand internally and at this level.
-- Chebify (Chebify a) => Chebify (Double -> a) where
  chebify f = (fmap chebify) f -

  Chebify Double where
  	chebify = id

-}
-- G(x,x') considered as a 2d function 
-- 1/r^2 also should seperate out into peices


-- integrate -- from 0 to x
-- differentiate -- differentiation with resepct to the coordinate
-- functional_differenation -- = differntation with resepct to the value held.
-- sample :: Double -> ChebFun a -> a -- function application 
-- fmap diff (For a composed chebfun this idffierentiates the third coordinate)
-- diff2 = diff . diff
-- 3d laplacian.
-- diff2 + fmap diff2 + fmap . fmap diff2



{-
increasingFourier :: [Complex Double] -> (Double -> Double) -> [Complex Double]
increasingFourier evens f | fourierGood evens = increasingFourier evens' f 
                          | otherwise = evens where
                                               evens' = (zipWith (+) evens ffto') <> (zipWith (-) evens ffto')
                                               odds = sample f oddpositions
                                               ffto =  fourier odds
                                               twiddle =    
                                               ffto' = zipWith      -- if evens acceptable, stop, otherwise 

--- increasing fourier is chebify basically
chebify = increasingFourier []
-- sinc interpolation I guess? just take nearest point of reverse fft? Linear interpolation of fft? All of these corresopnd to filters, the choice of
	-- which should not matter much

-- sum_x0  sinc(x-x0) * f(x0)
funcify = \x -> Chebfun
-}




main :: IO ()
main = someFunc
