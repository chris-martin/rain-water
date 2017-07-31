{-# LANGUAGE ConstraintKinds, FlexibleContexts, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses #-}

module Rain
    ( collectWater
    ) where

import           Data.Coerce (Coercible, coerce)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Numeric.Natural (Natural)
import qualified Protolude
import           Protolude hiding ((*), (-), floor)

-- | Multiplication among heterogeneous types.
class Multiplication a b c where
    (*) :: a -> b -> c
    infixl 7 *

-- | A width and a height multiply to form area in the normal geometric way.
instance Multiplication Width Height Area where
    Width w * Height h = Area (w Protolude.* h)

instance Multiplication Height Width Area where
    Height h * Width w = Area (w Protolude.* h)

class Subtraction a where
    (-) :: a -> a -> a
    infixl 6 -

-- | For natural numbers, we take subtraction to mean absolute difference, as
-- subtraction in the normal integer sense would not be total.
instance Subtraction Natural where
    a - b | a >= b    = a Protolude.- b
          | otherwise = b Protolude.- a

instance Subtraction a => Subtraction (Sum a) where
    Sum a - Sum b = Sum (a - b)

newtype Width = Width (Sum Natural)
    deriving (Eq, Monoid, Ord, Semigroup, Subtraction)

newtype Height = Height (Sum Natural)
    deriving (Eq, Monoid, Ord, Semigroup, Subtraction)

newtype Area = Area (Sum Natural)
    deriving (Eq, Monoid, Ord, Semigroup, Subtraction)

-- | Map of corner height to corner depth.
type Corners = Map Height Width

newtype LeftFace = LeftFace Corners

type RightFace = Dual LeftFace

-- | The outer shape of a structure, and the amount of water it holds.
data Structure = Structure { sLeft  :: LeftFace
                           , sRight :: RightFace
                           , sArea  :: Area
                           }

-- | Generalize 'LeftFace' and 'RightFace' as anything that can be converted
-- back and forth with 'Corners'.
type Face a = (Coercible Corners a, Coercible a Corners)

faceSize :: Face a => a -> (Height, Width)
faceSize face = let corners = coerce face :: Corners
                in  if null corners
                        then mempty
                        else Map.findMax corners

overlapFaces :: Face a => a -> a -> a
overlapFaces nearFace farFace = coerce (corners :: Corners)
  where
    near = coerce nearFace :: Corners
    far  = coerce farFace  :: Corners
    (nearHeight, nearWidth) = faceSize near
    far' = (<> nearWidth <> Width 1) <$> snd (Map.split nearHeight far)
    corners = near <> far'

emptyFace :: Face a => a
emptyFace = coerce (Map.empty :: Corners)

instance Semigroup LeftFace where near <> far = overlapFaces near far

instance Monoid LeftFace where mappend = (<>); mempty = emptyFace

instance Semigroup Structure where
    Structure left right water <> Structure left' right' water' =
        Structure (left <> left')
                  (right <> right')
                  (water <> water' <> waterBetween right left')

instance Monoid Structure where mappend = (<>); mempty = emptyStructure

emptyStructure :: Structure
emptyStructure = Structure mempty mempty mempty

waterBetween :: RightFace -> LeftFace -> Area
waterBetween face face' = fold $ go (Map.toAscList (coerce face :: Corners))
                                    (Map.toAscList (coerce face' :: Corners))
                                    mempty
  where
    go :: [(Height, Width)]
       -> [(Height, Width)]
       -> Height
       -> [Area]
    go l@((heightL, depthL) : restL)
       r@((heightR, depthR) : restR) floor = newWater : go l' r' floor'
      where
        (floor', l', r') = case compare heightL heightR of
            LT -> (heightL, restL, r    )
            GT -> (heightR, l,     restR)
            EQ -> (heightL, restL, restR)

        newWater = raised * width
        raised   = floor' - floor
        width    = depthL <> depthR

    go _ _ _ = []

structureSingleton :: Height -> Structure
structureSingleton height = Structure face face mempty
  where
    face :: Face a => a
    face = coerce (Map.singleton height mempty :: Corners)

collectWater :: [Natural] -> Natural
collectWater = coerce . sArea . foldMap (structureSingleton . coerce)
