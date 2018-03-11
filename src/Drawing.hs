-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Haskell module declaration
module Drawing where

import Types

-- | Miso framework import
import Miso (View)
import Miso.String (MisoString(..))
import Miso.Svg
import qualified Miso.Svg.Element   as SvgElem
import qualified Miso.Svg.Attribute as SvgAttr
import Data.JSString (pack)

import Miso.Html (Attribute(..), div_)
import Miso.Html.Property  (textProp)

import Data.Monoid

scaleHeight :: Integer
scaleHeight = 100

scaleWidth :: Integer
scaleWidth  = 100

-- | Height of a generator in abstract units
height :: Generator -> Integer
height x = case x of
  Twist -> 2
  Copy  -> 2
  Add   -> 2
  _     -> 1

-- | All generators are 1 units wide
width :: a -> Integer
width = const 1

attr :: MisoString -> MisoString -> Attribute action
attr = textProp

-- | The href attribute works where xlinkHref doesn\'t.
href_ :: MisoString -> Attribute action
href_ = attr "href"

-- RHS of rule B1 from
-- https://graphicallinearalgebra.net/2015/05/16/natural-numbers-diagrammatically/
ruleB1 = Diagram
  [ [ Copy, Copy ]
  , [ Identity, Twist, Identity ]
  , [ Add, Add   ]
  ]

ruleAssocLHS = Diagram
  [ [ Identity, Add ]
  , [ Add ]
  ]

nat3 = Diagram
  [ [ Copy ]
  , [ Copy, Identity ]
  , [ Add, Identity ]
  , [ Add ]
  ]

-- Generators annotated with info to help layout
-- TODO: replace this with a 2D euclidean vector type?
data Coords a = Coords
  { xpos :: a
  , ypos :: a
  } deriving(Eq, Ord, Read, Show, Functor)

addCoords (Coords x y) (Coords a b) = Coords (x+a) (y+b)

viewSVG :: View action
viewSVG = div_ [] [ a, b, c ]
  where
    a = diagramSvg $ viewDiagram ruleB1
    b = diagramSvg $ viewDiagram ruleAssocLHS
    c = diagramSvg $ viewDiagram nat3

--------------------- Drawing generators --------------------

-- | Given function mapping a list to its height, annotate
-- each element of the list with its y position.
cumulative :: Monoid s => (a -> s) -> [a] -> [(s, a)]
cumulative f xs = flip zip xs . scanl (<>) mempty $ fmap f xs

-- | Annotate a diagram so each generator knows its coordinates.
annotate :: Diagram Generator -> Diagram (Coords Integer, Generator)
annotate (Diagram stacks) = Diagram $ fmap (uncurry annotateStack) stacksWithWidth
  where
    stacksWithWidth =
      cumulative (Sum . (*scaleHeight) . width) stacks

    annotateStack x gs =
      let mkCoords y g = (fmap getSum (Coords x y), g)
      in  fmap (uncurry mkCoords) (cumulative (Sum . (*scaleWidth) . height) gs)


-- | A list of all the drawable elements of a diagram, positioned correctly.
viewDiagram :: Diagram Generator -> [View action]
viewDiagram d = concat elems ++ connections spaced
  where
    spaceOut (Coords x y, g) = (Coords (x*2) y, g)
    ffmap  = fmap . fmap
    spaced = fmap spaceOut . annotate $ d
    elems  = ffmap (uncurry viewAnnotated) . runDiagram $ spaced

-- | Render a single generator at the supplied coordinates
viewAnnotated :: Coords Integer -> Generator -> View action
viewAnnotated (Coords x y) gen =
  use_ [ x_ (mshow x), y_ (mshow y), href_ gurl ] []
  where
    gurl :: MisoString
    gurl  = pack $ '#' : show gen
    mshow = pack . show

--------------------- Drawing connections --------------------

-- | 'expand f xs' repeats each copy of xs 
--
-- >>> expand id [1,2,3] == [1,2,2,3,3,3]
-- True
expand :: (a -> Integer) -> [a] -> [(Integer, a)]
expand f = (>>= g)
  where g x = zip [0..] $ replicate (fromIntegral $ f x) x

-- | Return coordinates of the 'i'th port for a generator.
-- TODO: make this independent of "height" function!
--
-- NOTE: the 'k' function selects the left or right of a tuple - we cheekily
-- use it to also select the x coordinate by applying it to (0, 100).
portCoords k (i, (c, g)) = addCoords c $ Coords x y
  where
    y = (i * scaleHeight) + (scaleHeight * height g) `div` (2 * k (ports g))
    x = k (0, scaleWidth * width g)

-- | Connect two adjacent annotated stacks.
connect
  :: [(Coords Integer, Generator)]
  -> [(Coords Integer, Generator)]
  -> [(Coords Integer, Coords Integer)]
connect left right = zipWith f ls rs
  where
    hasL (_, g) = fst (ports g) > 0
    hasR (_, g) = snd (ports g) > 0

    -- Replicate each generator
    expandStack f = expand (f . ports . snd)

    -- Confusingly, we examine the LEFT stack for "RIGHT-side ports",
    -- and the RIGHT side for "LEFT-side ports"
    ls = expandStack snd $ filter hasR left
    rs = expandStack fst $ filter hasL right

    f l r = (portCoords snd l, portCoords fst r)

-- | Draw all connections between nodes.
--
-- Diagrams connect "top down" - If we have '[[Twist]', '[Identity]]' then the
-- top port of Twist is connected to Identity, and the bottom port is
-- disconnected (and the diagram is invalid).
connections :: Diagram (Coords Integer, Generator) -> [View action]
connections (Diagram stacks) = concat $ zipWith f stacks (drop 1 stacks)
  where f l r = fmap (uncurry drawConnector) $ connect l r

--------------------- SVG rendering --------------------

-- | Draw a bezier connector between two points, with the first to the left.
-- TODO: tidy nasty code
drawConnector :: Coords Integer -> Coords Integer -> View action
drawConnector (Coords x y) (Coords a b) =
  path_
    [ d_ (pack svgStr), fill_ "transparent", stroke_ "black"
    , strokeWidth_ "5"
    {-, opacity_ "0.5"-}
    ] []
  where
    midX = (a - x) `div` 2
    control1 = (x + midX, y)
    control2 = (a - midX, b)
    showPair (x,y) = show x ++ " " ++ show y
    svgStr = "M " ++ showPair (x,y) ++ " C " ++
             showPair control1 ++ " " ++ showPair control2 ++ " " ++ showPair (a,b)


-- | Inner CSS style for all diagrams.
innerStyle = style_ []
  [".generator {stroke: black; stroke-width: 5; fill: transparent;}"]

-- | Wrap a list of SVG elements representing generators with the outer SVG element
-- and definitions.
-- TODO: put the diagram elements in a viewBox, and set to 100% width and height.
--       as in here: https://stackoverflow.com/questions/19484707/
diagramSvg elems = svg_ [ height_ "500", width_ "1000" ]
  [ innerStyle
  , svgDefs
  , g_ [ transform_ "scale(0.5)" ] elems
  ]

svgDefs :: View action
svgDefs = defs_ [] (fmap mkDef generators)
  where
    mkDef gen = g_ [ id_ (pack $ show gen), class_' "generator" ] (toShapeDef gen)

-- | The insides of an SVG group
toShapeDef :: Generator -> [View action] 
toShapeDef x = case x of
  Identity    ->
    [ path_ [ d_ "M 0 50 L 100 50" ] []
    ]
  Twist ->
    [ g_ [ transform_ "translate(0, 50)" ]
      [ path_ [ d_ "M 0 0    C 50 0    50 100  100 100" ] []
      , path_ [ d_ "M 0 100  C 50 100  50 0    100 0  " ] []
      ]
    ]
  Zero  ->
    [ circle_ [ cx_ "80", cy_ "50", r_ "20" ] []
    , path_   [ d_ "M 0 50 L 60 50" ] []
    ]
  Discard ->
    [ circle_ [ cx_ "20", cy_ "50", r_ "20", fill_ "black" ] []
    , path_   [ d_ "M 20 50 L 100 50" ] []
    ]
  Add     -> 
    [ g_ [ transform_ "translate(0, 50)" ]
      [ maskDef "clip-circle"
      , circle_ [ cx_ "50", cy_ "50", r_ "20" ] []
      , path_   [ SvgAttr.mask_ "url(#clip-circle)"
                , d_ "M 0  0   C 50  0   50 50 50 50" ] []
      , path_   [ SvgAttr.mask_ "url(#clip-circle)"
                , d_ "M 0 100  C 50 100  50 50 50 50" ] []
      , path_   [ d_ "M 70 50 L 100 50" ] []
      ]
    ]
  Copy    -> 
    [ g_ [ transform_ "translate(0, 50)" ]
      [ circle_ [ cx_ "50", cy_ "50", r_ "20", fill_ "black" ] []
      , path_   [ d_ "M 50 50 C 50 50  50 0   100 0 " ] []
      , path_   [ d_ "M 50 50 C 50 50  50 100 100 100" ] []
      , path_   [ d_ "M 0 50 L 30 50" ] []
      ]
    ]
  where
    -- Used in the "Add" SVG to remove some lines
    maskDef idName =
      mask_  [ id_ idName ]
        [ rect_
            [ x_ "-50", y_ "-50", width_ "200", height_ "200", fill_ "white" ] []
        , circle_ [ cx_ "50", cy_ "50", r_ "20", fill_ "black" ] []
        ]


-- | A test image showing all the generators, without connections.
testImage = svg_ [height_ "500", width_ "1000", version_ "1.1"]
  [ innerStyle
  , svgDefs
  , g_ [ transform_ "scale(0.5)" ]
    [ rect_ [ height_ "100%", width_ "100%", fill_ "grey" ] []
    , use_  [ x_ "100", y_ "0", href_ "#Identity" ] []
    , use_  [ x_ "100", y_ "100", href_ "#Zero" ] []
    , use_  [ x_ "100", y_ "200", href_ "#Discard" ] []

    , use_  [ x_ "300", y_ "0", href_ "#Twist" ] []
    , use_  [ x_ "500", y_ "0", href_ "#Add" ] []
    , use_  [ x_ "700", y_ "0", href_ "#Copy" ] []
    ]
  ]

