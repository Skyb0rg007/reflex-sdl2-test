{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}
module Reflex.SDL2.Widgets
    ( runWidgets
    , ButtonState (..)
    , MonadWidget (..)
    , button
    ) where

import           Control.Concurrent   (threadDelay)
import           Control.Monad        (forM_, void)
import           Control.Monad.Reader (MonadReader, reader, runReaderT, ReaderT)
import           Data.Text            (Text)
import           Data.Text            as T ()
import           Reflex
import           Reflex.SDL2

class (ReflexSDL2 t m, DynamicWriter t [Performable m ()] m)
      => MonadWidget t m | m -> t where
    getSDLRenderer :: m Renderer
    getSDLWindow :: m Window

instance (ReflexSDL2 t m, DynamicWriter t [Performable m ()] m)
      => MonadWidget t (ReaderT (Renderer, Window) m) where
    getSDLRenderer = reader fst
    getSDLWindow = reader snd

runWidgets
    :: ReflexSDL2 t m
    => Renderer
    -> Window
    -> (forall m1 . MonadWidget t m1 => m1 a)
    -> m ()
runWidgets r w guest = do
    g <- runReaderT guest (r, w)
    undefined
{- runWidgets r window guest = do
    g <- runReaderT guest (r, window) :: _
    (_, dynLayers) <- runDynamicWriterT g
    performEvent_ $ ffor (updated dynLayers) $ \layers -> do
        rendererDrawColor r $= V4 0 0 0 255
        clear r
        sequence_ layers
        present r -}


within :: Ord a => (a, a) -> (a, a) -> Point V2 a -> Bool
within (x0, y0) (x1, y1) (P (V2 x y)) =
    x0 <= x && x <= x1 && y0 <= y && y <= y1

data ButtonState
    = ButtonUp
    | ButtonDown
    | ButtonOver
    deriving Eq

button
    :: MonadWidget t m
    => Text
    -> Event t Text
    -> m (Dynamic t ButtonState)
button initialText updateText = do
    evMotionData <- getMouseMotionEvent

    let position = V2 100 100
        size     = V2 200 200
        V2 tlx tly = position
        V2 brx bry = position + size
        evMotionPos = fmap fromIntegral . mouseMotionEventPos <$> evMotionData
        evMouseIsInside = ffor evMotionPos $ \(P (V2 x y)) ->
              (x >= tlx && x <= brx) && (y >= tly && y <= bry)
    dMouseIsInside <- holdDyn False evMouseIsInside

    evBtn <- getMouseButtonEvent
    let evBtnIsDown = ffor evBtn $ (== Pressed) . mouseButtonEventMotion
    dButtonIsDown <- holdDyn False evBtnIsDown

    let dButtonStatePre = buttonState <$> dMouseIsInside <*> dButtonIsDown
    evPB         <- getPostBuild
    dButtonState <- holdDyn ButtonUp $ leftmost [ updated dButtonStatePre
                                                , ButtonUp <$ evPB
                                                ]
    r <- getSDLRenderer
    tellDyn $ fmap pure $ ffor dButtonState $ \st -> do
        let color = case st of
                      ButtonUp   -> V4 192 192 192 255
                      ButtonOver -> 255
                      ButtonDown -> V4 128 128 128 255
        rendererDrawColor r $= color
        fillRect r $ Just $ Rectangle (P position) size

    holdUniqDyn dButtonState

    where
        buttonState :: Bool -> Bool -> ButtonState
        buttonState isInside isDown
          | not isInside = ButtonUp
          | isDown       = ButtonDown
          | otherwise    = ButtonOver


