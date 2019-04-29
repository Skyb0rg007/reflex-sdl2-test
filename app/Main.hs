{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Reflex
import           Reflex.SDL2
import           Reflex.SDL2.Widgets

app :: MonadWidget t m => m ()
app = do
    evPostBuild <- getPostBuild
    performEvent_ $ liftIO (putStrLn "starting up...") <$ evPostBuild

    button "initial" never

    evQuit <- getQuitEvent
    performEvent_ $ (liftIO . putStrLn $ "bye!") <$ evQuit
    shutdownOn evQuit

main :: IO ()
main = do
    initializeAll
    let cfg = defaultWindow { windowResizable = True
                            , windowInitialSize = V2 640 480
                            }
    window <- createWindow "reflex-sdl2-example" cfg
    render <- createRenderer window (-1) defaultRenderer
    rendererDrawBlendMode render $= BlendAlphaBlend

    host $ runWidgets render window app

    destroyRenderer render
    destroyWindow window
    quit
