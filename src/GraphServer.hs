module Main where

import Network.Socket hiding (recv)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B
import qualified Control.Concurrent.MVar as M (tryTakeMVar, tryPutMVar, newEmptyMVar, MVar) 
import qualified Control.Concurrent as C
import qualified System.Time as T
import qualified Graphics.UI.GLUT as GLUT

import Draw.Tools
import Draw.Draw
import Draw.Texture
import GLTypes


main::IO ()
main = do
    mVar <- M.newEmptyMVar
  
-- Socket Network Server
--    putStrLn "Starting server..."
--    sock <- socket AF_INET Stream defaultProtocol
--    bindSocket sock (SockAddrInet 4343 0)
--    listen sock 1
--    socketServerThread <- C.forkOS $ acceptConnections mVar sock

    startTime <- T.getClockTime

-- GL initializing
    GLUT.getArgsAndInitialize
    wnd <- GLUT.createWindow "Graph output"
    initGL
    texs <- textures
    GLUT.displayCallback GLUT.$= (drawSceneCallback mVar (GLResources texs) draw)
    GLUT.reshapeCallback GLUT.$= Just resizeSceneCallback

-- Thread which sends postRedisplayMessage (to redraw GL window) with some frequency.
    fpsThread <- C.forkOS $ fpsLoop 0 mVar (Just wnd) (T.TimeDiff 0 0 0 0 0 0 50000000000) startTime

    GLUT.mainLoop

--    C.killThread socketServerThread
--    C.killThread fpsThread
    putStrLn "Ok."
    

acceptConnections :: M.MVar B.ByteString -> Socket -> IO ()
acceptConnections mVar sock = do
    (conn, _) <- accept sock
    res <- recv conn 4096
    sClose conn
    B.putStr . B.pack $ "Received: "
    B.putStrLn res
    M.tryPutMVar mVar res
    case reverse . take 4 . reverse . B.unpack $ res of
        "exit" -> do
            putStrLn "Stoping server..."
            sClose sock
        _ -> acceptConnections mVar sock

fpsLoop n mVar wnd fpsRate prevClockTime = do
    curClockTime <- T.getClockTime
    case (curClockTime < T.addToClockTime fpsRate prevClockTime) of
      True  -> fpsLoop n mVar wnd fpsRate prevClockTime
      False -> do
            M.tryPutMVar mVar n
            postRedisplayMsg wnd
            fpsLoop (n+1) mVar wnd fpsRate curClockTime
    
    
    