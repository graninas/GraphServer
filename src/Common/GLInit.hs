module Common.GLInit where

import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw
import qualified Graphics.Rendering.GLU.Raw as GLURaw (gluPerspective)
import qualified Graphics.UI.GLUT as GLUT

import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B
import qualified Control.Concurrent.MVar as M (tryTakeMVar, tryPutMVar, newEmptyMVar, MVar) 

import Common.GLTypes


initGL :: IO ()
initGL = do
    GLUT.initialDisplayMode GLUT.$= [GLUT.RGBAMode, GLUT.WithDepthBuffer, GLUT.DoubleBuffered]
    GLRaw.glShadeModel GLRaw.gl_SMOOTH
    GLRaw.glClearColor 0 0 0 0
    GLRaw.glClearDepth 1
    GLRaw.glEnable GLRaw.gl_DEPTH_TEST
    GLRaw.glDepthFunc GLRaw.gl_LEQUAL
    GLRaw.glHint GLRaw.gl_PERSPECTIVE_CORRECTION_HINT GLRaw.gl_NICEST
    GLRaw.glEnable GLRaw.gl_TEXTURE_2D

drawSceneCallback :: M.MVar GLfloat -> GLResources -> DrawFunction -> IO ()
drawSceneCallback mVar ress draw = do
     maybeVar <- M.tryTakeMVar mVar
     case maybeVar of
        Nothing -> GLUT.swapBuffers
        Just n -> do
            draw ress n
            GLUT.swapBuffers

resizeSceneCallback (GLUT.Size w 0) = resizeSceneCallback (GLUT.Size w 1)
resizeSceneCallback (GLUT.Size width height) = do
    GLRaw.glViewport 0 0 (fromIntegral width) (fromIntegral height)
    GLRaw.glMatrixMode GLRaw.gl_PROJECTION
    GLRaw.glLoadIdentity
    GLURaw.gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100
    GLRaw.glMatrixMode GLRaw.gl_MODELVIEW
    GLRaw.glLoadIdentity
    GLUT.swapBuffers

postRedisplayMsg = GLUT.postRedisplay

