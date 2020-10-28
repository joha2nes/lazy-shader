{-# LANGUAGE GADTs #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module LazyShader.Runner (
    runShader
) where

import Data.Maybe (fromJust, fromMaybe)
import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Graphics.GL.Compatibility32
import Graphics.GL.Types

import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Linear (V4)

import LazyShader.Interpret
import LazyShader.Shader

runShader :: (Int, Int) -> Shader (V4 Float) -> IO ()
runShader (windowWidth, windowHeight) color = do
    _ <- GLFW.init
    monitor <- fromJust <$> getPrimaryMonitor
    videoMode <- fromJust <$> getVideoMode monitor
    let monitorWidth = videoModeWidth videoMode
        monitorHeight = videoModeHeight videoMode
        monitorRatio = toRational monitorWidth / toRational monitorHeight
    windowHint (WindowHint'Resizable False)
    window <- fromJust <$> createWindow windowWidth windowHeight "GLFW" Nothing Nothing
    makeContextCurrent (Just window)

    let viewportWidth = monitorWidth
        viewportHeight = monitorHeight - 200
        viewportRatio = toRational viewportWidth / toRational viewportHeight

    putStrLn $ "monitorSize: " ++ show (monitorWidth, monitorHeight)
    putStrLn $ "monitorRatio: " ++ show monitorRatio
    putStrLn $ "windowSize: " ++ show (windowWidth, windowHeight)
    putStrLn $ "viewportRatio: " ++ show viewportRatio

    glClearColor 0 0 0 1
    -- glViewport viewportX viewportY viewportWidth viewportHeight
    -- glScissor viewportX viewportY viewportWidth viewportHeight
    -- glEnable GL_SCISSOR_TEST

    let expr = interpret color

    putStrLn expr

    vertexShader <- createVertexShader vertexShaderCode
    fragmentShader <- createFragmentShader (formatFragmentShader expr)
    program <- createProgram vertexShader fragmentShader

    glLinkProgram program

    glUseProgram program

    -- uniforms
    locResolution <- glGetUniformLocation program =<< newCString "u_resolution"
    locTime <- glGetUniformLocation program =<< newCString "u_time"

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA  

    loopM do
        time <- realToFrac <$> fromMaybe 0 <$> getTime

        glClear GL_COLOR_BUFFER_BIT
        glLoadIdentity

        glUseProgram program
        glUniform2f locResolution (fromIntegral windowWidth) (fromIntegral windowHeight)
        glUniform1f locTime time

        glBegin GL_POLYGON
        do  glVertex2f (-1) (-1)
            glVertex2f (-1)  (1)
            glVertex2f  (1)  (1)
            glVertex2f  (1) (-1)
        glEnd

        swapBuffers window

        pollEvents
        threadDelay 1000

        close <- windowShouldClose window
        escape <- (==KeyState'Pressed) <$> getKey window Key'Escape
        pure (not $ close || escape)

    glDeleteProgram program

    destroyWindow window
    terminate

createShader :: GLenum -> String -> IO GLuint
createShader e code = do
    shaderId <- glCreateShader e
    fsAdapter <- new =<< newCString code
    glShaderSource shaderId 1 fsAdapter nullPtr
    glCompileShader shaderId

    compileSuccess <- malloc
    glGetShaderiv shaderId GL_COMPILE_STATUS compileSuccess
    success <- (/=) GL_FALSE <$> peek compileSuccess

    unless success do
        infoPtr <- mallocArray 512
        glGetShaderInfoLog shaderId 512 nullPtr infoPtr
        info <- peekCString infoPtr
        error info

    return shaderId

createVertexShader :: String -> IO GLuint
createVertexShader = createShader GL_VERTEX_SHADER

createFragmentShader :: String -> IO GLuint
createFragmentShader = createShader GL_FRAGMENT_SHADER

createProgram :: GLuint -> GLuint -> IO GLuint
createProgram vs fs = do
    programId <- glCreateProgram
    glAttachShader programId vs
    glAttachShader programId fs
    return programId

vertexShaderCode :: String
vertexShaderCode = 
    "\
    \#version 440 core\r\n\

    \layout (location = 0) in vec2 vertex;\
    
    \void main()\
    \{\
    \    gl_Position = vec4(vertex, 0.0, 1.0);\
    \}\
    \"

formatFragmentShader :: String -> String
formatFragmentShader s =
  "\
    \#ifdef GL_ES\r\n\
	\    precision mediump float;\r\n\
    \#endif\r\n\

    \out vec4 gl_FragColor;\

    \uniform vec2 u_resolution;\
    \uniform float u_time;\

    \void main()\
    \{\
        \gl_FragColor=\
    \"
        <> s <>
    ";\
    \}\
    \"

loopM :: Monad m => m Bool -> m ()
loopM m = do
    b <- m
    if b then loopM m else pure ()