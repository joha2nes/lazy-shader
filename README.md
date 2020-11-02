# lazy-shader
An embedded DSL for writing GLSL fragment shaders in Haskell

# Example: marbles
```haskell
tile :: Shader (V2 Float) -> Shader (V2 Float) -> Shader (V2 Float)
tile uv zoom = fract (uv * zoom)

marbles :: Shader (V4 Float)
marbles =
    let zoom = 10
        speed = 1
        time' = time * (speed / zoom)

        offsetX = time' * sign (S.mod (_y uvY * zoom) 2 * 0.5 - 0.5) * (1 - step 1 (S.mod (time' * zoom) 2))
        offsetY = time' * sign (S.mod (_x uvY * zoom) 2 * 0.5 - 0.5) * step 1 (S.mod (time' * zoom) 2)
        offset = vec2 offsetX offsetY

        uvTiled = tile (uvY + offset) (vec2 zoom zoom)
        dist = S.length $ 0.5 - uvTiled
        shine = 1.2 * pow (1 - S.length (0.6 - uvTiled)) 4
        ball = step 0.4 dist + shine * (1 - step 0.4 dist)
        
    in vec4 ball ball ball 1
```
![](demo/marbles.gif)

# Example: bamboo
```haskell
sstep :: (Fractional a, Eq a, GenType Float a) => Shader a -> Shader a -> Shader a
sstep edge x = smoothstep (edge - 0.04) (edge + 0.04) x

bamboo :: Shader (V4 Float)
bamboo =
    let zoom = vec2 30 12
        uv' = vec2 (_x uvX + 0.01 * time) (_y uvX)
        index = S.floor (uv' * zoom)
        rand = random $ vec2 (_x index) (_x index)
        uv'' = vec2 (_x uv') (_y uv' + rand - time * rand * 0.02)
        
        curve = 0.1
        ySpacing = 0.01
        xSpacing = 0.2
        
        uvt = tile uv'' zoom
        yPi = pi * _y uvt
        mask = sstep (sin yPi * curve) (_x uvt - xSpacing)
             * sstep (_x uvt + xSpacing) (1 - sin yPi * curve)
             * sstep ySpacing (_y uvt)
             * sstep (_y uvt) (1 - ySpacing)
             
        bg = vec4 0.1 0.2 0.1 1
        colorA = vec4 0.8353 0.7882 0.1451 1
        colorB = vec4 0.4824 0.8392 0.149 1
        color = mix' colorA colorB rand

    in mix' bg color mask
```
![](demo/bamboo.gif)
