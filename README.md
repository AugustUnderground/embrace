# EMBRAC²E

Haskell bidings for [AC²E](https://github.com/electronics-and-drives/ace).

## Example

```haskell
import ACE

main :: IO () 
main = withACE $ do

    -- Single Environment
    env <- mkEnv OP2 XH035
    randomSizing env >>= evaluate env >>= print

    -- Parallel Environment Pool
    envs <- mkEnvs OP2 XH035 5
    randomSizing' envs >>= evaluate' envs >>= print
```
