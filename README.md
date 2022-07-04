# EMBRAC²E

Haskell bidings for [AC²E](https://github.com/electronics-and-drives/ace).

## Example

```haskell
import ACE

main :: IO () 
main = withACE $ do
    env <- mkEnv OP2 XH035
    randomSizing env >>= simulate env >>= print
```
