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

## Usage

Add the following to `extra-deps` in your `stack.yaml`:

```yaml
  - git: https://github.com/AugustUnderground/embrace.git
    commit: 11bb17378399dca2a6b7824216ebd5f05775158b
```

Make sure the java paths are set correctly by sourcing `setenv` in your shell.
