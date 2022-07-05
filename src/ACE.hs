{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE RecordWildCards #-}

--{-# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin #-}

-- | AC²E Bindings
module ACE ( CKT (..)
           , PDK (..)
           , Env (..) 
           , mkEnv 
           , withACE
           , randomSizing
           , currentSizing
           , evaluate
           , mkEnvs
           , performanceParameters'
           , sizingParameters'
           , analyses'
           , initSizing'
           , randomSizing'
           , currentSizing'
           , evaluate'
           ) where

import qualified ACE.Internal          as AC
import           System.Directory            (getHomeDirectory)
import qualified Data.Map              as M
import           Data.ByteString.Char8       (pack)
import           Language.Java               (withJVM)

-- | Analog Circuit Characterization Environment
data Env = Env { -- | Raw ACE Java Object
                 jEnvironment          :: AC.SEOEnv
                 -- | List of Available Performance Parameters
               , performanceParameters :: [String]
                 -- | List of Available Sizing Parameters
               , sizingParameters      :: [String]
                 -- | List of available simulation analyses (blacklistable)
               , analyses              :: [String]
                 -- | Initial Sizing
               , initSizing            :: M.Map String Float
               }

-- | Syntactic Sugar for Environment Pool
type EnvPool = [Env]

-- | Create a new AC²E
mkEnv :: CKT -> PDK -> IO Env
mkEnv ckt pdk = do
    homeDir <- getHomeDirectory
    let simPath = "/tmp"
        cktPath = homeDir ++ "/.ace/" ++ show pdk ++ "/" ++ show ckt
        pdkPath = homeDir ++ "/.ace/" ++ show pdk ++ "/pdk"

    jenv <- AC.mkEnv simPath cktPath pdkPath

    pid  <- AC.performanceIds   jenv
    sps  <- AC.sizingParameters jenv
    iss  <- AC.initialSizing    jenv
    anl  <- AC.analyses         jenv

    pure $ Env { jEnvironment          = jenv
               , performanceParameters = pid
               , sizingParameters      = sps
               , analyses              = anl
               , initSizing            = iss
               }
                 
-- | Construct a number of environments for parallel execution
mkEnvs :: CKT -> PDK -> Int -> IO EnvPool
mkEnvs _   _   0   = pure []
mkEnvs ckt pdk num = do
    env  <- mkEnv ckt pdk
    envs <- mkEnvs ckt pdk $ pred num
    pure $ env:envs

-- | Performance Parameters for a pool of environments
performanceParameters' :: EnvPool -> [[String]]
performanceParameters' = map performanceParameters

-- | Sizing Parameters for a pool of environments
sizingParameters' :: EnvPool -> [[String]]
sizingParameters' = map sizingParameters

-- | Simulation analyses available for a pool of environments
analyses' :: EnvPool -> [[String]]
analyses' = map analyses

-- | Initial Sizing for a pool of environments
initSizing' :: EnvPool -> [M.Map String Float]
initSizing' = map initSizing

-- | Evaluate circuit performance
evaluate :: Env -> M.Map String Float -> IO (M.Map String Float)
evaluate Env{..} = AC.evaluate jEnvironment

-- | Evaluate circuit performance in parallel
evaluate' :: EnvPool -> [M.Map String Float] -> IO [M.Map String Float]
evaluate' envs = AC.evaluatePool jenvs
  where
    jenvs = map jEnvironment envs

-- | Random Sizing sample
randomSizing :: Env -> IO (M.Map String Float)
randomSizing Env{..} = AC.randomSizing jEnvironment

-- | Random Sizing sample for a pool of environments
randomSizing' :: EnvPool -> IO [M.Map String Float]
randomSizing' []     = pure []
randomSizing' (e:es) = do
    sizing  <- randomSizing  e
    sizings <- randomSizing' es
    pure $ sizing:sizings

-- | Current Sizing of Environment
currentSizing :: Env -> IO (M.Map String Float)
currentSizing Env{..} = AC.currentSizing jEnvironment

-- | Current Sizing for Environments in Pool
currentSizing' :: EnvPool -> IO [M.Map String Float]
currentSizing' []     = pure []
currentSizing' (e:es) = do
    sizing  <- currentSizing  e
    sizings <- currentSizing' es
    pure $ sizing:sizings

-- | IO Monad Wrapper for Java Communication
withACE :: IO a -> IO a
withACE a = do
    clsPath <- pack . ("-Djava.class.path=" ++) <$> AC.classPath
    withJVM [clsPath] a

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | Available Circuits
data CKT = OP1  -- ^ miller opamp with N differential pair
         | OP2  -- ^ symmetrical opamp with N differential pair
         | OP3  -- ^ (un) symmetrical opamp with N differential pair
         | OP4  -- ^ symmetrical opamp with N differential pair and cascodes
         | OP5  -- ^ (un) symmetrical opamp with N differential pair and cascode
         | OP6  -- ^ miller opamp with N differential pair
         | OP7  -- ^ feed-foward opamp
         | OP8  -- ^ folded-cascode
         | OP9  -- ^ folded-cascode with wide-swing current mirror
         | OP10 -- ^ rail-to rail folded-cascode
         | OP11 -- ^ rail-to rail folded-cascode with wide-swing current mirror
    deriving (Eq)

-- | Show instance of Circuit ID
instance Show CKT where
  show OP1  = "op1"
  show OP2  = "op2"
  show OP3  = "op3"
  show OP4  = "op4"
  show OP5  = "op5"
  show OP6  = "op6"
  show OP7  = "op7"
  show OP8  = "op8"
  show OP9  = "op9"
  show OP10 = "op10"
  show OP11 = "op11"

-- | Read instance of Circuit ID
instance Read CKT where
  readsPrec _ "op1"  = [(OP1,  "")]
  readsPrec _ "op2"  = [(OP2,  "")]
  readsPrec _ "op3"  = [(OP3,  "")]
  readsPrec _ "op4"  = [(OP4,  "")]
  readsPrec _ "op5"  = [(OP5,  "")]
  readsPrec _ "op6"  = [(OP6,  "")]
  readsPrec _ "op7"  = [(OP7,  "")]
  readsPrec _ "op8"  = [(OP8,  "")]
  readsPrec _ "op9"  = [(OP9,  "")]
  readsPrec _ "op10" = [(OP10, "")]
  readsPrec _ "op11" = [(OP11, "")]
  readsPrec _ _      = undefined

-- | Available PDKs
data PDK = XH035   -- ^ X-Fab 350nm Process
         | XH018   -- ^ X-Fab 180nm Process
         | XT018   -- ^ X-Fab 180nm Process
         | SKY130  -- ^ SkyWater 130nm Process
         | GPDK180 -- ^ Cadence Generic PDK 180nm Process
    deriving (Eq)

-- | Show Instance of PDK
instance Show PDK where
  show XH035   = "xh035-3V3"
  show XH018   = "xh018-1V8"
  show XT018   = "xt018-1V8"
  show SKY130  = "sky130-1V8"
  show GPDK180 = "gpkd180-1V8"

-- | Show Instance of PDK
instance Read PDK where
  readsPrec _ "xh035-3V3"   = [(XH035,   "")]
  readsPrec _ "xh018-1V8"   = [(XH018,   "")]
  readsPrec _ "xt018-1V8"   = [(XT018,   "")]
  readsPrec _ "sky130-1V8"  = [(SKY130,  "")]
  readsPrec _ "gpdk180-1V8" = [(GPDK180, "")]
  readsPrec _ _             = undefined
