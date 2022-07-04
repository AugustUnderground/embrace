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
           ) where

import qualified ACE.Internal          as AC
import           System.Directory            (getHomeDirectory)
import qualified Data.Map              as M
import           Data.ByteString.Char8       (pack)
import           Language.Java               (withJVM)

-- | Analog Circuit Characterization Environment
data Env = Env { -- | List of Available Performance Parameters
                 performanceParameters :: [String]
                 -- | List of Available Sizing Parameters
               , sizingParameters      :: [String]
                 -- | Simulate given sizing
               , simulate              :: M.Map String Float -> IO (M.Map String Float)
                 -- | Initial Sizing
               , initSizing            :: M.Map String Float
                 -- | Random Sizing
               , randomSizing          :: IO (M.Map String Float)
               }

-- | Create a new AC²E
mkEnv :: CKT -> PDK -> IO Env
mkEnv ckt pdk = do
    homeDir <- getHomeDirectory
    let simPath = "/tmp"
        cktPath = homeDir ++ "/.ace/" ++ show pdk ++ "/" ++ show ckt
        pdkPath = homeDir ++ "/.ace/" ++ show pdk ++ "/pdk"

    env <- AC.mkEnv simPath cktPath pdkPath

    performanceIds'   <- AC.performanceIds   env
    sizingParameters' <- AC.sizingParameters env
    initSizing'       <- AC.initialSizing    env

    pure $ Env { performanceParameters = performanceIds'
               , sizingParameters      = sizingParameters'
               , simulate              = AC.simulate env
               , initSizing            = initSizing'
               , randomSizing          = AC.randomSizing env
               }
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
