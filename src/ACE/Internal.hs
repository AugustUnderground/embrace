{-# OPTIONS_GHC -Wall                                #-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin #-}

-- | Internal Utility Functions for AC²E
module ACE.Internal where

import           System.Process             (readProcess)
import           GHC.Float                  (double2Float, float2Double)
import qualified Data.Text             as T
import qualified Data.Map              as M
import           Language.Java
import           Language.Java.Inline

imports "java.util.Set"
imports "java.util.Map"
imports "java.util.HashSet"
imports "java.util.HashMap"
imports "edlab.eda.ace.AnalogCircuitEnvironment"
imports "edlab.eda.ace.SingleEndedOpampEnvironment"
imports "edlab.eda.ace.EnvironmentPool"

-- | Convert coerced map from
map' :: T.Text -> Double -> (String, Float)
map' k v = (T.unpack k, double2Float v)

-- | Current AC²E version. Linked to
-- https://github.com/electronics-and-drives/ace
version :: String
version = "0.0.10"

-- | Get the default MAVEN home path
mvnHome :: IO String
mvnHome = head . filter (('[' /=) . head) . lines <$> readProcess "mvn" mvnArgs ""
  where
    mvnArgs = ["help:evaluate", "-Dexpression=settings.localRepository"]

-- | Get the default CLASSPATH to AC²E from local MAVEN install
classPath :: IO String
classPath = do
    mvnHome' <- mvnHome
    pure $ mvnHome' ++ "/edlab/eda/ace/" ++ version ++ "/ace-" 
                    ++ version ++ "-jar-with-dependencies.jar"

-- | Java AC²E Base Class
newtype ACEnv = ACEnv (J ('Class "edlab.eda.ace.AnalogCircuitEnvironment"))
    deriving Coercible

-- | Java AC²E Main Class
newtype SEOEnv = SEOEnv (J ('Class "edlab.eda.ace.SingleEndedOpampEnvironment"))
    deriving Coercible

-- | Java AC²E Environment Pool
newtype Pool = Pool (J ('Class "edlab.eda.ace.EnvironmentPool"))
    deriving Coercible

-- | Contruct ACE Object
mkEnv :: String -> String -> String -> IO SEOEnv
mkEnv simPath' cktPath' pdkPath' = do
    simPath <- reflect $ T.pack simPath'
    cktPath <- reflect $ T.pack cktPath'
    pdkPath <- reflect [ T.pack $ pdkPath' ]
    [java| SingleEndedOpampEnvironment.get($simPath, $cktPath, $pdkPath) |]

-- | Create Empty ACE Pool
mkPool :: IO Pool
mkPool = [java| new EnvironmentPool() |]

-- | Available Simulation Analyses
analyses :: SEOEnv -> IO [String]
analyses env = do
    as :: [T.Text] <- [java|
{
    java.util.Set<String> as = $env.getAnalyses();
    String [] analyses = as.toArray(new java.lang.String[as.size()]);
    return analyses;
}
                      |] >>= reify
    pure $ map T.unpack as

-- | Set sizing paramters in Environment
setSizing :: SEOEnv -> M.Map String Float -> IO ()
setSizing env sizing = do
    keys  <- reflect . M.keys  . M.mapKeys T.pack   $ sizing
    vals  <- reflect . M.elems . M.map float2Double $ sizing
    _ <- [java|
{
    for (int i = 0; i < $keys.length; i++) 
        { $env.set($keys[i], $vals[i]); }

    return 0.0;
}
         |] :: IO Double
    pure ()

-- | Get current circuit performance
currentPerformance :: SEOEnv -> IO (M.Map String Float)
currentPerformance env = do
    keys' :: [T.Text] <- [java| 
{
    java.util.Map<String, Double> res = $env.getPerformanceValues();
    java.lang.String[] keys = res.keySet().toArray(new java.lang.String[res.size()]);
    return keys;
}
                         |] >>= reify
    vals' :: [Double] <- [java| 
{
    java.util.Map<String, Double> res = $env.getPerformanceValues();
    java.lang.Double[] vals = res.values().toArray(new java.lang.Double[res.size()]);
    return vals;
}
                         |] >>= reify
    pure . M.fromList $ zipWith map' keys' vals'

-- | Simulate Environment with given analyses blacklist, corners and sizing
-- parameters
simulate' :: SEOEnv -> [String] -> [String] -> M.Map String Float 
          -> IO (M.Map String Float)
simulate' env blackList corners sizing = do 
    setSizing env sizing

    black <- reflect $ map T.pack blackList
    corns <- reflect $ map T.pack corners
    _  <- [java|
{
    java.util.HashSet<String> blackList = new java.util.HashSet<>();
    java.util.HashSet<String> corners   = new java.util.HashSet<>();

    for (int i = 0; i < $black.length; i++) 
        { blackList.add($black[i]); }
    for (int i = 0; i < $corns.length; i++) 
        { corners.add($corns[i]); }

    return $env.simulate();
}
          |] :: IO ACEnv
    currentPerformance env

-- | Simulate Environment for default corners with empty black list
evaluate :: SEOEnv -> M.Map String Float -> IO (M.Map String Float)
evaluate env = simulate' env [] []

-- | Add a list of Environments to Pool
poolAdd :: Pool -> [SEOEnv] -> IO Pool
poolAdd pool []         = pure pool
poolAdd pool (env:envs) = do
    _ <- [java|
{
    $pool.add($env);
    return $pool;
}
         |] :: IO Pool
    poolAdd pool envs

-- | Simulate list of Environments in parallel
evaluatePool :: [SEOEnv] -> [M.Map String Float] -> IO [M.Map String Float]
evaluatePool envs sizings = do
    mapM_ (uncurry setSizing) $ zip envs sizings
    pool <- mkPool >>= (flip poolAdd envs)
    _ <- [java|
{
    $pool.execute();
    return 0.0;
}
         |] :: IO Double
    mapM currentPerformance envs

-- | Get available performance parameters
performanceIds :: SEOEnv -> IO [String]
performanceIds env = do 
    pids :: [T.Text] <- [java|
{
    java.util.Set<String> pid_ = $env.getPerformanceIdentifiers();
    String[] pid = pid_.toArray(new String[pid_.size()]); 
    return pid;
}
                        |] >>= reify
    pure $ map T.unpack pids

-- | All netlist Parameters
parameters :: SEOEnv -> IO [String]
parameters env = do
    params :: [T.Text] <- [java|
{
    java.util.Set<String> ps = $env.getParameters().keySet();
    String [] params = ps.toArray(new java.lang.String[ps.size()]);
    return params;
}
                          |] >>= reify
    pure $ map T.unpack params

-- | Sizing Parameters
sizingParameters :: SEOEnv -> IO [String]
sizingParameters env = filter ((`elem` ['W', 'L', 'M']) . head) <$> parameters env

-- | Initial Sizing Parameters for given Environment
initialSizing :: SEOEnv -> IO (M.Map String Float)
initialSizing env = do
    keys :: [T.Text] <- [java|
{
    java.util.Map<String,Double> sizing = $env.getInitialSizingParameters();
    String [] keys = sizing.keySet().toArray(new java.lang.String[sizing.size()]);
    return keys;
}
                        |] >>= reify
    vals :: [Double] <- [java|
{
    java.util.Map<String,Double> sizing = $env.getInitialSizingParameters();
    Double [] vals = sizing.values().toArray(new java.lang.Double[sizing.size()]);
    return vals;
}
                        |] >>= reify
    pure . M.fromList $ zipWith map' keys vals

-- | Random sizing paramters for given Environment
randomSizing :: SEOEnv -> IO (M.Map String Float)
randomSizing env = do
    keys :: [T.Text] <- [java|
{
    java.util.Map<String,Double> sizing = $env.getRandomSizingParameters();
    String [] keys = sizing.keySet().toArray(new java.lang.String[sizing.size()]);
    return keys;
}
                        |] >>= reify
    vals :: [Double] <- [java|
{
    java.util.Map<String,Double> sizing = $env.getRandomSizingParameters();
    Double [] vals = sizing.values().toArray(new java.lang.Double[sizing.size()]);
    return vals;
}
                        |] >>= reify
    pure . M.fromList $ zipWith map' keys vals

-- | Current sizing of the Environment
currentSizing :: SEOEnv -> IO (M.Map String Float)
currentSizing env = do
    keys :: [T.Text] <- [java|
{
    java.util.Map<String,Double> sizing = $env.getParameterValues();
    String [] keys = sizing.keySet().toArray(new java.lang.String[sizing.size()]);
    return keys;
}
                        |] >>= reify
    vals :: [Double] <- [java|
{
    java.util.Map<String,Double> sizing = $env.getParameterValues();
    Double [] vals = sizing.values().toArray(new java.lang.Double[sizing.size()]);
    return vals;
}
                        |] >>= reify
    pure . M.fromList $ zipWith map' keys vals
