{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings #-}
module Stackage.Curator.PrepareSourceDirs
    ( prepareSourceDirs
    ) where

import Data.Yaml (decodeFileEither, object, (.=), encodeFile)
import Stackage.Prelude hiding (proc)
import Stackage.PackageIndex (getAllCabalHashesPath)
import System.Directory
import System.FilePath
import System.Process.Typed
import Data.Git
import Data.Git.Ref (fromHex)
import Crypto.Hash (Digest, SHA512)
import Crypto.Hash.Conduit
import System.IO (withBinaryFile, IOMode (WriteMode))

prepareSourceDirs :: FilePath -- ^ build plan
                  -> IO ()
prepareSourceDirs buildPlanFP = do
    buildPlan <- decodeFileEither buildPlanFP >>= either throwM return

    let allPackages = foldMap (uncurry singletonMap) (bpTools buildPlan)
                   ++ map ppVersion (bpPackages buildPlan)

    let root = "packages"
    createDirectoryIfMissing True root

    -- Clean up unused paths
    alreadyExist <- runConduitRes
        $ sourceDirectory root
       .| filterMC (liftIO . doesDirectoryExist)
       .| foldMapMC (\dir ->
            case simpleParse $ pack $ takeFileName dir of
                Just (PackageIdentifier name version)
                    | lookup name allPackages == Just version ->
                        return $ singletonMap name version
                _ -> do
                    liftIO $ removeDirectoryRecursive dir
                    return mempty)

    forM_ (mapToList $ allPackages `difference` alreadyExist)
        $ \(name, version) -> runProcess_ $ setWorkingDir root $ proc "stack"
            [ "unpack"
            , unpack $ display $ PackageIdentifier name version
            ]

    repoPath <- getAllCabalHashesPath

    -- FIXME ideally do this with hit as well
    -- Get the SHA for the current-hackage commit
    current <- withProcess_
        (setStdout createSource $ setWorkingDir repoPath $ proc "git"
            [ "rev-parse"
            , "current-hackage"
            ])
        (\p -> fmap fromHex
             $ runConduit
             $ getStdout p .| takeWhileCE (/= 10) .| foldC)
    withRepo (fromString repoPath) $ \git -> do
        {- this would be nice
        mcurrent <- resolveRevision git currentName
        current <-
            case mcurrent of
                Nothing -> error "Could not find current-hackage"
                Just current -> return current
        -}
        forM_ (mapToList alreadyExist) $ \(name, version) -> do
            {-
            let cabalPath = map (entName . encodeUtf8)
                    [ display name
                    , display version
                    , display name ++ ".cabal"
                    ]
            mref <- resolvePath git current cabalPath
            ref <- case mref of
                Nothing -> error $ concat
                    [ "Could not find cabal file "
                    , show cabalPath
                    , " in current-hackage "
                    , show current
                    ]
                Just ref -> return ref
            -}
            let cabalPath =
                    unpack (display name) </>
                    unpack (display version) </>
                    unpack (display name) <.> "cabal"
                fp =
                    root </>
                    unpack (display (PackageIdentifier name version)) </>
                    unpack (display name) <.> "cabal"
                pc = setStdout createSource
                   $ setWorkingDir repoPath
                   $ proc "git" ["show", "current-hackage:" ++ cabalPath]
            digest1 <- withProcess_ pc $ \p ->
                        runConduit $ getStdout p .| sinkHash
            edigest2 <- tryAny $ hashFile fp
            case edigest2 :: Either SomeException (Digest SHA512) of
                Right digest2 | digest1 == digest2 -> return ()
                _ -> withBinaryFile fp WriteMode $ \h ->
                         runProcess_ $ setStdout (useHandleClose h) pc

    encodeFile "stack.yaml" $ object
        [ "resolver" .= ("ghc-" ++ display (siGhcVersion (bpSystemInfo buildPlan)))
        , "packages" .= map
            (\(name, version) -> root </> unpack
                (display (PackageIdentifier name version)))
            (mapToList allPackages)
        , "flags" .= foldMap
            (\(name, pp) ->
                let pc = ppConstraints pp
                    flags = pcFlagOverrides pc
                    flags' = asMap
                           $ mapFromList
                           $ map (\(k, v) -> (unFlagName k, v))
                           $ mapToList flags
                 in if null flags
                        then asMap mempty
                        else singletonMap (display name) flags')
            (mapToList (bpPackages buildPlan))
        ]
