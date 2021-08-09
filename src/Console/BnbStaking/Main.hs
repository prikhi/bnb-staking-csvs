{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{- | CLI application harness.

-}
module Console.BnbStaking.Main
    ( run
    , getArgs
    , Args(..)
    ) where
import           Data.Csv                       ( encodeDefaultOrderedByName )
import           Data.Version                   ( showVersion )
import           Network.HTTP.Req               ( defaultHttpConfig
                                                , runReq
                                                )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , argPos
                                                , cmdArgs
                                                , def
                                                , help
                                                , helpArg
                                                , name
                                                , program
                                                , summary
                                                , typ
                                                )

import           Console.BnbStaking.Api         ( getAllRewards )
import           Console.BnbStaking.Csv         ( convertReward )
import           Paths_bnb_staking_csvs         ( version )

import qualified Data.ByteString.Lazy.Char8    as LBS
import qualified Data.Text                     as T

-- | Run the executable - parsing args, making queries, & printing the
-- results.
run :: Args -> IO ()
run Args {..} =
    runReq defaultHttpConfig (getAllRewards $ T.pack argPubKey)
        >>= mapM convertReward
        >>= (LBS.putStr . encodeDefaultOrderedByName)


-- | CLI arguments supported by the executable.
data Args = Args
    { argPubKey :: String
    -- ^ BinanceChain account's pubkey.
    }
    deriving (Show, Read, Eq, Data, Typeable)

-- | Parse the CLI arguments with 'System.Console.CmdArgs'.
getArgs :: IO Args
getArgs = cmdArgs argSpec

argSpec :: Args
argSpec =
    Args { argPubKey = def &= argPos 0 &= typ "ACCOUNT_PUBKEY" }
        &= summary
               (  "bnb-staking-csvs v"
               <> showVersion version
               <> ", Pavan Rikhi 2021"
               )
        &= program "bnb-staking-csvs"
        &= helpArg [name "h"]
        &= help "Generate CSV Exports of you BinanceChain Staking Rewards."
