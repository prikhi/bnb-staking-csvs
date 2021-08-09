{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{- | CLI application harness.

-}
module Console.BnbStaking.Main
    ( run
    , getArgs
    , Args(..)
    ) where
import           Data.Maybe                     ( fromMaybe )
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
                                                , explicit
                                                , help
                                                , helpArg
                                                , name
                                                , program
                                                , summary
                                                , typ
                                                )

import           Console.BnbStaking.Api         ( getAllRewards )
import           Console.BnbStaking.CoinTracking
                                                ( makeCoinTrackingImport )
import           Console.BnbStaking.Csv         ( makeCsvContents )
import           Paths_bnb_staking_csvs         ( version )

import qualified Data.ByteString.Lazy.Char8    as LBC
import qualified Data.Text                     as T

-- | Run the executable - parsing args, making queries, & printing the
-- results.
run :: Args -> IO ()
run Args {..} = do
    rewards <- runReq defaultHttpConfig $ getAllRewards $ T.pack argPubKey
    let outputFile = fromMaybe "-" argOutputFile
    if argCoinTracking
        then makeCoinTrackingImport outputFile argPubKey rewards
        else do
            output <- makeCsvContents rewards
            if outputFile == "-"
                then LBC.putStr output
                else LBC.writeFile outputFile output


-- | CLI arguments supported by the executable.
data Args = Args
    { argPubKey       :: String
    -- ^ BinanceChain account's pubkey.
    , argOutputFile   :: Maybe String
    -- ^ Optional output file. 'Nothing' or @'Just' "-"@ will print to
    -- 'System.IO.stdout'.
    , argCoinTracking :: Bool
    -- ^ Flag to enable writing/printing files formatted for CoinTracking
    -- Bulk Imports.
    }
    deriving (Show, Read, Eq, Data, Typeable)

-- | Parse the CLI arguments with 'System.Console.CmdArgs'.
getArgs :: IO Args
getArgs = cmdArgs argSpec

argSpec :: Args
argSpec =
    Args
            { argPubKey       = def &= argPos 0 &= typ "ACCOUNT_PUBKEY"
            , argOutputFile   =
                Nothing
                &= help "File to write the export to. Default: stdout"
                &= explicit
                &= name "output-file"
                &= name "o"
                &= typ "FILE"
            , argCoinTracking = False
                                &= help "Generate a CoinTracking Import file."
                                &= explicit
                                &= name "cointracking"
            }
        &= summary
               (  "bnb-staking-csvs v"
               <> showVersion version
               <> ", Pavan Rikhi 2021"
               )
        &= program "bnb-staking-csvs"
        &= helpArg [name "h"]
        &= help "Generate CSV Exports of you BinanceChain Staking Rewards."
