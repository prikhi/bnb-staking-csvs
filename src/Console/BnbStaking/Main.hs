{- | CLI application harness.

-}
module Console.BnbStaking.Main
    ( run
    ) where
import           Data.Csv                       ( encodeDefaultOrderedByName )
import           Network.HTTP.Req               ( defaultHttpConfig
                                                , runReq
                                                )
import           System.Environment             ( getArgs )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

import           Console.BnbStaking.Api         ( getAllRewards )
import           Console.BnbStaking.Csv         ( convertReward )

import qualified Data.ByteString.Lazy.Char8    as LBS
import qualified Data.Text                     as T

-- | Run the executable - parsing args, making queries, & printing the
-- results.
run :: IO ()
run = getArgs >>= \case
    [pubKey] ->
        runReq defaultHttpConfig (getAllRewards $ T.pack pubKey)
            >>= mapM convertReward
            >>= LBS.putStr
            .   encodeDefaultOrderedByName
    _ -> hPutStrLn
        stderr
        "bnb-staking-csvs: expected 1 argument - <DELEGATOR_PUBKEY>"
