{-| Contains all the functionality of the app.

TODO: Should split out into BnbStaking sub-modules: Main, Binance, Csv

-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Lib
    ( run
      -- * CSV Data
    , ExportData(..)
    , convertReward
    , MyZonedTime(..)
      -- * Binance.org API
    , getAllRewards
    , makeRequest
    , Endpoint(..)
    , RewardResponse(..)
    , Reward(..)
    ) where

import           Control.Monad                  ( forM )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(..)
                                                , withObject
                                                )
import           Data.Csv                       ( DefaultOrdered
                                                , ToField(..)
                                                , ToNamedRecord
                                                , encodeDefaultOrderedByName
                                                )
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( fromMaybe )
import           Data.Scientific                ( FPFormat(..)
                                                , Scientific
                                                , formatScientific
                                                )
import           Data.Time                      ( UTCTime
                                                , ZonedTime(..)
                                                , defaultTimeLocale
                                                , formatTime
                                                , parseTimeM
                                                , utcToLocalZonedTime
                                                )
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Req               ( (/:)
                                                , (=:)
                                                , GET(..)
                                                , MonadHttp
                                                , NoReqBody(..)
                                                , Scheme(Https)
                                                , Url
                                                , defaultHttpConfig
                                                , https
                                                , jsonResponse
                                                , req
                                                , responseBody
                                                , runReq
                                                )
import           System.Environment             ( getArgs )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

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


-- CSV Data

-- | Datatype representing a single row in the CSV export.
data ExportData = ExportData
    { time             :: MyZonedTime
    -- ^ The time of the reward.
    , amount           :: T.Text
    -- ^ The reward amount.
    , currency         :: T.Text
    -- ^ Always @BNB@, but sometimes a useful column for CSV imports.
    , delegator        :: T.Text
    -- ^ The address that was rewarded.
    , validator        :: T.Text
    -- ^ The validator's name.
    , validatorAddress :: T.Text
    -- ^ The address the delegator is validating to.
    , height           :: Integer
    -- ^ The height of the reward's block.
    }
    deriving (Show, Read, Generic)

instance ToNamedRecord ExportData
instance DefaultOrdered ExportData

-- | Render a 'Reward' into our target export data by converting to
-- localtime(respecting DST), & formatting the amount column to 8 decimal
-- places.
convertReward :: Reward -> IO ExportData
convertReward Reward {..} = do
    localRewardTime <- utcToLocalZonedTime rRewardTime
    return $ ExportData
        { time             = MyZonedTime localRewardTime
        , amount           = T.pack $ formatScientific Fixed (Just 8) rReward
        , currency         = "BNB"
        , delegator        = rDelegator
        , validator        = rValidatorName
        , validatorAddress = rValidatorAddress
        , height           = rHeight
        }

-- | Wrapper type to support custom 'ToField' instance.
newtype MyZonedTime = MyZonedTime { fromMyZonedTime :: ZonedTime } deriving (Show, Read)

instance ToField MyZonedTime where
    toField (MyZonedTime zt) =
        toField $ formatTime defaultTimeLocale "%FT%T%Q%Ez" zt


-- Binance.org API

-- | Fetch all rewards for the given Delegator PubKey.
getAllRewards :: MonadHttp m => T.Text -> m [Reward]
getAllRewards pubKey = do
    let pageSize = Just 50
    initialResp <- makeRequest $ GetRewards pubKey pageSize Nothing
    let rewardCount = rrTotal initialResp
    remainingRewards <- if Just rewardCount < pageSize
        then return []
        else fmap concat . forM [50, 100 .. rewardCount] $ \(Just -> offset) ->
            rrRewards <$> makeRequest (GetRewards pubKey pageSize offset)
    return . sortResults $ rrRewards initialResp <> remainingRewards
  where
    sortResults :: [Reward] -> [Reward]
    sortResults = sortOn rRewardTime


-- | Represents all endpoints of the binance.org api, as well as their
-- respective response data.
data Endpoint a where
    GetRewards ::T.Text -> Maybe Integer -> Maybe Integer -> Endpoint RewardResponse

-- | Make a request to an endpoint.
makeRequest :: MonadHttp m => Endpoint a -> m a
makeRequest e = case e of
    GetRewards _ mbLimit mbOffset -> responseBody <$> req
        GET
        url
        NoReqBody
        jsonResponse
        (("limit" =: fromMaybe 20 mbLimit) <> ("offset" =: fromMaybe 0 mbOffset)
        )
  where
    url :: Url 'Https
    url = case e of
        GetRewards pubKey _ _ ->
            baseUrl
                /: "staking"
                /: "chains"
                /: "bsc"
                /: "delegators"
                /: pubKey
                /: "rewards"
    baseUrl :: Url 'Https
    baseUrl = https "api.binance.org" /: "v1"


-- | Response of requesting a delegator's rewards.
data RewardResponse = RewardResponse
    { rrTotal   :: Integer
    -- ^ Total number of rewards.
    , rrRewards :: [Reward]
    -- ^ Rewards in this page.
    }
    deriving (Show, Read, Eq, Generic)

instance FromJSON RewardResponse where
    parseJSON = withObject "RewardResponse" $ \o -> do
        rrTotal   <- o .: "total"
        rrRewards <- o .: "rewardDetails"
        return $ RewardResponse { .. }


-- | A single staking reward.
data Reward = Reward
    { rValidatorName    :: T.Text
    , rValidatorAddress :: T.Text
    , rDelegator        :: T.Text
    , rChainId          :: T.Text
    -- ^ Always @bsc@ at the moment - no testnet rewards supported.
    , rHeight           :: Integer
    , rReward           :: Scientific
    , rRewardTime       :: UTCTime
    }
    deriving (Show, Read, Eq, Generic)

instance FromJSON Reward where
    parseJSON = withObject "Reward" $ \o -> do
        rValidatorName    <- o .: "valName"
        rValidatorAddress <- o .: "validator"
        rDelegator        <- o .: "delegator"
        rChainId          <- o .: "chainId"
        rHeight           <- o .: "height"
        rReward           <- o .: "reward"
        rRewardTime       <- o .: "rewardTime" >>= parseTimeM True
                                                              defaultTimeLocale
                                                              "%FT%T%Q%Ez"
        return $ Reward { .. }
