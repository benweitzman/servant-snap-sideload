{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Servant.Server.SideloadedSpec where

import Control.Monad

import Data.Aeson
import qualified Data.Aeson as A
import Data.Aeson.Versions
import Data.Aeson.Versions.Sideload

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

import Data.Maybe

import Data.Proxy

import Data.Tagged

import qualified Data.Text as T

import Network.HTTP.Types (hContentType, hAccept)
import qualified Network.HTTP.Types

import Servant.API
import Servant.API.ContentTypes.AesonVersioned

import Servant.Server
import Servant.Server.Internal
import Servant.Server.Sideloaded

import Snap.Core hiding (Headers, addHeader)
import qualified Snap.Core as SC

import Snap.Snaplet
import qualified Snap.Snaplet.Test as SST
import qualified Snap.Test as ST

import Test.Hspec
import qualified Test.HUnit as HU

data App = App
type AppHandler = Handler App App

app :: SnapletInit App App
app = app' []

app' :: [(B8.ByteString, AppHandler ())] -> SnapletInit App App
app' rs = makeSnaplet "servantsnap" "A test app for servant-snap" Nothing $ do
    addRoutes rs
    return App

type SideloadAPI =
         "foo" :> Versioned Get '[] Vfile :> Sideloaded "X-Sideload"
    :<|> "foos" :> Versioned Get '[] [Vfile] :> Sideloaded "X-Sideload"
--    :<|>  "bah" :> Get '[JSON] Int

sideloadApi :: Proxy SideloadAPI
sideloadApi = Proxy

sideloadApiServer :: (Monad m) => Server SideloadAPI m
sideloadApiServer =
       return someVfile
  :<|> return [someVfile]
--  :<|> return 3

runReq :: B8.ByteString
       -> [Network.HTTP.Types.Header]
       -> IO (Either T.Text Response)
runReq url headers = runReqOnApi sideloadApi sideloadApiServer SC.GET url "" headers ""

spec :: Spec
spec = do
  describe "sideloaded servant" $ do
    it "can be bypassed" $ do
      resp <- runReq "/foo" [("Accept", "application/json;version=1.0")]
      resp `shouldDecodeTo` (toJSON (Tagged someVfile :: Tagged V1 Vfile))
    it "sideloads single" $ do
      resp <- runReq "/foo" [("Accept", "application/json;version=1.0"), ("X-Sideload", "True")]
      inflated <- inflate someVfile
      let Just val = mToJSON (Tagged inflated :: Tagged V1 _)
      resp `shouldDecodeTo` val
    it "sideloads multiple" $ do
      resp <- runReq "/foos" [("Accept", "application/json;version=1.0"), ("X-Sideload", "True")]
      inflated <- inflate [someVfile]
      let Just val = mToJSON (Tagged inflated :: Tagged V1 _)
      resp `shouldDecodeTo` val



-------------------------
-- TEST DATA/INSTANCES --
-------------------------

full :: Full '[User, Media] Vfile
full = undefined

x = mToJSON (Tagged full :: Tagged V1 _)

newtype UserId = UserId Integer deriving (Ord, Eq, Show)
newtype VfileId = VfileId Integer deriving (Ord, Eq, Show)
newtype MediaId = MediaId Integer deriving (Ord, Eq, Show)

instance ToJSON UserId where
    toJSON (UserId uid) = Number (fromInteger uid)

instance ToJSON MediaId where
    toJSON (MediaId pid) = Number (fromInteger pid)

instance ToJSON VfileId where
    toJSON (VfileId pid) = Number (fromInteger pid)


data User = User { userId :: UserId
                 , userName :: String
                 } deriving (Eq, Show)

type instance Id User = UserId
type instance EntityName User = "User"

data Media = Media { mediaId :: MediaId
                   , mediaOwner :: UserId
                   , mediaCaption :: String
                   } deriving (Eq, Show)

type instance Id Media = MediaId
type instance EntityName Media = "Media"

instance ToJSON (Tagged V1 Media) where
    toJSON (Tagged (Media mid pid cap)) = object [ "mediaId" .= mid
                                                 , "ownerId" .= pid
                                                 , "caption" .= cap
                                                 ]

instance SerializedVersion Media where
  type SerializerVersions Media = '[V1]

instance Inflatable '[User] Media where
    type Support Media = '[ '(V1, '[ '( Media, V1)
                                   , '( User, V1)
                                   ]
                             )
                          , '(V2, '[ '( Media, V1)
                                   , '( User, V1)
                                   ]
                             )
                          ]

    dependencies (Media mid pid cap) = [pid] :-: DependenciesNil

    inflaters _ = inflatePerson :^: InflateNil
        where inflatePerson pid = return . Just $ User pid "ben"


instance ToJSON (Tagged V1 User) where
    toJSON (Tagged (User pid name)) = object [ "id" .= pid
                                             , "name" .= name
                                             ]

instance SerializedVersion User where
  type SerializerVersions User = '[V1]


data Vfile = Vfile { vfileId :: VfileId
                   , vfileOwner :: UserId
                   , vfileTitle :: String
                   , vfileMedia :: [MediaId]
                   } deriving (Eq, Show)

instance ToJSON (Tagged V1 Vfile) where
    toJSON (Tagged (Vfile vid mid title mids)) = object [ "vfileId" .= vid
                                                        , "ownerId" .= mid
                                                        , "title" .= title
                                                        , "mediaIds" .= mids
                                                        ]

instance SerializedVersion Vfile where
  type SerializerVersions Vfile = '[V1]

type instance Id Vfile = VfileId
type instance EntityName Vfile = "Vfile"

instance Inflatable '[User, Media] Vfile where
    type Support Vfile = '[ '(V1, '[ '(Vfile, V1), '(User, V1), '(Media, V1)])]

    dependencies (Vfile _ pid _ mids) = [pid] :-: mids :-: DependenciesNil

    inflaters _ = inflatePerson :^: inflateMedia :^: InflateNil
        where inflatePerson pid = return . Just $ User pid "ben"
              inflateMedia mid = return . Just $ Media mid (UserId 1) "caption"


data FeedEvent = LikedVfile UserId VfileId
               | FiledMedia UserId MediaId VfileId deriving (Eq, Show)

instance ToJSON (Tagged V1 FeedEvent) where
  toJSON (Tagged (LikedVfile uid vid)) = object [ "type" .= ("LikedVfile" :: String)
                                                , "data" .= object
                                                  [ "userId" .= uid
                                                  , "vfileId" .= vid
                                                  ]
                                                ]
  toJSON (Tagged (FiledMedia uid mid vid)) = object [ "type" .= ("FiledMedia" :: String)
                                                    , "data" .= object
                                                      [ "userId" .= uid
                                                      , "mediaId" .= mid
                                                      , "vfileId" .= vid
                                                      ]
                                                    ]

instance Inflatable '[User, Media, Vfile] FeedEvent where
  type Support FeedEvent = '[ '(V1, '[ '(FeedEvent, V1)
                                     , '(User, V1)
                                     , '(Media, V1)
                                     , '(Vfile, V1)
                                     ])]
  dependencies (LikedVfile uid vid) = [uid] :-: [] :-: [vid] :-: DependenciesNil
  dependencies (FiledMedia uid mid vid) = [uid] :-: [mid] :-: [vid] :-: DependenciesNil


  inflaters _ = inflatePerson :^: inflateMedia :^: inflateVfile :^: InflateNil
      where inflatePerson pid = return . Just $ User pid "ben"
            inflateMedia mid = return . Just $ Media mid (UserId 1) "caption"
            inflateVfile vid = return . Just $ Vfile vid (UserId 1) "vfile caption" [MediaId 1]

someMedia :: Media
someMedia = Media (MediaId 1) (UserId 1) "caption"

someVfile :: Vfile
someVfile = Vfile (VfileId 1) (UserId 1) "vfile title" [MediaId 1, MediaId 2]

someFeedEvents :: [FeedEvent]
someFeedEvents = [LikedVfile (UserId 1) (VfileId 1)
                 ,FiledMedia (UserId 1) (MediaId 1) (VfileId 1)
                 ]

-------------------
-- HSPEC HELPERS --
-------------------

shouldDecodeTo :: (FromJSON a, Eq a, Show a)
               => Either T.Text Response
               -> a
               -> IO ()
shouldDecodeTo (Left e) _ = HU.assertFailure $
                            "Failed to respond: " ++ T.unpack e
shouldDecodeTo (Right resp) a = do
  bod <- ST.getResponseBody resp
  case A.decode' $ BL.fromStrict bod of
    Just x | x == a -> return ()
    Just _ -> HU.assertFailure $
              "Failed to decode response to " ++ show a ++
              " from body: " ++ B8.unpack bod
    Nothing -> HU.assertFailure $
               "Failed to decode respone from body: " ++
               B8.unpack bod ++ "\nResponse: " ++ show resp

----------------------------
-- ASSORTRED SNAP HELPERS --
----------------------------

mkInitAndServerWithContext :: HasServer api ctx AppHandler
                           => Context ctx
                           -> Proxy (api :: *)
                           -> Server api AppHandler
                           -> (SnapletInit App App, AppHandler ())
mkInitAndServerWithContext ctx api serv = let sRoute = serveSnapWithContext api ctx serv
                                          in  (app' [("", sRoute)], sRoute)

mkInitAndServer :: HasServer api '[] AppHandler
                => Proxy (api :: *)
                -> Server api AppHandler
                -> (SnapletInit App App, AppHandler ())
mkInitAndServer = mkInitAndServerWithContext EmptyContext

mkRequest :: Method
          -> B8.ByteString
          -> B8.ByteString
          -> [Network.HTTP.Types.Header]
          -> B8.ByteString
          -> ST.RequestBuilder IO ()
mkRequest mth pth qs hds bdy = do
  let ct = fromMaybe "" (Prelude.lookup hContentType hds)
  ST.postRaw pth ct bdy
  ST.setQueryStringRaw qs
  unless (mth == SC.POST) $ ST.setRequestType (ST.RequestWithRawBody mth bdy)
  forM_ hds (\(k, v) -> unless (k == hContentType) $ ST.addHeader k v)
  -- req <- State.get -- Useful for debugging
  -- liftIO $ print req

runReqOnApiWithContext :: HasServer api ctx AppHandler
                       => Context ctx
                       -> Proxy (api :: *)
                       -> Server api AppHandler
                       -> Method
                       -> B8.ByteString
                       -> B8.ByteString
                       -> [Network.HTTP.Types.Header]
                       -> B8.ByteString
                       -> IO (Either T.Text Response)
runReqOnApiWithContext ctx api serv method route qs hds bod =
  let (sInit, serv') = mkInitAndServerWithContext ctx api serv
  in SST.runHandler Nothing (mkRequest method route qs hds bod) serv' sInit

runReqOnApi :: HasServer api '[] AppHandler
            => Proxy (api :: *)
            -> Server api AppHandler
            -> Method
            -> B8.ByteString
            -> B8.ByteString
            -> [Network.HTTP.Types.Header]
            -> B8.ByteString
            -> IO (Either T.Text Response)
runReqOnApi = runReqOnApiWithContext EmptyContext

routes :: HasServer api '[] AppHandler
       => Proxy (api :: *)
       -> Server api AppHandler
       -> [(B8.ByteString, AppHandler ())]
routes p s = [("", serveSnap p s)]
