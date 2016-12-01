{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Server.Sideloaded where

import Control.Monad.IO.Class

import Data.Aeson.Versions
import Data.Aeson.Versions.Internal
import Data.Aeson.Versions.Sideload

import qualified Data.ByteString as BS

import Data.CaseInsensitive

import Data.Maybe

import Data.Proxy

import Data.Singletons.Prelude hiding ((:>))

import Data.String

import GHC.TypeLits

import Network.HTTP.Types

import Servant.API
import Servant.API.ContentTypes
import Servant.API.ContentTypes.AesonVersioned hiding (Map)

import Servant.Server.Internal

import Snap.Core hiding (Method)

data Sideloaded (s :: Symbol)

methodRouterSideloaded :: forall ctypes a deps m env baseType.
                          (AllCTRender ctypes a
                          ,AllCTRender (Map (TyCon1 JSONVersioned) (Keys (SupportBase baseType))) (UsingSingle (Full deps a))
                          ,AllSatisfy (Ord' :.$$$ Id') deps
                          ,InflatableBase deps baseType a
                          ,MonadSnap m
                          ,InflateCBase a m
                          )
                       => Proxy deps -> Proxy baseType -> String -> Method -> Proxy ctypes -> Status
                       -> Delayed m env (m a)
                       -> Router m env -- Request (RoutingApplication m) m
methodRouterSideloaded _ bt hdr method proxy status action = leafRouter route'
  where
    -- route' :: env -> Request -> (RouteResult Response -> m a) -> m a
    route' env request respond =
      let accH = fromMaybe ct_wildcard $ getHeader hAccept request -- lookup hAccept $ requestHeaders request
          sideloadH = getHeader (fromString hdr) request

          inflatedProxy :: Proxy (Map (TyCon1 JSONVersioned) (Keys (SupportBase baseType)))
          inflatedProxy = Proxy

      in case sideloadH of
          Nothing -> runAction (action `addMethodCheck` methodCheck method request
                                       `addAcceptCheck` acceptCheck proxy accH
                               ) env request respond $ \ output -> do
            let handleA = handleAcceptH proxy  (AcceptHeader accH) output
            processMethodRouter handleA status method Nothing request
          _ -> runAction (action `actionBind` inflateP bt
                                 `addMethodCheck` methodCheck method request
                                 `addAcceptCheck` acceptCheck inflatedProxy accH
                         ) env request respond $ \ (output :: Full deps a) -> do
              let handleA = handleAcceptH inflatedProxy (AcceptHeader accH) (UsingSingle output)
              processMethodRouter handleA status method Nothing request

actionBind :: (Monad m) => Delayed m env (m a) -> (a -> m b) -> Delayed m env (m b)
actionBind Delayed{..} f =
  Delayed
    { serverD = \c a b r -> case serverD c a b r of
        Route mx -> Route $ mx >>= f
        Fail e -> Fail e
        FailFatal e -> FailFatal e
    , ..
    }


instance {-# OVERLAPPABLE #-} (AllCTRender ctypes a
                              ,AllCTRender (Map (TyCon1 JSONVersioned) (Keys (SupportBase b))) (UsingSingle (Full deps a))
                              ,AllSatisfy (Ord' :.$$$ Id') deps
                              ,InflatableBase deps b a
                              ,InflateCBase a m
                              ,ReflectMethod method
                              ,KnownNat status
                              ,KnownSymbol hdr)
                              => HasServer (Verb method status ctypes a :> Sideloaded hdr) ctx m where
  type ServerT (Verb method status ctypes a :> (Sideloaded hdr)) m = m a

  route Proxy _ = methodRouterSideloaded (Proxy :: Proxy deps) (Proxy :: Proxy b) header method (Proxy :: Proxy ctypes) status

    where method = reflectMethod (Proxy :: Proxy method)
          status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)
          header = symbolVal (Proxy :: Proxy hdr)

{-
instance {-# OVERLAPPABLE #-} (AllCTRender ctypes (t a)
                              ,AllCTRender (Map (TyCon1 JSONVersioned) (Keys (SupportBase a))) (Full deps (t a))
                              ,AllSatisfy (Ord' :.$$$ Id') deps
                              ,InflatableBase deps a (t a)
                              ,Foldable t
                              ,Functor t
                              ,Monoid (DependenciesList deps)
                              ,ReflectMethod method
                              ,KnownNat status)
                              => HasServer (Verb method status ctypes (t a) :> Sideloaded) ctx m where
  type ServerT (Verb method status ctypes (t a) :> Sideloaded) m = m (t a)

  route Proxy _ = methodRouterSideloaded (Proxy :: Proxy deps) (Proxy :: Proxy a) method (Proxy :: Proxy ctypes) status

    where method = reflectMethod (Proxy :: Proxy method)
          status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)
-}
