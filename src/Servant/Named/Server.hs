{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- | This module just exports orphan instances to make named-servant
-- work with servers
module Servant.Named.Server () where
import Servant.API
import Servant.Server
import Servant.Named
import Data.Proxy
import GHC.TypeLits
import Named

instance (KnownSymbol sym, FromHttpApiData a, HasServer api context)
      => HasServer (NamedQueryParams sym a :> api) context where

  type ServerT (NamedQueryParams sym a :> api) m =
    sym :! [a] -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy (QueryParams sym a :> api)) context $
    fmap (\f x -> f (Arg x)) subserver

instance (KnownSymbol sym, FromHttpApiData a, HasServer api context)
      => HasServer (NamedQueryParam' mods sym a :> api) context where

  type ServerT (NamedQueryParam' mods sym a :> api) m =
    sym :! [a] -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy (QueryParams sym a :> api)) context $
    fmap (\f x -> f (Arg x)) subserver
