module Makkori where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, EffFn4, mkEffFn2, runEffFn1, runEffFn2, runEffFn3, runEffFn4)
import Data.Foreign (Foreign)
import Data.Newtype (class Newtype)
import Node.HTTP (Server)
import Unsafe.Coerce (unsafeCoerce)

foreign import data App :: Type
foreign import data Middleware :: Type

foreign import data Request :: Type
foreign import data Response :: Type

type Handler e = EffFn2 e Request Response Unit

makeHandler :: forall e. (Request -> Response -> Eff e Unit) -> Handler e
makeHandler = mkEffFn2

newtype Path = Path String
derive instance newtypePath :: Newtype Path _

newtype Port = Port Int
derive instance newtypePort :: Newtype Port _

makeApp :: forall e. Eff e App
makeApp = _makeApp

foreign import data Method :: Type

getMethod :: Method
getMethod = unsafeCoerce "get"

putMethod :: Method
putMethod = unsafeCoerce "put"

postMethod :: Method
postMethod = unsafeCoerce "post"

deleteMethod :: Method
deleteMethod = unsafeCoerce "delete"

registerMethod :: forall e. Method -> Path -> Handler e -> App -> Eff e Unit
registerMethod = runEffFn4 _registerMethod

get :: forall e. Path -> Handler e -> App -> Eff e Unit
get = registerMethod getMethod

post :: forall e. Path -> Handler e -> App -> Eff e Unit
post = registerMethod postMethod

put :: forall e. Path -> Handler e -> App -> Eff e Unit
put = registerMethod putMethod

delete :: forall e. Path -> Handler e -> App -> Eff e Unit
delete = registerMethod deleteMethod

type StaticOptions =
  (
  )

makeStaticMiddleware
  :: forall options trash e
   . Union options trash StaticOptions
  => Path
  -> Record options
  -> Eff e Middleware
makeStaticMiddleware = runEffFn2 _makeStaticMiddleware

type JSONOptions =
  (
  )

makeJSONMiddleware
  :: forall options trash e
   . Union options trash JSONOptions
  => Record options
  -> Eff e Middleware
makeJSONMiddleware = runEffFn1 _makeJSONMiddleware

use :: forall e. Path -> Middleware -> App -> Eff e Unit
use = runEffFn3 _use

listen :: forall e. Port -> Eff e Unit -> App -> Eff e Server
listen = runEffFn3 _listen

close :: forall e. Eff e Unit -> Server -> Eff e Unit
close = runEffFn2 _close

sendResponse :: forall e. String -> Response -> Eff e Unit
sendResponse = runEffFn2 _sendResponse

getBody :: forall e. Request -> Eff e Foreign
getBody = runEffFn1 _getBody

foreign import _makeApp :: forall e. Eff e App
foreign import _registerMethod :: forall e. EffFn4 e Method Path (Handler e) App Unit
foreign import _makeStaticMiddleware :: forall e options. EffFn2 e Path options Middleware
foreign import _makeJSONMiddleware :: forall e options. EffFn1 e options Middleware
foreign import _use :: forall e. EffFn3 e Path Middleware App Unit
foreign import _listen :: forall e. EffFn3 e Port (Eff e Unit) App Server
foreign import _close :: forall e. EffFn2 e (Eff e Unit) Server Unit
foreign import _sendResponse :: forall e. EffFn2 e String Response Unit
foreign import _getBody :: forall e. EffFn1 e Request Foreign
