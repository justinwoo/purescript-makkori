module Makkori where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Uncurried as EU
import Foreign (Foreign)
import Node.HTTP (Server)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)

foreign import data App :: Type
foreign import data Middleware :: Type

foreign import data Request :: Type
foreign import data Response :: Type

type Handler = EU.EffectFn2 Request Response Unit

makeHandler :: (Request -> Response -> Effect Unit) -> Handler
makeHandler = EU.mkEffectFn2

newtype Path = Path String
derive instance newtypePath :: Newtype Path _

newtype Port = Port Int
derive instance newtypePort :: Newtype Port _

makeApp :: Effect App
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

registerMethod :: Method -> Path -> Handler -> App -> Effect Unit
registerMethod = EU.runEffectFn4 _registerMethod

get :: Path -> Handler -> App -> Effect Unit
get = registerMethod getMethod

post :: Path -> Handler -> App -> Effect Unit
post = registerMethod postMethod

put :: Path -> Handler -> App -> Effect Unit
put = registerMethod putMethod

delete :: Path -> Handler -> App -> Effect Unit
delete = registerMethod deleteMethod

type StaticOptions =
  (
  )

makeStaticMiddleware
  :: forall options trash
   . Row.Union options trash StaticOptions
  => Path
  -> Record options
  -> Effect Middleware
makeStaticMiddleware = EU.runEffectFn2 _makeStaticMiddleware

type JSONOptions =
  (
  )

makeJSONMiddleware
  :: forall options trash
   . Row.Union options trash JSONOptions
  => Record options
  -> Effect Middleware
makeJSONMiddleware = EU.runEffectFn1 _makeJSONMiddleware

use :: Path -> Middleware -> App -> Effect Unit
use = EU.runEffectFn3 _use

listen :: Port -> Effect Unit -> App -> Effect Server
listen = EU.runEffectFn3 _listen

close :: Effect Unit -> Server -> Effect Unit
close = EU.runEffectFn2 _close

sendResponse :: String -> Response -> Effect Unit
sendResponse = EU.runEffectFn2 _sendResponse

setHeader :: String -> String -> Response -> Effect Unit
setHeader = EU.runEffectFn3 _set

setStatus :: Int -> Response -> Effect Unit
setStatus = EU.runEffectFn2 _setStatus

getBody :: Request -> Effect Foreign
getBody = EU.runEffectFn1 _getBody

getParams :: Request -> Effect Foreign
getParams = EU.runEffectFn1 _getParams

foreign import _makeApp :: Effect App
foreign import _registerMethod :: EU.EffectFn4 Method Path Handler App Unit
foreign import _makeStaticMiddleware :: forall options. EU.EffectFn2 Path options Middleware
foreign import _makeJSONMiddleware :: forall options. EU.EffectFn1 options Middleware
foreign import _use :: EU.EffectFn3 Path Middleware App Unit
foreign import _listen :: EU.EffectFn3 Port (Effect Unit) App Server
foreign import _close :: EU.EffectFn2 (Effect Unit) Server Unit
foreign import _sendResponse :: EU.EffectFn2 String Response Unit
foreign import _set :: EU.EffectFn3 String String Response Unit
foreign import _setStatus :: EU.EffectFn2 Int Response Unit
foreign import _getBody :: EU.EffectFn1 Request Foreign
foreign import _getParams :: EU.EffectFn1 Request Foreign
