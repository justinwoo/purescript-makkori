module Test.Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Uncurried as EU
import Foreign (readString)
import Foreign.Index (readProp)
import Makkori as M
import Milkis as Milkis
import Milkis.Impl.Node (nodeFetch)
import Test.Unit (failure, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Unsafe.Coerce (unsafeCoerce)

fetch :: Milkis.Fetch
fetch = Milkis.fetch nodeFetch

foreign import ffiGet :: EU.EffectFn3 M.Path M.Handler M.App Unit

testResponse url options status expected = do
  response <- fetch url options
  text <- Milkis.text response
  Assert.equal (unsafeCoerce response).status status
  Assert.equal text expected

main :: Effect Unit
main = runTest do
  suite "makkori" do
    test "server test" do
      ref <- liftEffect $ Ref.new mempty
      server <- liftEffect do
        app <- M.makeApp
        json <- M.makeJSONMiddleware {}
        M.use (M.Path "/") json app
        EU.runEffectFn3
          ffiGet
          (M.Path "/get-test")
          (M.makeHandler
            (\req res -> do
                M.setHeader "Content-Type" "text/html" res
                M.sendResponse "GET OK" res))
          app
        M.post
          (M.Path "/post-test")
          (M.makeHandler
            (\req res -> do
                body <- (readString <=< readProp "a") <$> M.getBody req
                Ref.write (pure $ runExcept body) ref
                M.sendResponse "POST OK" res))
          app
        M.get
          (M.Path "/400")
          (M.makeHandler
            (\req res -> do
               M.setStatus 400 res
               M.sendResponse "ASDF" res))
          app
        M.listen (M.Port 9999) mempty app

      testResponse
        (Milkis.URL "http://localhost:9999/post-test")
        { method: Milkis.postMethod
        , body: """{ "a" : "apple" }"""
        , headers: Milkis.makeHeaders { "Content-Type": "application/json" }
        }
        200 "POST OK"
      testResponse (Milkis.URL "http://localhost:9999/get-test") { method: Milkis.getMethod } 200 "GET OK"
      testResponse (Milkis.URL "http://localhost:9999/400") { method: Milkis.getMethod } 400 "ASDF"

      value <- liftEffect $ Ref.read ref
      case value of
        Just (Right s) -> assert "POST got the right request body" (s == "apple")
        _ -> failure "failed to get request body in POST"
      liftEffect $ M.close mempty server

