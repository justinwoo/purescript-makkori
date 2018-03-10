module Test.Main where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Eff.Uncurried as EU
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (readString)
import Data.Foreign.Index (readProp)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Makkori as M
import Milkis as Milkis
import Test.Unit (failure, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Unsafe.Coerce (unsafeCoerce)

foreign import ffiGet :: forall e. EU.EffFn3 e M.Path (M.Handler e) M.App Unit

testResponse url options status expected = do
  response <- Milkis.fetch url options
  text <- Milkis.text response
  Assert.equal (unsafeCoerce response).status status
  Assert.equal text expected

main :: _
main = runTest do
  suite "makkori" do
    test "server test" do
      ref <- liftEff $ newRef mempty
      server <- liftEff do
        app <- M.makeApp
        json <- M.makeJSONMiddleware {}
        M.use (M.Path "/") json app
        EU.runEffFn3
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
                writeRef ref $ pure $ runExcept body
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

      value <- liftEff $ readRef ref
      case value of
        Just (Right s) -> assert "POST got the right request body" (s == "apple")
        _ -> failure "failed to get request body in POST"
      liftEff $ M.close mempty server

