module Test.Main where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
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

testResponse url options expected = do
  response
      <- Milkis.text
    =<< Milkis.fetch url options
  Assert.equal response expected

main :: _
main = runTest do
  suite "makkori" do
    test "server test" do
      ref <- liftEff $ newRef mempty
      server <- liftEff do
        app <- M.makeApp
        server <- M.listen (M.Port 9999) mempty app
        json <- M.makeJSONMiddleware {}
        M.use (M.Path "/") json app
        M.get
          (M.Path "/get-test")
          (M.makeHandler (\_ res -> M.sendResponse "GET OK" res))
          app
        M.post
          (M.Path "/post-test")
          (M.makeHandler
            (\req res -> do
                body <- (readString <=< readProp "a") <$> M.getBody req
                writeRef ref $ pure $ runExcept body
                M.sendResponse "POST OK" res))
          app
        pure server

      testResponse
        (Milkis.URL "http://localhost:9999/post-test")
        { method: Milkis.postMethod
        , body: """{ "a" : "apple" }"""
        , headers: Milkis.makeHeaders { "Content-Type": "application/json" }
        }
        "POST OK"
      testResponse (Milkis.URL "http://localhost:9999/get-test") { method: Milkis.getMethod } "GET OK"

      value <- liftEff $ readRef ref
      case value of
        Just (Right s) -> assert "POST got the right request body" (s == "apple")
        _ -> failure "failed to get request body in POST"
      liftEff $ M.close mempty server

