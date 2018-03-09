# PureScript Makkori

[![Build Status](https://travis-ci.org/justinwoo/purescript-makkori.svg?branch=master)](https://travis-ci.org/justinwoo/purescript-makkori)

Simple bindings to Express made for easy interop.

![](https://i.imgur.com/VK9ADpH.png)

This is an incomplete library. If you find that something is missing, make some FFI/coerced functions to work with and make some issues or PRs. Or make your own library, that's fine too.

## Example

```hs
main = do
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
	  M.sendResponse "POST OK" res))
    app

  M.close mempty server
```
