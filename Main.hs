{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Network.Wreq
import qualified Network.Wreq.Session as S

main :: IO ()
main = S.withSession $ \sess -> do
  -- First request: tell the server to set a cookie
  S.get sess "http://httpbin.org/cookies/set?name=hi"

  -- Second request: the cookie should still be set afterwards.
  r <- S.post sess "http://httpbin.org/post" ["a" := (3 :: Int)]
  print $ r ^. responseCookie "name" . cookieValue
