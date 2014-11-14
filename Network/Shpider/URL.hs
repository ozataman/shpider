{-
 -
 - Copyright (c) 2009-2010 Johnny Morrice
 -
 - Permission is hereby granted, free of charge, to any person
 - obtaining a copy of this software and associated documentation
 - files (the "Software"), to deal in the Software without
 - restriction, including without limitation the rights to use, copy,
 - modify, merge, publish, distribute, sublicense, and/or sell copies
 - of the Software, and to permit persons to whom the Software is
 - furnished to do so, subject to the following conditions:
 -
 - The above copyright notice and this permission notice shall be
 - included in all copies or substantial portions of the Software.
 -
 - THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 - EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 - MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 - NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 - BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 - ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 - CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 - SOFTWARE.
 -
-}

module Network.Shpider.URL
   ( module Network.URL
   , isSameDomain
   , mkAbsoluteUrl
   , isAbsoluteUrl
   , isMailto
   , isHttp
   , getDomain
   , getFolder
   )
   where

import           Control.Monad.State
import           Network.Shpider.State
import           Network.Shpider.TextUtils
import           Network.URL
import           Text.Regex.Posix

-- | is the second url on the same domain as the first? Note: this will return False if either URL is invalid.
isSameDomain :: String -> String -> Bool
isSameDomain urlYourOn urlYourNot =
   let url1 =
          importURL urlYourOn
       url2 =
          importURL urlYourNot
   in
   maybe False
         ( \ t1 ->
              maybe False
                    ( \ t2 ->
                         let urlType1 = url_type t1
                             urlType2 = url_type t2
                             isRelative ut = ( ut == PathRelative ) || ( ut == HostRelative )
                             ut1Relative = isRelative urlType1
                             ut2Relative = isRelative urlType2
                             utsSame = urlType1 == urlType2
                         in
                         ( ut1Relative || ut2Relative || utsSame )
                    )
                    url2
         )
         url1

-- | Assumes the given URL is relative to `currentPage`.
mkAbsoluteUrl :: String -> Shpider ( Maybe String )
mkAbsoluteUrl uncleanUrl = do
   if not $ isMailto uncleanUrl
      then do
         shpider <- get
         let cleanUrlStr = escapeSpaces uncleanUrl
             maybeUrl = importURL cleanUrlStr
             currentAddr = addr $ currentPage shpider
         maybe ( return Nothing )
               ( \ url ->
                    case url_type url of
                       PathRelative ->
                          return $ Just $ getFolder currentAddr ++ urlStr
                       HostRelative ->
                          return $ Just $ getDomain currentAddr ++ urlStr
                       _ ->
                          return $ Just urlStr
               )
               maybeUrl
      else
         return Nothing
   where
   urlStr =
      escapeSpaces uncleanUrl

-- | True if the url is absolute
isAbsoluteUrl :: String -> Bool
isAbsoluteUrl urlStr =
   case importURL urlStr of
      Just url ->
         case url_type url of
            Absolute _ ->
               True
            _ ->
               False
      _ ->
         False

-- | is the given string of form \"mailto:person.com\"?
isMailto :: String -> Bool
isMailto =
   flip (=~) ("mailto:.*" :: String)

-- | is the url a http url?
isHttp :: String -> Bool
isHttp =
    flip (=~) ("(http://|https://).*" :: String)

-- | Get the protocol and domain from a URL eg
--
-- @
--    getDomain \"widdle:\/\/owqueer.co.uk\/strangeanticsofsailors\/jimmy\"
--    -- \"widdle:\/\/owqueer.co.uk\"
-- @
getDomain :: String -> String
getDomain =
   flip (=~) ("[^:]+://[^/]+" :: String)

-- | Get the whole url up to and including the current folder of the present document.
--
-- @
--    getFolder \"widdle:\/\/owqueer.co.uk\/strangeanticsofsailors\/jimmy\"
--    -- \"widdle:\/\/owqueer.co.uk\/strangeanticsofsailors\/\"
-- @
getFolder :: String -> String
getFolder url =
  let ms =
         url =~ ("[^/]*/[^/]*" :: String)
      l =
         length ms
  in
  if l > 0
     then
        ( concat $ take ( l - 1 ) $ map head ms ) ++ "/"
     else
        ""

