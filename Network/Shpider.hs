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

{-# OPTIONS -XScopedTypeVariables #-}
-- | This module exposes the main functionality of shpider
-- It allows you to quickly write crawlers, and for simple cases even without reading the page source eg.
--
-- @
-- `runShpider` $ do
--    `download` \"http:\/\/hackage.haskell.org\/packages\/archive\/pkg-list.html\"
--    l : _ <- `getLinksByText` \"shpider\"
--    `download` $ linkAddress l
-- @
module Network.Shpider 
   ( module Network.Shpider.Code
   , module Network.Shpider.State
   , module Network.Shpider.URL
   , module Network.Shpider.Options
   , module Network.Shpider.Forms
   , module Network.Shpider.Links
   -- * Crawl Functions
   , download
   , sendForm
   -- * Basic Parsing/Decision Support
   , getLinksByText
   , getLinksByTextRegex
   , getLinksByAddressRegex
   , getFormsByAction
   , getFormsHasAction
   , currentLinks
   , currentForms
   -- * Utilities
   , parsePage
   , isAuthorizedDomain
   , withAuthorizedDomain
   , haveVisited
   ) 
   where

import           Control.Concurrent
import           Control.Monad.State
import qualified Data.Map                as M
import           Data.Maybe
import           Data.Time
import           Data.URLEncoded         (export, urlEncode)
import           Network.Curl
import           Network.Shpider.Code
import           Network.Shpider.Forms
import           Network.Shpider.Links
import           Network.Shpider.Options
import           Network.Shpider.State
import           Network.Shpider.URL
import           System.Directory
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup
import           Text.Regex.Posix

-- | if `keepTrack` has been set, then haveVisited will return `True` if the given URL has been visited.
haveVisited :: String -> Shpider Bool
haveVisited uncleanUrl = do
   murl <- mkAbsoluteUrl uncleanUrl
   maybe ( return False )
         ( \ url -> do
              shpider <- get
              return $ maybe False 
                             ( \ vs ->
                                  elem url vs
                             )
                             ( visited shpider )
         )
         murl

-- | Parse a given URL and source html into the `Page` datatype.
-- This will set the current page.
parsePage :: String -> String -> Shpider Page
parsePage paddr html = do
   let ts =
          parseTags html
       ls =
          gatherLinks ts
       fs =
          gatherForms ts
       nPge = emptyPage { addr = paddr }
   -- seems weird, but this is the side effect needed here to create the absolute urls next
   setCurrentPage nPge
   maybeAbsFormActions <- mapM mkAbsoluteUrl $ map action fs 
   maybeAbsLinkAddrs <- mapM mkAbsoluteUrl $ map linkAddress ls
   let absLinkAddrs = catMaybes maybeAbsLinkAddrs
       absFormActions = catMaybes maybeAbsFormActions
       absFs = zipWith ( \ form a -> form { action = a }) fs absFormActions 
       absLinks = zipWith ( \ laddr l -> l { linkAddress = laddr }) absLinkAddrs ls
       newP = 
          nPge { links = absLinks 
               , forms = absFs
               , source = html
               , tags = ts
               , addr = paddr
               }
   setCurrentPage newP
   return newP


-------------------------------------------------------------------------------
-- | Perform the given operation subject to blocking throttling based
-- on the last time there was a download.
withThrottle :: Shpider a -> Shpider a
withThrottle f = do
  let perform = do
        res <- f
        sh <- get
        now <- liftIO $ getCurrentTime
        put $ sh { lastDownloadTime = Just now }
        return res
  thOpt <- gets downloadThrottle
  lastD <- gets lastDownloadTime
  case thOpt of
    Nothing -> perform
    Just n -> do
      case lastD of 
        Nothing -> perform
        Just ld -> do
          th <- liftIO $ shouldThrottle n ld
          case th of
            Just x -> liftIO (threadDelay x) >> perform
            Nothing -> perform
            

-------------------------------------------------------------------------------
-- | Test whether we need to throttle
shouldThrottle :: Int -> UTCTime -> IO (Maybe Int)
shouldThrottle n lastTime = do
  now <- getCurrentTime
  let n' = fromIntegral n / 1000000
      diff = diffUTCTime now lastTime
      delta = round . (* 1000000) $ n' - diff
  return $  if delta > 0 then (Just delta) else Nothing


mkRes url ( curlCode , html ) = do
   p <- if curlCode == CurlOK
           then
              parsePage url html
           else
              return emptyPage
   return ( ccToSh curlCode , p )


validContentType :: String -> Bool
validContentType ct =
   or $ map ( \ htmlct ->
                 ct =~ htmlct
            )
            htmlContentTypes

htmlContentTypes :: [String]
htmlContentTypes =
   [ "text/html"
   , "application/xhtml+xml"
   ]

-- | Fetch whatever is at this address, and attempt to parse the content into a Page. 
-- Return the status code with the parsed content.
download :: String -> Shpider ( ShpiderCode , Page )
download messyUrl = do
   shpider <- get
   let maybeWrite u =
          maybe ( return ( ) )
                ( \ vs ->
                     put $ shpider { visited = Just $ u : vs }
                )
                ( visited shpider ) 
   if not $ isMailto messyUrl
      then do 
         murl <- mkAbsoluteUrl messyUrl
         maybe ( return ( InvalidURL , emptyPage ) )
               ( \ url -> withAuthorizedDomain url $ do
                    res@( c , page ) <- downloadAPage url
                    maybeWrite $ addr page 
                    return res
               )
               murl
      else do
         maybeWrite messyUrl --if it's mail we want to write it so we don't try it again
         return ( UnsupportedProtocol , emptyPage )


downloadAPage :: String -> Shpider (ShpiderCode, Page)
downloadAPage url = do
   shpider <- get 
   if htmlOnlyDownloads shpider
      then do
         if isHttp url
            then do
               response <- withThrottle $ liftIO $
                  curlGetResponse_ url (curlOpts shpider)
               maybe ( mkRes url (respCurlCode response, respBody response))
                     ( \ ct ->
                          if validContentType ct
                             then
                                mkRes url (CurlOK, respBody response)
                             else
                                return (WrongData, emptyPage)
                     )
                     (lookup ("Content-Type" :: String) $ respHeaders response)
            else
               getURL url
      else
         getURL url


-- | withAuthorizedDomain will execute the function if the url given is an authorized domain.
-- See `isAuthorizedDomain`.
withAuthorizedDomain :: String -> Shpider ( ShpiderCode , Page ) -> Shpider ( ShpiderCode , Page )
withAuthorizedDomain url f = do
   shpider <- get
   if dontLeaveDomain shpider
      then do
         let d = startPage shpider 
         if isSameDomain d url 
            then
               f
            else
               return ( OffSite , emptyPage )
      else
         f

-- | Send a form to the URL specified in its action attribute
sendForm :: Form -> Shpider ( ShpiderCode , Page )
sendForm form = do
   mabsAddr <- mkAbsoluteUrl $ action form
   maybe ( return (InvalidURL , emptyPage ) )
         ( \ absAddr -> withAuthorizedDomain absAddr $ do
              case method form of
                 GET -> do
                    let Just u = importURL addr -- we can do the indisputable pattern match because mkAbsoluteUrl already calls importURL
                        addr = exportURL $ foldl ( \ a i -> add_param a i
                                                 ) 
                                                 u
                                                 ( M.toList $ inputs form )
                    getURL addr
                 POST ->
                    postURL absAddr $ M.toList $ inputs form          
         )
         mabsAddr
   
-- encodeUrl = undefined

-- toPostField :: (String, String) -> String
-- toPostField ( name , value ) =
--    encodeUrl name ++ "=" ++ encodeUrl value

-- | Return the links on the current page.
currentLinks :: Shpider [ Link ]
currentLinks = do
   p <- getCurrentPage
   return $ links p

-- | Return the forms on the current page.
currentForms :: Shpider [ Form ]
currentForms = do
   p <- getCurrentPage
   return $ forms p

-- | Get all links which match this text.
getLinksByText :: String -> Shpider [ Link ]
getLinksByText t = do
   cls <- currentLinks
   return $ filter ( (==) t . linkText )
                   cls

-- | If `stayOnDomain` has been set to true, then isAuthorizedDomain returns `True` if the given URL is on the domain and false otherwise.  If `stayOnDomain` has not been set to True, then it returns `True`.
isAuthorizedDomain :: String -> Shpider Bool
isAuthorizedDomain url = do
   shpider <- get
   return $ if dontLeaveDomain shpider
               then
                  isSameDomain ( startPage shpider ) url
               else
                  True 

-- | Get all links whose text matches this regex.
getLinksByTextRegex :: String -> Shpider [ Link ]
getLinksByTextRegex r = do
   cls <- currentLinks
   return $ filter ( flip (=~) r . linkText )
                   cls

-- | Get all forms whose action matches the given action
getFormsByAction :: String -> Shpider [ Form ]
getFormsByAction a = do
   murl <- mkAbsoluteUrl a
   maybe ( return [ ] )
         ( \ url -> fmap (filter $ (==) url . action) currentForms )
         murl

getFormsHasAction :: (String -> Bool) -> Shpider [Form] 
getFormsHasAction f = fmap (filter $ f . action ) currentForms
  

-- | Get all links whose address matches this regex.
getLinksByAddressRegex :: String -> Shpider [ Link ]
getLinksByAddressRegex r = do
   cls <- currentLinks
   return $ filter ( flip (=~) r . linkAddress )
                   cls


------------------------------------------------------------------------------
-- | Gets the contents of the page at the specified URL.  If currently in
-- offline mode, then the file is read from disk.  Otherwise it is downloaded.
getURL :: String -> Shpider (ShpiderCode, Page)
getURL url = withThrottle $ do
    shpider <- get
    cachedRequest url (curlOpts shpider)


------------------------------------------------------------------------------
-- | Sends an HTTP POST request to a url.
postURL url fields = withThrottle $ do
   shpider <- get
   cachedRequest url (opts shpider)
   where
    opts sh =
      [ CurlPostFields (map (export . urlEncode) fields)
      , CurlPost True
      ] ++ curlOpts sh


------------------------------------------------------------------------------
-- |
cachedRequest :: String -> [CurlOption] -> Shpider (ShpiderCode, Page)
cachedRequest url opts = getPageFile url >>= go
  where
    go Nothing = error "Trying to get a stored page without a directory!  This shouldn't happen"
    go (Just file) = do
        exists <- liftIO $ doesFileExist file
        if exists then readCached file else requestAndStore file url opts

    readCached file = do
        liftIO $ putStrLn $ "Reading local copy of "++url
        (storedUrl,rest) <- span (/='\n') `fmap` liftIO (readFile file)
        when (storedUrl /= url) $ do
            error "Access pattern doesn't match stored data.  You probably should delete the stored pages and re-run."
        mkRes url (CurlOK, drop 1 rest)


------------------------------------------------------------------------------
-- |
requestAndStore :: String -> String -> [CurlOption] -> Shpider (ShpiderCode, Page)
requestAndStore file url opts = do
    offline <- gets offlineMode
    when offline $ error "Offline mode: no more cache, stopping."
    liftIO $ putStrLn $ "Requesting " ++ url
    contents <- liftIO $ curlGetString_ url opts
    liftIO $ writeFile file (unlines [url, snd contents])
    mkRes url contents

