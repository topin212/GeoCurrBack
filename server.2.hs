{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, OverloadedStrings #-}

module Main where 

import Control.Applicative ((<$>), optional)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe

import Data.Text 
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder.Int
import GHC.Generics

import Database.MySQL.Simple
import Data.Aeson
import Happstack.Lite

import qualified Network.HTTP as HTTP

import Data.Time.Clock
import Data.Time.Calendar

import Text.XML.HXT.Core
import Text.HandsomeSoup
import System.Process

data Coordinates = Coordinates{
	latitude1:: Text,
	longitude1:: Text 
} deriving (Generic, Show)

data Banks = Banks{
    bank :: Text,
    adress :: Text, 
    latitude :: String,
    longitude :: String
} deriving (Generic, Show)

instance ToJSON Banks where 
    toEncoding = genericToEncoding defaultOptions

config :: ServerConfig
config = ServerConfig {
	port = 8080,
	ramQuota = 1*10^6,
	diskQuota = 20* 10^6,
	tmpDir = "/tmp"}

main :: IO()
main = serve (Just config) geoCurr

geoCurr :: ServerPart Response
geoCurr = msum
	[dir "gc"   $ addJsonHeaders reverseHandler
	,dir "err"  $ addJsonHeaders errorHandler
	,dir "ajax" $ dir "current" $ addJsonHeaders getCoursesHandler
	,dir "current" $ addJsonHeaders getCoursesHandler
	,dir "ajax" $ dir "banks" $ addJsonHeaders getBanksHandler
	,defaultHandler]

--fix  this 
reverseHandler:: ServerPart Response
--reverseHandler = liftIO $ toResponse . openURL.requestFormer =<< lookText ("adress"::String)
--reverseHandler = fmap toResponse $ liftIO result
--				where result = do
--					quer <- lookText ("adress"::String)
--					ok (openURL.requestFormer $ unpack $ toStrict $ quer)
--reverseHandler = defaultHandler

reverseHandler = do
		quer <- lookText ("adress"::String)
		toResponse <$> liftIO (do 
			kek <- openURL $ requestFormer (unpack $ toStrict $ quer)
			return kek)


{-
optional - returns IO String, and sends that string to a combination of

--liftIO $ toResponse . openURL . requestFormer =<< optional $ lookText "adress"
--return $ openURL $ requestFormer $ optional $ lookText ("adress"::String)
--liftIO $ toResponse $ 
				--optional $ lookText ("adress"::String) >>= openURL . requestFormer  

-}

getCoursesHandler :: ServerPart Response
--getCoursesHandler = liftIO $ toResponse $ json
--	where json = readProcess "/home/ubuntu/workspace/1/doit.php" [] []
--getCoursesHandler = defaultHandler
getCoursesHandler = toResponse <$> liftIO (readProcess "/home/ubuntu/workspace/1/doit.php" [] [])

getBanksHandler :: ServerPart Response

-- 				lifts IO	response    string	   io[string]	   io[int, string]
-- 				lifts IO	response    io string  io[banks]	   io[(abcd)]
getBanksHandler = toResponse . encode . tuplesToBanks <$> liftIO sqlBanksAsker
{-
--getBanksHandler = liftIO $ toResponse . encode . tuplesToBanks <$> sqlBanksAsker
-- 				lifts IO	response    string	   io[string]	   io[int, string]
--getBanksHandler = liftIO $ toResponse . unwords . tuplesToString <$> sqlAsker

getBanksHandler = ok $ toResponse $ liftIO $ do
	dbResp <- sqlAsker
	unwords $ tuplesToString dbResp
-}	
{-
	ok $ toResponse $ do 
		dbResp <- sqlAsker
		forM_ kek $ \(id, date)->
			T.unpack date ++ " has id of " ++ show(id::Int)
	
	Actually this guy needs some refitting. This case will only work if toResponse took an encapsulated
	value, and did some magic with it. But it is an ordinary function, that may take a String.
	So the plan is simple. Tomorow I'll need to refit it to something like 

	getBanksHandler = do
		dbResp <- sqlAsker
		forM_kek $ '(id, date) ->
			//stack a lot of strings into one big here
			bigString++ (black magic here)
		ok $ toResponse bigString

	And it should work.
-}
{-here we need to grab the last part of request (c9users.io/ajax/banks/current)
 					and      c9users.io/ajax/banks/all
or whatsoever.
-}


ajaxHandler :: ServerPart Response
ajaxHandler = ok $ toResponse ("ajax" :: String)
errorHandler :: ServerPart Response
errorHandler = ok $ toResponse ("error" :: String)



addJsonHeaders :: ServerPart Response -> ServerPart Response
addJsonHeaders x= do
	setHeaderM ("Content-Type"::String) ("application/json"::String)
	addHeaderM ("Content-Type"::String) ("application/json"::String)
	addHeaderM ("Access-Control-Allow-Origin"::String) ("*"::String)
	x





sqlAsker :: IO [(Int,String)]
sqlAsker = do
  conn <- connect defaultConnectInfo {connectUser = "topin212"}
  result <- query_ conn "select id, data from lel"
  return (result :: [(Int, String)])

tuplesToString :: [(Int, String)] -> [String]
tuplesToString ((a, b):xs) = show a : b : tuplesToString xs
tuplesToString [] = []
{-
jsonHandler :: ServerPart Response
jsonHandler = do 	
    			lat <- optional $ lookText "lat"
    			lon <- optional $ lookText "lon"
    			ok $ template "Lat Lon" $ do 
    				let json = encode (Coordinates (toStrict $ fromMaybe "kek" lat) (toStrict $ fromMaybe "kek" lon))
    				toHtml(show json)
-}

tuplesToBanks :: [(Text, Text, Double, Double)] -> [Banks]
tuplesToBanks ((a,b,c,d):xs) = Banks a b (show c) (show d) : tuplesToBanks xs 
tuplesToBanks [] = []


sqlBanksAsker :: IO [(Text, Text, Double, Double)]
sqlBanksAsker = do 
    conn <- connect defaultConnectInfo {connectUser="topin212", connectDatabase="GC"}
    query_ conn "call getAllBanksOfKiev()"



openURL :: String -> IO String
openURL x = HTTP.getResponseBody =<< HTTP.simpleHTTP (HTTP.getRequest x)

requestFormer :: String -> String
requestFormer x = ("http://nominatim.openstreetmap.org/search?q=" :: String) ++ x ++ ("&format=json&limit=1" :: String)


usdRequestFormer :: IO String
usdRequestFormer = do 
		dateStr <- dateToString . reverseDate =<< getDate
		return $ ("http://kurs.com.ua/ajax/major_nalichnie/all/usd/" :: String) ++ dateStr

eurRequestFormer :: IO String
eurRequestFormer = do 
		dateStr <- dateToString . reverseDate =<< getDate
		return $ ("http://kurs.com.ua/ajax/major_nalichnie/all/eur/" :: String) ++ dateStr

rubRequestFormer :: IO String
rubRequestFormer = do 
		dateStr <- dateToString . reverseDate =<< getDate
		return $ ("http://kurs.com.ua/ajax/major_nalichnie/all/rub/" :: String) ++ dateStr



getDate :: IO (Integer,Int,Int) -- :: (year,month,day)
getDate = getCurrentTime >>= return . toGregorian . utctDay

reverseDate :: (Integer, Int, Int) -> (Int, Int, Integer)
reverseDate (a,b,c) = (c,b,a)

dateToString :: (Int, Int, Integer) -> IO String
dateToString (a,b,c)
	| a < 10 && b < 10 = return $ ("0"::String) ++ show a ++ ("."::String) ++ ("0"::String) ++ show b ++ ("."::String) ++ show c
	| a < 10 = return $ ("0"::String) ++ show a ++ ("."::String) ++ show b ++ ("."::String) ++ show c
	| b < 10 = return $ show a ++ ("."::String) ++ ("0"::String) ++ show b ++ ("."::String) ++ show c

defaultHandler = ok $ toResponse ("ok"::String)