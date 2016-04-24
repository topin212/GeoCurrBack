{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, OverloadedStrings #-}

module Main where
import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack, toStrict)
import Data.Text.Lazy.Builder.Int

import Database.MySQL.Simple

import GHC.Generics
import Data.Aeson ((.:), (.:?), decode,genericToEncoding, toEncoding, encode, FromJSON(..), ToJSON(..), Value(..))

import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes(action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as Tb
import qualified Text.Blaze.Html5.Attributes as TbAttr

data Coordinates = Coordinates{
	latitude:: Text,
	longitude:: Text 
} deriving (Generic, Show)

config :: ServerConfig
config = ServerConfig { port = 8080
             , ramQuota  = 1 * 10^6
             , diskQuota = 20 * 10^6
             , tmpDir    = "/tmp/"
             }
             
instance ToJSON Coordinates where 
--	toJSON (Coordinates latitude longitude) = Object ["lat".=latitude, "lon".=longitude]
---toEncoding = genericToEncoding defaultOptions

main :: IO()
main = serve (Just config) helloWorldApp

helloWorldApp :: ServerPart Response
helloWorldApp = msum 
	[dir "json" $ jsonHandler
	,dir "ajax" $ ajaxHandler
	,dir "kek"  $ kek
	,defaultHandler]

template :: Text->Html->Response
template title body = toResponse $
	Tb.html $ do
		Tb.html $ do
			Tb.title  (toHtml title)
		Tb.body $ do
			body
			p $ a ! href "/" $ "Home"

ajaxHandler :: ServerPart Response
ajaxHandler = do
			dbAnswer <- decisionMaker "banks"
			ok $ toResponse $ show $ toHtml dbAnswer
			 
{-
	{-
	ok $ template "ajaxHandler" $ do 
		dbAnswer <- decisionMaker msg :: IO String						--so must this
		p $ "here's your info :: " >> toHtml dbAnswer					--and how to do this? if 
	--p $ "you requested " >> toHtml msg-}
	
	How do i tell this motherfucker, tha I know what to do?
	
	well, if it waits the "do" statement to be an Html, then why it thinks it is not an hml already? 
	it should be, it should create me a variable, with the type string, and put a string there as far as we are in a do block
	and then transfer pure string to `toHtml` function. 
	
	I need to practice in recreating do statements, removing all the syntactic sugar/
	
-}

decisionMaker :: String -> IO String
decisionMaker msg
	| msg == "banks" = do 
						num <- ajaxBanksHandler
						return $ show num
    | msg == "current" = do 
						num <- ajaxCurrentHadler
						return $ show num



defaultHandler :: ServerPart Response
defaultHandler = 
	ok $ template "Main page" $ do
		Tb.h1 "Hey there"
		Tb.h2 "I just figured out what the hell this shit is!"
		Tb.p  "Here are some easy links to click on:"
		Tb.p $ a ! href "/lol/value" $ "Simple echo"
		Tb.p $ a ! href "/ajax/banks/Kiev" $ "Info on Banks"
		Tb.p $ a ! href "/ajax/current/Kiev" $ "Info on buy/sell rate"

jsonHandler :: ServerPart Response
jsonHandler = do 	
    			lat <- optional $ lookText "lat"
    			lon <- optional $ lookText "lon"
    			ok $ template "Lat Lon" $ do 
    				let json = encode (Coordinates (toStrict $ fromMaybe "kek" lat) (toStrict $ fromMaybe "kek" lon))
    				toHtml(show json)

echoHandler :: ServerPart Response
echoHandler = 
	path $ \(msg :: String) -> 
	ok $ template "Lol" $ do
	p "Dynamic addres part is " >> toHtml msg

kek :: ServerPart Response
kek = ok $ template "hell error" $ do
	p "Server threw this error at you because you did something wrong."


ajaxBanksHandler :: IO Int 
ajaxBanksHandler = do 
	conn <- connect defaultConnectInfo { connectUser = "topin212" }
	--[Only result] <- query_ conn "SELECT latitude, longitude from Coords2Adress":: [ (Int, Int)]
	[Only result] <- query_ conn "select 2*3"
	return result
     
ajaxCurrentHadler ::  IO Int 
ajaxCurrentHadler = do 
	conn <- connect defaultConnectInfo { connectUser = "topin212" } 
	--[Only result] <- query_ conn "SELECT buy, sell from nCurses" :: [(Double, Double)]
	[Only result]<- query_ conn "select 2+2"
	return result