{-# LANGUAGE OverloadedStrings #-}


module ProfScrapeLib
    (  numProfessors
    ) where


import Text.HTML.Scalpel
import Data.List
import Debug.Trace

validSection = ["Research & Teaching" , "Professional, Administrative & Support" , "Affiliate"] :: [[Char]]


numProfessors :: String -> IO (Maybe Int)
numProfessors x = do 
                pageItems <- scrapeURL ("https://www.gla.ac.uk/schools/" ++ x ++ "/staff/") scrapePageItems
                let profList = fmap (fmap (filter (isInfixOf "Professor"))) pageItems
                let profCount = fmap (fmap length) profList
                return $ fmap sum profCount

scrapePageItems = chroot ("div" @: ["id" @= "tabs"]) $ chroots ("div" @: [hasClass "maincontent"]) scrapeCheck

scrapeCheck :: Scraper String [String]
scrapeCheck = do 
                checkHeader <- text "h2"
                if elem checkHeader validSection 
                then scrapeProfs
                else return []

scrapeProfs :: Scraper String [String]
scrapeProfs =  chroot "ul" $ chroots "li" $ do text "a" 


