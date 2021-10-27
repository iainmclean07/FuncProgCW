module Main where

import Text.HTML.Scalpel
import ProfScrapeLib

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Maybe

schools :: [String]
schools = [
          "chemistry",
          "computing",
          "engineering",
          "ges",
          "mathematicsstatistics",
          "physics",
          "psychology"
         ]

pitem :: (String, Maybe Int) -> PieItem
pitem (s,v) = pitem_value .~ case v of 
                              Just v -> fromIntegral v
                              Nothing -> error ("No Value")
               $ pitem_label .~ s
               $ def

main :: IO ()
main = do
     counts <- mapM numProfessors schools
     let results = zip schools counts
     mapM_ (\x -> putStrLn $ (fst x) ++ ": " ++ ((show.snd) x)) results
     toFile def "ProfessorChart.png" $ do
          pie_title  .= "Number of Professors"
          pie_plot . pie_data .= fmap pitem results 
