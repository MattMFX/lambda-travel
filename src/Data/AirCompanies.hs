module Data.AirCompanies (loadAirCompanies) where

loadAirCompanies :: IO [String]
loadAirCompanies = do
    content <- readFile "data/air-companies.txt"
    return $ lines content