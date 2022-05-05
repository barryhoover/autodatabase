import Auto.Auto
import System.IO (hFlush, stdout, readFile, writeFile)
import System.Directory (renameFile)
import Safe (readDef, readMay)
import Data.List (genericIndex)
import Data.Aeson (encode)


optionDisplayAllAutos            = 1  :: Integer
optionDisplayAutosByYear         = 2  :: Integer
optionDisplayAutosByMakeAndModel = 3  :: Integer
optionDisplayAutosByMileage      = 4  :: Integer
optionDisplayAutosByColor        = 5  :: Integer
optionDisplayAutosByEngine       = 6  :: Integer
optionDisplayAutosByPrice        = 7  :: Integer
optionQuit                       = 8  :: Integer
optionRepeat                     = 9  :: Integer
optionToJson                     = 10 :: Integer
options                  = [optionDisplayAllAutos, optionDisplayAutosByYear, optionDisplayAutosByMakeAndModel, optionDisplayAutosByMileage,
                           optionDisplayAutosByColor, optionDisplayAutosByEngine, optionDisplayAutosByPrice, optionQuit, optionRepeat]
optionPrompt             = "Enter your choice (1-10) ===> "
inventoryFilename        = "inventory.data"

printMenu :: IO ()
printMenu = do
  putStrLn $ "_____________________________________________________________________________________________\n"   ++
             "                                                                                             \n"   ++
             "                                       Hoover Automobiles                                    \n"   ++
             "                                    * Find Your Automobile! *                                 \n"   ++
             "_____________________________________________________________________________________________\n\n" ++
             "Please choose from the following options:\n\n"                                                     ++
             "_____________________________________________________________________________________________\n"   ++
             show optionDisplayAllAutos            ++ "  -  Display all autos in inventory.\n\n"                 ++
             show optionDisplayAutosByYear         ++ "  -  Display all autos within a yearly range.\n\n"        ++
             show optionDisplayAutosByMakeAndModel ++ "  -  Display all autos of a specific make and model.\n\n"   ++
             show optionDisplayAutosByMileage      ++ "  -  Display all autos within a mileage range.\n\n"       ++
             show optionDisplayAutosByColor        ++ "  -  Display autos of specified color.\n\n"               ++
             show optionDisplayAutosByEngine       ++ "  -  Display autos of a specified engine.\n\n"            ++
             show optionDisplayAutosByPrice        ++ "  -  Display autos within a price range.\n\n"             ++
             show optionQuit                       ++ "  -  Quit. (exit inventory tracking system)\n\n"          ++
             "______________________________________________________________________________________________"

promptUserString :: String -> IO String
promptUserString message = do
  putStr message
  hFlush stdout
  getLine

promptUserInteger :: String -> IO Integer
promptUserInteger message = do
  putStr message
  hFlush stdout
  response <- getLine
  case readMay response of
    Just value -> return value
    Nothing    -> promptUserInteger message

promptUserDouble :: String -> IO Double
promptUserDouble message = do
  putStr message
  hFlush stdout
  response <- getLine
  case readMay response of
    Just value -> return value
    Nothing    -> promptUserDouble message

getOption :: IO Integer
getOption = do
  option <- promptUserString optionPrompt
  if elem (readDef optionRepeat option) options
    then return (readDef optionRepeat option)
    else getOption

promptAuto :: IO Auto
promptAuto = do
  putStrLn $  "Input data for new automobile:\n" ++
              "________________________________\n\n"
  _year         <- promptUserInteger "Input year: "
  _makeandmodel <- promptUserString  "Input makeandmodel: "
  _miles        <- promptUserString  "Input mileage: "
  _color        <- promptUserString  "Input color: "
  _engine       <- promptUserString  "Input engine (V6 or V8): "
  _price        <- promptUserDouble  "Input price: "
  return Auto  { year            = _year
               , makeandmodel    = _makeandmodel
               , mileage         = _miles
               , color           = _color
               , engine          = _engine
               , price           = _price
               }
 
writeAutos :: [Auto] -> String -> IO ()
writeAutos autos inventoryFilename = do
  writeFile tempFilename (showAutos autos)
  renameFile tempFilename inventoryFilename
  where tempFilename        = inventoryFilename ++ ".tmp"
        showAutos []         = ""
        showAutos (auto:autos) =
          (show $ year auto) ++ " " ++ makeandmodel auto ++ " " ++
          mileage auto ++ "\n" ++
          color auto ++ "\n" ++
          engine auto ++ "\n" ++
          showAutos autos

-- encode :: ToJSON a => a -> ByteString  --none of 3 are in scope
encode :: [Auto] -> a0

-- encode = ???  -- encode lacks an accompanying binding
-- encode a0 = do  --error: Empty 'do' block
handleOption :: [Auto] -> Integer -> String -> IO ([Auto], Bool)
handleOption autos option inventoryFilename
  | optionDisplayAllAutos == option = do
      displayAutos autos
      return (autos, False)
  | optionDisplayAutosByYear == option = do
      low  <- promptUserDouble "Enter the low end of the year range to search: "
      high <- promptUserDouble "Enter the high end of the year range to search: "
      displayAutos $ filter (\auto -> (low <= fromIntegral (year auto)) && (fromIntegral (year auto) <= high)) autos
      return (autos, False)
  | optionDisplayAutosByMakeAndModel == option = do
      _makeandmodel <- promptUserString "Enter the make and model of the automobile you are searching for: "
      displayAutos $ filter (\auto -> makeandmodel auto == _makeandmodel) autos
      return (autos, False)
  | optionDisplayAutosByMileage == option = do
      low  <- promptUserDouble "Enter the low end of the mileage range to search: "
      high <- promptUserDouble "Enter the high end of the mileage range to search: "
      displayAutos $ filter (\auto -> (low <= (read (mileage auto) :: Double)) && ((read (mileage auto) :: Double) <= high)) autos
      return (autos, False)
  | optionDisplayAutosByColor == option = do
      _color <- promptUserString "Enter the color of the automobile you are searching for: "
      displayAutos $ filter (\auto -> color auto == _color) autos
      return (autos, False)
  | optionDisplayAutosByEngine == option = do
      _engine <- promptUserString "Enter either V6 or V8 for the engine of the automobile you are searching for: "
      displayAutos $ filter (\auto -> engine auto == _engine) autos
      return (autos, False)
  | optionDisplayAutosByPrice == option = do
      low  <- promptUserDouble "Enter the low end of the price range to search: "
      high <- promptUserDouble "Enter the high end of the price range to search: "
      displayAutos $ filter (\auto -> (low <= price auto) && (price auto <= high)) autos
      return (autos, False)
  | optionToJson == option = do
      putStrLn $ read $ show $ encode autos
      return (autos, False)
  | optionQuit == option = do
      putStrLn "Thank you for shopping at Hoover Automobiles.  Please come back soon!"
      return (autos, True)
  | otherwise = do
      return (autos, False)


displayAutos :: [Auto] -> IO ()
displayAutos [] = do return ()
displayAutos (auto:remainingAutos) = do
  displayAuto auto
  displayAutos remainingAutos
  where displayAuto auto = putStrLn $ (show $ year auto) ++ ", " ++ makeandmodel auto ++ ", " ++ "\n" ++
                                    "miles: " ++ mileage auto ++ "\n" ++
                                    "color: " ++ color auto ++ "\n" ++
                                    "engine: " ++ engine auto ++ "\n" ++
                                    "price: " ++ (show $ price auto) ++ "\n" ++
                                    "________________________________\n\n"

readAutos :: String -> IO [Auto]
readAutos filename = do
  autoFileData <- readFile filename
  return $ map autoFromData (autoData $ lines autoFileData)
  where
    autoData autoFileData
      | length autoFileData < 5 = []
      | otherwise              = [(take 5  $ fst $ splitAt 6 autoFileData)] ++
                                  (autoData $ snd $ splitAt 6 autoFileData)
    autoFromData [line1, miles, color, engine, price] =
      Auto { year            = read   ((words line1) `genericIndex` 0)
           , makeandmodel    = unwords $ drop 1 (words line1)
           , mileage         = miles 
           , color           = color
           , engine          = engine
           , price           = read price
           }

menuLoop :: [Auto] -> String -> IO ()
menuLoop autos inventoryFilename = do
  printMenu
  option <- getOption
  (newAutos, shouldQuit) <- handleOption autos option inventoryFilename
  if shouldQuit
    then return ()
    else menuLoop newAutos inventoryFilename

main :: IO ()
main = do
  autos <- readAutos inventoryFilename
  menuLoop autos inventoryFilename

