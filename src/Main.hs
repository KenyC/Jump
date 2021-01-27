module Main where

import System.Console.GetOpt
import System.Environment
import System.Directory
import System.IO
--
import Control.Monad (forM_)
--
import Data.Maybe (maybe, listToMaybe)
import Data.List
import Data.Char  (isSpace, toLower)
--
import Text.Parsec 

---------------------------------------------------------------------------------

config_file_directory :: IO FilePath
config_file_directory = (++ "/.config/jump/") <$> getHomeDirectory
-- config_file_directory = "/home/keny/.config/jump/"

bookmark_db_location :: IO FilePath
bookmark_db_location = (++ "bookmarks.txt") <$> config_file_directory


---------------------------------------------------------------------------------

echo :: String -> IO ()
echo string  = putStrLn $ "echo \"" ++ string ++ "\""

cd :: FilePath -> IO ()
cd file_path = putStrLn $ "cd \"" ++ file_path ++ "\"" 

---------------------------------------------------------------------------------

safe_head = listToMaybe

isBlank :: String -> Bool
isBlank = all isSpace

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

---------------------------------------------------------------------------------

data Bookmark = Bookmark {
    bookmark_name :: String,
    bookmark_path :: FilePath
} 

instance Show Bookmark where
    show bookmark = (bookmark_name bookmark) ++ " " ++ (bookmark_path bookmark) 


parse_bookmarks :: SourceName -> String -> Either ParseError [Bookmark]
parse_bookmarks = parse $ many $ do
        name  <- manyTill anyChar $ space >> spaces       
        path  <- manyTill anyChar (char '\n')  
        return $ Bookmark {
            bookmark_name = name,
            bookmark_path = path
        }


get_bookmarks :: FilePath -> IO (Maybe [Bookmark])
get_bookmarks file_path = do
    raw_bookmarks <- readFile file_path

    return $ case parse_bookmarks "" raw_bookmarks of
        Left  _       -> Nothing
        Right result  -> Just result

write_bookmark :: FilePath -> Bookmark -> IO ()
write_bookmark file_path bookmark = appendFile file_path $ (show bookmark) ++ "\n"

with_bookmarks :: Maybe [Bookmark] -> ([Bookmark] -> IO ()) -> IO ()
with_bookmarks = flip $ maybe corrupted_config_file
                 where
                    corrupted_config_file = do
                        bookmark_db_location_ <- bookmark_db_location
                        echo $ "Ill-formatted config file ; can't parse " ++  bookmark_db_location_

---------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    -- putStrLn "jzfpo"

    case args of 
        "--bookmark":rest  -> bookmark $! mconcat rest 
        "--clear":rest     -> clear    $! mconcat rest 
        "--list" :rest     -> list_bookmarks    
        "--names":rest     -> print_names    
        otherwise          -> jump_to  $! mconcat args



list_bookmarks :: IO ()
list_bookmarks = do
    maybe_bookmarks <- get_bookmarks =<< bookmark_db_location
    with_bookmarks maybe_bookmarks $ \bookmarks -> do
        let max_len_name  = maximum $ map (length . bookmark_name) $ bookmarks 
            size_matched_bookmark :: Int -> Bookmark -> String
            size_matched_bookmark size_name bookmark = size_matched_name ++ "  :  " ++ (bookmark_path bookmark)
                                                       where name              = bookmark_name bookmark
                                                             missing_length    = size_name - (length name) 
                                                             size_matched_name = name ++ replicate missing_length ' '
            
        forM_ bookmarks $ \bookmark -> do
            echo $ size_matched_bookmark max_len_name bookmark

print_names :: IO ()
print_names = do
    maybe_bookmarks <- get_bookmarks =<< bookmark_db_location
    with_bookmarks maybe_bookmarks $ \bookmarks -> do
        forM_ bookmarks $ echo . bookmark_name


clear :: String -> IO ()
clear name = case isBlank name of 
                  True -> do
                        maybe_bookmarks <- get_bookmarks =<< bookmark_db_location
                        current_dir     <- getCurrentDirectory

                        with_bookmarks maybe_bookmarks $ \bookmarks -> do
                            let bookmarks_wo_element = filter ((/= name) . bookmark_path) $ bookmarks

                            if length bookmarks_wo_element == length bookmarks
                            then
                                echo "Couldn't find any bookmark attached to this directory"
                            else 
                                do
                                    bookmark_db_location_ <- bookmark_db_location
                                    writeFile bookmark_db_location_ ""
                                    forM_ bookmarks_wo_element $ write_bookmark bookmark_db_location_


                  False -> do
                        maybe_bookmarks <- get_bookmarks =<< bookmark_db_location

                        with_bookmarks maybe_bookmarks $ \bookmarks -> do

                            let bookmarks_wo_element = filter ((/= name) . bookmark_name) $ bookmarks
                            -- print bookmarks
                            bookmark_db_location_ <- bookmark_db_location
                            writeFile bookmark_db_location_ ""
                            forM_ bookmarks_wo_element $ write_bookmark bookmark_db_location_
                            -- writeFile bookmark_db_location $ mconcat [line ++ "\n" | line <- lines_wo_name]
                        --


bookmark :: String -> IO ()
bookmark raw_name = do 
    current_dir <- getCurrentDirectory
    let basename :: FilePath -> String
        basename file_path = case break (== '/') file_path of
                                (name, "")  -> name
                                (_, rest)   -> basename $ tail rest
        name = case isBlank raw_name of 
                     True   -> map toLower $ basename current_dir
                     False  -> strip raw_name

        bookmark = Bookmark {
            bookmark_name = name,
            bookmark_path = current_dir
        }

    bookmark_db_location_ <- bookmark_db_location
    write_bookmark bookmark_db_location_ bookmark
    echo $ "Added " ++ current_dir ++ " as " ++ name


jump_to :: String -> IO ()
jump_to name = do
    maybe_bookmarks <- get_bookmarks =<< bookmark_db_location

    with_bookmarks maybe_bookmarks $ \bookmarks -> do
            -- print bookmarks
            let paths = map bookmark_path $ filter ((== name) . bookmark_name) $ bookmarks
            -- print $ filter ((isPrefixOf name) . bookmark_name) $ bookmarks

            case paths of 
                []        -> echo "Path not found"
                [path]    -> cd path
                path:rest -> echo "Conflicting shortcut names"