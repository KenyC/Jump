{-# LANGUAGE TemplateHaskell #-}
module Main where

#ifdef ORIGINAL_GETOPTS
import System.Console.GetOpt
#else
import CustomGetOpt
#endif
import System.Environment
import System.Directory
import System.IO
--
import Control.Monad (forM_, join)
--
import Data.Maybe (maybe, fromMaybe, listToMaybe)
import Data.List
import Data.Char  (isSpace, toLower)
--
import Text.Parsec 

---------------------------------------------------------------------------------


config_file_directory :: IO FilePath
#ifdef CONFIG_FOLDER
config_file_directory = return $ CONFIG_FOLDER
#else
config_file_directory = (++ "/.config/jump/") <$> getHomeDirectory
#endif

bookmark_db_location :: IO FilePath
bookmark_db_location = (++ "bookmarks.txt") <$> config_file_directory

default_env_file :: IO FilePath
default_env_file = (++ "default_env.sh") <$> config_file_directory

default_file_to_source :: FilePath
default_file_to_source = "/tmp/___verylongandarbitraryname"

---------------------------------------------------------------------------------

echo :: Handle -> String -> IO ()
echo handle string  = hPutStrLn handle $ "echo \"" ++ (escape string) ++ "\""

cd :: Handle -> FilePath -> IO ()
cd handle file_path = hPutStrLn handle $ "cd \"" ++ (escape file_path) ++ "\"" 

source :: Handle -> FilePath -> IO ()
source handle file_path = hPutStrLn handle =<< (readFile file_path) 

title :: Handle -> String -> IO ()
title handle title_cmd = hPutStrLn handle $ before ++ (escape title_cmd) ++ after
                         where
                          before, after :: String
                          before = join [ "if [[ -z \"$ORIG\" ]]; then\n"
                                        , "  ORIG=\"$PS1\"\n"
                                        , "fi\n"
                                        , "TITLE=\"\\[\\e]2;"]
                          after = join [ "\\a\\]\" \n"
                                       , "PS1=\"${ORIG}${TITLE}\"\n"]

---------------------------------------------------------------------------------

safe_head = listToMaybe

isBlank :: String -> Bool
isBlank = all isSpace

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

escape :: String -> String
escape = foldr escape_char ""
         where escape_char '"' = ('\\':) . ('"':)
               escape_char x   = (x:) 

---------------------------------------------------------------------------------

data Options = JumpTo {
                _bookmark_to_jump :: String,
                _env :: Maybe (Maybe String), -- if Nothing, no env required ; if Just Nothing, default env 
                _must_set_title :: Bool
             }
             | MakeBookmark (Maybe String) 
             | Clear        (Maybe String) 
             | List 
             | Names

start_options :: Options
start_options = JumpTo {
  _bookmark_to_jump = "",
  _env              = Nothing,
  _must_set_title   = False
}


options :: [OptDescr (Options -> Options)]
options = [Option ['e'] ["env"]      (OptArg set_env "ENVIRONMENT")                  "Environment to source after jump",
           Option ['b'] ["bookmark"] (OptArg (const . MakeBookmark) "BOOKMARK_NAME") "Create bookmark (default: directory name lower case)",
           Option ['n'] ["names"]    (NoArg  (const Names))                          "List bookmark names",
           Option ['t'] ["title"]    (NoArg  set_title)                              "Set terminal name after jumping",
           Option ['l'] ["list"]     (NoArg  (const List))                           "List bookmark names and location",
           Option ['c'] ["clear"]    (OptArg (const . Clear) "BOOKMARK_NAME")        "Delete bookmark"
          ]
          where set_env maybe_env jumpto@(JumpTo _ _ _) = jumpto {_env = Just maybe_env}   -- set env value only if Options is a JumpTo constructor
                set_env _         opt                   = opt

                set_title jumpto@(JumpTo _ _ _)         = jumpto {_must_set_title = True}  -- set title value only if Options is a JumpTo constructor
                set_title opt                           = opt


------------------------------------------------------------------------------

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
                        putStrLn $ "Ill-formatted config file ; can't parse " ++  bookmark_db_location_

---------------------------------------------------------------------------------
main :: IO ()
main = do
    args_raw <- getArgs

    let (file_to_source, args) = case args_raw of
                                     (filename:rest) -> (filename, rest)
                                     _               -> (default_file_to_source, args)

    withFile file_to_source WriteMode $ \handle -> do

        let header     = "Usage info:"
            print_help = putStrLn $ usageInfo header options 

        case getOpt Permute options args of
            (opts, arguments, []) -> let action = foldr ($) start_options opts
                                     in case action of 
                                        Clear         bookmark_        -> clear bookmark_
                                        MakeBookmark  bookmark_        -> bookmark bookmark_
                                        List                           -> list_bookmarks
                                        Names                          -> print_names
                                        JumpTo _ environment title     -> case arguments of
                                                                                  []            -> putStrLn "No bookmark specified!"
                                                                                  destination:_ -> jump_to handle destination environment title
            (_, _, errors) -> do
                putStrLn $ concat errors
                print_help



list_bookmarks :: IO ()
list_bookmarks = do
    maybe_bookmarks <- get_bookmarks =<< bookmark_db_location

    with_bookmarks maybe_bookmarks $ \bookmarks -> do
        let alphabetically_sorted_bookmarks :: [Bookmark]
            alphabetically_sorted_bookmarks = sortOn bookmark_name bookmarks

            max_len_name  = maximum $ map (length . bookmark_name) $ alphabetically_sorted_bookmarks 
            size_matched_bookmark :: Int -> Bookmark -> String
            size_matched_bookmark size_name bookmark = size_matched_name ++ "    " ++ (bookmark_path bookmark)
                                                       where name              = bookmark_name bookmark
                                                             missing_length    = size_name - (length name) 
                                                             size_matched_name = name ++ replicate missing_length ' '
            
        forM_ alphabetically_sorted_bookmarks $ \bookmark -> do
            putStrLn $ size_matched_bookmark max_len_name bookmark

print_names :: IO ()
print_names = do
    maybe_bookmarks <- get_bookmarks =<< bookmark_db_location
    with_bookmarks maybe_bookmarks $ \bookmarks -> do
        forM_ bookmarks $ putStrLn . bookmark_name


clear :: Maybe String -> IO ()
clear maybe_name = case maybe_name of 
                        Nothing -> do
                              maybe_bookmarks <- get_bookmarks =<< bookmark_db_location
                              current_dir     <- getCurrentDirectory

                              with_bookmarks maybe_bookmarks $ \bookmarks -> do
                                  let bookmarks_wo_element = filter ((/= current_dir) . bookmark_path) $ bookmarks

                                  if length bookmarks_wo_element == length bookmarks
                                  then
                                      putStrLn "Couldn't find any bookmark attached to this directory"
                                  else 
                                      do
                                          bookmark_db_location_ <- bookmark_db_location
                                          writeFile bookmark_db_location_ ""
                                          forM_ bookmarks_wo_element $ write_bookmark bookmark_db_location_


                        Just name -> do
                              maybe_bookmarks <- get_bookmarks =<< bookmark_db_location

                              with_bookmarks maybe_bookmarks $ \bookmarks -> do

                                  let bookmarks_wo_element = filter ((/= name) . bookmark_name) $ bookmarks
                                  -- print bookmarks
                                  bookmark_db_location_ <- bookmark_db_location
                                  writeFile bookmark_db_location_ ""
                                  forM_ bookmarks_wo_element $ write_bookmark bookmark_db_location_
                                  putStrLn $ "Removed bookmark \"" ++ name ++ "\""
                          -- writeFile bookmark_db_location $ mconcat [line ++ "\n" | line <- lines_wo_name]
                    --


bookmark :: Maybe String -> IO ()
bookmark maybe_name = do 
    current_dir <- getCurrentDirectory
    let basename :: FilePath -> String
        basename file_path = case break (== '/') file_path of
                                (name, "")  -> name
                                (_, rest)   -> basename $ tail rest
        name = case maybe_name of 
                     Nothing       -> map toLower $ filter (`notElem` " \t\n") $ basename current_dir -- remove spaces, all lowercase
                     Just raw_name -> strip raw_name

        bookmark = Bookmark {
            bookmark_name = name,
            bookmark_path = current_dir
        }

    bookmark_db_location_ <- bookmark_db_location
    write_bookmark bookmark_db_location_ bookmark
    putStrLn $ "Added bookmark \"" ++ current_dir ++ "\" as \"" ++ name ++ "\""


jump_to :: Handle                    -- ^ file containing bash commands
        -> String                    -- ^ bookmark name
        -> (Maybe (Maybe String))    -- ^ maybe an environment (maybe the default one)
        -> Bool                      -- ^ sets title of terminal?
        -> IO ()
jump_to handle name maybe_env must_set_title = do
    -- let name, flags  =  fromMaybe 
    --                         ("", [])
    --                         (uncons name_and_flags)
    maybe_bookmarks <- get_bookmarks =<< bookmark_db_location

    with_bookmarks maybe_bookmarks $ \bookmarks -> do
            -- print bookmarks
            let paths = map bookmark_path $ filter ((== name) . bookmark_name) $ bookmarks
                set_title = if must_set_title then title handle name else return () 
                source_env = case maybe_env of
                                Nothing              -> return ()
                                Just maybe_env_name  -> (=<<) (source handle) $ case maybe_env_name of 
                                                              Nothing       -> default_env_file
                                                              Just env_name -> (++ env_name ++ ".sh") <$> config_file_directory
            -- print $ filter ((isPrefixOf name) . bookmark_name) $ bookmarks

            case paths of 
                []        -> putStrLn $ "Bookmark " ++ name ++ " not found!"
                [path]    -> cd handle path >> source_env >> set_title
                path:rest -> putStrLn "Two bookmarks have the same name."