import Control.Monad (forM,foldM,mapM_)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>) , takeBaseName,takeExtension)
import System.IO
import GHC.IO.Encoding(latin1)

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", "..",".svn",".git"]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path 
        if isDirectory 
              then getRecursiveContents path
        else return [path]
    return $ filter (\p -> takeExtension p `elem` [".cpp",".h",".hpp",".ipp"]) (concat paths)

 
linesCount :: FilePath -> IO Int    
linesCount filepath=do
         fileHandle<-openFile filepath ReadMode
         hSetEncoding fileHandle latin1
         content <- hGetContents fileHandle
         needreturn<-return $ length $ lines $ content
         if needreturn>0
            then return needreturn
         else
            return needreturn

quicksort :: (Ord b, Eq a) => [(a, b)] -> [(a, b)]
quicksort li 
    | li==[]=[]
    | otherwise=quicksort [a| a<-xs,snd(a)<=snd(x)]++[x]++quicksort [a|a<-xs,snd(a)>snd(x)]
    where (x:xs)=li
          

totalLines filepath=do
    paths<-getRecursiveContents filepath
    listLineCount<-forM paths linesCount
    sequence (map print (quicksort $ zip paths listLineCount))
    return $ foldl (+) 0 listLineCount