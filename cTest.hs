import Network
import System.IO
import System.Environment (getArgs)

main = do
    putStrLn "type your input? (type QUIT to exit)"
    input <- getLine
    handleMain <- connectTo "localhost" (PortNumber 8000)
    hSetBuffering handleMain LineBuffering
    hPutStr handleMain ("GET /echo.php?message=" ++ input ++ "\nHTTP1.0\nHost: localhost:8000  \r\n\r\n")
    --hPutStr handleMain "GET /~ebarrett/lectures/cs4032/echo.php?message=sample HTTP1.1\nHost: www.scss.tcd.ie  \r\n\r\n"
    contents <- hGetContents handleMain
    putStrLn (contents)
    hClose handleMain
    if input /= "QUIT"
        then main
        else return ()
                
