
--
--import Network 
import Network
import System.IO
import System.Environment (getArgs)
import Control.Exception.Base
import Control.Concurrent
import Control.Monad
import Text.Printf


talk :: Handle -> PortNumber -> HostName-> Socket -> IO ()
talk h port host s= do

  hSetBuffering h LineBuffering                                -- First, we set the buffering mode for the Handle to line buffering. 
  loop   
                                                        -- We enter a loop to respond to requests from the client.
 where
  loop = do
    line <- hGetLine h                                         -- Each iteration of the loop reads a new line of text.
    if line == "KILL_SERVICE"                                           -- Then it checks whether the client sent "end".
       then sClose s
    else if line =="HELO text"
       then hPutStrLn h ("HELO text\nIP:" ++ host ++ "\nPort:" ++ (show port)++"\nStudentID:12301561\n" ) 
    else do hPutStrLn h ("im still a skeleton") -- If not, we attempt to interpret the line as an integer and output double it.
    loop                                            -- Finally, we call loop again to read the next request.






main = do
  args <- getArgs
  let port = (read $ head args :: Int)
  sock <- listenOn (PortNumber (fromIntegral port))              -- First, we create a network socket to listen on port 44444.
  printf "Listening on port %d\n" port
  forever $ do                                                   -- Then we enter a loop to accept connections from clients.
     (handle, host, port) <- accept sock                         -- blocks connection
     printf "Accepted connection from %s: %s\n" host (show port)
     forkFinally (talk handle port host sock) (\_ -> hClose handle)             -- makes sure handle is closed



