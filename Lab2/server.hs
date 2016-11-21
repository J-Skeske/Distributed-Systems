
--Jack Skehan 12301561
import Network
import System.IO
import System.Environment (getArgs)
import Control.Exception.Base
import Control.Concurrent
import Control.Monad
import Text.Printf


talk :: Handle -> PortNumber -> HostName-> Socket -> IO ()
talk h port host s= do

  hSetBuffering h $ BlockBuffering Nothing                               -- First, we set the buffering mode for the Handle to line buffering. 
  loop   
                                                        -- We enter a loop to respond to requests from the client.
 where
  loop = do
    line <- hGetLine h   
    let input = words line                                      -- Each iteration of the loop reads a new line of text.
    if line == "KILL_SERVICE"                                           -- Then it checks whether the client sent "end".
       then sClose s
    else if (input !! 0) =="HELO"
       then helo h
    else do going h -- If not, we attempt to interpret the line as an integer and output double it.
    loop                                            -- Finally, we call loop again to read the next request.

helo :: Handle -> IO()
helo h = do
  hPutStr h ("HELO BASE_TEST\n" ++ "IP:10.62.0.235\n" ++ "Port:8080\n" ++ "StudentID:12301561\n" )
  hFlush h

going :: Handle -> IO()
going h = do
  hPutStr h ("im still a skeleton\n" )
  hFlush h



main = do
  args <- getArgs
  let port = (read $ head args :: Int) --HELO jbvdvnofdi  list<-line spliton " " newline (list !! 0)
  sock <- listenOn (PortNumber (fromIntegral port))              -- First, we create a network socket to listen on port 44444.
  printf "Listening on port %d\n" port
  forever $ do                                                   -- Then we enter a loop to accept connections from clients.
     (handle, host, port) <- accept sock                         -- blocks connection
     printf "Accepted connection from %s: %s\n" host (show port)
     forkFinally (talk handle port host sock) (\_ -> hClose handle)             -- makes sure handle is closed



