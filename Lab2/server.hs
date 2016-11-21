
--Jack Skehan 12301561

{-# LANGUAGE BangPatterns #-}

import Network
import System.IO
import System.Environment (getArgs)
import Control.Exception.Base
import Control.Concurrent
import Control.Monad
import Network.BSD
import Text.Printf
import System.Exit

maxConnections :: Int
maxConnections = 200

getFQDN :: IO HostName
getFQDN = liftM hostName (getHostName >>= getHostByName)

-- Semaphore

newtype Semaphore = Semaphore (MVar Int)

newSemaphore :: Int -> IO Semaphore
newSemaphore i = do
  m <- newMVar i
  return (Semaphore m)

checkSemaphore :: Semaphore -> IO Bool
checkSemaphore (Semaphore m) =
    modifyMVar m $ \i ->
        if i == 0 then return (i, False)
        else let !z = i-1 in return (z, True)

signalSemaphore :: Semaphore -> IO ()
signalSemaphore (Semaphore m) =
    modifyMVar m $ \i ->
        let !z = i+1 in return (z, ())
-----------------------------------------------


main = do
  args <- getArgs
  let port = (read $ head args :: Int) --HELO jbvdvnofdi  list<-line spliton " " newline (list !! 0)
  sock <- listenOn (PortNumber (fromIntegral port))              -- First, we create a network socket to listen on port 44444.
  sem <- newSemaphore maxConnections
  host <- getFQDN
  
  acceptConnections sock host port sem

acceptConnections :: Socket -> HostName -> Int -> Semaphore -> IO ()
acceptConnections sock host port sem = do
    res <- try $ accept sock :: IO (Either IOError (Handle, HostName, PortNumber))
    case res of
        Left _ -> do
            putStrLn "Terminating..."
            exitSuccess
        Right (handle, _, _) -> do
            hSetBuffering handle NoBuffering

            canAquireSem <- checkSemaphore sem
            if canAquireSem then do
                void $ forkIO $ talk handle port host sock sem
                acceptConnections sock host port sem
            else do
                hPutStrLn handle "SERVER_BUSY"
                hClose handle
                acceptConnections sock host port sem




talk :: Handle -> Int -> HostName-> Socket -> Semaphore -> IO ()
talk h port host s sem= do

  hSetBuffering h $ BlockBuffering Nothing                               -- First, we set the buffering mode for the Handle to line buffering. 
  loop   
                                                        -- We enter a loop to respond to requests from the client.
 where
  loop = do
    line <- hGetLine h     
    case head $ words line of                                   -- Each iteration of the loop reads a new line of text.
      "HELO" -> hPutStr h ("HELO BASE_TEST\n" ++ "IP:10.62.0.235\n" ++ "Port:8080\n" ++ "StudentID:12301561\n")
      "KILL_SERVICE" ->hPutStr h "Terminating server...\n" >> sClose s >> exitSuccess
      _ -> hPutStr h "im still a skeleton\n" 
    hFlush h
    signalSemaphore sem
    loop

  

                                 -- unimplemented

  --printf "Listening on port %d\n" port
  --forever $ do                                                   -- Then we enter a loop to accept connections from clients.
  --   (handle, host, port) <- accept sock                         -- blocks connection
  --   printf "Accepted connection from %s: %s\n" host (show port)
  --   forkFinally (talk handle port host sock) (\_ -> hClose handle)             -- makes sure handle is closed



