-- This will act as the main server for allowing clients to access their files

import Network.Socket

main = runServer

runServer :: IO()
runServer = do
  s <- socket AF_INET Stream defaultProtocol
  --creates a TCP socket
  setSocketOption s ReuseAddr 1
  bind s (SockAddrInet 3001 iNADDR_ANY)
  listen s 5
  handleConn s

handleConn :: Socket -> IO()
handleConn s = do
  connection <- accept s
  handleData connection
  putStrLn (show connection)

handleData :: (Socket, SockAddr) -> IO()
handleData (s, _) = do
  send s "You have connected to the server, well done!"
  close s
