import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B8
import System.IO

client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
                addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
                let serverAddr = head addrInfo
                sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                connect sock (addrAddress serverAddr)
                msgSender sock
                sClose sock

msgSender :: Socket -> IO ()
msgSender sock = do
  let msg = B8.pack "GET /server.php?message=its+high+noon HTTP/1.0\r\n\r\n"
  send sock msg
  rMsg <- recv sock 4096
  B8.putStrLn rMsg
  
main :: IO ()
main = client "localhost" 8000
