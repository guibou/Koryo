import Server
import System.Environment

main :: IO ()
main = do
  startServer =<< getArgs
