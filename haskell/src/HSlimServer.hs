import Slim.Slim
import Slim.SlimServer
import System(getArgs)


main = do args <- getArgs 
          let port = readInt (head args)
          doServe port


