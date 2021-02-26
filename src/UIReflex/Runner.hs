module UIReflex.Runner where

import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Network.WebSockets (defaultConnectionOptions)

import Language.Javascript.JSaddle.Types (JSM)
import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.WebSockets
import Network.Wai.Middleware.Static (static)

import qualified UIReflex.UI as UI

run :: Int -> JSM () -> IO ()
run port f = do
    app <- jsaddleOr defaultConnectionOptions (f >> syncPoint) jsaddleApp
    runSettings (setPort port (setTimeout 3600 defaultSettings)) (static app)

runUI names = run 3003 (UI.ui names)

runUIDeveloper names = run 3003 (UI.uiDeveloper names)
