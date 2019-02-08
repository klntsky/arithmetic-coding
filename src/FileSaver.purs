module FileSaver (saveAsPlainText) where

import Prelude (Unit)
import Effect (Effect)


foreign import saveAsPlainText :: String -> String -> Effect Unit
