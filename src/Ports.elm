port module Ports exposing (storeSettings)

import Json.Encode as JE


port storeSettings : JE.Value -> Cmd msg
