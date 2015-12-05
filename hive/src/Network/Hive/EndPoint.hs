{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | Module implementing the specification of end points for the Hive.
module Network.Hive.EndPoint
    ( EndPoint (..)
    , HttpEndPoint (..)
    , WsEndPoint (..)
    , Accept (..)
    , Path (..)
    , HttpRoute (..)
    , WsRoute (..)
    , GuardedHttpRoute
    , EndPointPairs
    , Hive
    , runHive
    , separateEndPoints

    -- Create HttpRoutes.
    , get
    , post
    , put
    , delete
    , defaultRoute

    -- Create WsRoutes
    , webSocket

    -- Manipulate Routes.
    , (</>)
    , (</:>)

    -- Manipulate HttpRoutes.
    , accepts

    -- Insert EndPoints.
    , handledBy
    , servedBy

    -- Default method for the default route.
    , methodDefault
    ) where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Writer.Strict ( WriterT
                                   , MonadWriter
                                   , execWriterT
                                   , tell
                                   )
import Data.List (foldl')
import Data.Text (Text)
import Network.Hive.Handler (Handler, HandlerResponse)
import Network.Hive.Server (Server)
import Network.HTTP.Types ( Method
                          , methodGet
                          , methodPost
                          , methodPut
                          , methodDelete
                          )

-- | The Hive monad. The Hive is a writer to which EndPoints are written
-- when the Hive is built.
newtype Hive a =
    Hive { extrWriter :: WriterT [EndPoint] Identity a }
    deriving (Applicative, Functor, Monad, MonadWriter [EndPoint])

-- | EndPoint descriptors which are mapping a route to the actions serving
-- it.
data EndPoint
    = Http      !HttpEndPoint
    | WebSocket !WsEndPoint

data HttpEndPoint
    = HttpEndPoint
        { httpMethod  :: !Method
        , httpPath    :: ![Path]
        , httpAccept  :: !Accept
        , httpHandler :: Handler HandlerResponse
        }

data WsEndPoint
    = WsEndPoint
        { wsPath   :: ![Path]
        , wsServer :: Server ()
        } 

-- | Content type quard to Http routes. The Accept is guarding the "Accept"
-- header field in the request.
data Accept
    = Anything
    deriving (Eq, Show)

-- | Components of a URL path.
data Path
    = Path !Text
    -- ^ A "constant" component.
    --
    | Capture !Text
    -- ^ A variable component that can be captured.
    deriving (Eq, Show)

-- | A Http route. Carries both a method and a path.
data HttpRoute = 
    HttpRoute
      { method :: !Method
      , path   :: ![Path]
      }
    deriving (Eq, Show)

-- | A Web Socket route. Carries only a path.
newtype WsRoute = WsRoute [Path]
    deriving (Eq, Show)

-- | A guarded Http route.
type GuardedHttpRoute = (HttpRoute, Accept)

-- | A separation of the different kind of EndPoints
type EndPointPairs = ([HttpEndPoint], [WsEndPoint])

-- | Type class to provide a generalized interface towards both Http and
-- Web Socket routes.
class Route a where
    -- Append a path to the route.
    addPath :: a -> Path -> a

-- | Route instance for HttpRoute.
instance Route HttpRoute where
    addPath route p = route { path = path route ++ [p] }

-- | Route instance for WsRoute.
instance Route WsRoute where
    addPath (WsRoute p) p' = WsRoute (p ++ [p'])

-- | Run the Hive monad. The output is a list of EndPoints.
runHive :: Hive () -> [EndPoint]
runHive action = runIdentity $ execWriterT (extrWriter action)

-- | Separate the list of generic enpoints into a pair of list with the
-- specialized type.
separateEndPoints :: [EndPoint] -> EndPointPairs
separateEndPoints = foldl' separate ([], [])
    where
      separate :: EndPointPairs -> EndPoint -> EndPointPairs
      separate (hs, ws) (Http ep)      = (hs ++ [ep], ws)
      separate (hs, ws) (WebSocket ep) = (hs, ws ++ [ep])

-- | Make an empty route for "GET".
get :: HttpRoute
get = HttpRoute
        { method = methodGet
        , path   = []
        }

-- | Make an empty route for "POST".
post :: HttpRoute
post = HttpRoute
         { method = methodPost
         , path   = []
         }

-- | Make an empty route for "PUT".
put :: HttpRoute
put = HttpRoute
        { method = methodPut
        , path   = []
        }

-- | Make an empty route for "DELETE".
delete :: HttpRoute
delete = HttpRoute
           { method = methodDelete
           , path   = []
           }

-- | Make a default route.
defaultRoute :: GuardedHttpRoute
defaultRoute = ( HttpRoute
                   { method = methodDefault
                   , path   = []
                   }
               , Anything )

-- | Make an empty route for a Web Socket.
webSocket :: WsRoute
webSocket = WsRoute []

-- | Append a path component to a route.
(</>) :: Route a => a -> Text -> a
(</>) route text = addPath route (Path text)

-- | Append a capture component to a route.
(</:>) :: Route a => a -> Text -> a
(</:>) route text = addPath route (Capture text)

-- | Build a GuardedHttpRoute.
accepts :: HttpRoute -> Accept -> GuardedHttpRoute
accepts = (,)

-- | Insert a GuardedHttpRoute with its Handler into the Hive.
handledBy :: GuardedHttpRoute -> Handler HandlerResponse -> Hive ()
handledBy (route, accept) handler = 
    tell 
      [ Http HttpEndPoint
               { httpMethod  = method route
               , httpPath    = path route
               , httpAccept  = accept
               , httpHandler = handler
               }
      ] 

-- | Insert a WsRoute with its Server into the Hive.
servedBy :: WsRoute -> Server () -> Hive ()
servedBy (WsRoute p) server =
    tell
      [ WebSocket WsEndPoint
                  { wsPath   = p
                  , wsServer = server 
                  }
      ]

-- | A "magic" http method for implementing default routes.
methodDefault :: Method
methodDefault = "DEFAULT"

