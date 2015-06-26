{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

import           Control.Monad
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Text                   (Text)
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.TH
import           GHC.Generics
import           Network.Wai
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name Text
    age Int Maybe
    deriving Show Generic
|]

instance FromJSON Person
instance ToJSON Person

type PersonAPI = "person" :> Get '[JSON] [Person]
            :<|> "person" :> ReqBody '[JSON] Person :> Put '[JSON] Person

personAPI :: Proxy PersonAPI
personAPI = Proxy


server :: ConnectionPool -> Server PersonAPI
server pool = (listPeople pool) :<|> (processPerson pool)


listPeople :: ConnectionPool -> EitherT ServantErr IO [Person]
listPeople pool = do
  persons <- liftIO $ flip runSqlPersistMPool pool $ do
    selectList [] []
  let persons' = map entityVal persons :: [Person]
  return persons'

processPerson :: ConnectionPool -> Person -> EitherT ServantErr IO Person
processPerson pool p = do
  _ <- liftIO $ flip runSqlPersistMPool pool $ do
    insert p
  return p

app :: ConnectionPool -> Application
app pool = serve personAPI (server pool)


-- mkPerson :: IO ()
mkPerson pool = do
    liftIO $ flip runSqlPersistMPool pool $ do
      insert $ Person "test" Nothing
    return ()
  where dbInfo = defaultConnectInfo { connectUser = "cody" }

migrateDB pool = do
  liftIO $ flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    return ()
  where dbInfo = defaultConnectInfo { connectUser = "cody" }


main :: IO ()
main = do
  runNoLoggingT . withMySQLPool defaultConnectInfo { connectUser = "cody" } 400 $ \pool -> liftIO $ do
    migrateDB pool
    -- add 5 people
    forM_ [1..5] (const (mkPerson pool))
    liftIO $ putStrLn "Done adding people, server starting..."
    run 8080 (app pool)
