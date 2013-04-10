{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
module MusicBrainz.Email (Email(..), Template(..), outboxExchange) where

--------------------------------------------------------------------------------
import Data.Data (Data, Typeable)


--------------------------------------------------------------------------------
import qualified Data.Text as Text
import qualified Network.Mail.Mime as Mail


--------------------------------------------------------------------------------
deriving instance Data Mail.Address
deriving instance Typeable Mail.Address

data Email = Email
    { emailTemplate :: Template
    , emailTo :: Mail.Address
    , emailFrom :: Mail.Address
    }
  deriving (Data, Typeable)


--------------------------------------------------------------------------------
data Template = PasswordReset { passwordResetEditor :: Text.Text }
  deriving (Data, Typeable)


--------------------------------------------------------------------------------
outboxExchange :: String
outboxExchange = "outbox"
