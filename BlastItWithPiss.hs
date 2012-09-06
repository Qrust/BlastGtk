module BlastItWithPiss where
import Import
import BlastItWithPiss.Image
import BlastItWithPiss.Board
import BlastItWithPiss.Parsing
import BlastItWithPiss.Choice
import BlastItWithPiss.Post
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue

data MuSettings = MuSettings {pastas :: [String]
                             ,images :: [Image]}

data InMessage = ReloadCaptcha

data OutMessage = OutcomeMessage Int Outcome
                | NeedCaptcha

-- | Entry point should always be forked.
-- > t <- atomically newTQueue
-- > forkIO (entryPoint t)
entryPoint :: Board -> TVar MuSettings -> TQueue InMessage -> TQueue OutMessage -> IO ()
entryPoint b s input output = do
    undefined
