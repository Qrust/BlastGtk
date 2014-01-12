{-# LANGUAGE RankNTypes #-}
module BlastItWithPiss.Types
    (module BlastItWithPiss.Types
    ) where

import Import

import BlastItWithPiss.Blast
import BlastItWithPiss.Captcha
import BlastItWithPiss.Image
import BlastItWithPiss.Board
import BlastItWithPiss.Parsing
import BlastItWithPiss.Choice

import Control.Concurrent.STM.FinalizerTVar

import Text.Recognition.Antigate

import qualified Data.Text as T
import Data.Set (Set)

import Control.Concurrent.Lifted
import Control.Concurrent.STM
    (
      TVar
    , newTVarIO
    )

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Resource

import Data.Time.Clock.POSIX

import qualified Text.Show as Show

-- | Global
data ShSettings = ShSettings
    -- NOTE HACK FIXME all browser state accumulated in *gens is lost.
    -- NOTE HACK FIXME *gens do not bypass cloudflare
    {tpastagen         :: !(TVar (TempGenType TempBlastPastaChannel))
    ,timagegen         :: !(TVar (TempGenType Image))
    ,tvideogen         :: !(TVar (TempGenType Text))

    ,tuseimages        :: !(TVar Bool)
    ,tappendjunkimages :: !(TVar Bool)
    ,tmakewatermark    :: !(TVar Bool)

    ,tallowedmodes     :: !(TVar (Set Mode))
    ,tsagemode         :: !(TVar SageMode)

    ,tposttimeout      :: !(TVar (Maybe Double))
    ,tthreadtimeout    :: !(TVar (Maybe Double))
    ,tfluctuation      :: !(TVar (Maybe Double))

    ,tcaptchaserver    :: !(FinalizerTVar (CaptchaServer Blast (ResourceT IO)))
    ,tstartsignal      :: !(TVar Bool)
    }

-- | PerBoard
data MuSettings = MuSettings
    {mthread           :: !(TVar (Maybe Int))
    ,mmode             :: !(TVar (Maybe Mode))
    ,mposttimeout      :: !(TVar (Maybe Double))
    ,mthreadtimeout    :: !(TVar (Maybe Double))

    ,_mMutAgentCount   :: !(TVar Int)
    }

-- | PerProxy
data ProxySettings = ProxyS
    {pSharedCookies    :: !(MVar (Maybe CookieJar))
    ,pUserAgent        :: !UserAgent
    }

defMuS :: IO MuSettings
defMuS = do
    mthread <- newTVarIO Nothing
    mmode <- newTVarIO Nothing
    mposttimeout <- newTVarIO Nothing
    mthreadtimeout <- newTVarIO Nothing
    _mMutAgentCount <- newTVarIO 0
    return MuSettings{..}

defPrS :: IO ProxySettings
defPrS = do
    pSharedCookies <- newMVar Nothing
    pUserAgent <- newUserAgent
    return ProxyS{..}

data PresolverState
    = PresolverState
        { presolverStored   :: !Int
        , presolverEnRoute  :: !Int
        , presolverMaxLimit :: !Rational
        , presolverKeysIn   :: !Int
        }
  deriving (Eq, Show)

newtype CaptchaServer m m' = CaptchaServer
    { getCaptchaAnswer
        :: Maybe Int
        -> BlastLog (Maybe (CaptchaAnswerWithReport m m'))
    }

data CaptchaAnswerWithReport m m'
    = CAWR
        {
        -- | Retrieve captcha answer
         cawrAnswer :: !(CAnswer m m')
        -- | Use this when the answer was wrong
        ,cawrReportFailure :: !(OriginStamp -> m ())

{- If we want to save captchas for future use, to train OCR
        ,cawrReportSuccess :: !(OriginStamp -> m ())
-}
{- Nah. Once captcha is checked once, it cannot be reused.
        -- | Use this if you didn't have an opportunity to use canswer, so that
        -- others in the pool can use it.
        --
        --  cawrAnswer should not be used afterwards
        ,cawrReturn :: m ()
-}
        }

-- Merge with OriginStamp
data CaptchaType
    = CaptchaPosting
    | CaptchaCloudflare

data CaptchaRequest = CaptchaRequest
    {
    -- | Captcha's origin — cloudflare or ssach itself
     captchaType     :: !CaptchaType
    -- | Image itself (image type is either not sent or is in the filename)
    ,captchaBytes    :: !LByteString
    -- | Filename MIGHT OR MIGHT NOT contain correct file extension
    ,captchaFilename :: !String
    -- | Captcha properties
    ,captchaConf     :: !CaptchaConf
    -- | Send captcha answer through this closure
    ,captchaSend     :: !(CaptchaAnswer -> IO ())
    }

data CaptchaAnswer
    = Answer !String !(OriginStamp -> IO ())
    | ReloadCaptcha
    | AbortCaptcha

data Message
    = OutcomeMessage !Outcome
    | LogMessage     !Text
--    | SolveCaptcha   !CaptchaRequest

data CaptchaOrigin
    = AgentCaptcha !OriginStamp
    | PresolverCaptcha
  deriving (Eq)

renderCaptchaOrigin :: CaptchaOrigin -> Text
renderCaptchaOrigin (AgentCaptcha o) = renderCompactStamp o
renderCaptchaOrigin PresolverCaptcha = "[presolver]"

data Perhaps a
    = Certainly a
    | Undecided
  deriving (Eq)

instance Show a => Show (Perhaps a) where
    show Undecided     = "Undecided"
    show (Certainly a) = show a

data OriginStamp
    = OriginStamp
        {oProxy  :: !BlastProxy
        ,oBoard  :: !Board
        ,oMode   :: !(Perhaps Mode)
        ,oThread :: !(Maybe Int)
        }
  deriving (Eq)

data OutMessage = OutMessage !OriginStamp !Message

data BlastLogData = BlastLogData
    {bProxy       :: !BlastProxy
    ,bBoard       :: !Board
    ,bShS         :: !ShSettings
    ,bMuS         :: !MuSettings
    ,bPrS         :: !ProxySettings
    ,bOut         :: !(OutMessage -> IO ())
    ,bCaptchaOut  :: !(CaptchaOrigin
                    -> CaptchaRequest
                    -> IO ())
    -- | Kludge
    ,bOtherFields :: ![Part Blast (ResourceT IO)]
    }

data OriginState = OriginState
    {gmode   :: !(Perhaps Mode)
    ,gthread :: !(Maybe Int)
    }

data BlastLogState = BlastLogState
    {bsOriginState             :: !OriginState
    ,bsAdaptivityIn            :: !Bool
    ,bsOptimisticLastPostTime  :: !POSIXTime
    ,bsPessimisticLastPostTime :: !POSIXTime
    ,bsLastThreadTime          :: !POSIXTime
    -- ^ Always pessimistic
    }

instance Default BlastLogState where
    def = BlastLogState def False 0 0 0

type BlastLog = ReaderT BlastLogData (StateT BlastLogState Blast)

instance Show CaptchaAnswer where
    show (Answer a _) = "Answer " ++ show a ++ " <repBad>"
    show ReloadCaptcha = "ReloadCaptcha"
    show AbortCaptcha = "AbortCaptcha"

renderCompactStamp :: OriginStamp -> Text
renderCompactStamp (OriginStamp proxy board _ _) =
    renderBoard board ++ " {" ++ show proxy ++ "}"

renderFullStamp :: OriginStamp -> Text
renderFullStamp (OriginStamp proxy board mode thread) =
    "{" ++ show proxy ++ "} " ++ renderBoard board ++ " "
    ++ show mode ++ if isJust thread
      then " [| " ++ ssachThread board thread ++ " |]"
      else ""

instance Show Message where
    show (OutcomeMessage o) = show o
    show (LogMessage o) = T.unpack o
--    show SolveCaptcha{} = "SolveCaptcha"

instance Default OriginState where
    def = OriginState Undecided Nothing

instance NFData CaptchaType

instance NFData CaptchaAnswer where
    rnf (Answer s r) = r `seq` rnf s
    rnf _ = ()

instance NFData a => NFData (Perhaps a) where
    rnf (Certainly a) = rnf a
    rnf Undecided = ()

instance NFData OriginStamp where
    rnf (OriginStamp p b m th) = rnf (p,b,m,th)

instance NFData CaptchaRequest where
    rnf (CaptchaRequest a b c d e) = rnf(a,b,c,d,e)

instance NFData Message where
    rnf (OutcomeMessage o) = rnf o
    rnf (LogMessage s) = rnf s
--    rnf (SolveCaptcha s) = rnf s
--    rnf _ = ()

instance NFData OutMessage where
    rnf (OutMessage os m) = os `deepseq` m `deepseq` ()

data SageMode
    = SageDisabled
    | SageEnabled
    | SageAccordingToMode
  deriving (Eq, Show)

fromSageMode :: SageMode -> Mode -> Bool
fromSageMode s m = case s of
    SageDisabled -> False
    SageEnabled  -> True
    SageAccordingToMode -> obligatorySageMode m

newtype TempGenType a =
    TempGenType ((Int -> IO Thread) -> Maybe Page -> Maybe Int -> IO a)

mkFullGen
    :: ((Int -> IO Thread) -> Maybe Page -> Maybe Int -> IO a)
    -> TempGenType a
mkFullGen = TempGenType

mkIgnoreGen :: IO a -> TempGenType a
mkIgnoreGen = TempGenType . const . const . const

mkConstGen :: a -> TempGenType a
mkConstGen = mkIgnoreGen . return

evalGen
    :: TempGenType a
    -> (Int -> IO Thread)
    -> Maybe Page -> Maybe Int
    -> BlastLog a
evalGen (TempGenType gen) ioGetThread mpage thread = do
    liftIO $ gen ioGetThread mpage thread

runBlastLog :: BlastLogData -> BlastLog a -> Blast a
runBlastLog d m = evalStateT (runReaderT m d) def

runBlastLogSt :: BlastLogData -> BlastLogState -> BlastLog a -> Blast a
runBlastLogSt d o m = evalStateT (runReaderT m d) o

blastLogControl :: ((forall a. BlastLog a -> IO a) -> BlastLog b) -> BlastLog b
blastLogControl b = do
    _readerState <- ask
    _stateState <- lift get
    _blastState <- blast getBlastState
    _manager <- blast httpGetManager

    b $ runBlastFromState _manager _blastState
      . runBlastLogSt _readerState _stateState

blast :: Blast a -> BlastLog a
blast = lift . lift

setAdaptive :: Bool -> BlastLog ()
setAdaptive n = lift $ modify $ \s -> s{bsAdaptivityIn = n}

recMode :: Mode -> BlastLog ()
recMode m =
    lift $ modify $ \s@BlastLogState{ bsOriginState } ->
        s{bsOriginState=bsOriginState{gmode = Certainly m}}

recThread :: (Maybe Int) -> BlastLog ()
recThread t =
    lift $ modify $ \s@BlastLogState{ bsOriginState } ->
        s{bsOriginState=bsOriginState{gthread = t}}

recResetMode :: BlastLog ()
recResetMode =
    lift $ modify $ \s@BlastLogState{ bsOriginState } ->
        s{bsOriginState=bsOriginState{gmode = Undecided}}

recResetThread :: BlastLog ()
recResetThread =
    lift $ modify $ \s@BlastLogState{ bsOriginState }
        -> s{bsOriginState=bsOriginState{gthread = Nothing}}

setLastThreadTime :: POSIXTime -> BlastLog ()
setLastThreadTime t =
    lift $ modify $ \s -> s{bsLastThreadTime = t}

setOptimisticLastPostTime :: POSIXTime -> BlastLog ()
setOptimisticLastPostTime t =
    lift $ modify $ \s -> s{bsOptimisticLastPostTime = t}

setPessimisticLastPostTime :: POSIXTime -> BlastLog ()
setPessimisticLastPostTime t =
    lift $ modify $ \s -> s{bsPessimisticLastPostTime = t}

genOriginStamp :: BlastLog OriginStamp
genOriginStamp = do
    BlastLogData {
      bBoard
    , bProxy
    } <- ask

    BlastLogState{
      bsOriginState = OriginState
        { gmode
        , gthread
        }
    } <- lift get

    return $ OriginStamp bProxy bBoard gmode gthread

blastOut :: Message -> BlastLog ()
blastOut msg = do
    BlastLogData{ bOut } <- ask

    stamp <- genOriginStamp

    liftIO $ bOut $!! OutMessage stamp msg

blastCaptcha :: CaptchaRequest -> BlastLog ()
blastCaptcha cr = do
    BlastLogData { bCaptchaOut } <- ask

    stamp <- genOriginStamp

    liftIO $ bCaptchaOut (AgentCaptcha stamp) cr

blastLog :: Text -> BlastLog ()
blastLog = blastOut . LogMessage

data TempBlastPastaChannel
    = TBPC
        {escinv :: !Bool
        ,escwrd :: !Bool
        ,pasta  :: !String
        }
