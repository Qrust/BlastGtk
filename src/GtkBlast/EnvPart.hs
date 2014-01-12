{-# LANGUAGE ExistentialQuantification #-}
module GtkBlast.EnvPart
    (EnvPart(..)
    ,runEnvPart
    ,runEnvParts
    ) where
import Import
import GtkBlast.Environment
import GtkBlast.Conf

data EnvPart
    = forall m. EP
        {toMutable :: Env -> Conf -> IO m
        ,toConf    :: m -> Conf -> IO Conf
        ,toEnv     :: m -> Env -> Env
        }

instance Monoid EnvPart where
    mempty = EP (\_ _ -> return undefined) (const return) (const id)
    mappend (EP tm1 tc1 te1) (EP tm2 tc2 te2) = EP
        (\ e c      -> (,) <$> tm1 e c <*> tm2 e c)
        (\ (m1, m2) -> tc1 m1 >=> tc2 m2          )
        (\ (m1, m2) -> te1 m1 >>> te2 m2          )

runEnvPart :: EnvPart -> Env -> Conf -> IO (Env -> Env, Conf -> IO Conf)
runEnvPart EP{..} e c = do
    m <- toMutable e c
    return (toEnv m, toConf m)

runEnvParts :: [EnvPart] -> Env -> Conf -> IO (Env -> Env, Conf -> IO Conf)
runEnvParts = runEnvPart . mconcat
