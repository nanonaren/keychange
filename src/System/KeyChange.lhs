Utility for converting a set of keys of some type
to a contiguous set of ints. Useful for data mining applications
where you don't want to carry around large keys and where
the key content is irrelevant (e.g. strings,text,non-contig numbers,...)

In addition, the library doesn't force you to maintain the index in memory.

Naren Sundar

\begin{code}

module System.KeyChange
    (
      addKey
    , emptyToInt
    , commitToInt
    , restoreToInt
    , toFromInt
    , keycount
    , keyFromInt
    , deleteKeyChange
    , ToInt
    , FromInt
    , KeyChangeID
    ) where

import qualified Data.HashMap.Strict as H
import Data.Hashable
import Data.Tuple (swap)
import System.Directory (getCurrentDirectory,removeFile)
import System.IO
import Control.DeepSeq

newtype KeyChangeID = KID FilePath
newtype ToInt a = ToInt (H.HashMap a Int)
newtype FromInt a = FromInt (H.HashMap Int a)

emptyToInt = ToInt H.empty

keycount (ToInt mp) = H.size mp

keyFromInt :: FromInt a -> Int -> a
keyFromInt (FromInt h) i = h H.! i

addKey :: (Eq a,Hashable a) => a -> ToInt a -> Either (ToInt a,Int) Int
addKey a (ToInt h) =
    case H.lookup a h of
      Nothing -> let idx = H.size h + 1
                 in idx `seq` Left $ (ToInt $ H.insert a idx h, idx)
      Just key -> Right key

commitToInt :: (Eq a,Show a,Hashable a) => ToInt a -> IO KeyChangeID
commitToInt (ToInt hmap) = do
  dir <- getCurrentDirectory
  (fp,h) <- openTempFile dir "keychange.dat"
  hPutStr h (show $ H.toList hmap)
  hClose h
  return (KID fp)

restoreToInt :: (NFData a,Eq a,Hashable a,Read a) => KeyChangeID -> IO (ToInt a)
restoreToInt (KID fp) = do
  hs <- read `fmap` readFile fp
  return $ ToInt $!! H.fromList $ hs

toFromInt :: (Eq a,Hashable a) => ToInt a -> FromInt a
toFromInt (ToInt h) = FromInt $ H.fromList.map swap.H.toList $ h

deleteKeyChange :: KeyChangeID -> IO ()
deleteKeyChange (KID fp) = removeFile fp

\end{code}