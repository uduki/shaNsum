{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.Digest.Pure.SHA
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prelude hiding (catch)
import System.Environment (getArgs)

getHasher :: Int -> B.ByteString -> T.Text
getHasher n
    | n == 224  = f sha224 " SHA224"
    | n == 256  = f sha256 " SHA256"
    | n == 384  = f sha384 " SHA384"
    | n == 512  = f sha512 " SHA512"
    | otherwise = f sha1 " SHA1"
    where
    f g ex = (`T.append` ex) . T.pack . showDigest . g


appHasher :: (B.ByteString -> T.Text) -> [FilePath] -> IO [Either T.Text T.Text]
appHasher h = mapM (f h)
    where
    f g fp = (Right . (`T.append` T.pack (' ':fp)) . g <$> B.readFile fp) `catch` (\(SomeException e) -> return $ Left . T.pack . show $ e)


main :: IO ()
main = getArgs >>= main'
    where
    main' xs
        | (a:as) <- xs
        , all isDigit a
            = f (read a) as
        | otherwise
            = f 1 xs
    f a b = appHasher (getHasher a) b >>= mapM_ (either TIO.putStrLn TIO.putStrLn)


