module FastxPipe where

import Data.Attoparsec.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Blaze.ByteString.Builder
import Control.Monad
import Control.Applicative
import Control.Exception
import System.IO
import Data.Monoid
import Pipes
import Pipes.Internal
import qualified Pipes.ByteString as Pb
import qualified Pipes.Prelude as P
import Pipes.Attoparsec hiding (parse)

takeTill' c = takeTill (== c) <* char c
skipTill c = skipWhile (/= c) *> char c *> pure ()

parseFasta :: Parser (ByteString, ByteString)
parseFasta = (,) <$> (char '>' *> takeTill' '\n') <*> multiline where
    multiline = toByteString . mconcat <$> (many1 $ fromByteString <$> do
        Just c <- peekChar
        if c == '>' then fail "next" else takeTill' '\n')

hFastqProd :: Handle -> Producer (ByteString, ByteString, ByteString) IO ()
hFastqProd h = go where
    go = do
        let getline = B.hGetLine h
            doread = do
                line1 <- getline
                head <- case B8.uncons line1 of
                    Just ('@', rest) -> return rest
                    _ -> ioError $ userError "bad format"
                sq <- getline
                blank <- getline
                case B8.uncons blank of
                    Just ('+', _) -> return ()
                    _ -> ioError $ userError "bad format"
                qual <- getline
                when (B.length qual /= B.length sq) (ioError $ userError "bad format")
                return $ Just (head, sq, qual)
            catcher :: IOException -> IO (Maybe (ByteString, ByteString, ByteString))
            catcher _ = return Nothing
        mbread <- liftIO $ catch doread catcher
        case mbread of
            Just j -> yield j >> go
            Nothing -> return ()
