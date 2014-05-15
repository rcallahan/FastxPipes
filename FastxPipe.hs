module FastxPipe where

import Data.Attoparsec.Char8
import qualified Data.ByteString as B
import Blaze.ByteString.Builder
import Control.Monad
import Control.Applicative
import Data.Monoid
import Pipes
import Pipes.Internal
import qualified Pipes.ByteString as Pb
import qualified Pipes.Prelude as P
import Pipes.Attoparsec hiding (parse)

takeTill' c = takeTill (== c) <* char c
skipTill c = skipWhile (/= c) *> char c *> pure ()

parseFastq :: Parser (B.ByteString, B.ByteString, B.ByteString)
parseFastq = (,,) <$> (char '@' *> takeTill' '\n')
    <*> (takeTill' '\n' <* skipTill '\n')
    <*> (takeTill' '\n')

parseFasta :: Parser (B.ByteString, B.ByteString)
parseFasta = (,) <$> (char '>' *> takeTill' '\n') <*> multiline where
    multiline = toByteString . mconcat <$> (many1 $ fromByteString <$> do
        Just c <- peekChar
        if c == '>' then fail "next" else takeTill' '\n')

hFastqProd h = parseMany parseFastq $ Pb.fromHandle h
