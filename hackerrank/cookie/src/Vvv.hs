import Data.Vector.Unboxed (Vector)
import Data.ByteString (ByteString)

import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed.Mutable as MV

import Control.Monad.ST.Strict
import Data.Word
import Control.Monad

main =
  do f <- B.readFile "text.txt"
     print $ parseRGB 3 $ B.take 100 f

-- This is just the part that parses the three vectors for the colors.
-- Of course, you can embed this into an Attoparsec computation by taking
-- the current input, feeding it to parseRGB, or you can just take the right
-- sized chunk in the parser and omit the "Maybe" test from the code below.
parseRGB :: Int -> ByteString -> Maybe (Vector Word8, Vector Word8, Vector Word8)
parseRGB size input
    | 3* size > B.length input = Nothing
    | otherwise = Just $ runST $ do

        -- We are allocating three mutable vectors of size "size"
        -- This is usually a bit of pain for new users, because we have to
        -- specify the correct type somewhere, and it's not an exactly simple type.
        -- In the ST monad there is always an "s" type parameter that labels the
        -- state of the action. A type of "ST s something" is a bit similar to
        -- "IO something", except that the inner type often also contains "s" as
        -- parameter. The purpose of that "s" is to statically disallow mutable
        -- variables from escaping the ST action.
        [r, g, b] <- replicateM 3 $ MV.new size :: ST s [MV.MVector s Word8]

        -- forM_ = flip mapM_
        -- In ST code forM_ is a nicer looking approximation of the usual
        -- imperative loop.
        forM_ [0..size - 1] $ \i -> do
            let i' = 3 * i
            MV.unsafeWrite r i (B.index input $ i'    )
            MV.unsafeWrite g i (B.index input $ i' + 1)
            MV.unsafeWrite b i (B.index input $ i' + 2)

        -- freeze converts a mutable vector living in the ST monad into
        -- a regular vector, which can be then returned from the action
        -- since its type no longer depends on that pesky "s".
        -- unsafeFreeze does the conversion in place without copying.
        -- This implies that the original mutable vector should not be used after
        -- unsafeFreezing.
        [r, g, b] <- mapM V.unsafeFreeze [r, g, b]
        return (r, g, b)
