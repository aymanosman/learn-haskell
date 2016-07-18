import System.IO (hSetBuffering, BufferMode(..), stdin)
import Control.Concurrent (forkIO)
import qualified Sound.Pulse.Simple as Pulse

main = do
  -- print 24
  _ <- run
  main

run = do
  hSetBuffering stdin NoBuffering
  maybeNumber <- fromChar <$> getChar
  case maybeNumber of
    Just n -> dial [n]
    _ -> return ()

dial [] = return ()
dial ((x, y):xs) = do
  s1 <- getConn
  s2 <- getConn
  forkIO $ play s1 (mkPattern x)
  play s2 (mkPattern y)
  dial xs
  where
    play s p =
      Pulse.simpleWrite s p >> Pulse.simpleDrain s >> Pulse.simpleFree s

mkPattern :: Float -> [Float]
mkPattern herts =
    [sin $ 2*pi*herts*(t/44100) | t <- [1..44100*0.3]]

zero = (1336, 941)
one = (1209, 697)
two = (1336, 697)
three = (1477, 697)
four = (1209, 770)
five = (1336, 770)
six = (1477, 770)
seven = (1209, 852)
eight = (1336, 852)
nine = (1477, 852)

fromChar :: Char -> Maybe (Float, Float)
fromChar '1' = Just one
fromChar '2' = Just two
fromChar '3' = Just three
fromChar '4' = Just four
fromChar '5' = Just five
fromChar '6' = Just six
fromChar '7' = Just seven
fromChar '8' = Just eight
fromChar '9' = Just nine
fromChar '0' = Just zero
fromChar _ = Nothing

pause = (0,0)

getConn =
  let
    spec = Pulse.SampleSpec (Pulse.F32 Pulse.LittleEndian) 44100 1
    desc = "this is an example application"
  in
  Pulse.simpleNew Nothing "example" Pulse.Play Nothing desc spec Nothing
  Nothing

