import RotN
import OTP

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
    encode Rot text = rotEncoder text
    decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a*seed + b) `mod` maxNumber

randomCharSequence :: Int -> Int -> Int -> Int -> [Char]
randomCharSequence a b maxNumber seed = nextChar:(randomCharSequence a b maxNumber newSeed)
  where newSeed = prng a b maxNumber seed
        nextChar = toEnum newSeed

data StreamCipher = StreamCipher Int Int Int Int

instance Cipher StreamCipher where
  encode (StreamCipher a b maxNumber seed) text = applyOTP (randomCharSequence a b maxNumber seed) text
  decode (StreamCipher a b maxNumber seed) text = applyOTP (randomCharSequence a b maxNumber seed) text
