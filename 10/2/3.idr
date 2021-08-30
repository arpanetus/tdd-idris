import Data.Nat.Views

toBinary : (num: Nat) -> String
toBinary num with (halfRec num)
  toBinary Z | HalfRecZ = ""
  toBinary (n + n) | (HalfRecEven rec) = toBinary n ++ "0"
  toBinary (S (n + n)) | (HalfRecOdd rec) = toBinary n ++ "1"


-- I managed to accomplish it on my own 😳

