everyOther : Stream ty -> Stream ty
everyOther (value :: (x :: xs)) = x :: everyOther xs

