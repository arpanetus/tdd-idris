-- I didn't get a thing tbh, this is extremely insane           
same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons Refl = Refl
