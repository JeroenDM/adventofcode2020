doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
  then x
  else x*2

removeLowercase st = [c | c <-st, elem c ['A'..'Z']]

iszero 0 = True
iszero _ = False
