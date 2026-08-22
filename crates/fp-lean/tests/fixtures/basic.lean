-- basic.lean: exercises every construct in the v1 subset
def add (x : Nat) (y : Nat) : Nat := x + y

def maxi (a : Nat) (b : Nat) : Nat :=
  if a > b then a else b

def compute (n : Nat) : Nat :=
  let doubled := n * 2;
  let incremented := doubled + 1;
  maxi incremented n

def greet (flag : Bool) : String :=
  if flag then "yes" else "no"

def safe_index (x : {n : Nat // n >= 0}) : Nat := x
