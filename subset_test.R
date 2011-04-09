# quick test/demo of assignment problem with subset constraint

# assume 5 items and 3 customers. random utility.
# each item only assigned at most once
# items 1 and 2 only assigned at most once
num.items <- 5
num.custs <- 3
U <- matrix(rlnorm(num.items*num.custs), nrow=5, 
  dimnames=list(sprintf('item%d', 1:num.items), sprintf('cust%d', 1:num.custs)))
  
# generate constraint matrixes (list thereof)
con.list <- list(
    itemcon=0*U