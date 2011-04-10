# Suppose you had 5 items you wanted to assign to 3 people. Each item has a different
# value for each person. You want to maximize the total amount of value in a 
# utilitarian sort of way. But:
# Each item can only be assigned once (obviously)
# Only one of items 1 and 2 can be assigned (will give example later)
# Want relatively equitable assignments, so ensure each person gets at least
# half their total value.

library(plyr)
library(Rsymphony)

num.items <- 6
num.pers <- 3

# objective matrix (utilities of possible assignments)
# log-norm, then normalize so each person has a total utility of 1
obj <- matrix(rlnorm(num.items*num.pers), 
              nrow=num.items, 
              dimnames=list(sprintf('item%d', 1:num.items), sprintf('pers%d', 1:num.pers)))
obj <- obj/matrix(colSums(obj), nrow=num.items, ncol=num.pers, byrow=TRUE) #normalize
  
# constraint matrix has one row per constraint, of the same size as the obj matrix
mat.0 <- obj*0

# for each item/row, enforce that the sum of indicators for its assignment are <= 1
mat <- unlist(llply(1:num.items, function(ii) { x<- mat.0; x[ii, ] <- 1 ; x}))
dir <- rep('<=', num.items)
rhs <- rep(1, num.items)

# for rows 1 and 2, enforce that the sum of indicators for their assignments are <= 1
mat <- c(mat, as.vector(matrix(c(1, 1, rep(0, num.items-2)), nrow=num.items, ncol=num.pers)))
dir <- c(dir, '<=')
rhs <- c(rhs, 1)

num.bool.consts <- length(mat)/length(obj)
num.cont.consts <- num.pers

# for each person/col, find a solution that maximizes equality
# sum of each column is greater than a (meta-optimized) constant
mat <- c(mat, unlist(llply(1:num.pers, function(ii) { x <- mat.0; x[, ii] <- obj[,ii]; x})))
dir <- c(dir, rep('>=', num.pers))
min.utility <- 1 # start with a perfectly equitable solution and back off as needed
rhs.final <- c(rhs, rep(min.utility, num.pers))

# munge back into matrix form; human sorta-readable
mat <- matrix(mat, nrow=num.bool.consts+num.cont.consts, byrow=TRUE)

# this is a mixed-integer problem; some Boolean constraints, some continuous
types <- c(rep('B', num.bool.consts), rep('C', num.cont.consts))
max <- TRUE # maximizing utility

# now, repeatedly solve the problem, with weaker and weaker contraints on
# the equitability constraints, until we get something that works.
backoff <- .8
repeat
{
  soln <- Rsymphony_solve_LP(obj, mat, dir, rhs.final, types=types, max=max)
  
  if (!is.na(soln$status) && soln$status == 0) {
    break
  } else {
    min.utility <- min.utility * backoff
    rhs.final <- c(rhs, rep(min.utility, num.pers))
  }
}

# pretty-print: foreach person, list their items and their total utility
# then report the total of the objective function and the minimum utility fraction
pers.groups <- as.vector(matrix(1:num.pers, nrow=num.items, ncol=num.pers, byrow=TRUE))
items.list <- llply(split(soln$solution, pers.groups), 
                    function(x) paste(which(as.logical(x)), collapse=', '))
pers.utility <- colSums(obj * soln$solution)
cat(sprintf('Person #%d got Items %s worth %0.2f\n',
            1:num.pers,
            items.list,
            pers.utility), sep='')
            
cat(sprintf('\nTotal Value Assigned: %0.3f (minimum of %0.3f/person)\n',
      soln$objval, min.utility))
  
cat('\nDetails (objective, assigned):\n')
print(obj, digits=2)
print(obj*soln$solution, digits=2)
