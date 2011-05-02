# Suppose you had 5 items you wanted to assign to 3 people. Each item has a different
# value for each person. You want to maximize the total amount of value in a 
# utilitarian sort of way. But:
# Each item can only be assigned once (obviously)
# Only one of items 1 and 2 can be assigned (will give example later)
# Want relatively equitable assignments, so ensure each person gets at least
# half their total value.

library(plyr)
library(Rsymphony)

set.seed(31415)

num.items <- 6
num.pers <- 3

# objective matrix is in 2 parts
# part 1 is utilities of possible assignments
# log-norm, then normalize so each person has a total utility of 1
item.names <- sprintf('item%d', 1:num.items)
pers.names <- sprintf('pers%d', 1:num.pers)
obj.utility <- matrix(rlnorm(num.items*num.pers), 
              nrow=num.items, 
              dimnames=list(item.names, pers.names))
obj.utility <- obj.utility/matrix(colSums(obj.utility), nrow=num.items, ncol=num.pers, byrow=TRUE) #normalize
# part 2 is the two parity variables, d.upper and d.lower
lambda <- 1
obj.parity <- c(-lambda, -(-lambda))
obj <- c(as.double(obj.utility), obj.parity)

# constraint matrix has one row per constraint, of the same size as the obj matrix
mat.0 <- obj*0
mat.utility.0 <- obj.utility*0
mat.parity.0 <- obj.parity*0

# for each item/row, enforce that the sum of indicators for its assignment are <= 1
# (appending 0s for parity variables)
mat <- unlist(llply(1:num.items, function(ii) { 
    x<- mat.utility.0; 
    x[ii, ] <- 1 ; 
    c(as.double(x), mat.parity.0)
  }))
dir <- rep('<=', num.items)
rhs <- rep(1, num.items)

# for rows 1 and 2, enforce that the sum of indicators for their assignments are <= 1
mat <- c(mat, c(as.vector(matrix(c(1, 1, rep(0, num.items-2)), nrow=num.items, ncol=num.pers)),
                mat.parity.0))
dir <- c(dir, '<=')
rhs <- c(rhs, 1)

# now for those d.upper and d.lower variables
# \forall p, \sum_i u_i x_{i,p} - d.upper \le 0
# \forall p, \sum_i u_i x_{i,p} - d.lower \ge 0
# so, two more rows per person
d.constraint <- function(jj, ul) { # ul = 1 for upper, 0 for lower
  x<- mat.utility.0
  x[, jj ] <- 1 
  x <- x * obj.utility
  c(as.double(x), (if (ul) c(-1,0) else c(0,-1)))
}
mat <- c(mat, unlist(mlply(expand.grid(jj=1:num.pers, ul=c(1,0)), d.constraint, .expand=FALSE)))
dir <- c(dir, c(rep('<=', num.pers), rep('>=', num.pers)))
rhs <- c(rhs, rep(0, num.pers*2))

# #OLD
# # for each person/col, find a solution that maximizes equality
# # sum of each column is greater than a (meta-optimized) constant
# mat <- c(mat, unlist(llply(1:num.pers, function(ii) { x <- mat.0; x[, ii] <- obj[,ii]; x})))
# dir <- c(dir, rep('>=', num.pers))
# min.utility <- 1 # start with a perfectly equitable solution and back off as needed
# rhs.final <- c(rhs, rep(min.utility, num.pers))

# munge back into matrix form; human sorta-readable
mat <- matrix(mat, nrow=length(rhs), byrow=TRUE)

# this is a mixed-integer problem; some Boolean constraints, some continuous
num.bool.consts <- num.items * num.pers
num.cont.consts <- 2

types <- c(rep('B', num.bool.consts), rep('C', num.cont.consts))
max <- TRUE # maximizing utility

soln <- Rsymphony_solve_LP(obj, mat, dir, rhs, types=types, max=max)

# pretty-print: foreach person, list their items and their total utility
# then report the total of the objective function and the minimum utility fraction
pers.groups <- as.vector(matrix(1:num.pers, nrow=num.items, ncol=num.pers, byrow=TRUE))
items.list <- llply(split(soln$solution[1:length(pers.groups)], pers.groups), 
                    function(x) paste(which(as.logical(x)), collapse=', '))
pers.utility <- colSums(matrix((obj * soln$solution)[1:length(pers.groups)], nrow=num.items))
cat(sprintf('Person #%d got Items %s worth %0.2f\n',
            1:num.pers,
            items.list,
            pers.utility), sep='')
            
# cat(sprintf('\nTotal Value Assigned: %0.3f (minimum of %0.3f/person)\n',
#       soln$objval, min.utility))

obj.pretty <- function(qq, ll=length(pers.groups), rr=num.items) {
  list(weights=matrix(qq[1:ll], nrow=rr, dimnames=list(item.names, pers.names)), 
      parity=qq[(ll+1):(ll+2)])
}

cat('\nObjective function table:\n')
print(obj.pretty(obj), digits=2)
cat('\nAssigned value table:\n')
print(obj.pretty(obj*soln$solution), digits=2)
