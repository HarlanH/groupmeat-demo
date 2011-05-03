# Suppose you had 5 items you wanted to assign to 3 people. Each item has a different
# value for each person. You want to maximize the total amount of value in a 
# utilitarian sort of way. But:
# Each item can only be assigned once (obviously)
# Only one of items 1 and 2 can be assigned (will give example later)
# Want relatively fair assignments, so minimize the divergence between the lowest and
# highest per-person utility.

# We'll do this twice, once without the fairness constraint, then with.

library(plyr)
library(Rsymphony)

set.seed(31415)

num.items <- 6
num.pers <- 3

# objective matrix: utilities of possible assignments
# log-norm, then normalize so each person has a total utility of 1
item.names <- sprintf('item%d', 1:num.items)
pers.names <- sprintf('pers%d', 1:num.pers)
obj.utility <- matrix(rlnorm(num.items*num.pers), 
              nrow=num.items, 
              dimnames=list(item.names, pers.names))
obj.utility <- obj.utility/matrix(colSums(obj.utility), nrow=num.items, ncol=num.pers, byrow=TRUE) #normalize
obj <- obj.utility # no parity yet!

print(obj, digits=2)

# constraint matrix has one row per constraint, of the same size as the obj matrix
mat.0 <- obj*0
mat.utility.0 <- obj.utility*0 # want this later

# for each item/row, enforce that the sum of indicators for its assignment are <= 1
mat <- laply(1:num.items, function(ii) { x <- mat.0; x[ii, ] <- 1; as.double(x) })
dir <- rep('<=', num.items)
rhs <- rep(1, num.items)

# for rows 1 and 2, enforce that the sum of indicators for their assignments are <= 1
mat <- rbind(mat, matrix(matrix(c(1, 1, rep(0, num.items-2)), nrow=num.items, ncol=num.pers), nrow=1))
dir <- c(dir, '<=')
rhs <- c(rhs, 1)

# this is an IP problem, for now
types <- rep('B', num.items * num.pers)
max <- TRUE # maximizing utility

soln <- Rsymphony_solve_LP(obj, mat, dir, rhs, types=types, max=max)

cat('\n\n---------------- Utility Only ------------------\n\n')

print(soln, digits=3)

# which items go with which person?
pers.groups <- as.vector(matrix(1:num.pers, nrow=num.items, ncol=num.pers, byrow=TRUE))

# take the solution vector, split it, and collapse as comma-sep strings
soln2assign <- function(soln, groups) {
  llply(split(soln[seq_along(groups)], groups), 
     function(x) paste(which(as.logical(x)), collapse=', '))
}
items.list <- soln2assign(soln$solution, pers.groups)

# compute the utility of that assignment for each person
soln2utility <- function(obj, soln, groups) {
  llply(split(soln[seq_along(groups)] * obj[seq_along(groups)], groups), sum)
}
pers.utility <- soln2utility(obj, soln$solution, pers.groups)

cat(sprintf('Person #%d got Items %s worth %0.2f\n',
            1:num.pers,
            items.list,
            pers.utility), sep='')
 

#######################################################################

# now add two variables to the objective function, d.upper and d.lower, that
# constrain the range of solutions

# lambda is a scaling factor indicating how much to trade off utility and parity
lambda <- 1
obj.parity <- c(-lambda, -(-lambda))
# later on, we'll turn obj.utility into a vector, and tag these on to it

# add two columns of 0s to the existing constraints, for parity
mat <- cbind(mat, 0, 0)

# now for those d.upper and d.lower variables
# \forall p, \sum_i u_i x_{i,p} - d.upper \le 0
# \forall p, \sum_i u_i x_{i,p} - d.lower \ge 0
# so, two more rows per person
d.constraint <- function(iperson, ul) { # ul = 1 for upper, 0 for lower
  x<- mat.utility.0
  x[, iperson ] <- 1 
  x <- x * obj.utility
  c(as.double(x), (if (ul) c(-1,0) else c(0,-1)))
}
mat <- rbind(mat, maply(expand.grid(iperson=1:num.pers, ul=c(1,0)), d.constraint, .expand=FALSE))
dir <- c(dir, c(rep('<=', num.pers), rep('>=', num.pers)))
rhs <- c(rhs, rep(0, num.pers*2))

# now this is a mixed-integer problem; some Boolean constraints, some continuous
num.bool.consts <- num.items * num.pers
num.cont.consts <- 2

types <- c(rep('B', num.bool.consts), rep('C', num.cont.consts))
max <- TRUE # maximizing utility

# finally, create the longer object function matrix
obj <- c(as.numeric(obj.utility), obj.parity)

soln <- Rsymphony_solve_LP(obj, mat, dir, rhs, types=types, max=max)

cat('\n\n------------- Utility + Parity ---------------\n\n')

print(soln, digits=3)

items.list <- soln2assign(soln$solution, pers.groups)

pers.utility <- soln2utility(obj, soln$solution, pers.groups)

cat(sprintf('Person #%d got Items %s worth %0.2f\n',
            1:num.pers,
            items.list,
            pers.utility), sep='')
 


