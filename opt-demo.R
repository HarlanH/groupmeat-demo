# demo of using LP to optimize orders for groupme.at

options(stringsAsFactors=FALSE)

library(plyr)
library(RCurl)
library(lpSolve)

url.fmt <- 'http://spreadsheets.google.com/pub?key=0AnaXKp9bt6OXdFBpR0xoVGpIaW9YN2g1R3EzV3RyNkE&hl=en&single=true&gid=%d&output=csv'

sheets.idx <- 0:6
names(sheets.idx) <- c('farms', 'cuts', 'sellable', 'customer', 'customer_cut_prefs', 'customer_farm_prefs', 'customer_overall_prefs')

dat <- llply(seq_along(sheets.idx), function(i) {
  url.str <- sprintf(url.fmt, sheets.idx[[i]])
  read.csv(url(url.str))
})
names(dat) <- names(sheets.idx)

# OK, the goal is to populate a Boolean assignment matrix A_{s,c} where s is 
# a sellable item and c is a customer id. With the following constraints:

# S is all of sellable, S' is subset with sold==0
# each unsold item is assigned to no more than 1 person : 
#  forall {i \in S'}, sum_{j \in C} A_{i,j} <= 1 
# customer utility is defined as sum of farm, cut, and price prefs
# UF_c = sum_{i \in S'} A_{i,c} * cfp_{c,farm(S'_i)}
# UC_c = sum_{i \in S'} A_{i,c} * ccp_{c,cut(S'_i)}
# UP_c = sum_{i \in S'} A_{i,c} * (4-ppp(S'_i)/5)
# then weight UF,UC,UP by normalized overall prefs
# U_c = = UF_c * cop_{c,farm} + UC_C * cop_{c,cut} + UP_c * cop_{c,price}

# Or:
# Maximize \sum_i \sum_j A_{i,j} * U_{i,j} (i = customer, j = item)
# Where U_{i,j} = cop_{i,farm} + cfp_{i,farm(S'_j)} + ...
# (U is fixed weights}
# Constraints:
# Items sold exactly once
# \foreach j, \sum_i A_{i,j} = 1

inventory <- subset(dat$sellable, subset=sold==0)

# build utility matrix
U <- matrix(0, nrow=nrow(dat$customer), ncol=nrow(inventory))

for (i in seq_len(nrow(dat$customer))) {
  cust_id <- dat$customer[[i, 'customer_id']]

  for (j in seq_len(nrow(inventory))) {
  

    U[i, j] <- subset(dat$customer_overall_prefs, 
	  subset=customer_id==cust_id & weight_type=='farm')$pref_score[[1]] *
      subset(dat$customer_farm_prefs, subset=customer_id==cust_id & 
	farm_id==inventory[[j, 'farm_id']])$pref_score[[1]] +
    subset(dat$customer_overall_prefs, 
	  subset=customer_id==cust_id & weight_type=='cut')$pref_score[[1]] *
      subset(dat$customer_cut_prefs, subset=customer_id==cust_id & 
	cut_id==inventory[[j, 'cut_id']])$pref_score[[1]] +
    subset(dat$customer_overall_prefs, 
	  subset=customer_id==cust_id & weight_type=='price')$pref_score[[1]] *
	max(0, min(4, (4-inventory[[j, 'price_per_pound']]/5)))
  }
}

# create the other constraints
# contraint matrix is length(U) columns, with each row
# being a flattened matrix 
# first set of ncol(U) rows: each product goes to one customer
# second set of nrow(U) rows: each customer gets at least 75% of their portion
#this is wrong: generate in matrix format first, then vectorize only at the end
con <- rbind(laply(1:ncol(U), function(x) { 
		ret <- matrix(0, nrow(U), ncol(U))
		ret[, x] <- 1
		as.vector(ret) }),
	     laply(1:nrow(U), function(x) {
		ret <- matrix(0, nrow(U), ncol(U))
		ret[x, ] <- inventory$price_per_pound
		as.vector(ret) }))
con.dir <- c(rep('<=', ncol(U)), rep('>=', nrow(U)))
con.rhs <- c(rep(1, ncol(U)), rep(.25*sum(with(inventory, price_per_pound*weight))/nrow(U), nrow(U)))

# solve
sol <- lp('max', U, con, con.dir, con.rhs, all.bin=TRUE)

# pretty-print solution
#matrix(sol$solution, 5)

for (cust in 1:nrow(dat$customer)) {
	cat(dat$customer[[cust, 'customer_name']], ':\n')
	items <- inventory[as.logical(matrix(sol$solution, nrow(U))[cust, ]), ]
	items <- merge(merge(items, dat$cuts), dat$farms)
	a_ply(items, 1, function(item) {
				with(item, cat(cut_name, ' (', farm_name, '): ', weight, ' lbs @ $', price_per_pound, 
											 '/lb = $', weight*price_per_pound, '\n', sep='')) })
}

