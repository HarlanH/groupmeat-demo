# demo of using LP to optimize orders for groupme.at

options(stringsAsFactors=FALSE)

library(plyr)
library(RCurl)

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
	farm_id==inventory[[j, 'farm_id']])$pref_score[[1]]
	# TODO: add the other components
  }
}

# TODO: create the other constraints

# TODO: solve

# TODO: pretty-print solution

