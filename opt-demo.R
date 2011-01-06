# demo of using LP to optimize orders for groupme.at

options(stringsAsFactors=FALSE)

library(plyr)
library(Rcurl)

url.fmt <- 'http://spreadsheets.google.com/pub?key=0AnaXKp9bt6OXdFBpR0xoVGpIaW9YN2g1R3EzV3RyNkE&hl=en&single=true&gid=%d&output=csv'

sheets.idx <- 0:6
names(sheets.idx) <- c('farms', 'cuts', 'sellable', 'customer', 'customer_cut_prefs', 'customer_farm_prefs', 'customer_overall_prefs')

dat <- llply(seq_along(sheets.idx), function(i) {
  url.str <- sprintf(url.fmt, sheets.idx[[i]])
  read.csv(url(url.str))
})
names(dat) <- names(sheets.idx)

# OK, the goal is to populate a Boolean assignment matrix A_{s,c} where s is a sellable item
# and c is a customer id. With the following constraints:

# S is all of sellable, S' is subset with sold==0
# each unsold item is assigned to no more than 1 person : forall {i \in S'}, sum_{j \in C} A_{i,j} <= 1 
# customer utility is defined as sum of farm, cut, and price prefs
# UF_c = sum_{i \in S'} A_{i,c} * cfp_{c,farm(S'_i)}
# UC_c = sum_{i \in S'} A_{i,c} * ccp_{c,cut(S'_i)}
# UP_c = sum_{i \in S'} A_{i,c} * ???
# then weight UF,UC,UP by normalized overall prefs
