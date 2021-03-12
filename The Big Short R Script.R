library(dplyr)
library(tidyverse)

# Suppose a bank gives out 10000 loans. Default rate=0.03 and the bank lose $ 200000 in each foreclosure.
n<- 10000
loss_per_foreclosure<- -200000
p_default<- 0.03
set.seed(1)
defaults<- sample(c(0,1),n,replace=TRUE,prob=c(1-p_default,p_default))
S<- sum(defaults*loss_per_foreclosure)
S # Total amount of money lost across all foreclosures

# Monte Carlo Simulation to compute the sum of losses
B<- 10000
S<- replicate(B,{
  defaults<- sample(c(0,1),n,replace=TRUE,prob=c(1-p_default,p_default))
  sum(defaults*loss_per_foreclosure)
  
})
hist(S)

#Expected losses and standard error over 10000 loans
n*(p_default*loss_per_foreclosure)
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p_default*(1-p_default))

# Assumption: the bank gives out loans for $180000
#How much money do we need to make when people pay their loans so that our net loss is $0?
#Interest rate that needs to be charged inorder to not lose money
#x is the total amount necessary to have an expected outcome of $0
x<- -loss_per_foreclosure*p_default/(1-p_default)
x
x/180000
#With 3.4% of interest rate, the bank will lose money 50% of the time.Let's calculate the interest rate so that chance of bank losing money is 1 in 20
z<- qnorm(0.05)
z
x<- -loss_per_foreclosure * (n*p_default-z*sqrt(n*p_default*(1-p_default)))/(n*(1-p_default)+z*sqrt(n*p_default*(1-p_default)))
x
x/180000 # Interest Rate for 5% probability of losing money

