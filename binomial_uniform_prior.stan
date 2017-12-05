// Infering a Rate "theta" of a binary process
// Section 3.1 
// Figure 3.2
data {
  int<lower=1> n; // at least one observation
  int<lower=0> k; // success count can not be negative
}

parameters {
  real<lower=0, upper=1> theta; // rate bounded by 0 and 1
}

model {
  //Prior Distribution for Rate Theta
  theta ~ beta(1, 1);
  
  // Observed Counts
  // k has a binomial distribution of n observations with theta variation
  k ~ binomial(n, theta);
}
