// Difference Between Two Rates
// Section 3.2 
// Figure 3.3
data { 
  int<lower=1> n1; 
  int<lower=1> n2; 
  int<lower=0> k1;
  int<lower=0> k2;
} 
parameters {
  real<lower=0,upper=1> theta1;
  real<lower=0,upper=1> theta2;
} 
transformed parameters {
  real<lower=-1,upper=1> delta;
  // deterministic variable "delta" is the difference
  // between rates from two processes producing k1 and k2 outcomes
  delta = theta1 - theta2;
}
model {
  // Prior Distribution for Rate Theta
  theta1 ~ beta(1, 1);
  theta2 ~ beta(1, 1);
  // Observed Counts
  k1 ~ binomial(n1, theta1);
  k2 ~ binomial(n2, theta2);
}
