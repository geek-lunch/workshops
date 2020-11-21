//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower = 1> n_obs; // indicates my number of observation (length).
  
  int<lower = 1, upper = 3> sex_no [n_obs]; // a table containing for each observation (length) the corresponding sex. Female = 1, Male = 2, 
                                            // Unknown = 3.
  
  vector[n_obs] tarsus; // The vector tarsus has a length of n_obs.
  
  int<lower = 1> n_dam; // indicates the number of dam (clusters) in the data frame. We could look for that information in the environement, but we are way too lazy for that.
    
  int<lower = 1, upper = n_dam> dam_no[n_obs]; // a table containing for each observation (length) the corresponding dam number.
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b_0; // Population intercept.
  real u_dam[n_dam]; // Random effects.
  real<lower = 0> sigma_u_dam; // Standard deviation of random effects.
  real b_sex; // Population slope
  real<lower = 0> sigma; // Population standard deviation.
}

transformed parameters {
  real b_dam[n_dam]; // Varying intercepts (one per cluster)
  real mu[n_obs]; // Individual mean
  
  for (j in 1:n_dam){
    b_dam[n_dam] = b_0 + u_dam[n_dam];
  } // Varying intercepts definition
  
    for (i in 1:n_obs){
    mu[i] = b_dam[dam_no[i]] + b_sex * sex_no[i];
    }
  
  }

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  b_0 ~ std_normal();
  u_dam ~ std_normal();
  sigma_u_dam ~ std_normal();
  b_sex ~ std_normal();
  sigma ~ std_normal();
  u_dam ~ normal(0, sigma_u_dam); // Random effects distribution.
  
  for (i in 1:n_obs){
    tarsus[i] ~ normal(mu[i], sigma); //
}

}
