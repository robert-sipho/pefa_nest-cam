data {
  int num_basis; 
  matrix[num_basis, n_ind] B; 
  int<lower=0> n_ind;                  // number of nests
  int<lower=1> n_years;
  int<lower=1,upper=n_years> year[n_ind];     // Groups
  int<lower=0> last[n_ind];            // day of last observation (alive or dead)
  int<lower=0> first[n_ind];           // day of first observation (alive or dead)
  int<lower=0> max_age;                  // maximum of last
  int<lower=0> y[n_ind, max_age];       // indicator of alive nests
  real age[max_age];                 // a covariate of the nest
  }

parameters {
  real b0;                          // coef of linear pred for S
  real b_age;                          // coef of linear pred for S
  vector[n_years] epsilon_year;
  real mean_epsilon;  
  real<lower=0,upper=10> sigma;
  row_vector[num_basis] a_raw; 

}

model {
  real S[n_ind, max_age-1];             // survival probability

  a = a_raw*tau;                
  for(i in 1:n_ind){  
    for(t in first[i]:(last[i]-1)){   
      S[i,t] = inv_logit(b0 + b_age*age[t] + to_vector(b*B) + epsilon_year[year[i]]); 
    }
  }

  // priors
  b0~normal(0,5);
  a_raw ~ normal(0, 1); 
  tau ~ cauchy(0, 1);   
  b_age~normal(0,3);  
  epsilon_year ~ normal(mean_epsilon, sigma);

  // likelihood
  for (i in 1:n_ind) {
    for(t in (first[i]+1):last[i]){
      y[i,t]~bernoulli(y[i,t-1]*S[i,t-1]);
    }
  }
}
