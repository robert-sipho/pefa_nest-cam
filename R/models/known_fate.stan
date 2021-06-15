

functions {
  int hatch(int[] y_i) {
    for (k in 1:size(y_i))
      if (y_i[k])
        return k;
    return 0;
  }

  int last_alive(int[] y_i) {
    for (k_rev in 0:(size(y_i) - 1)) {
      // Compoud declaration was enabled in Stan 2.13
      int k = size(y_i) - k_rev;
      //      int k;
      //      k = size(y_i) - k_rev;
      if (y_i[k])
        return k;
    }
    return 0;
  }

}

data {
  int<lower=0> nind;            // Number of individuals
  int<lower=2> n_occasions;     // Number of capture occasions
  int<lower=0,upper=1> y[nind, n_occasions];    // Capture-history
  int<lower=1> max_age;         // Maximum age
  int<lower=0,upper=max_age> x[nind, n_occasions - 1];  // Age
}
transformed data {
  int n_occ_minus_1 = n_occasions - 1;
  //  int n_occ_minus_1;
  int<lower=0,upper=n_occasions> first[nind];
  int<lower=0,upper=n_occasions> last[nind];

  //  n_occ_minus_1 = n_occasions - 1;
  for (i in 1:nind)
    first[i] = hatch(y[i]);
  for (i in 1:nind)
    last[i] = last_alive(y[i]);
}
parameters {
  vector<lower=0,upper=1>[max_age] beta;  // age-survival
  real<lower=0,upper=1> mean_phi;         // Mean survival
  vector[n_occ_minus_1] epsilon;
  vector[nind] gamma;
  real<lower=0,upper=10> sigma_year;
  real<lower=0,upper=10> sigma_site;
}
transformed parameters {
  matrix<lower=0,upper=1>[nind, n_occ_minus_1] phi;   // Survival
  real mu = logit(mean_phi);

  // Constraints
  //mu = logit(beta);
  for (i in 1:nind) {
    for (t in 1:(first[i] - 1)) {
      phi[i, t] = 0;
    }

    for (t in first[i]:n_occ_minus_1) {
      phi[i, t] = inv_logit(mu + beta[x[i, t]] + epsilon[t] + gamma[i]);
    }
  }
}
model {
  // Priors
  // Uniform priors are implicitly defined.
  //  beta ~ uniform(0, 1);
  //  mean_p ~ uniform(0, 1);
  // Likelihood
  epsilon ~ normal(0, sigma_year);
  gamma ~ normal(0, sigma_site);

  for (i in 1:nind) {
      for (t in (first[i] + 1):last[i]) {
        y[i, t] ~ bernoulli(phi[i, t - 1]);
    }
  }
}
