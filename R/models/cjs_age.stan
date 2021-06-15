
functions {
  int first_capture(int[] y_i) {
    for (k in 1:size(y_i))
      if (y_i[k])
        return k;
    return 0;
  }

  int last_capture(int[] y_i) {
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
    first[i] = first_capture(y[i]);
  for (i in 1:nind)
    last[i] = last_capture(y[i]);
}

parameters {
  vector<lower=0,upper=1>[max_age] beta;  // Mean survival
}

transformed parameters {
  matrix<lower=0,upper=1>[nind, n_occ_minus_1] phi;   // Survival

  // Constraints
  for (i in 1:nind) {
    for (t in first[i]:n_occ_minus_1) {
      phi[i, t] = beta[x[i, t]];
    }
  }
}

model {
  // Priors
  // Uniform priors are implicitly defined.
  //  beta ~ uniform(0, 1);
  //  mean_p ~ uniform(0, 1);

  // Likelihood
  for (i in 1:nind) {
      if (first[i] > 0) {
      for (t in (first[i] + 1):last[i]) {
        y[i, t] ~ bernoulli(phi[i, t - 1]);
      }
      1 ~ bernoulli(phi[i, t])
    }
  }
}
