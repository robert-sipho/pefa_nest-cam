
data {
    int<lower=1> nind;            // Number of individuals
    int<lower=0> first[nind];
    int<lower=0> last[nind];
    int<lower=2> n_occasions;     // total obs
    int<lower=1> n_years;
    int<lower=1,upper=n_years> year[nind];     // Groups
    int<lower=0,upper=1> y[nind, n_occasions];    //obs history
    vector[n_occasions - 1] age;    // age covariate
}

transformed data {
    int n_occ_minus_1 = n_occasions - 1;
}

parameters {
    real beta[n_occ_minus_1 ];  //slope for age
    real<lower=0,upper=1> mean_phi;
    real mean_epsilon;  
    vector[n_years] epsilon_year;
    real<lower=0,upper=10> sigma;
}

transformed parameters {
    matrix<lower=0,upper=1>[nind, n_occ_minus_1] phi;
    matrix<lower=0,upper=1>[nind, n_occ_minus_1] mu;
    real b0 = logit(mean_phi);

    for (i in 1:nind){
        for (t in 1:n_occ_minus_1){
            phi[i,t] = inv_logit(b0 + beta[t] * age[t] + epsilon_year[year[i]]);
            mu[i,t] = phi[i,t] * y[i,t-1];
        }
    }
}

model {
    beta ~ normal(0,10);
    epsilon_year ~ normal(mean_epsilon, sigma);

    for (i in 1:nind){
        for (t in 2:last[i]){
            y[i,t] ~ bernoulli(mu[i,t]);
        }
    }
}

generated quantities {
    real<lower=0> sigma2;
    vector<lower=0,upper=1>[n_occ_minus_1] phi_est;

    sigma2 = square(sigma);
    for (i in 1:nind){
        for (t in 1:n_occ_minus_1){
            phi_est[t] = inv_logit(mu + beta[t] * age[t] + epsilon_year[year[i]]); // age dependant surv
        }
    }
}
