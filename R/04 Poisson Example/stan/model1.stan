data {
  int N;                // number of observations
  array[N] int cases;   // case data
}

parameters {
   real<lower = 0> lambda; // parameter to estimate
}

model {
  cases ~ poisson(lambda); // statistical model for data
  lambda ~ lognormal(0, 1); // prior distribution for lambda
}

generated quantities{
    // return useful additional quantities
    real prior_lambda;
    real log_lik;
    prior_lambda = lognormal_rng(0,1);
    log_lik = poisson_lpmf(cases | lambda);
}
