data{
  int n;
  // real mu;
  // real sigma;
  real alpha;
  real beta;
}
parameters{
}
generated quantities{
  real rnd[n];
  
  for(i in 1:n){
    // rnd[i] = lognormal_rng(mu, sigma);
    rnd[i] = gamma_rng(alpha, beta);
  }
}
