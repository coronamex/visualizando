functions {
  real[] sir(
    real t,
    real[] y,
    real[] params,
    real[] x_r,
    int[] x_i){

      real dy[3];

      dy[1] = - params[1] * y[1] * y[2];
      dy[2] = params[1] * y[1] * y[2] - params[2] * y[2];
      dy[3] = params[2] * y[2];

      return dy;
  }

  real[] seir(
    real t,
    real[] estado,
    real[] params,
    real[] x_r,
    int[] x_i){

      real dydt[4];
      real S;
      real E;
      real I;
      real R;
      real beta;
      real alpha;
      real gamma;

      S = estado[1];
      E = estado[2];
      I = estado[3];
      R = estado[4];

      beta = params[1];
      alpha = params[2];
      gamma = params[3];

      dydt[1] = - beta * I * S;
      dydt[2] = beta * I * S - alpha * E;
      dydt[3] = alpha * E - gamma * I;
      dydt[4] = gamma * I;

      return dydt;
  }
}

data {
  int<lower = 1> n_obs;
  int<lower = 1> n_params;
  int<lower = 1> n_difeq;
  // int<lower = 1> n_sample;
  real<lower = 1> pob;

  int y[n_obs];
  real t0;
  real ts[n_obs];

  real y0[n_difeq];

  // real fake_ts[n_fake];
}

transformed data {
  real x_r[0];
  int x_i[0];
}


parameters {
  real<lower = 0> T_inc;
  real<lower = 0> T_inf;

  // real<lower = 0, upper = 1> S0;
}

transformed parameters {
  real y_hat[n_obs, n_difeq];
  real<lower = 0> params[n_params];
  real E_hat[n_obs];
  // real y0[n_difeq];
  //
  // y0[1] = S0;
  // y0[2] = 1 - S0;
  // y0[3] = 0;
  params[1] = 2.2 / T_inf;
  params[2] = 1 / T_inc;
  params[3] = 1 / T_inf;

  // y_hat = integrate_ode_rk45(sir, y0, t0, ts, params, x_r, x_i);
  y_hat = integrate_ode_rk45(seir, y0, t0, ts, params, x_r, x_i);

  for (i in 1:n_obs){
    E_hat[i] = pob * y_hat[i, 3];
  }
}

model {
  T_inc ~ gamma(2.03, 2.54);
  T_inf ~ gamma(2.712, 4.06);

  // params ~ normal(0, 2);
  // S0 ~ normal(0.5, 0.5);

  // y ~ binomial(n_sample, y_hat[, 2]);
  y ~ poisson(E_hat);
}
