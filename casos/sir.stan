functions {
  // Función para r_beta_variable en tiempo
  real r_beta_t(
    real r_beta_0,
    real t,
    real periodo){
      return r_beta_0;
  }
  
  // Modelo SEIR básico
  real[] seir(
    real t,
    real[] estado,
    real[] params,
    real[] x_r,
    int[] x_i){

      // Renombrando variables
      real dydt[4];
      real S;
      real E;
      real I;
      real R;
      real r_beta;
      real alpha;
      real gamma;

      S = estado[1];
      E = estado[2];
      I = estado[3];
      R = estado[4];

      r_beta = r_beta_t(params[1], t, 10);
      alpha = params[2];
      gamma = params[3];

      // Ecuaciones diferenciales ordinarias (ODEs)
      dydt[1] = - r_beta * I * S;
      dydt[2] = r_beta * I * S - alpha * E;
      dydt[3] = alpha * E - gamma * I;
      dydt[4] = gamma * I;

      return dydt;
  }
}

data {
  int<lower = 1> n_obs;
  int<lower = 1> n_params;
  int<lower = 1> n_difeq;
  real<lower = 1> pob;
  int y[n_obs];
  real t0;
  real ts[n_obs];
  real y0[n_difeq];
}
transformed data {
  // Esto es necesario para integrar numéricamente las ODEs
  real x_r[0];
  int x_i[0];
}
parameters {
  real<lower = 0> r_beta;
}

model {
  real E_hoy[n_obs];
  real y_hat[n_obs, n_difeq];
  real acumulados_ayer;
  real params[n_params];
  
  // Estimado r_beta = contactos * transmisibilidad
  r_beta ~ normal(0.2, 0.5);
  
  params[1] = r_beta;
  params[2] = 1.0 / 5;
  params[3] = 1.0 / 10;
  
  // Integrando ODEs
  y_hat = integrate_ode_rk45(seir, y0, t0, ts, params, x_r, x_i);

  // Casos esperados por día
  for (i in 1:n_obs){
    if(i == 1)
      acumulados_ayer = 0;
    else
      acumulados_ayer = y_hat[i - 1, 3] + y_hat[i - 1, 4];

    E_hoy[i] = pob * ((y_hat[i, 3] + y_hat[i, 4]) - acumulados_ayer);
  }
  
  // Se distribuyen poisson
  y ~ poisson(E_hoy);
}
