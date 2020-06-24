functions {
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
      // real T;
      real r_beta;
      real alpha;
      real gamma;

      S = estado[1];
      E = estado[2];
      I = estado[3];
      R = estado[4];
      // T = estado[5];

      r_beta = params[1];
      alpha = params[2];
      gamma = params[3];
      
      // Equivalente a for con números reales
      {
        int i = 1;
        while (i < params[4]) {
          if(t >= (4 + params[4] + i)){
            // Actualizando r_beta de acuerdo a tiempo
            r_beta = params[1] * params[4 + i];
          }
          i = i + 1;
        }
      }

      // Ecuaciones diferenciales ordinarias (ODEs)
      dydt[1] = - r_beta * I * S;
      dydt[2] = r_beta * I * S - alpha * E;
      dydt[3] = alpha * E - gamma * I;
      dydt[4] = gamma * I;
      // dydt[5] = 1;

      return dydt;
  }
}

data {
  int<lower = 1> n_obs; // Número de días
  int<lower = 1> n_params;
  int<lower = 1> n_difeq; // Número de cajas en modelo
  real<lower = 1> pob; // Población
  int y[n_obs];  // Número de casos por día
  real t0;  // Día del estado inicial
  real ts[n_obs];
  real y0[n_difeq]; // Estado inicial
  int<lower = 0> n_int; // Número de intervenciones (cambios en r_beta)
  int fechas_dias[n_int]; // Días de las intervenciones 
}

transformed data {
  // Esto es necesario para integrar numéricamente las ODEs
  real x_r[0];
  int x_i[0];
}

parameters {
  real<lower = 0> T_inc;
  real<lower = 0> T_inf;
  real<lower = 0> r_beta;
  vector<lower = 0>[n_int] f_int;
}

transformed parameters {
  real<lower = 0> y_hat[n_obs, n_difeq];
  real<lower = 0> params[n_params + 1 + n_int + n_int];
  real E_hoy[n_obs];
  real acumulados_ayer;
  real<lower = 0> R_0;

  // Convirtiendo tiempos de infección a parámetros
  params[1] = r_beta;
  params[2] = 1 / T_inc;
  params[3] = 1 / T_inf;
  
  // Añadiendo effectos de intervención
  params[4] = n_int;
  for(i in 1:n_int){
    params[4 + i] = f_int[i];
    params[4 + n_int + i] = fechas_dias[i];
  }

  // Calculando R_0
  R_0 = params[1] / params[3];

  // Integrando ODEs
  y_hat = integrate_ode_rk45(seir, y0, t0, ts, params, x_r, x_i);

  // Casos esperados por día
  for (i in 1:n_obs){
    if(i == 1)
      acumulados_ayer = 0;
    else
      acumulados_ayer = y_hat[i -1, 3] + y_hat[i - 1, 4];

    E_hoy[i] = pob * ((y_hat[i, 3] + y_hat[i, 4]) - acumulados_ayer);
  }
}

model {
  T_inc ~ gamma(2.03, 2.54);
  T_inf ~ gamma(2.712, 4.06);
  r_beta ~ lognormal(3, 1);
  f_int ~ lognormal(1, 0.5);
  y ~ poisson(E_hoy);
}