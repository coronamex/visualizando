functions {
  // Modelo SEIR básico
  real[] seir_simple(
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
      int n_ints;

      S = estado[1];
      E = estado[2];
      I = estado[3];
      R = estado[4];
      
      alpha = params[1];
      gamma = params[2];
      
      // Convertir params[3] a int.
      n_ints = 1;
      while(n_ints < params[3]){
        n_ints = n_ints + 1;
      }
      
      for(i in 1:n_ints){
        if(t >= params[3 + n_ints + i]){
          // Actualizando r_beta de acuerdo a tiempo
          r_beta = params[3 + i];
        }
      }
      
      // Ecuaciones diferenciales ordinarias (ODEs)
      dydt[1] = - r_beta * I * S;
      dydt[2] = r_beta * I * S - alpha * E;
      dydt[3] = alpha * E - gamma * I;
      dydt[4] = gamma * I;
      
      return dydt;
  }
}

data {
  int<lower = 1> n_obs; // Número de días para ajustar
  int<lower = 1> n_difeq; // Número de cajas en modelo
  real<lower = 1> pob; // Población
  int y[n_obs];  // Número de casos observados por día
  real t0;  // Día del estado inicial (0)
  real ts[n_obs]; // Días en que hay observaciones para ajustar
  real y0[n_difeq]; // Estado inicial
  int<lower = 0> n_int; // Número de periodos (betas)
  int fechas_dias[n_int]; // Días de los cambios en beta
  real<lower = 0> T_inc; // Tiempo de incubación promedio
  real<lower = 0> T_inf; // Tiempo de contagioso promedio
  int<lower = 0, upper = 1> likelihood; // Bandera, ¿incorporar verosimilitud?
  real f_red; // Factor de reducción en beta a priori para cada periodo consecutivo
}

transformed data {
  // Esto es necesario para integrar numéricamente las ODEs
  real x_r[0];
  int x_i[0];
}

parameters {
  // Al tomar el valor de una lognormal a priori no necesito checar la no negatividad
  real logphi;  // Logaritmo de parametro de sobredispersión. Se usa el logaritmo para reducir la curvatura del modelo.
  vector[n_int] r_betas; // Vector de r_betas 
}

transformed parameters {
  real<lower = 0> y_hat[n_obs, n_difeq]; // Proporciones de poblacion en cada caja de acuerdo al modelo
  real params[3 + n_int + n_int]; // Parámetros para la funcion que calcula modelo seir
  real I_hoy[n_obs]; // Promedio de casos infecciosos (sintomáticos) esperados por día.
  real acumulados_ayer; // Sirve para calcular casos nuevos
  real phi; // Parámetro de sobredispersión
  
  phi = exp(logphi);
  
  // Pre parando parámetros para ODE solver
  // Convirtiendo tiempos de infección a parámetros
  params[1] = 1/ T_inc;
  params[2] = 1 / T_inf;

  // Añadiendo r_betas y días de cambio de periodo
  params[3] = n_int;
  for(i in 1:n_int){
    params[3 + i] = r_betas[i];
    params[3 + n_int + i] = fechas_dias[i];
  }

  // Integrando ODEs
  y_hat = integrate_ode_rk45(seir_simple, y0, t0, ts, params, x_r, x_i);

  // Casos infecciosos (sintomáticos) esperados por día
  for (i in 1:n_obs){
    if(i == 1)
      // acumulados_ayer = 0;
      acumulados_ayer = y0[3] + y0[4];
    else
      acumulados_ayer = y_hat[i - 1, 3] + y_hat[i - 1, 4];

    I_hoy[i] = pob * ((y_hat[i, 3] + y_hat[i, 4]) - acumulados_ayer);
  }
}

model {
  // Distribuciones a priori
  // T_inc ~ gamma(2.03, 1/2.54);
  // T_inf ~ gamma(2.712, 1/4.06);
  logphi ~ normal(3, 0.5);
  
  // Tratando de suavizar el cambio en el effecto
  for(i in 1:n_int){
    if (i == 1)
      r_betas[i] ~ lognormal(-0.5, 0.2);
    else
      r_betas[i] ~ lognormal(log(r_betas[i - 1]) - 0.02 - f_red, 0.2);
  }
  
  // Verosimilitud
  if(likelihood)
    y ~ neg_binomial_2(I_hoy, phi);
}
