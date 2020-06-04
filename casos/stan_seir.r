library(tidyverse)
library(rstan)

# 
# 
# 
# library(deSolve)
# 
# # To simulate the data, we need to assign initial conditions.
# # In practice, these will likely be unknown, but can be estimated from the data.
# 
# I0 = 0.02    # initial fraction infected
# S0 = 1 - I0 # initial fraction susceptible
# R0 = 0
# 
# # Assign transmission and pathogen-induced death rates:
# beta = 0.60
# gamma = 0.10
# 
# # We will use the package deSolve to integrate, which requires certain data structures.
# # Store parameters and initial values
# # Parameters must be stored in a named list.
# params <- list(beta = beta,
#                gamma = gamma)
# 
# # Initial conditions are stored in a vector
# inits <- c(S0, I0, R0)
# 
# # Create a time series over which to integrate.
# # Here we have an epidemic that is observed over t_max number of days (or weeks or etc).
# t_min = 0
# t_max = 50
# times = t_min:t_max
# 
# # We must create a function for the system of ODEs.
# # See the 'ode' function documentation for further insights.
# SIR <- function(t, y, params) {
#   with(as.list(c(params, y)), {
#     
#     dS = - beta * y[1] * y[2]
#     
#     dI = beta * y[1] * y[2] - gamma * y[2]
#     
#     dR = gamma * y[2]
#     
#     res <- c(dS,dI,dR)
#     list(res)
#   })
# }
# 
# # Run the integration:
# out <- ode(inits, times, SIR, params, method="ode45")
# 
# # Store the output in a data frame:
# out <- data.frame(out)
# colnames(out) <- c("time", "S", "I", "R")
# 
# # quick plot of the epidemic
# plot(NA,NA, xlim = c(t_min, t_max), ylim=c(0, 1), xlab = "Time", ylab="Fraction of Host Population")
# lines(out$S ~ out$time, col="black")
# lines(out$I ~ out$time, col="red")
# legend(x = 30, y = 0.8, legend = c("Susceptible", "Infected"), 
#        col = c("black", "red"), lty = c(1, 1), bty="n")
# 
# 
# 














args <- list(serie_real = "../datos/datos_abiertos/serie_tiempo_nacional_confirmados.csv",
             modelo_stan = "casos/sir.stan")
Dat <- read_csv(args$serie_real)
Dat <- Dat %>% select(fecha, sintomas_acumulados)
Dat <- Dat[50: 70, ]
Dat <- Dat %>% mutate(dia = as.numeric(fecha - min(fecha)) + 1)
Dat

stan_datos <- list(n_obs = nrow(Dat),
                   n_params = 2,
                   n_difeq = 3,
                   n_sample = 1e6,
                   n_fake = nrow(Dat) + 10,
                   y = Dat$sintomas_acumulados,
                   t0 = 0,
                   ts = Dat$dia,
                   fake_ts = 1:(nrow(Dat) + 10))

# sample_days = 20 # number of days sampled throughout the epidemic
# sample_n = 25 # number of host individuals sampled per day
# # Choose which days the samples were taken. 
# # Ideally this would be daily, but we all know that is difficult.
# sample_time = sort(sample(1:t_max, sample_days, replace=F))
# 
# # Extract the "true" fraction of the population that is infected on each of the sampled days:
# sample_propinf = out[out$time %in% sample_time, 3]
# 
# # Generate binomially distributed data.
# # So, on each day we sample a given number of people (sample_n), and measure how many are infected.
# # We expect binomially distributed error in this estimate, hence the random number generation.
# sample_y = rbinom(sample_days, sample_n, sample_propinf)
# 
# stan_datos <-  list(n_obs = sample_days,
#                     n_params = length(params),
#                     n_difeq = length(inits),
#                     n_sample = sample_n,
#                     n_fake = length(1:t_max),
#                     y = sample_y,
#                     t0 = 0,
#                     ts = sample_time,
#                     fake_ts = c(1:t_max))

m1.stan <- stan("casos/sir.stan", data = stan_datos,
                pars = c("y_hat", "y0", "params", "fake_I"),
                chains = 1, iter = 100)
