library(tidyverse)
library(brms)

#' Probabilidades posteriorers regresión logística riesgos
#'
#' @param d_pred  
#' @param model 
#' @param coef_ii 
#' @param re_ii 
#'
#' @return
#' @export
#'
#' @examples
pred_brms_logistic <- function(d_pred, model, coef_ii = 1:15, re_ii = 16:17){
  # d_pred <- d_pred
  # model <- m.def
  # coef_ii <- 1:15
  # re_ii <- 16:17
  
  # Extraer posterior de modelo brms
  post <- posterior_samples(model, subset = NULL)[,c(coef_ii, re_ii)] %>%
    as_tibble()
  # post
  
  # Re ajustar indices
  coef_ii <- 1:length(coef_ii)
  re_ii <- (length(coef_ii) + 1):(length(coef_ii) + length(re_ii))
  
  # Cambiar "efectos aleatorios" por predicción
  for(i in re_ii){
    post[,i] <- rnorm(n = nrow(post), mean = 0, sd = post[,i] %>% unlist %>% as.numeric())
  }
  
  # Diseño efectos "fijos"
  x <- model.matrix(~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG +
                      DIABETES + EPOC + ASMA + INMUSUPR +
                      HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
                      OBESIDAD + RENAL_CRONICA + TABAQUISMO,
                    data = d_pred)
  colnames(x)[1] <- "Intercept"
  
  # Renombrar coefs en post
  names(post) <- names(post) %>%
    str_remove(pattern = "^b_") %>%
    str_remove(pattern = "^sd_") %>%
    str_remove(pattern = "__Intercept$")
  
  # Calcular predictor lineal para cada muestra de la posterior
  apply(post, 1, function(coef, x, coef_ii, re_ii){
    
    # Coeficientes efectos "fijos"
    beta <- coef[colnames(x)]
    u <- coef[!(names(coef) %in% colnames(x))]
    # print(u)
    # cat("==========\n")
    
    # Ecuación normal más efectos aleatorios
    x %*% matrix(beta, ncol = 1) + sum(u)
  }, x = x, coef_ii = coef_ii, re_ii = re_ii) %>%
    apply(., 1, quantile, probs = c(0.1, 0.5, 0.9)) %>%
    t %>%
    as_tibble() %>%
    rename(p_def_q10 = '10%',
           p_def_q50 = '50%',
           p_def_q90 = '90%') %>%
    mutate_at(.vars = c("p_def_q10", "p_def_q50", "p_def_q90"), .funs = logistic)
}

#' Logistic function
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
logistic <- function(x){
  1 / (1 + exp(-x))
}

##########


load("m.def.rdat")

# Simular individuos sin comorbilidades
d_pred <- tibble(EDAD = (rep(seq(from = 20, to = 70, by = 5), each = 2) - edad_mu) / edad_sd,
                 SEXO = rep(0:1, times = 11),
                 EMBARAZO = 0,
                 HABLA_LENGUA_INDIG = 0,
                 DIABETES = 0,
                 EPOC = 0,
                 ASMA = 0,
                 INMUSUPR = 0,
                 HIPERTENSION = 0,
                 OTRA_COM = 0,
                 CARDIOVASCULAR = 0,
                 OBESIDAD = 0,
                 RENAL_CRONICA = 0,
                 TABAQUISMO = 0)
d_pred %>% print(n = 50)

f_riesgo <- c("EMBARAZO",
  "HABLA_LENGUA_INDIG", "DIABETES",
  "EPOC", "ASMA", "INMUSUPR",
  "HIPERTENSION",
  "OTRA_COM",
  "CARDIOVASCULAR",
  "OBESIDAD",
  "RENAL_CRONICA",
  "TABAQUISMO")

# Individuos sin comorbilidades
Res <- pred_brms_logistic(d_pred = d_pred, model = m.def, coef_ii = 1:15, re_ii = 16:17) %>%
  bind_cols(d_pred %>%
              select(EDAD, SEXO)) %>%
  mutate(SEXO = as.character(SEXO),
         f_riesgo = "Sin factores de riesgo")

# Individuos con factoes de riesgo
for(fr in f_riesgo){
  d_riesgo <- d_pred
  d_riesgo[fr] <- 1
  
  res <- pred_brms_logistic(d_pred = d_riesgo, model = m.def, coef_ii = 1:15, re_ii = 16:17) %>%
    bind_cols(d_riesgo %>%
                select(EDAD, SEXO)) %>%
    mutate(SEXO = as.character(SEXO),
           f_riesgo = fr)

  Res <- Res %>%
    bind_rows(res)
}

# tab <- summary(m.def, prob=0.9)
# or <- tibble(par = row.names(tab$fixed),
#        mean = tab$fixed[,1] %>% as.numeric() %>% exp,
#        lower = tab$fixed[,3] %>% as.numeric() %>% exp,
#        upper = tab$fixed[,4] %>% as.numeric() %>% exp) %>%
#   arrange(desc(mean)) %>%
#   filter(!(par %in% c("Intercept", "EDAD")))
# or

or <- posterior_samples(m.def)[1:15] %>%
  exp %>%
  apply(., 2, quantile, prob = c(0.05, 0.25, 0.5, 0.75, 0.95)) %>%
  t %>% 
  as.data.frame() %>%
  rownames_to_column("par") %>%
  mutate(par = str_remove(par, "^b_")) %>%
  rename(lower_outer = "5%", lower_inner = "25%", median = "50%",
         upper_inner = "75%", upper_outer = "95%") %>%
  arrange(desc(median)) %>%
  filter(!(par %in% c("Intercept", "EDAD"))) %>%
  as_tibble()
  
or %>%
  mutate(par = factor(par, levels = rev(unique(par)))) %>%
  ggplot(aes(y = par)) +
  geom_segment(aes(x = lower_outer, xend = upper_outer, yend = par)) +
  geom_segment(aes(x = lower_inner, xend = upper_inner, yend = par), size = 2) +
  geom_point(aes(x = median), shape = 21, size = 2, fill = "white", col = "black") +
  geom_vline(xintercept = 1, col = "red", size = 2, alpha = 0.3) +
  xlab(label = "Razón de momios") +
  AMOR::theme_blackbox()
  

Res %>%
  mutate(EDAD = EDAD * edad_sd + edad_mu) %>%
  mutate(f_riesgo = factor(f_riesgo, levels = c(unique(or$par), "Sin factores de riesgo"))) %>%
  ggplot(aes(x = EDAD, group = SEXO, col = SEXO, fill = SEXO)) +
  facet_wrap(. ~ f_riesgo, ncol = 3) +
  geom_line(aes(y = p_def_q50)) +
  geom_ribbon(aes(ymin = p_def_q10, ymax = p_def_q90), alpha = 0.2) +
  # geom_hline(yintercept = 0.5) +
  # geom_vline(xintercept = 50) +
  theme_classic()


summary(m.def)
# mcmc_plot(m.def, type = "intervals", transformations = exp, pars = c("EDAD", "SEXO", "EMBARAZO",
#                                                                      "HABLA_LENGUA_INDIG", "DIABETES",
#                                                                      "EPOC", "ASMA", "INMUSUPR",
#                                                                      "HIPERTENSION",
#                                                                      "OTRA_COM",
#                                                                      "CARDIOVASCULAR",
#                                                                      "OBESIDAD",
#                                                                      "RENAL_CRONICA",
#                                                                      "TABAQUISMO"),
#           prob = 0.5,
#           prob_outer = 0.89,
#           point_est = "median")


predict(m.def, newdata = d_pred, re_formula = NA)


