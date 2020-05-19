library(tidyverse)

args <- list(cdi_base = "../socioeconomicos/cdi/cdi-base-indicadores-2015.csv",
             datos_municipios = "estimados/municipios_obs_esp.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")

# Leer CDI
Cdi <- read_csv(args$cdi_base)
stop_for_problems(Cdi)
Cdi <- Cdi %>%
  select(INEGI, ENT, NOMENT, MPO, NOMMUN, `GRADOMARGI 2015`, TPOBTOT, IPOB_INDI, NOMTIPO) %>%
  filter(NOMMUN != "Estados Unidos Mexicanos") %>%
  filter(NOMMUN != "Total Estatal") %>%
  mutate(clave = paste(ENT, MPO, sep = "_")) %>%
  mutate(prop_indigena = IPOB_INDI / TPOBTOT) %>%
  select(-INEGI, -ENT, -NOMENT, -MPO, -NOMMUN,
         -TPOBTOT, -IPOB_INDI,
         gradomargi_2015=`GRADOMARGI 2015`)
# Casos
Casos <- read_csv(args$datos_municipios)

# Unir
Dat <- Casos %>%
  left_join(Cdi, by = "clave")

# Unir pob y casos
p1 <- Dat %>%
  filter(!is.na(incidencia)) %>%
  ggplot(aes(x = prop_indigena, y = resid_incidencia) ) +
  geom_point(aes(col = gradomargi_2015), size = 2) +
  scale_color_brewer(palette = "Dark2", 
                     name = "Grado de\nmarginación") +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = function(x) scales::percent(x = x, accuracy = 0.1)) +
  scale_y_continuous(labels = function(x){
    labs <- (10 ^ x)
    scales::number(labs, accuracy = 0.1)
  }) +
  xlab("Población indígena municipal") +
  ylab(expression(bold(frac("Casos observados en municipio","Casos esperados en municipio")))) +
  guides(col = guide_legend(nrow=2, override.aes = list(size = 3))) +
  AMOR::theme_blackbox() +
  theme(legend.position = "top",
        plot.margin = margin(l = 20, r = 20),
        legend.key = element_blank(),
        axis.title.y = element_text(face = "bold"))
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)  
archivo <- file.path(args$dir_salida, "pob_indigena_exceso_incidencia.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "pob_indigena_exceso_incidencia@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
