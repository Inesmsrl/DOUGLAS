################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  scico                # Palettes de couleur
)

################################################################################################################################
#                                             2. Initialisation des paramètres                                                 #
################################################################################################################################

# Bornes temporelles du modèle
  year_i <- 2019
  year_f <- 2050

# Dataframe contenant toutes les années de la période temporelle
  year_n <- tibble(year_i:year_f) %>% 
    rename("year" = "year_i:year_f")

# Time to full effect
  delta = 20

# Paramètre de modulation de la courbe logistique
  p <- 1

# Paramètre de modulation de la courbe sigmoïde
  lambda <- 10
  
# Paramètre de modulation de la courbe logarithmique
  eta <- 1
  

################################################################################################################################
#                                             3. Time to full effect linéaire                                                  #
################################################################################################################################

calc_ttfe_lin <- function(year_i, year_n, delta){
  -year_n/(delta) + 1 + year_i/delta
}

ttfe_lin <- year_n %>% 
  mutate(ttfe = case_when(year > year_i + delta ~ 0,
                          year <= year_i + delta ~ calc_ttfe_lin(year_i, year, delta)))


graph_ttfe_lin <- ggplot(ttfe_lin, aes(x = year,
                     y = ttfe))+
  geom_line(color = "darkseagreen", size = 1, alpha = 0.8)+
  labs(title = "Weight of mortality rate associated with initial diet with time since change in diet",
       x = "",
       y = "")
################################################################################################################################
#                                             4. Time to full effect logistique                                                #
################################################################################################################################

calc_ttfe_logistic <- function(year_i, year_n, delta, p){
  1- (1 - cos(pi*((year_n - year_i)/delta)^p))/2
}

ttfe_logistic <- year_n %>% 
  mutate(ttfe = case_when(year > year_i + delta ~ 0,
                          year <= year_i + delta ~ calc_ttfe_logistic(year_i, year, delta, p)))
  
graph_ttfe_logistic <- ggplot(ttfe_logistic, aes(x = year,
                                                 y = ttfe))+
  geom_line(color = "darkseagreen", size = 1, alpha = 0.8)+
  labs(title = "Weight of mortality rate associated with initial diet with time since change in diet",
       x = "",
       y = "")

# Variation de la valeur de p

  p_values <- seq(0, 2, by = 0.25)
  
  ttfe_logistic_var <- expand.grid(year = year_n$year, p = p_values) %>% 
    mutate(ttfe = case_when(year > year_i + delta ~ 0,
                            year <= year_i + delta ~ mapply(calc_ttfe_logistic, year_i, year, delta, p)))
             
graph_ttfe_logistic_var <- ggplot(ttfe_logistic_var, aes(x = year,
                                                         y = ttfe,
                                                         color = as.factor(p)))+
    geom_line(size = 1, alpha = 0.8)+
    labs(title = "Weight of mortality rate associated with initial diet with time since change in diet",
         x = "",
         y = "",
         color = "p")+
    scale_color_scico_d(palette = "managua")


################################################################################################################################
#                                             5. Time to full effect sigmoidal                                                #
################################################################################################################################

calc_ttfe_sig <- function(year_n, year_i, delta, lambda) {
  1/2 - (1 / (1 + exp(-lambda * (year_n - year_i - delta/2)/delta)) - 1/2) * (-1 / (2 / (1 + exp(lambda/2)) - 1))
}

ttfe_sig <- year_n %>% 
  mutate(ttfe = case_when(year > year_i + delta ~ 0,
                          year <= year_i + delta ~ calc_ttfe_sig(year, year_i, delta, lambda)))

graph_ttfe_sig <- ggplot(ttfe_sig, aes(x = year,
                                       y = ttfe))+
  geom_line(color = "darkseagreen", size = 1, alpha = 0.8)+
  labs(title = "Weight of mortality rate associated with initial diet with time since change in diet",
       x = "",
       y = "")

# Variation de la valeur de lambda

  lambda_values <- seq(1, 20, by = 2)
  
  ttfe_sig_var <- expand.grid(year = year_n$year, lambda = lambda_values) %>% 
    mutate(ttfe = case_when(year > year_i + delta ~ 0,
                            year <= year_i + delta ~ mapply(calc_ttfe_sig, year, year_i, delta, lambda)))
  
  graph_ttfe_sig_var <- ggplot(ttfe_sig_var, aes(x = year,
                                                 y = ttfe,
                                                 color = as.factor(lambda)))+
      geom_line(size = 1, alpha = 0.8)+
      labs(title = "Weight of mortality rate associated with initial diet with time since change in diet",
           x = "",
           y = "",
           color = "lambda")+
      scale_color_scico_d(palette = "managua")

################################################################################################################################
#                                             5. Time to full effect logarithmique                                             #
################################################################################################################################

calc_ttfe_ln <- function(year_n, year_i, delta, eta) {
  1 - log(1 + eta * (year_n - year_i)/delta) / log(1 + eta)
}

ttfe_ln <- year_n %>%
  mutate(ttfe = case_when(year > year_i + delta ~ 0,
                          year <= year_i + delta ~ calc_ttfe_ln(year, year_i, delta, eta)))

graph_ttfe_ln <- ggplot(ttfe_ln, aes(x = year,
                                      y = ttfe))+
  geom_line(color = "darkseagreen", size = 1, alpha = 0.8)+
  labs(title = "Weight of mortality rate associated with initial diet with time since change in diet",
       x = "",
       y = "")

# Variation de la valeur de eta

eta_values <- seq(1, 20, by = 2)

ttfe_ln_var <- expand.grid(year = year_n$year, eta = eta_values) %>% 
  mutate(ttfe = case_when(year > year_i + delta ~ 0,
                          year <= year_i + delta ~ mapply(calc_ttfe_ln, year, year_i, delta, eta)))

graph_ttfe_ln_var <- ggplot(ttfe_ln_var, aes(x = year,
                                             y = ttfe,
                                             color = as.factor(eta)))+
  geom_line(size = 1, alpha = 0.8)+
  labs(title = "Weight of mortality rate associated with initial diet with time since change in diet",
       x = "",
       y = "",
       color = "eta")+
  scale_color_scico_d(palette = "managua")

################################################################################################################################
#                                             §. Exportation des données                                                       #
################################################################################################################################

# Time to full effect linéaire
ggsave(here("results", "full_effect_lin.pdf"), plot = graph_ttfe_lin)

# Time to full effect logistique
ggsave(here("results", "full_effect_logistic.pdf"), plot = graph_ttfe_logistic)
ggsave(here("results", "full_effect_logistic_var_p.pdf"), plot = graph_ttfe_logistic_var)

# Time to full effect sigmoïdal
ggsave(here("results", "full_effect_sig.pdf"), plot = graph_ttfe_sig)
ggsave(here("results", "full_effect_sig_var_lambda.pdf"), plot = graph_ttfe_sig_var)

# Time to full effect logarithmique
ggsave(here("results", "full_effect_ln.pdf"), plot = graph_ttfe_ln)
ggsave(here("results", 'full_effect_ln_var_eta.pdf'), plot = graph_ttfe_ln_var)
