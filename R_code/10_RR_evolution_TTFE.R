################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,               # Manipulation des données
  purrr                # Opérations itératives
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

# RR des régimes complets de chaque scénario par année
  rr_diet_lin <- import(here("data_clean", "combined_rr_lin_2.xlsx"))
  
# RR de chaque aliment par année
  rr_food_lin <-  import(here("data_clean", "rr_evo_lin_2.xlsx"))
  
# Time to full effect linéaire sur 20 ans
  ttfe_lin <- import(here("data_clean", "ttfe_lin_20.xlsx"))
  
################################################################################################################################
#                                             3. Initialisation des paramètres                                                 #
################################################################################################################################

# Paramètre de modulation de la courbe d'interpolation cosinus
  p <- 1
  
# Paramètre de modulation de la courbe sigmoïde
  lambda <- 10
  
# Paramètre de modulation de la courbe logarithmique
  eta <- 1
  
  
################################################################################################################################
#                                             4. Calcul des RR avec ttfe linéaire                                              #
################################################################################################################################

# Implémentation linéaire des régimes 
# Time to full effect linéaire sur 20 ans 
  
# RR des régimes complets
  rr_evo <- rr_diet_lin %>% 
    rowwise() %>% 
    mutate(year_n = list(seq(from = min(rr_diet_lin$year), 
                                 to = max(rr_diet_lin$year)))) %>%
    unnest(year_n) %>% 
    mutate(rr_n = case_when(
      year_n < year ~ NA_real_,
      year_n >= year & year_n <= year + max(ttfe_lin$x) ~ 
       1 + (combined_rr - 1) * ttfe_lin$ttfe[match(year_n - year, ttfe_lin$x)],
      year_n > year + max(ttfe_lin$x) ~ combined_rr
    )) %>% 
    ungroup() %>%
    rename("year_i" = "year")
  
  rr_evo_combined <- rr_evo %>% 
    group_by(scenario, year_n) %>% 
    summarize(mean_rr = mean(rr_n, na.rm = TRUE), .groups = 'drop')
  
# RR de chaque aliment
  rr_evo_food <- rr_food_lin %>% 
    rowwise() %>% 
    mutate(year_n = list(seq(from = min(rr_food_lin$year), 
                             to = max(rr_food_lin$year)))) %>%
    unnest(year_n) %>% 
    mutate(rr_n = case_when(
      year_n < year ~ NA_real_,
      year_n >= year & year_n <= year + max(ttfe_lin$x) ~ 
        1 + (RR_interpolated - 1) * ttfe_lin$ttfe[match(year_n - year, ttfe_lin$x)],
      year_n > year + max(ttfe_lin$x) ~ RR_interpolated
    )) %>% 
    ungroup() %>%
    rename("year_i" = "year")
  
  rr_evo_food_combined <- rr_evo_food %>% 
    group_by(scenario, year_n, food_group) %>% 
    summarize(mean_rr = mean(rr_n, na.rm = TRUE), .groups = 'drop')
  
################################################################################################################################
#                                             5. Calcul des RR avec ttfe par interpolation cosinus                             #
################################################################################################################################
  
# Implémentation linéaire des régimes 
# Time to full effect = 20 ans 
  
# RR de chaque aliment
  rr_evo_food <- rr_food_lin %>% 
    rowwise() %>% 
    mutate(year_n = list(seq(from = min(rr_food_lin$year), 
                             to = max(rr_food_lin$year)))) %>%
    unnest(year_n) %>% 
    mutate(rr_n = case_when(
      year_n < year ~ NA_real_,
      year_n >= year & year_n <= year + max(ttfe_lin$x) ~ 
        1 + (RR_interpolated - 1) * ((1 - cos((pi * ttfe_lin$ttfe[match(year_n - year, ttfe_lin$x)])^p)) / 2),
      year_n > year + max(ttfe_lin$x) ~ RR_interpolated
    )) %>% 
    ungroup() %>%
    rename("year_i" = "year")
  
  rr_evo_food_combined <- rr_evo_food %>% 
    group_by(scenario, year_n, food_group) %>% 
    summarize(mean_rr = mean(rr_n, na.rm = TRUE), .groups = 'drop')
  
################################################################################################################################
#                                             6. Calcul des RR avec ttfe sigmoïdal                                             #
################################################################################################################################
  
  # Implémentation linéaire des régimes 
  # Time to full effect = 20 ans 
  
  # RR de chaque aliment
  rr_evo_food <- rr_food_lin %>% 
    rowwise() %>% 
    mutate(year_n = list(seq(from = min(rr_food_lin$year), 
                             to = max(rr_food_lin$year)))) %>%
    unnest(year_n) %>% 
    mutate(rr_n = case_when(
      year_n < year ~ NA_real_,
      year_n >= year & year_n <= year + max(ttfe_lin$x) ~ 
        (1 + RR_interpolated)/2 + (RR_interpolated -1) * (1 / (1 + exp(-lambda * (ttfe_lin$ttfe[match(year_n - year, ttfe_lin$x)] - 1/2)))- 1/2) * (-1 / (2 / (1 + exp(lambda/2)) - 1)),
      year_n > year + max(ttfe_lin$x) ~ RR_interpolated
    )) %>% 
    ungroup() %>%
    rename("year_i" = "year")
  
  rr_evo_food_combined <- rr_evo_food %>% 
    group_by(scenario, year_n, food_group) %>% 
    summarize(mean_rr = mean(rr_n, na.rm = TRUE), .groups = 'drop')
################################################################################################################################
#                                             7. Calcul des RR avec ttfe logarithmique                                         #
################################################################################################################################
  
# Implémentation linéaire des régimes 
# Time to full effect = 20 ans 
  
# RR de chaque aliment
  rr_evo_food <- rr_food_lin %>% 
    rowwise() %>% 
    mutate(year_n = list(seq(from = min(rr_food_lin$year), 
                             to = max(rr_food_lin$year)))) %>%
    unnest(year_n) %>% 
    mutate(rr_n = case_when(
      year_n < year ~ NA_real_,
      year_n >= year & year_n <= year + max(ttfe_lin$x) ~ 
        1 + (RR_interpolated - 1) * log(1 + eta * ttfe_lin$ttfe[match(year_n - year, ttfe_lin$x)]) / log(1 + eta),
      year_n > year + max(ttfe_lin$x) ~ RR_interpolated
    )) %>% 
    ungroup() %>%
    rename("year_i" = "year")
  
  rr_evo_food_combined <- rr_evo_food %>% 
    group_by(scenario, year_n, food_group) %>% 
    summarize(mean_rr = mean(rr_n, na.rm = TRUE), .groups = 'drop')
  
################################################################################################################################
#                                             5. Combinaison des RR                                                            #
################################################################################################################################
  
  calc_combined_rr <- function(df) {
    df %>%
      group_by(scenario, year_n) %>%
      summarize(combined_rr = prod(mean_rr, na.rm = TRUE)) %>%
      ungroup()
  }  

# rr en fonction de l'implémentation linéaire des régimes 
  combined_rr_table_lin <- calc_combined_rr(rr_evo_food_combined)
  