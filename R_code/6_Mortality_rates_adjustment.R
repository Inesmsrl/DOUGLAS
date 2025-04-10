################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
    rio, # Importation de fichiers
    here, # Localisation des fichiers dans le dossier du projet
    dplyr, # Manipulation des données
    tidyr # Manipulation des données
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

# Taux de mortalité (INSEE)
MR <- import(here("data_clean", "MR_table.xlsx"))

# GBD 2019
MR_GBD_CHINA_W <- import(here("results", "FADNES_2022_repro", "MR", "GBD_2019_CHINA_W_complete.xlsx"))
MR_GBD_CHINA_M <- import(here("results", "FADNES_2022_repro", "MR", "GBD_2019_CHINA_M_complete.xlsx"))
MR_GBD_USA_W <- import(here("results", "FADNES_2022_repro", "MR", "GBD_2019_USA_W_complete.xlsx"))
MR_GBD_USA_M <- import(here("results", "FADNES_2022_repro", "MR", "GBD_2019_USA_M_complete.xlsx"))
MR_GBD_EU_W <- import(here("results", "FADNES_2022_repro", "MR", "GBD_2019_EU_W_complete.xlsx"))
MR_GBD_EU_M <- import(here("results", "FADNES_2022_repro", "MR", "GBD_2019_EU_M_complete.xlsx"))

# Effectifs de population par age et par année (INSEE)
population <- import(here("data_clean", "population_clean.xlsx"))

# RR des régimes
rr_evo_diets <- import(here("results", "2_WG_S3_S4", "RR", "rr_evo_diets.csv"))

################################################################################################################################
#                                             3. Initialisation des paramètres                                                 #
################################################################################################################################

# Bornes temporelles des changements de régime alimentaire (années)
year_i <- 2025 # Année initiale
year_f <- 2050 # Année finale

# Borne inférieure de l'âge de la population du modèle (années)
age_limit <- 18

#  Time to full effect
# durée (années)
ttfe_time <- 10

################################################################################################################################
#                                             4. Préparation des données                                                       #
################################################################################################################################

# Sélectionner les MR entre les bornes temporelles du modèle et au dessus de la limite d'age
# Pivoter le dataframe en format long
MR_select <- MR %>%
  select(age, !!sym(as.character(year_i - 20)):!!sym(as.character(year_f + 2 * ttfe_time))) %>%
  filter(age >= age_limit) %>%
  pivot_longer(
    cols = !!sym(as.character(year_i - 20)):!!sym(as.character(year_f + 2 * ttfe_time)),
    names_to = "year",
    values_to = "MR"
  ) %>%
  mutate(year = as.numeric(year))

# Sélectionner les effectifs de population entre les bornes temporelles du modèle et au dessus de la limite d'age
# Pivoter le dataframe en format long
population_select <- population %>%
  select(age, !!sym(as.character(year_i - 20)):!!sym(as.character(year_f + 2 * ttfe_time))) %>%
  filter(age >= age_limit) %>%
  pivot_longer(
    cols = !!sym(as.character(year_i - 20)):!!sym(as.character(year_f + 2 * ttfe_time)),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(age)

################################################################################################################################
#                                             5. Ajustement des taux de mortalité                                             #
################################################################################################################################

# MRa = MRO*RR(scenario)/RR(actuel), où MRO est le taux de mortalité projeté par l'INSEE

# Ajustement des taux de mortalité
MR_adjusted <- MR_select %>%
  inner_join(rr_evo_diets, by = "year", relationship = "many-to-many") %>%
  group_by(age, year, simulation_id) %>%
  mutate(adjusted_mr = MR * relative_rr) %>%
  ungroup()

MR_adjust <- function(MR, rr_evo_diets) {
  # Ajustement des taux de mortalité
  MR_adjusted <- MR %>%
    inner_join(rr_evo_diets, by = "year", relationship = "many-to-many") %>%
    group_by(age, year, simulation_id) %>%
    mutate(adjusted_mr = MR * relative_rr) %>%
    ungroup()
  
  return(MR_adjusted)
}

MR_adj_CHINA_W <- MR_adjust(MR_GBD_CHINA_W, rr_evo_diets)
MR_adj_CHINA_M <- MR_adjust(MR_GBD_CHINA_M, rr_evo_diets)
MR_adj_USA_W <- MR_adjust(MR_GBD_USA_W, rr_evo_diets)
MR_adj_USA_M <- MR_adjust(MR_GBD_USA_M, rr_evo_diets)
MR_adj_EU_W <- MR_adjust(MR_GBD_EU_W, rr_evo_diets)
MR_adj_EU_M <- MR_adjust(MR_GBD_EU_M, rr_evo_diets)

# Calculer la moyenne et les IC95 pour chaque année
simulations_summary_mr_adjusted <- MR_adjusted %>%
  group_by(age, scenario, year) %>%
  summarise(
    mean_mr = mean(adjusted_mr, na.rm = TRUE),
    lower_ci = quantile(adjusted_mr, 0.025, na.rm = TRUE), # Limite inférieure de l'IC à 95%
    upper_ci = quantile(adjusted_mr, 0.975, na.rm = TRUE) # Limite supérieure de l'IC à 95%
  )

MR_adjust_summary <- function(MR_adjusted) {
  # Calculer la moyenne et les IC95 pour chaque année
  simulations_summary_mr_adjusted <- MR_adjusted %>%
    group_by(age, scenario, year) %>%
    summarise(
      mean_mr = mean(adjusted_mr, na.rm = TRUE),
      lower_ci = quantile(adjusted_mr, 0.025, na.rm = TRUE), # Limite inférieure de l'IC à 95%
      upper_ci = quantile(adjusted_mr, 0.975, na.rm = TRUE) # Limite supérieure de l'IC à 95%
    )
  
  return(simulations_summary_mr_adjusted)
}

simulations_summary_mr_adjusted_CHINA_W <- MR_adjust_summary(MR_adj_CHINA_W)
simulations_summary_mr_adjusted_CHINA_M <- MR_adjust_summary(MR_adj_CHINA_M)
simulations_summary_mr_adjusted_USA_W <- MR_adjust_summary(MR_adj_USA_W)
simulations_summary_mr_adjusted_USA_M <- MR_adjust_summary(MR_adj_USA_M)
simulations_summary_mr_adjusted_EU_W <- MR_adjust_summary(MR_adj_EU_W)
simulations_summary_mr_adjusted_EU_M <- MR_adjust_summary(MR_adj_EU_M)

################################################################################################################################
#                                             6. Association aux données de population                                         #
################################################################################################################################

pop_data <- MR_adjusted %>%
  left_join(population_select, by = c("age", "year"), relationship = "many-to-many") %>% # Jointure des données de population et des taux de mortalité ajustés
  select(age, year, scenario, simulation_id, adjusted_mr, population) %>% # Sélection des variables pertinentes)
  mutate(
    deaths = NA_real_,  #Préparation du tableau pour le calcul des décès
    avoided_deaths = NA_real_,
    simulation_id = as.numeric(gsub(".*_", "", simulation_id))
  ) %>% # Variable numérique pour l'ID de simulation
  arrange(simulation_id, year, age, scenario)

################################################################################################################################
#                                             11. Exportation des données                                                      #
################################################################################################################################

# Taux de mortalité ajustés
export(MR_adjusted, here("results", "2_WG_S3_S4", "MR", "MR_adjusted.csv"))

export(MR_adj_CHINA_W, here("results", "FADNES_2022_repro", "CORRECTION", "MR", "MR_adjusted_CHINA_W.csv"))
export(MR_adj_CHINA_M, here("results", "FADNES_2022_repro", "CORRECTION", "MR", "MR_adjusted_CHINA_M.csv"))
export(MR_adj_USA_W, here("results", "FADNES_2022_repro", "CORRECTION", "MR", "MR_adjusted_USA_W.csv"))
export(MR_adj_USA_M, here("results", "FADNES_2022_repro", "CORRECTION", "MR", "MR_adjusted_USA_M.csv"))
export(MR_adj_EU_W, here("results", "FADNES_2022_repro", "CORRECTION", "MR", "MR_adjusted_EU_W.csv"))
export(MR_adj_EU_M, here("results", "FADNES_2022_repro", "CORRECTION", "MR", "MR_adjusted_EU_M.csv"))

# Résumé des simulations ajustées
export(simulations_summary_mr_adjusted, here("results", "2_WG_S3_S4", "MR", "simulations_summary_mr_adjusted.csv"))

export(simulations_summary_mr_adjusted_CHINA_W, here("results", "FADNES_2022_repro", "CORRECTION", "MR", "simulations_summary_mr_adjusted_CHINA_W.csv"))
export(simulations_summary_mr_adjusted_CHINA_M, here("results", "FADNES_2022_repro", "CORRECTION", "MR", "simulations_summary_mr_adjusted_CHINA_M.csv"))
export(simulations_summary_mr_adjusted_USA_W, here("results", "FADNES_2022_repro", "CORRECTION", "MR", "simulations_summary_mr_adjusted_USA_W.csv"))
export(simulations_summary_mr_adjusted_USA_M, here("results", "FADNES_2022_repro", "CORRECTION", "MR", "simulations_summary_mr_adjusted_USA_M.csv"))
export(simulations_summary_mr_adjusted_EU_W, here("results", "FADNES_2022_repro", "CORRECTION", "MR", "simulations_summary_mr_adjusted_EU_W.csv"))
export(simulations_summary_mr_adjusted_EU_M, here("results", "FADNES_2022_repro", "CORRECTION", "MR", "simulations_summary_mr_adjusted_EU_M.csv"))

# Tableau de données de population et MR ajustés
export(pop_data, here("results", "2_WG_S3_S4", "MR", "pop_data.csv"))
