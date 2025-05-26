################################################################################################################################
#                                             1. Loading packages                                                              #
################################################################################################################################

pacman::p_load(
    rio, # File import/export
    here, # File path management
    dplyr, # Data manipulation
    tidyr # Data manipulation
)

################################################################################################################################
#                                             2. Data improtation                                                              #
################################################################################################################################

# Mortality rates (INSEE)
MR <- import(here("data_clean", "MR_table.xlsx"))

# Projected population size by age and year (INSEE)
population <- import(here("data_clean", "population_clean.xlsx"))

# RR of diets
rr_evo_diets <- import(here("results", "6_actuel_Fadnes2024", "RR", "rr_evo_diets.csv"))

################################################################################################################################
#                                             3. Parameters                                                                    #
################################################################################################################################

source(here("R_code", "0_parameters.R"))

################################################################################################################################
#                                             4. Data preparation                                                              #
################################################################################################################################

# Select the MR between the model time limits and above the age limit
# Pivot the dataframe in long format
MR_select <- MR %>%
  select(age, !!sym(as.character(year_i - 20)):!!sym(as.character(year_f + 2 * ttfe_time))) %>%
  filter(age >= age_limit) %>%
  pivot_longer(
    cols = !!sym(as.character(year_i - 20)):!!sym(as.character(year_f + 2 * ttfe_time)),
    names_to = "year",
    values_to = "MR"
  ) %>%
  mutate(year = as.numeric(year))

# Select the population size between the model time limits and above the age limit
# Pivot the dataframe in long format
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
#                                             5. MR adjustment                                                                 #
################################################################################################################################

# MRa = MRO*RR(scenario)/RR(actuel), where MR0 is the projected MR (INSEE)

MR_adjust <- function(MR, rr_evo_diets) {
  # Ajustement des taux de mortalité
  MR_adjusted <- MR %>%
    inner_join(rr_evo_diets, by = "year", relationship = "many-to-many") %>%
    group_by(age, year, simulation_id) %>%
    mutate(adjusted_mr = MR * relative_rr) %>%
    ungroup()
  
  return(MR_adjusted)
}

MR_adjusted <- MR_adjust(MR_select, rr_evo_diets)

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

simulations_summary_mr_adjusted <- MR_adjust_summary(MR_adjusted)

################################################################################################################################
#                                             6. Association of the adjusted MR with the population data                       #
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
#                                             7. Data exportation                                                              #
################################################################################################################################

# Adjusted mortality rates
export(MR_adjusted, here("results", "6_actuel_Fadnes2024", "MR", "MR_adjusted.csv"))
export(simulations_summary_mr_adjusted, here("results", "6_actuel_Fadnes2024", "MR", "simulations_summary_mr_adjusted.csv"))

# Table with population data and adjusted mortality rates
export(pop_data, here("results", "6_actuel_Fadnes2024", "MR", "pop_data.csv"))
