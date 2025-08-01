# 1. Loading packages
# 2. Data importation
# 3. Parameters
# 4. Data preparation
# 5. MR adjustment
# 6. Association of the adjusted MR with the population data
# 7. Data exportation

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
MR <- import(here("data_clean", "MR_table.csv"))

# Projected population size by age and year (INSEE)
population <- import(here("data_clean", "population_clean.xlsx"))

# RR of diets over time
rr_evo_diets <- import(here("results", "RR", "rr_evo_diets.csv"))

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
  select(age, !!sym(as.character(year_i - 20)):!!sym(as.character(year_f + 20))) %>%
  filter(age >= age_limit) %>%
  pivot_longer(
    cols = !!sym(as.character(year_i - 20)):!!sym(as.character(year_f + 20)),
    names_to = "year",
    values_to = "MR"
  ) %>%
  mutate(year = as.numeric(year))


# Select the population size between the model time limits and above the age limit
# Pivot the dataframe in long format
population_select <- population %>%
  select(age, !!sym(as.character(year_i - 20)):!!sym(as.character(year_f + 20))) %>%
  filter(age >= age_limit) %>%
  pivot_longer(
    cols = !!sym(as.character(year_i - 20)):!!sym(as.character(year_f + 20)),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(age)

################################################################################################################################
#                                             5. MR adjustment                                                                 #
################################################################################################################################

# MRa = MRO * RR(scenario) / RR(actuel), where MR0 is the projected MR (INSEE)

  MR_adjusted <- MR_select %>%
    inner_join(rr_evo_diets, by = "year", relationship = "many-to-many") %>%
    group_by(age, year, simulation_id) %>%
    mutate(adjusted_mr = MR * relative_rr) %>%
    ungroup()


# Calculation of the mean and 95% CI for each year
  ic95_mr_adjusted <- MR_adjusted %>%
    group_by(age, scenario, year) %>%
    summarise(
      mean_mr = mean(adjusted_mr, na.rm = TRUE),
      lower_ci = quantile(adjusted_mr, 0.025, na.rm = TRUE), # Lower limit of the 95% CI
      upper_ci = quantile(adjusted_mr, 0.975, na.rm = TRUE) # Upper limit of the 95% CI
    ) %>% 
    ungroup()
  
################################################################################################################################
#                                             6. Association of the adjusted MR with the population data                       #
################################################################################################################################

# This table will be used in the Python code that calculates the prevented deaths

pop_data <- MR_adjusted %>%
  left_join(population_select, by = c("age", "year"), relationship = "many-to-many") %>% 
  select(age, year, scenario, simulation_id, adjusted_mr, population) %>% 
  mutate(
    deaths = NA_real_,  # Preparation of variables for the prevented deaths calculation
    avoided_deaths = NA_real_,
    simulation_id = as.numeric(gsub(".*_", "", simulation_id))
  ) %>% # Variable numérique pour l'ID de simulation
  arrange(simulation_id, year, age, scenario)

################################################################################################################################
#                                             7. Data exportation                                                              #
################################################################################################################################

# Adjusted mortality rates
export(MR_adjusted, here("results", "MR", "MR_adjusted.csv"))
export(ic95_mr_adjusted, here("results", "MR", "ic95_mr_adjusted.xlsx"))

# Table with population data and adjusted mortality rates
export(pop_data, here("results", "MR", "pop_data.csv"))
