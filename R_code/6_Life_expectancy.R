################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr
)


################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

population_evo <- import(here("results", "deaths.csv"))

################################################################################################################################
#                                             3. Espérance de vie conditionnelle                                               #
################################################################################################################################

population_evo <- population_evo %>% 
  select("age", "year", "scenario", "simulation_id", "adjusted_mr", "population", "deaths") %>% 
  group_by(year, simulation_id, age) %>% 
  mutate(avoided_deaths = deaths[scenario == "actuel"] - deaths)

calc_conditional_LE <- function(df) {
  df %>%
    arrange(year, age) %>%
    group_by(year, simulation_id, scenario) %>%
    mutate(
      qx = adjusted_mr,                           # Taux de mortalité par âge (déjà dans les données)
      px = 1 - qx,                                   # Probabilité de survie à chaque âge
      lx = cumprod(lag(px, default = 1)),            # Probabilité de survie jusqu'à chaque âge
      dx = lx * qx,                                  # Nombre de décès attendu à chaque âge
      Tx = rev(cumsum(rev(lx)))                      # Somme cumulée des survivants pour espérance de vie
    ) %>%
    mutate(
      ex = Tx / lx                                   # Espérance de vie conditionnelle à chaque âge
    ) 
}

population_evo <- calc_conditional_LE(population_evo)

population_evo <- population_evo %>% 
  group_by(age, year, scenario, simulation_id) %>% 
  mutate(le = age + ex,
         ylg = avoided_deaths * (le - age)) %>% 
  select(simulation_id, age, year, scenario, avoided_deaths, le)

  

################################################################################################################################
#                                             4. Exportation des données                                                       #
################################################################################################################################

export(le, here("data_clean", "life_expectancy.xlsx"))
