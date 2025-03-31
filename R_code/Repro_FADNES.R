################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,                # Manipulation des données
  purrr
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################
  
# Effectifs de population (hommes et femmes) par âge de 1962 à 2021 et projetées jusqu'en 2120
  population <- import(here("data", "GBD_EU_population.csv"))
  
# Décès par âge de 1962 à 2021 et projetées jusqu'en 2120
  deaths <- import(here("data", "GBD_EU_deaths.csv"))

# MR du GBD
  MR_GBD_USA_W <- import(here("data", "GBD_MR_2019.xlsx"), sheet = "USA W")
  MR_GBD_USA_M <- import(here("data", "GBD_MR_2019.xlsx"), sheet = "USA M")
  MR_GBD_EU_W <- import(here("data", "GBD_MR_2019.xlsx"), sheet = "EU W")
  MR_GBD_EU_M <- import(here("data", "GBD_MR_2019.xlsx"), sheet = "EU M")
  MR_GBD_CHINA_W <- import(here("data", "GBD_MR_2019.xlsx"), sheet = "China W")
  MR_GBD_CHINA_M <- import(here("data", "GBD_MR_2019.xlsx"), sheet = "China M")

################################################################################################################################
#                                             3. Initialisation des paramètres                                                 #
################################################################################################################################

# Bornes temporelles des changements de régime alimentaire (années)
year_i <- 2019 # Année initiale
year_f <- 2039 # Année finale

# Durée du time to full effect (années)
ttfe_time <- 10
  
################################################################################################################################
#                                             3. Nettoyage des données                                                         #
################################################################################################################################
  
# Rendre les données homogènes (noms de variables, catégories d'âge,...) et utilisables pour l'EQIS
  
  # Effectifs de population H/F par âge et par année
  population <- population %>% 
    mutate(year = as.numeric(year)) %>% 
    select(sex, age, year, val) %>% 
    rename("population" = val)
  
  
  # Nombre de décès H/F par âge et par année
  deaths <- deaths %>%
    select(sex, age, year, val) %>% 
    rename("deaths" = val)
  
# Les vérifications doivent renvoyer TRUE
  all(names(population == names(deaths)))   # Mêmes variables dans les jeux de données
  all(population$age == deaths$age)         # Mêmes catégories d'âge dans les jeux de données
  

# MR GBD 2019
  MR_GBD <- MR_GBD %>%
    select(sex, age, year, val, upper, lower) %>% 
    rename("mr" = val,
           "mr_upper" = upper,
           "mr_lower" = lower)  
  

################################################################################################################################
#                                             4. Données H/F                                                                 #
################################################################################################################################
  
# Données Hommes
  population_m <- population %>%
    filter(sex == "Homme") %>% 
    select(-sex)
  
  deaths_m <- deaths %>%
    filter(sex == "Homme") %>% 
    select(-sex)

  MR_GBD_m <- MR_GBD %>%
    filter(sex == "Homme") %>%
    select(-sex)
  
# Données Femmes
  population_f <- population %>%
    filter(sex == "Femme") %>% 
    select(-sex) 

  deaths_f <- deaths %>%
    filter(sex == "Femme") %>% 
    select(-sex)  
  
  MR_GBD_f <- MR_GBD %>%
    filter(sex == "Femme") %>%
    select(-sex)

################################################################################################################################
#                                             5. Calcul des taux de mortalité                                                  #
################################################################################################################################

# Femmes
MR_f <- population_f %>%
  left_join(deaths_f, by = c("age", "year")) %>%
  mutate(mr = deaths/population) %>%
  select(age, year, mr)

# Hommes
MR_m <- population_m %>%
  left_join(deaths_m, by = c("age", "year")) %>%
  mutate(mr = deaths/population) %>%
  select(age, year, mr)


################################################################################################################################
#                                             6. Exportation des données                                                       #
################################################################################################################################
  
  # Effectifs de population par âge
  export(population_f, here("data_clean","GBD_population_EU_f.xlsx"))
  export(population_m, here("data_clean","GBD_population_EU_m.xlsx"))
  
  # Décès par âge
  export(deaths_f, here("data_clean","GBD_deaths_EU_f.xlsx"))
  export(deaths_m, here("data_clean","GBD_deaths_EU_m.xlsx"))

  # Taux de mortalité par âge
  export(MR_f, here("data_clean", "GBD_MR_FR_f.xlsx"))
  export(MR_m, here("data_clean", "GBD_MR_FR_m.xlsx"))

  export(MR_GBD_f, here("data_clean", "GBD_MR_2019_f.xlsx"))
  export(MR_GBD_m, here("data_clean", "GBD_MR_2019_m.xlsx"))



# Modification des dataframes sur excel #

# A partir des effectifs et décès en population
MR_f <- import(here("data_clean", "GBD_MR_FR_f_clean.xlsx"))
MR_m <- import(here("data_clean", "GBD_MR_FR_m_clean.xlsx"))

years <- as.vector((year_i - 2*ttfe_time) : (year_f + 2*ttfe_time))

MR_complete <- function(df_mr, years) {
  bind_rows(lapply(years, function(y) df_mr %>% mutate(year = y)))
}

MR_GBD_CHINA_W <- MR_complete(MR_GBD_CHINA_W, years)
MR_GBD_CHINA_M <- MR_complete(MR_GBD_CHINA_M, years)
MR_GBD_EU_W <- MR_complete(MR_GBD_EU_W, years)
MR_GBD_EU_M <- MR_complete(MR_GBD_EU_M, years)
MR_GBD_USA_W <- MR_complete(MR_GBD_USA_W, years)
MR_GBD_USA_M <- MR_complete(MR_GBD_USA_M, years)


export(MR_GBD_CHINA_W, here("results", "FADNES_2022_repro", "MR", "GBD_2019_CHINA_W_complete.xlsx"))
export(MR_GBD_CHINA_M, here("results", "FADNES_2022_repro", "MR", "GBD_2019_CHINA_M_complete.xlsx"))
export(MR_GBD_EU_W, here("results", "FADNES_2022_repro", "MR", "GBD_2019_EU_W_complete.xlsx"))
export(MR_GBD_EU_M, here("results", "FADNES_2022_repro", "MR", "GBD_2019_EU_M_complete.xlsx"))
export(MR_GBD_USA_W, here("results", "FADNES_2022_repro", "MR", "GBD_2019_USA_W_complete.xlsx"))
export(MR_GBD_USA_M, here("results", "FADNES_2022_repro", "MR", "GBD_2019_USA_M_complete.xlsx"))
