################################################################################################################################
#                                             1. Loading packages                                                              #
################################################################################################################################

pacman::p_load(
  rio,                 # file import/export
  here,                # file path management in R projects
  dplyr,               # data manipulation
  tidyr                # data manipulation
)

################################################################################################################################
#                                             2. Data importation                                                              #
################################################################################################################################

# Population size (male and female together) by age from 1962 to 2021 and projected until 2120
population <- import(here("data", "INSEE_Population.xlsx"))

# Deaths by age from 1962 to 2021 and projected until 2120
deaths <- import(here("data", "INSEE_Deaths.xlsx"))

################################################################################################################################
#                                             3. Data cleaning                                                                 #
################################################################################################################################

# Homogenize the data (variable names, age categories,...) and make them usable for HIA

# Population size (male and female together) by age and year
  population <- population %>% 
      rename("age" = "Âge au 1er janvier",
                 "2019" = "2019 (p)",
                 "2020" = "2020(p)",
                 "2021" = "2021(p)") %>%    # Rename variables
      select(-c("2121")) %>%                # Delete the column "2121" not inclcluded in deaths file
      filter(row_number() %in% 1:106) %>%   # Delete the line "Total"
      mutate(age = recode(age,
                          "105+" = "105")) %>% # Transform the value 105+ into 105 to be able to numerize the age variable
      mutate(age = as.numeric(age))         # Numerize the age variable
  

# Deaths by age and year
  deaths <- deaths %>% 
      rename("age" = "Âge atteint dans l'année",
                 "2020" = "2020 p)"
                 ) %>%                      # Rename variables
      select(-c("...161")) %>%              # Delete the column "...161"
      filter(row_number() %in% 1:106) %>%   # Delete the line "Total"
      mutate(age = recode(age,
                          "105+" = "105")) %>% # Transform the value 105+ into 105 to be able to numerize the age variable
      mutate(age = as.numeric(age))            # Numerize the age variable

################################################################################################################################
#                                             4. Checks                                                                        #
################################################################################################################################
  
# Checks must send back TRUE
  
  all(names(population == names(deaths)))   # Same variable names in the two datasets
  all(population$age == deaths$age)         # Same age categories in the two datasets
  
  
################################################################################################################################
#                                             5. Data exportation                                                              #
################################################################################################################################

# Population size projection by age and year
  export(population,here("data_clean","population_clean.xlsx"))

# Deaths projection by age and year
  export(deaths,here("data_clean","deaths_clean.xlsx"))