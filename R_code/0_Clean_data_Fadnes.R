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

# Risk data
rr_table_mid <- import(here("Fadnes_data", "data", "haz-v2.csv"), header = TRUE, dec = ",")
rr_table_low <- import(here("Fadnes_data", "data", "lci-v2.csv"), header = TRUE, dec = ",")
rr_table_up <- import(here("Fadnes_data", "data", "uci-v2.csv"), header = TRUE, dec = ",")

# Diet data
diet <- import(here("Fadnes_data", "data", "food_intake-v3b-female.xlsx")) # same diet for male and female

################################################################################################################################
#                                             3. Data cleaning                                                                 #
################################################################################################################################

# RR tables
rr_table_cleaning <- function(rr_table) {
  rr_table <- rr_table %>%
  rename("food_group" = "V1") %>%
  mutate(food_group = recode(food_group,
                             "Whole grains" = "whole_grains",
                             "Refined grains" = "reffined_grains",
                             "Veg" = "vegetables",
                             "Fruit" = "fruits",
                             "Meat, red" = "red_meat",
                             "Meat, processed" = "processed_meat",
                             "Meat, white" = "white_meat",
                             "Fish" = "fish",
                             "Milk" = "dairy",
                             "Egg" = "eggs",
                             "Nuts" = "nuts",
                             "Legumes" = "legumes",
                             "Sugar sweetened beverages" = "sugar_sweetened_beverages"
  )) %>%
  filter(food_group %in% c("red_meat", "processed_meat", "white_meat", "fish", "eggs", "dairy",
  "fruits", "vegetables", "legumes", "nuts", "whole_grains", "reffined_grains",
  "added_plant_oils", "sugar_sweetened_beverages"))
}

rr_table_mid <- rr_table_cleaning(rr_table_mid)
rr_table_low <- rr_table_cleaning(rr_table_low)
rr_table_up <- rr_table_cleaning(rr_table_up)

# Intakes must be in g/day, so we divide by 10 to convert from mg/day to g/day
names(rr_table_mid) <- ifelse(
  grepl("^[0-9]+$", names(rr_table_mid)),
  as.character(as.numeric(names(rr_table_mid)) / 10),
  names(rr_table_mid))

names(rr_table_low) <- ifelse(
  grepl("^[0-9]+$", names(rr_table_low)),
  as.character(as.numeric(names(rr_table_low)) / 10),
  names(rr_table_low))

names(rr_table_up) <- ifelse(
  grepl("^[0-9]+$", names(rr_table_up)),
  as.character(as.numeric(names(rr_table_up)) / 10),
  names(rr_table_up))

# Diet tables
diet <- diet %>%
    select("...1", "FRA", "Typical western", "Feasibility-approach", "Optimized", "Vegetarian optimal") %>%
    rename("food_group" = "...1",
    "actuel" = "FRA",
    "sc1" = "Typical western",
    "sc2" = "Feasibility-approach",
    "sc3" = "Optimized",
    "sc4" = "Vegetarian optimal") %>%
    mutate(food_group = recode(food_group,
                               "Whole grain" = "whole_grains",
                               "Total refined grains (dry weight) " = "reffined_grains",
                               "Vegetable" = "vegetables",
                               "Fruit" = "fruits",
                               "Red meat" = "red_meat",
                               "Processed meat" = "processed_meat",
                               "White meat" = "white_meat",
                               "Fish" = "fish",
                               "Milk" = "dairy",
                               "Egg" = "eggs",
                               "Nuts" = "nuts",
                               "Legumes" = "legumes",
                               "Added plant oils" = "added_plant_oils",
                               "Sugar-sweetened beverages" = "sugar_sweetened_beverages"
    )) %>%
    filter(food_group %in% c("red_meat", "processed_meat", "white_meat", "fish", "eggs", "dairy",
  "fruits", "vegetables", "legumes", "nuts", "whole_grains", "reffined_grains",
  "added_plant_oils", "sugar_sweetened_beverages"))

################################################################################################################################
#                                             4. Data exportation                                                              #
################################################################################################################################

# RR tables
export(rr_table_mid, here("Fadnes_data", "data_clean", "rr_table_mid.xlsx"))
export(rr_table_low, here("Fadnes_data", "data_clean", "rr_table_low.xlsx"))
export(rr_table_up, here("Fadnes_data", "data_clean", "rr_table_up.xlsx"))

# Diet table
export(diet, here("Fadnes_data", "data_clean", "diet.xlsx"))