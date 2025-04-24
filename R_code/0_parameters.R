################################################################################################################################
#                                             1. Parameters                                                                    #
################################################################################################################################

## POPULATION

    age_limit <- 18 # years old

## TIME PERIOD

    # Time bounds for changes in diet (years)
    year_i <- 2025 # initial year
    year_f <- 2050 # final year

## DIETS IMPLEMENTATION

    # Implementation dynamics of diets (immediate, linear, cosine, sigmoidal)
    implementation <- "cosine"

    # Cosine interpolation curve parameter
    p <- 1

    # sigmoidal curve parameter
    lambda <- 8

## TIME TO FULL EFFECT

    # time to full effect duration (years)
    ttfe_time <- 10

    # stationary state duration (years)
    # Before the diet shift : ttfe_time
    # After the diet shift : 2 x ttfe_time

    # TTFE dynamics (immediate, linear, cosine, sigmoidal, log)
    ttfe_dynamics <- "linear"

    # cosine interpolation curve parameter
    p_ttfe <- 1

    # sigmoidal curve parameter
    lambda_ttfe <- 8

    # log curve parameter
    eta_ttfe <- 1

## RR

    # Effect mofification factor of the RR
    # 0.5 - 1 = reduction in effect, model conservative
    # 1 - 1.5 = increase in effect, model optimistic
    m <- 0.75

## COST OF A LIFE YEAR (€)

#2040
cost_2040 <- 188000

#2050
cost_2050 <- 210000

#2060
cost_2060 <- 238000

################################################################################################################################
#                                             2. Graphic chart                                                              #
################################################################################################################################

# Color palette for the scenarios and observed and optimized diets
col_scenario <- c(
  "actuel" = "azure4",
  "sc0" = "palevioletred3",
  "sc1" = "#699cc2",
  "sc2" = "#974175",
  "sc3" = "#50cd9f",
  "sc4" = "#cb6c2d",
  "sc5" = "royalblue4",
  "meat3" = "#cd0030",
  "meat2" = "#d95668",
  "meat1" = "#fcb901",
  "pesce" = "#027474",
  "vege" = "#007643",
  "vegan" = "#54a300",
  "meat3_optim" = "#98606d",
  "meat2_optim" = "#f4a1ac",
  "meat1_optim" = "#f1d68c",
  "pesce_optim" = "#5a8787",
  "vege_optim" = "#568470",
  "vegan_optim" = "#a3c084"
)

# color palette for the food groups
col_food_groups <- c(
  "red_meat" = "#ff1047",
  "processed_meat" = "#650115",
  "white_meat" = "#FF9DC8",
  "dairy" = "#022f66",
  "fish" = "#4993a2",
  "eggs" = "#ff764d",
  "fruits" = "#00CBA7",
  "nuts" = "#ffc744",
  "vegetables" = "#00735C",
  "legumes" = "#703895",
  "whole_grains" = "#572d00",
  "reffined_grains" = "#cbb4a1",
  "added_plant_oils" = "#FF6E3A",
  "sugar_sweetened_beverages" = "#1b1b1b"
)

# order of the food groups
order_food_groups <- c(
  "red_meat", "processed_meat", "white_meat", "fish", "eggs", "dairy",
  "fruits", "vegetables", "legumes", "nuts", "whole_grains", "reffined_grains",
  "added_plant_oils", "sugar_sweetened_beverages"
)

# labels for the scenarios and diets
labels_scenario <- c(
    "meat3" = "Omnivore-1",
    "meat2" = "Omnivore-2",
    "meat1" = "Flexitarian",
    "pesce" = "Pescetarian",
    "vege" = "Vegetarian",
    "vegan" = "Vegan",
    "meat3_optim" = "Omnivore-1 optimized",
    "meat2_optim" = "Omnivore-2 optimized",
    "meat1_optim" = "Flexitarian optimized",
    "pesce_optim" = "Pescetarian optimized",
    "vege_optim" = "Vegetarian optimized",
    "vegan_optim" = "Vegan optimized",
    "actuel" = "Current diet",
    "actuel_calage" = "Current diet (calibrated)",
    "sc0" = "Tendancial",
    "sc1" = "Scenario 1",
    "sc2" = "Scenario 2",
    "sc3" = "Scenario 3",
    "sc4" = "Scenario 4",
    "sc5" = "SNBC"
)
# labels for the food groups
labels_food_groups <- c(
  "red_meat" = "Red meat",
  "processed_meat" = "Processed meat",
  "white_meat" = "White meat",
  "dairy" = "Dairy",
  "fish" = "Fish",
  "eggs" = "Eggs",
  "fruits" = "Fruits",
  "nuts" = "Nuts",
  "vegetables" = "Vegetables",
  "legumes" = "Legumes",
  "whole_grains" = "Whole grains",
  "reffined_grains" = "Refined grains",
  "added_plant_oils" = "Added plant oils",
  "sugar_sweetened_beverages" = "SSB"
)

# labels for the food groups with a delta symbol
labels_food_groups_delta <- c(
  "red_meat" = expression(Delta ~ "Red meat"),
  "processed_meat" = expression(Delta ~ "Processed meat"),
  "white_meat" = expression(Delta ~ "White meat"),
  "dairy" = expression(Delta ~ "Dairy"),
  "fish" = expression(Delta ~ "Fish"),
  "eggs" = expression(Delta ~ "Eggs"),
  "fruits" = expression(Delta ~ "Fruits"),
  "nuts" = expression(Delta ~ "Nuts"),
  "vegetables" = expression(Delta ~ "Vegetables"),
  "legumes" = expression(Delta ~ "Legumes"),
  "whole_grains" = expression(Delta ~ "Whole grains"),
  "reffined_grains" = expression(Delta ~ "Refined grains"),
  "sugar_sweetened_beverages" = expression(Delta ~ "SSB")
)

order_diets <- c(
    "actuel",
    "actuel_calage",
    "meat3",
    "meat3_optim",
    "meat2",
    "meat2_optim",
    "meat1",
    "meat1_optim",
    "pesce",
    "pesce_optim",
    "vege",
    "vege_optim",
    "vegan",
    "vegan_optim",
    "sc0",
    "sc1",
    "sc2",
    "sc3",
    "sc4",
    "sc5"
)

label_diets <- c(
    "meat3" = "Omnivore-1",
    "meat2" = "Omnivore-2",
    "meat1" = "Flexitarian",
    "pesce" = "Pescetarian",
    "vege" = "Vegetarian",
    "vegan" = "Vegan",
    "meat3_optim" = "Omnivore-1 optimized",
    "meat2_optim" = "Omnivore-2 optimized",
    "meat1_optim" = "Flexitarian optimized",
    "pesce_optim" = "Pescetarian optimized",
    "vege_optim" = "Vegetarian optimized",
    "vegan_optim" = "Vegan optimized",
    "actuel" = "Current diet",
    "actuel_calage" = "Current diet (calibrated)",
    "sc0" = "Tendancial",
    "sc1" = "Scenario 1",
    "sc2" = "Scenario 2",
    "sc3" = "Scenario 3",
    "sc4" = "Scenario 4",
    "sc5" = "SNBC"
)

col_diets <- c(
    "meat3" = "#cd0030",
    "meat2" = "#d95668",
    "meat1" = "#fcb901",
    "pesce" = "#027474",
    "vege" = "#007643",
    "vegan" = "#54a300",
    "meat3_optim" = "#98606d",
    "meat2_optim" = "#f4a1ac",
    "meat1_optim" = "#f1d68c",
    "pesce_optim" = "#5a8787",
    "vege_optim" = "#568470",
    "vegan_optim" = "#a3c084"
)