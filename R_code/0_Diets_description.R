################################################################################################################################
#                                             1. Loading packages                                                              #
################################################################################################################################

pacman::p_load(
    rio, # File import/export
    here, # File path management
    dplyr, # Data manipulation
    tidyr, # Data manipulation
    tidyverse # Data management, ggplot included
)

################################################################################################################################
#                                             2. Data importation                                                              #
################################################################################################################################

# Food intakes
DOUGLAS_diets <- import(here("data", "DOUGLAS_diets.xlsx"))

################################################################################################################################
#                                             3. Parameters                                                                    #
################################################################################################################################

source(here("R_code", "0_parameters.R"))

################################################################################################################################
#                                             4. Intakes in the scenarios                                                      #
################################################################################################################################

DOUGLAS_diets_long <- DOUGLAS_diets %>%
  pivot_longer(
    cols = -food_group,
    names_to = "diet",
    values_to = "intake"
  ) %>%
  mutate(food_group = factor(food_group, levels = order_food_groups),
         diet = factor(diet, levels = order_diets))

graph_diets_desc <- ggplot(data = DOUGLAS_diets_long %>%
    filter(food_group != "added_plant_oils",
           diet %in% c("actuel", "sc1", "sc2", "sc3", "sc4")), # Change the selection of diets here if needed
                            aes(x = diet, y = intake, fill = food_group)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_manual(
    values = col_food_groups,
    labels = labels_food_groups
  )+
  labs(
    title = "",
    x = "",
    y = "Intake (g/d/pers)",
    fill = "Food groups"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
  plot.title = element_text(size = rel(2), face = "bold", hjust = 0.5),
  axis.title.x = element_text(size = rel(1.5)),
  axis.title.y = element_text(size = rel(1.5)), 
  legend.position = "bottom",
  legend.title = element_text(face = "bold", size = rel(1.5)))+
  scale_x_discrete(labels = label_diets)+
  guides(fill = guide_legend(
    nrow = 2,
    title.position = "top",
    title.hjust = 0.5
  ))

plot(graph_diets_desc)
################################################################################################################################
#                                             6. Data exportation                                                              #
################################################################################################################################

ggsave(here("results", "diets", "diets.pdf"), plot = graph_diets_desc)
