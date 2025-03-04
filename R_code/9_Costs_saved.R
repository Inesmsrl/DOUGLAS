################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,
  tidyverse
)


################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

yll <- import(here("results", "HIA", "IC95_yll.xlsx"))

cost_2040 <- 188000

cost_2050 <- 210000

cost_2060 <- 238000

################################################################################################################################
#                                             3. Coûs énconomisés                                                              #
################################################################################################################################

costs <- yll %>% 
  group_by(scenario) %>% 
  mutate(costs = case_when(
    year == 2040 ~ mean_yll * cost_2040 / 1000000000,
    year == 2050 ~ mean_yll * cost_2050 / 1000000000,
    year == 2060 ~ mean_yll * cost_2060 / 1000000000
  ),
  costs_ic_lower = case_when(
    year == 2040 ~ lower_ci * cost_2040 / 1000000000,
    year == 2050 ~ lower_ci * cost_2050 / 1000000000,
    year == 2060 ~ lower_ci * cost_2060 / 1000000000
  ),
  costs_ic_upper = case_when(
    year == 2040 ~ upper_ci * cost_2040 / 1000000000,
    year == 2050 ~ upper_ci * cost_2050 / 1000000000,
    year == 2060 ~ upper_ci * cost_2060 / 1000000000
  ))

costs <- costs %>% 
  filter(year %in% c(2040, 2050, 2060))
  
graph_yll_costs_dates <- ggplot(costs %>% 
                                  filter(scenario != "actuel"),
                            aes(x = scenario,
                                y = costs,
                                fill = scenario))+
    geom_bar(stat = "identity",
             position = "dodge",
             alpha = 0.7)+
    geom_errorbar(aes(ymin = costs_ic_lower,
                      ymax = costs_ic_upper),
                  width = 0.2,
                  position = position_dodge(0.9))+
    facet_wrap(~year,
               ncol = 3)+
    scale_y_continuous(labels = scales :: label_comma())+
    scale_fill_manual(values = col_scenario,
                      labels = labels_scenario)+
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom")+
    labs(title = "",
         x = "",
         y = "Costs saved (billion)")+
    guides(fill = guide_legend(title = NULL))
  
################################################################################################################################
#                                             4. Exportation des données                                                       #
################################################################################################################################

export(costs, here("results", "HIA", "IC95_costs_avoided.xlsx"))
ggsave(here("results", "HIA", "costs_avoided_dates.pdf"), plot = graph_yll_costs_dates)  

