################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,                 # Importation de fichiers
  here,                # Localisation des fichiers dans le dossier du projet
  dplyr,               # Manipulation des données
  tidyr,
  tidyverse,
  gtsummary
)

################################################################################################################################
#                                             2. Importation des données                                                       #
################################################################################################################################

sensi_data <- import(here("data", "sensitivity_analysis_2.xlsx"))

################################################################################################################################
#                                             3. Charte graphique                                                              #
################################################################################################################################

col_scenario <- c(
  "actuel" = "azure4",
  "sc0" = "palevioletred3",
  "sc1" = "#699cc2",
  "sc2" = "#974175",
  "sc3" = "#50cd9f",
  "sc4" = "#cb6c2d",
  "sc5" = "royalblue4"
)

labels_scenario <- c("actuel" = "BAU",
                     "sc1" = "Scenario 1",
                     "sc2" = "Scenario 2",
                     "sc3" = "Scenario 3",
                     "sc4" = "Scenario 4")

labels_analysis <- c("main" = "Main Analysis",
                     "sensi1" = "Immediate time to full effect",
                     "sensi2" = "Linear 20-year time to full effect",
                     "sensi3" = "Sigmoidal 10-year time to full effect",
                     "sensi4" = "No RR effect reduction",
                     "sensi5" = "50% RR effect reduction",
                     "sensi6" = "Immediate diet implementation",
                     "sensi7" = "Linear diet implementation",
                     "sensi8" = "Sigmoidal diet implementation")

order_scenarios <- c("sc1", "sc2", "sc3", "sc4")

sensi_data$scenario <- factor(sensi_data$scenario, levels = order_scenarios)

################################################################################################################################
#                                             4. Représentation graphique                                                      #
################################################################################################################################

graph_sensi <- ggplot(sensi_data %>% 
         filter(scenario != "actuel"), 
       aes(x = avoided_deaths,
                       y = scenario,
                       color = factor(scenario)))+
  geom_point(position = position_dodge(width = 0.5), size = 2.5, shape = 18)+
  geom_segment(aes(x = ic_lower,
               xend = ic_upper,
               y = scenario,
               yend = scenario),
               position = position_dodge(width = 0.5),
               linewidth = 0.3)+
  facet_grid(rows = vars(analysis),
             cols = vars(year),
             labeller = labeller(analysis = labels_analysis)
             )+
  scale_color_manual(values = col_scenario,
                     labels = labels_scenario)+
  labs(title = "",
       y = NULL,
       x = "Number of deaths prevented",
       color = "Scenario")+
  theme(strip.text.y = element_text(angle = 0, hjust = 0, size = 9),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 7),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank())+
  scale_x_continuous(limits = c(0, max(sensi_data$ic_upper, na.rm = TRUE)),
                     breaks = c(50000, 100000, 150000),
                     labels = scales::label_comma())+
  geom_vline(xintercept = 100000, linetype = "dashed", color = "black", linewidth = 0.3)+
  guides(color = guide_legend(title = NULL))

################################################################################################################################
#                                             5. Tableau                                                      #
################################################################################################################################

sensi_data_table <- sensi_data %>%
  filter(scenario != "actuel") %>%
  select(analysis, year, scenario, shift) %>%
  arrange(analysis, year, scenario) %>% 
  mutate(shift = shift*100)


################################################################################################################################
#                                             6. Exportation des données                                                      #
################################################################################################################################

ggsave(here("results", "sensitivity_analysis.pdf"), graph_sensi)
