################################################################################################################################
#                                             1. Chargement des packages                                                       #
################################################################################################################################

pacman::p_load(
  rio,
  here,
  dplyr,
  tidyr,
  tidyverse,
  flextable,
  scales
)

################################################################################################################################
#                                             2. Data importation                                                              #
################################################################################################################################

sensi_data <- import(here("sensi_analysis", "Sensitivity_Analysis.xlsx"))

################################################################################################################################
#                                             3. Parameters                                                                    #
################################################################################################################################

source(here("R_code", "0_parameters.R"))

labels_analysis <- c("main" = "Main Analysis",
                     "ttfe_0" = "Immediate time to full effect",
                     "ttfe_20" = "20-year time to full effect",
                     "m_1" = "No reduction in RR effect",
                     "m_05" = "50% reduction in RR effect",
                     "diet_0" = "Immediate diet implementation",
                     "diet_lin" = "Linear diet implementation")

order_scenarios  <- c("sc1", "sc2", "sc3", "sc4")

################################################################################################################################
#                                             4. Data preparation                                                              #
################################################################################################################################

# Identify scondary analyses (other than main_analysis)
other_analyses <- sensi_data %>%
  filter(analysis != "main") %>%
  pull(analysis) %>%
  unique()

# Duplicate the main analysis date in each facet
main_overlay <- sensi_data %>%
  filter(analysis == "main", scenario != "actuel") %>%
  rename(analysis_orig = analysis) %>%
  tidyr::crossing(analysis = other_analyses) %>%  # dupliquer pour chaque analysis
  mutate(group = "main_overlay")

# Prepare the secondary analyses data
main_data <- sensi_data %>%
  filter(analysis != "main", scenario != "actuel") %>%
  mutate(group = "normal")

# Merge data for plotting
plot_data <- bind_rows(main_data, main_overlay)%>% 
  mutate(scenario = forcats::fct_rev(scenario))

################################################################################################################################
#                                             5. Graphical representation                                                      #
################################################################################################################################

graph_sensi <- ggplot(plot_data,
       aes(x = nprev_mean,
           y = scenario,
           color = factor(scenario),
           alpha = group)) +
  geom_point(size = 2.5, shape = 18) +
  geom_segment(aes(x = nprev_lci,
                   xend = nprev_uci,
                   y = scenario,
                   yend = scenario),
                   linewidth = 0.3) +
  facet_grid(rows = vars(analysis), cols = vars(year),
             labeller = labeller(analysis = labels_analysis)) +
  scale_color_manual(values = col_scenario,
                     labels = labels_scenario) +
  scale_alpha_manual(values = c(normal = 1, main_overlay = 0.35)) +  # Level of transparency for main overlay
  labs(title = "",
       y = NULL,
       x = "Prevented deaths",
       color = "Scenario") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, size = 9),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 7),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, max(sensi_data$nprev_uci, na.rm = TRUE)),
                     breaks = c(50000, 100000, 150000, 200000, 250000),
                     labels = scales::label_comma()) +
  geom_vline(xintercept = 150000, linetype = "dashed", color = "black", linewidth = 0.3) +
  guides(color = guide_legend(title = NULL),
         alpha = "none")  # pas de légende pour alpha

plot(graph_sensi)

################################################################################################################################
#                                             5. Tableau                                                      #
################################################################################################################################

sensi_data_table <- sensi_data %>%
  group_by(year, scenario) %>%
  mutate(shift = percent((nprev_mean - nprev_mean[analysis == "main"]) / nprev_mean[analysis == "main"], 0.1),
         analysis = factor(analysis,
                           levels = c("main", "ttfe_0", "ttfe_20", "m_1", "m_05", "diet_0", "diet_lin"),
                           labels = labels_analysis),
         year = as.character(year)) %>%
  ungroup() %>% 
  filter(scenario != "actuel",
         analysis != "Main Analysis") %>%
  select(analysis, year, scenario, shift) %>%
  arrange(analysis, year, scenario)

sensi_data_table <- sensi_data_table %>%
  pivot_wider(names_from = scenario, values_from = shift) %>%
  qflextable() %>% # Create the table with qflextable
  set_header_labels(
    "analysis" = "Analysis", 
    "year" = "Year",
    "sc1" = "Scenario 1",
    "sc2" = "Scenario 2",
    "sc3" = "Scenario 3",
    "sc4" = "Scenario 4") %>%
  width(j = 2, width = 1) %>% 
  width(j=c(3,4,5, 6), width = 1.5) %>% 
  vline(part = "all", j = 1) %>% # Vertical line after the 2nd column
  vline(part = "all", j = 2) %>%
  vline(part = "all", j = 3) %>% 
  vline(part = "all", j = 4) %>%
  vline(part = "all", j = 5) %>%
  align(align = "center", j = c(2:6), part = "all") %>% # Center the text in all columns except Analysis
  bold(i = 1, part = "header") %>% # Bold the first row of the header
  bg(part = "all", bg = "white") # Set the background color of the table to white

plot(sensi_data_table)

################################################################################################################################
#                                             6. Exportation des données                                                      #
################################################################################################################################

ggsave(here("sensi_analysis", "sensitivity_analysis.pdf"), graph_sensi)

save_as_image(sensi_data_table, here("sensi_analysis", "sensitivity_analysis_table.png"))
