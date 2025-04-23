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
#                                             2. Importation des données                                                       #
################################################################################################################################

# Deaths and prevented deaths by year
deaths_data <- import(here("Python_code", "data_python.csv"))

################################################################################################################################
#                                             3. Parameters                                                                    #
################################################################################################################################

source(here("R_code", "0_parameters.R"))

################################################################################################################################
#                                             4. Data preparation                                                              #
################################################################################################################################

# Delete the 5% most extreme values
deaths_data <- deaths_data %>%
  group_by(scenario, year, age) %>%
  filter(between(deaths, quantile(deaths, 0.025), quantile(deaths, 0.975)))

################################################################################################################################
#                                             5. Deaths by age                                                                 #
################################################################################################################################

# Mean and 95% CI of deaths by age
simulations_summary_deaths <- deaths_data %>%
  group_by(scenario, year, age) %>%
  summarise(
    mean_deaths = mean(deaths, na.rm = TRUE),
    lower_ci = quantile(deaths, 0.025, na.rm = TRUE), # Lower limit of the 95% CI
    upper_ci = quantile(deaths, 0.975, na.rm = TRUE) # Upper limit of the 95% CI
  )

################################################################################################################################
#                                             6. Prevented deaths to baseline by age                                           #
################################################################################################################################

av_deaths <- deaths_data %>%
  group_by(scenario, year, age, simulation_id) %>%
  summarise(prevented_deaths = sum(avoided_deaths, na.rm = TRUE))

# Mean and 95% CI of prevented deaths by age
simulations_summary_av_deaths <- av_deaths %>%
  group_by(scenario, age, year) %>%
  summarise(
    mean_prev_deaths = mean(prevented_deaths, na.rm = TRUE),
    lower_ci = quantile(prevented_deaths, 0.025, na.rm = TRUE), # Lower limit of the 95% CI
    upper_ci = quantile(prevented_deaths, 0.975, na.rm = TRUE) # Upper limit of the 95% CI
  )

################################################################################################################################
#                                             7. Total deaths / year / scenario                                                #
################################################################################################################################

tot_deaths <- deaths_data %>%
  group_by(scenario, year, simulation_id) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE))

# Calculation of the mean and 95% CI
simulations_summary_tot_deaths <- tot_deaths %>%
  group_by(scenario, year) %>%
  summarise(
    mean_tot_deaths = mean(total_deaths, na.rm = TRUE),
    lower_ci = quantile(total_deaths, 0.025, na.rm = TRUE), # Lower limit of the 95% CI
    upper_ci = quantile(total_deaths, 0.975, na.rm = TRUE) # Upper limit of the 95% CI
  )

################################################################################################################################
#                                             8. Total prevented deaths / year / scenario                                      #
################################################################################################################################

tot_av_deaths <- deaths_data %>%
  group_by(scenario, year, simulation_id) %>%
  summarise(total_av_deaths = sum(avoided_deaths, na.rm = TRUE))

# Mean and 95% CI of total prevented deaths
simulations_summary_tot_av_deaths <- tot_av_deaths %>%
  group_by(scenario, year) %>%
  summarise(
    mean_rr = mean(total_av_deaths, na.rm = TRUE),
    lower_ci = quantile(total_av_deaths, 0.025, na.rm = TRUE), # Lower limit of the 95% CI
    upper_ci = quantile(total_av_deaths, 0.975, na.rm = TRUE) # Upper limit of the 95% CI
  )

################################################################################################################################
#                                             9. Figures : Total prevented deaths                                              #
################################################################################################################################

# During the all period of time
graph_tot_av_deaths <- ggplot(
  simulations_summary_tot_av_deaths %>%
    filter(scenario != "actuel"),
  aes(
    x = year,
    y = mean_rr,
    group = scenario,
    color = scenario
  )
) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = scenario), alpha = 0.5, linetype = 0) +
  geom_line(size = 0.6, na.rm = TRUE) +
  labs(
    title = "",
    x = "",
    y = "Number of deaths prevented"
  ) +
  scale_color_manual(
    values = col_scenario,
    labels = labels_scenario
  ) +
  scale_fill_manual(
    values = col_scenario,
    labels = labels_scenario
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(face = "bold", size = rel(1)),
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(title = NULL),
    fill = guide_legend(title = NULL)
  )

# At specific dates
graph_tot_av_deaths_dates <- ggplot(
  simulations_summary_tot_av_deaths %>%
    filter(
      year %in% c(2040, 2050, 2060),
      scenario != "actuel"
    ),
  aes(
    x = scenario,
    y = mean_rr,
    fill = scenario
  )
) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    alpha = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = lower_ci,
      ymax = upper_ci
    ),
    width = 0.2,
    position = position_dodge(0.9)
  ) +
  facet_wrap(~year,
    ncol = 3
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_fill_manual(
    values = col_scenario,
    labels = labels_scenario
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "",
    x = "",
    y = "Deaths prevented"
  ) +
  guides(fill = guide_legend(title = NULL))


################################################################################################################################
#                                             10. Data exportation                                                             #
################################################################################################################################

# Deaths by age
export(simulations_summary_deaths, here("results", "5_actuel_meat2", "HIA", "IC95_deaths.xlsx"))

# Prevented deaths by age
export(av_deaths, here("results", "5_actuel_meat2", "HIA", "av_deaths.csv"))
export(simulations_summary_av_deaths, here("results", "5_actuel_meat2", "HIA", "IC95_av_deaths.xlsx"))

# Total deaths by year
export(tot_deaths, here("results", "5_actuel_meat2", "HIA", "tot_deaths.csv"))
export(simulations_summary_tot_deaths, here("results", "5_actuel_meat2", "HIA", "IC95_tot_deaths.xlsx"))

# Total prevented deaths by year
export(tot_av_deaths, here("results", "5_actuel_meat2", "HIA", "tot_deaths_prev.csv"))
export(simulations_summary_tot_av_deaths, here("results", "5_actuel_meat2", "HIA", "IC95_tot_deaths_prev.csv"))

ggsave(here("results", "5_actuel_meat2", "HIA", "tot_deaths_prev.pdf"), graph_tot_av_deaths)
ggsave(here("results", "5_actuel_meat2", "HIA", "tot_deaths_prev_dates.pdf"), graph_tot_av_deaths_dates)
