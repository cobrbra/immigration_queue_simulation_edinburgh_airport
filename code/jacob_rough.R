library(here)
library(targets)
library(tidyverse)
library(showtext)
library(cowplot)
library(lubridate)

tar_source(files = c(
  here("code/process_data.R"), 
  here("code/step_1_aircraft.R"),
  here("code/step_2_route.R"),
  here("code/step_3_immigration.R"),
  here("code/kpis.R"),
  here("code/analysis.R"),
  here("code/get_figures.R"),
  here("code/get_tables.R")
))


##### EXAMPLE SINGLE DATASET

# generate desks

n_desks <- 9
desk_means <- pmax(0, rep(90, n_desks))
bordercheck_desks <- list(n_borderchecks = n_desks, 
                          bordercheck_means = desk_means,
                          bordercheck_sd = 14)

n_egates <- 10
bordercheck_egates = list(
  n_borderchecks = n_egates, 
  bordercheck_mean = 45,
  bordercheck_sd = 5)

simulated_arrivals <- tar_read(future_aircrafts_arrivals) %>% 
  complete_aircrafts_arrivals(tar_read(hubs), tar_read(countries), tar_read(prop_nationality), 
                              tar_read(delay_dist), tar_read(n_passengers_quantiles), 
                              tar_read(load_factor), tar_read(future_coached_levels)) %>% 
  get_passengers_after_aircrafts() %>% 
  get_passengers_after_routes(tar_read(coach_dist), tar_read(walk_dist), tar_read(base_walk_dist))


simulated_queue <- simulated_arrivals %>% 
  immigration_queue(bordercheck_desks = bordercheck_desks, 
             bordercheck_egates = bordercheck_egates, 
             egate_uptake_prop = .7, target_eligibility = .85, 
             egate_failure_prop = tar_read(egate_failure_prop), 
             failed_egate_priority = tar_read(failed_egate_priority),
             seed = 1)   


# EXAMPLE MULTI-SIM EXPERIMENT

sim_settings <- generate_sim_settings()

sim_results <- sim_analysis_data(sim_settings,
                                 tar_read(future_aircrafts_arrivals), 
                                 hubs = tar_read(hubs), 
                                 countries = tar_read(countries), 
                                 prop_nationality = tar_read(prop_nationality), 
                                 delay_dist = tar_read(delay_dist), 
                                 n_passengers_quantiles = tar_read(n_passengers_quantiles), 
                                 egate_failure_prop = tar_read(egate_failure_prop),
                                 failed_egate_priority = tar_read(failed_egate_priority),
                                 coached_levels = tar_read(future_coached_levels), 
                                 coach_dist = tar_read(coach_dist), 
                                 walk_dist = tar_read(walk_dist), 
                                 base_walk_dist = tar_read(base_walk_dist),
                                 wait_time_kpis = c("mean_wait_time", "wait_time_60", "wait_time_15"),
                                 save_data = FALSE)


# EXAMPLE MULTI-SIM EXPERIMENT SPECIFYING TRAJECTORY

sim_settings <- specify_sim_settings(n_egates = c(10, 13, 15, 18, 21),
                                    n_gen_arrivals = 5,
                                    n_gen_queues = 5)

sim_results <- sim_analysis_data(sim_settings,
                                 tar_read(future_aircrafts_arrivals), 
                                 hubs = tar_read(hubs), 
                                 countries = tar_read(countries), 
                                 prop_nationality = tar_read(prop_nationality), 
                                 delay_dist = tar_read(delay_dist), 
                                 n_passengers_quantiles = tar_read(n_passengers_quantiles), 
                                 egate_failure_prop = tar_read(egate_failure_prop),
                                 failed_egate_priority = tar_read(failed_egate_priority),
                                 coached_levels = tar_read(future_coached_levels), 
                                 coach_dist = tar_read(coach_dist), 
                                 walk_dist = tar_read(walk_dist), 
                                 base_walk_dist = tar_read(base_walk_dist),
                                 wait_time_kpis = c("mean_wait_time", "wait_time_60", "wait_time_15"),
                                 save_data = FALSE)

core_recommendation_stats <- sim_results %>% 
  summarise(wait_time_60_egate = mean(wait_time_60_egate), 
            wait_time_60_desk = mean(wait_time_60_desk),
            wait_time_15_desk = mean(wait_time_15_desk),
            wait_time_15_egate = mean(wait_time_15_egate),
            .by = c("year", "n_egates", "egate_uptake", "target_eligibility")) %>% 
  pivot_longer(cols = c(wait_time_60_desk, wait_time_60_egate, wait_time_15_desk, wait_time_15_egate),
               names_to = "which_kpi",
               values_to = "kpi") %>%
  mutate(overall_egate_usage = egate_uptake * target_eligibility,
         check = if_else(str_detect(which_kpi, "desk"), "Desk", "eGate"),
         which_kpi = if_else(str_detect(which_kpi, "15"), 
                             "Proportion waits < 15mins", 
                             "Proportion waits < 60mins"))


panel_1 <- core_recommendation_stats %>%
  distinct(year, egate_uptake, target_eligibility, overall_egate_usage, n_egates) %>% 
  pivot_longer(c(egate_uptake, target_eligibility, overall_egate_usage, n_egates), 
               names_to = "assumption", values_to = "value") %>%
  mutate(
    assumption = factor(
      case_when(
        assumption == "egate_uptake" ~ "eGate Uptake",
        assumption == "overall_egate_usage" ~ "Overall eGate Usage",
        assumption == "target_eligibility" ~ "eGate Eligibility",
        assumption == "n_egates" ~ "n_egates"
      ),
      levels = c("n_egates", "eGate Eligibility", "eGate Uptake", "Overall eGate Usage")),
    fac = factor(if_else(assumption == "n_egates", 
                         "Recommended Number of eGates", 
                         "Core Assumptions"),
                 level = c("Recommended Number of eGates",
                           "Core Assumptions"))) %>% 
    ggplot(aes(x = year, y = value, fill = assumption)) + 
    geom_col(position = "dodge", alpha = 0.9) + 
    facet_wrap(~fac, ncol = 1, scales = "free_y") + 
    theme_edi_airport() +
    theme(legend.title = element_blank(),
          legend.position = "bottom") + 
    scale_fill_manual(breaks = c("eGate Eligibility", "eGate Uptake", "Overall eGate Usage"),
                      values = c("Overall eGate Usage" = edi_airport_colours[4],
                                 "eGate Uptake" = edi_airport_colours[5], 
                                 "eGate Eligibility" = edi_airport_colours[6],
                                 "n_egates" = edi_airport_colours[7])) + 
    labs(x = "Year", y = "") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

panel_2 <- core_recommendation_stats %>% 
  ggplot(aes(x = year, y = kpi, fill = check)) + 
  geom_col(position = "dodge", alpha = 0.9) + 
  facet_wrap(~which_kpi, ncol = 1) + 
  labs(x = "Year", y = "") + 
  theme_edi_airport() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  scale_fill_manual(values = edi_airport_colours[2:1]) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

prototype_rec_fig <- plot_grid(
  (panel_1),
  (panel_2)
)
prototype_rec_fig

# SAVE MANUALLY
ggsave(filename = here("figures/prototype_rec_fig.png"),
       plot = prototype_rec_fig,
       width = 12, 
       height = 5)













