# Reproduce Fear of Elephants Analyses ----
#
# Reproduction script for:
# "Fear of Elephants Shapes Hadza Hunter-Gatherer Movement and Creates a Refuge for
#  their Prey" (Wood, Deffner, Paolo, Anyawire, Mabulla, Kiffner)
#
# Geographic coordinates in data_public/ have been translated by a fixed private offset
# (distances/shapes preserved, true position unrecoverable; see data_public/README.md).
# Models are loaded pre-fit from fit_models_public/ rather than refit.
#
# To run: open Fear of Elephants.Rproj in RStudio, source this file, then call:
#   reproduce_all(data_location, figures_location, tables_location, max_figure_dimensions)

library("ggplot2") 
library("magick")  
library("ggthemes") 
library("dplyr") 
library("patchwork") 
library("viridis") 
library("MASS")         
library("CircStats")    
library("fitdistrplus") 
library("itsadug")
library("scales") 
library("lubridate")  
library("tidyr")      
library("activity") 
library("overlap")  
library("rethinking")
library("brms")       
library("posterior") 
library("purrr")    
library("geosphere")
library("loo")
library("HDInterval")

# Paths -- relative to the project root (open Fear of Elephants.Rproj first, or
# setwd() to the folder containing this script's parent "code/" directory).
data_location        <- "data_public/"
figures_location      <- "figures_public/"
tables_location       <- "tables_public/"
text_output_folder    <- "text_reported_results_public/"
fit_models_location   <- "fit_models_public/"

dir.create(figures_location, showWarnings = FALSE)
dir.create(tables_location, showWarnings = FALSE)
dir.create(text_output_folder, showWarnings = FALSE)
dir.create(file.path(figures_location, "science_submission"), showWarnings = FALSE, recursive = TRUE)

ci_level <- 0.90

#ungulate species list
ungulates <- c("Kirk's dik-dik", "Impala", "Greater kudu", "Bushbuck", "Giraffe", "Bushpig", "Bush duiker", "Klipspringer", "Warthog", "Eland", "Plains zebra")

# Maximum figure dimensions
max_figure_dimensions <- list(
  onecol = list(width = 8.8, max_length = 22.0),
  twocol = list(width = 18.0, max_length = 22.5)
)




save_figures <- function(data_location, figures_location, max_figure_dimensions) {

  #This is figure 1
  plot_and_save_figure_interview_results(data_location, figures_location, height_in=3, width_in=9)
 
  #This if figure 2
  plot_and_save_camera_grid_schema_and_GPS_tracks(data_location, figures_location, height_in = 3, width_in = 9)
  
  #this is figure 3
  save_figure_3_and_figure_S1_patchwork(data_location, figures_location, sim_method="rot", run_all=FALSE)
  
  diurnality_data_30_min_filter <- read.csv(file.path(data_location, "camera_trap_data_for_diurnality_analysis.csv"))
  diurnality_model_30_min_filter <- readRDS(paste0(fit_models_location, "model_7.rds"))

  #this is figure 4
  # NHB submission change: now calls the _NHB variant (adds Panel C, the posterior
  # difference distribution); see the commented-out original
  # plot_and_save_empirical_and_model_predicted_diurnality() definition below.
  plot_and_save_figure_4 <- plot_and_save_empirical_and_model_predicted_diurnality_NHB(figures_location = figures_location,
                                                         data=diurnality_data_30_min_filter,
                                                         model=diurnality_model_30_min_filter,
                                                         ci_level=ci_level,
                                                         figure_name="fig_4_empirical_and_modeled_diurnality_30_min_time_filter")


  #Figure 5
  # NHB submission change: now calls the _NHB variant (adds Panel C, percent change in RAI
  # under Model 10); see the commented-out original
  # plot_and_save_RAI_by_species_and_elephant_zone_empirical_and_model_predictions()
  # definition below.
  plot_and_save_RAI_by_species_and_elephant_zone_empirical_and_model_predictions_NHB(figure_name="Figure_5_RAI",
                                                                                 data_location = data_location,
                                                                                 fit_models_location = fit_models_location,
                                                                                 figures_location=figures_location,
                                                                                 width_in=9, height_in=3,
                                                                                 show_outliers=FALSE)
  
  #this is figure S1
  save_figure_3_and_figure_S1_patchwork(data_location, figures_location, sim_method="bcrw", run_all=FALSE)
  
  #figure S2
  plot_and_save_supp_figure_of_diurnality_diff_distribution(figures_location = figures_location, model=diurnality_model_30_min_filter, figure_name="figure_S2_posterior_distribution_of_difference_in_daytime_detection_probability")
  
  #Figure S3
  plot_and_save_fig_S3_RAI_percent_diff_distribution(fit_models_location = fit_models_location,
                                                                       tables_location     = tables_location,
                                                                       data_location       = data_location,
                                                                       figures_location    = figures_location,
                                                                       ci_level            = ci_level)

  #Figures S4
  plot_and_save_posterior_predictions_model_10(
    fit_models_location = fit_models_location,
    data_location       = data_location,
    figures_location    = figures_location,
    ci_level            = ci_level
  )
  
  
}




save_text_reported_results <- function(data_location, fit_models_location, text_output_folder)
{
  interview_data_path <- file.path(data_location, "dangerous_animals_interview_data.csv")
  interview_data <- read.csv(interview_data_path, stringsAsFactors = FALSE)
  figure_4_data <- read.csv(file.path(data_location, "camera_trap_data_for_diurnality_analysis.csv"))
  
  ##Interview Text Reported Results ----
  n_females_interviewed <- nrow(filter(interview_data, sex=="F"))
  n_females_seen_elephant <- nrow(filter(interview_data, sex=="F" & seen_elephant==TRUE))
  percent_females_seen_elephant <- round(n_females_seen_elephant/n_females_interviewed*100,0)
  n_males_interviewed <- nrow(filter(interview_data, sex=="M"))
  n_males_seen_elephant <- nrow(filter(interview_data, sex=="M" & seen_elephant==TRUE))
  percent_males_seen_elephant <- round(n_males_seen_elephant/n_males_interviewed*100,0)
  n_females_know_person_killed_by_elephant <- nrow(filter(interview_data, sex=="F" & seen_person_who_died_from_elephant==TRUE ))
  n_males_know_person_killed_by_elephant <- nrow(filter(interview_data, sex=="M" & seen_person_who_died_from_elephant==TRUE ))
  percent_females_know_person_killed_by_elephant <- round(n_females_know_person_killed_by_elephant/n_females_interviewed*100,0)
  percent_males_know_person_killed_by_elephant <- round(n_males_know_person_killed_by_elephant/n_males_interviewed*100,0)

  # Free-Listed Dangerous Animals Text Reported Results
  free_list_cols <- paste0("dangerous_animal_", 1:5)
  count_free_listed <- function(animal) {
    sum(trimws(unlist(interview_data[free_list_cols])) == animal, na.rm = TRUE)
  }
  n_free_listed_lion     <- count_free_listed("Lion")
  n_free_listed_snake    <- count_free_listed("Snake")
  n_free_listed_elephant <- count_free_listed("Elephant")
  n_free_listed_leopard  <- count_free_listed("Leopard")

  message("Free-listed dangerous animal counts (n = ", nrow(interview_data), " interviews):")
  message("  Lion: ",     n_free_listed_lion)
  message("  Snake: ",    n_free_listed_snake)
  message("  Elephant: ", n_free_listed_elephant)
  message("  Leopard: ",  n_free_listed_leopard)

  # Create individual rows for the table
  row1 <- data.frame(
    `text reported result` = "n_females_interviewed",
    value = n_females_interviewed
  )
  
  row2 <- data.frame(
    `text reported result` = "n_females_seen_elephant",
    value = n_females_seen_elephant
  )
  
  row3 <- data.frame(
    `text reported result` = "percent_females_seen_elephant",
    value = percent_females_seen_elephant
  )
  
  row4 <- data.frame(
    `text reported result` = "n_males_interviewed",
    value = n_males_interviewed
  )
  
  row5 <- data.frame(
    `text reported result` = "n_males_seen_elephant",
    value = n_males_seen_elephant
  )
  
  row6 <- data.frame(
    `text reported result` = "percent_males_seen_elephant",
    value = percent_males_seen_elephant
  )
  
  row7 <- data.frame(
    `text reported result` = "n_females_know_person_killed_by_elephant",
    value = n_females_know_person_killed_by_elephant
  )
  
  row8 <- data.frame(
    `text reported result` = "n_males_know_person_killed_by_elephant",
    value = n_males_know_person_killed_by_elephant
  )
  
  row9 <- data.frame(
    `text reported result` = "percent_females_know_person_killed_by_elephant",
    value = percent_females_know_person_killed_by_elephant
  )
  
  row10 <- data.frame(
    `text reported result` = "percent_males_know_person_killed_by_elephant",
    value = percent_males_know_person_killed_by_elephant
  )
  
  # Elephant Avoidance Text Reported Results
  
  #get samples from posterior distribution of PSA model
  s <- readRDS(paste0(fit_models_location, "samples_Elephants_Environment.rds"))
  
  row11 <- data.frame(
    `text reported result` = "female_ruggedness_effect_mean",
    value = round(mean(s$weights[,1,1]),2)
  ) 
  
  row12 <- data.frame(
    `text reported result` = "female_ruggedness_effect_lower",
    value = round(HPDI(s$weights[,1,1], ci_level)[[1]],2)
  ) 
  
  row13 <- data.frame(
    `text reported result` = "female_ruggedness_effect_upper",
    value = round(HPDI(s$weights[,1,1], ci_level)[[2]],2)
  ) 
  
  row14 <- data.frame(
    `text reported result` = "male_ruggedness_effect_mean",
    value = round(mean(s$weights[,1,2]),2)
  ) 
  
  row15 <- data.frame(
    `text reported result` = "male_ruggedness_effect_lower",
    value = round(HPDI(s$weights[,1,2], ci_level)[[1]],2)
  ) 
  
  row16 <- data.frame(
    `text reported result` = "male_ruggedness_effect_upper",
    value = round(HPDI(s$weights[,1,2], ci_level)[[2]],2)
  ) 
  
  #Elephant Effects on Travel
  row17 <- data.frame(
    `text reported result` = "female_elephant_effect_mean",
    value = round(mean(s$weights[,2,1]),2)
  ) 
  
  row18 <- data.frame(
    `text reported result` = "female_elephant_effect_lower",
    value = round(HPDI(s$weights[,2,1], ci_level)[[1]],2)
  ) 
  
  row19 <- data.frame(
    `text reported result` = "female_elephant_effect_upper",
    value = round(HPDI(s$weights[,2,1], ci_level)[[2]],2)
  ) 
  
  row20 <- data.frame(
    `text reported result` = "male_elephant_effect_mean",
    value = round(mean(s$weights[,2,2]),2)
  ) 
  
  row21 <- data.frame(
    `text reported result` = "male_elephant_effect_lower",
    value = round(HPDI(s$weights[,2,2], ci_level)[[1]],2)
  ) 
  
  row22 <- data.frame(
    `text reported result` = "male_elephant_effect_upper",
    value = round(HPDI(s$weights[,2,2], ci_level)[[2]],2)
  ) 
  
  #Road Effects on Travel
  row23 <- data.frame(
    `text reported result` = "female_road_effect_mean",
    value = round(mean(s$weights[,3,1]),2)
  ) 
  
  row24 <- data.frame(
    `text reported result` = "female_road_effect_lower",
    value = round(HPDI(s$weights[,3,1], ci_level)[[1]],2)
  ) 
  
  row25 <- data.frame(
    `text reported result` = "female_road_effect_upper",
    value = round(HPDI(s$weights[,3,1], ci_level)[[2]],2)
  ) 
  
  row26 <- data.frame(
    `text reported result` = "male_road_effect_mean",
    value = round(mean(s$weights[,3,2]),2)
  ) 
  
  row27 <- data.frame(
    `text reported result` = "male_road_effect_lower",
    value = round(HPDI(s$weights[,3,2], ci_level)[[1]],2)
  ) 
  
  row28 <- data.frame(
    `text reported result` = "male_road_effect_upper",
    value = round(HPDI(s$weights[,3,2], ci_level)[[2]],2)
  ) 
  
  #River Effects on Travel
  row29 <- data.frame(
    `text reported result` = "female_river_effect_mean",
    value = round(mean(s$weights[,4,1]),2)
  ) 
  
  row30 <- data.frame(
    `text reported result` = "female_river_effect_lower",
    value = round(HPDI(s$weights[,4,1], ci_level)[[1]],2)
  ) 
  
  row31 <- data.frame(
    `text reported result` = "female_river_effect_upper",
    value = round(HPDI(s$weights[,4,1], ci_level)[[2]],2)
  ) 
  
  row32 <- data.frame(
    `text reported result` = "male_river_effect_mean",
    value = round(mean(s$weights[,4,2]),2)
  ) 
  
  row33 <- data.frame(
    `text reported result` = "male_river_effect_lower",
    value = round(HPDI(s$weights[,4,2], ci_level)[[1]],2)
  ) 
  
  row34 <- data.frame(
    `text reported result` = "male_river_effect_upper",
    value = round(HPDI(s$weights[,4,2], ci_level)[[2]],2)
  ) 
  
  model_1_2_3_comparison <- read.csv(paste0(tables_location, "Table_S2_PSA_model_comparison.csv"))
  
  model_weight_elephants_environment_loo_psa_rotation <- model_1_2_3_comparison$LOO.Pseudo.BMA.Weight[model_1_2_3_comparison$Model==3]
  
  row35 <- data.frame(
    `text reported result` = "model_weight_elephants_environment_loo_psa_rotation",
    value = model_weight_elephants_environment_loo_psa_rotation)
  
  
  delta_elpd_m3_m1 <- model_1_2_3_comparison$Delta.ELPD[model_1_2_3_comparison$Model==1]
  se_diff <- model_1_2_3_comparison$SE.Delta.ELPD[model_1_2_3_comparison$Model==1]
  
  # Calculate a 90% interval based on the Standard Error (z = 1.645)
  delta_elpd_m3_m1_lower_90 <- delta_elpd_m3_m1 - 1.645 * se_diff
  delta_elpd_m3_m1_upper_90 <- delta_elpd_m3_m1 + 1.645 * se_diff
  
  row36 <- data.frame(
    `text reported result` = "delta_elpd_elephants_environment_vs_just_environment_loo_psa_rotation",
    value = delta_elpd_m3_m1)
  
  row37 <- data.frame(
    `text reported result` = "delta_elpd_lower_elephants_environment_vs_just_environment_loo_psa_rotation",
    value = delta_elpd_m3_m1_lower_90)
  
  row38 <- data.frame(
    `text reported result` = "delta_elpd_upper_elephants_environment_vs_just_environment_loo_psa_rotation",
    value = delta_elpd_m3_m1_upper_90)
  
  # Diurnality Analysis
  # Marginal posterior probability of daytime detection by elephant zone
  #
  # We use add_epred_draws() to generate posterior predicted probabilities 
  # that incorporate both the fixed effects and the species-level random effects (intercepts and slopes). 
  # This gives us a predicted P(nocturnal) for every combination of species x zone x
  # posterior draw.
  #
  # We then convert to P(diurnal) = 1 - P(nocturnal) for each species x draw.
  #
  # To get the average across species, we average P(diurnal) across all species
  # within each posterior draw before building a summary. This order of operations is
  # important. Averaging within draws and then computing the HPDI correctly
  # propagates posterior uncertainty, whereas averaging across draws first would
  # produce artificially narrow intervals.
  #
  # Averaging on the probability scale within each draw (rather than on the logit
  # scale) also avoids Jensen's inequality bias; the mean of a nonlinear
  # transformation is not the same as the transformation of the mean.
  #
  # The result is a marginal estimate... the expected probability of daytime
  # detection for a species drawn at random from the observed sample, averaged
  # over full posterior uncertainty in all fixed and random effects. This is the
  # appropriate estimand for a summary statement about the average species in the
  # dataset.
  
  the_diurnality_model <- readRDS(paste0(fit_models_location, "model_7.rds"))
  
  # Get the species list from the model data
  species_list <- unique(the_diurnality_model$data$species)
  
  # Build a prediction grid: all species x both elephant zones
  pred_grid <- expand.grid(
    species       = species_list,
    elephant_zone = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  
  # Draw posterior samples on the probability scale
  posterior_epred <- add_epred_draws(
    newdata          = pred_grid,
    object           = the_diurnality_model,
    allow_new_levels = FALSE
  )
  
  # .epred is P(diurnal=TRUE | zone, species).
  # We map it directly to .diurnal
  posterior_epred <- posterior_epred %>%
    mutate(.diurnal = .epred)
  
  # Per-species x zone credible intervals ──────────────────────────────────
  
  hpdi_by_species <- posterior_epred %>%
    group_by(species, elephant_zone) %>%
    summarise(
      mean_diurnal  = mean(.diurnal),
      median_diurnal = median(.diurnal),
      lower_90_hpdi = HDInterval::hdi(.diurnal, credMass = ci_level)[1],
      upper_90_hpdi = HDInterval::hdi(.diurnal, credMass = ci_level)[2],
      .groups = "drop"
    ) %>%
    mutate(zone_label = ifelse(elephant_zone, "Elephant zone", "Non-elephant zone")) %>%
    arrange(zone_label, species)
  
  # Average across all species (marginalising over species) ────────────────
  # Average the diurnal probability across species within each draw first,
  # then build a summary — this respects the posterior covariance structure.
  
  hpdi_average <- posterior_epred %>%
    group_by(elephant_zone, .draw) %>%
    summarise(mean_diurnal_draw = mean(.diurnal), .groups = "drop") %>%
    group_by(elephant_zone) %>%
    summarise(
      mean_diurnal   = mean(mean_diurnal_draw),
      median_diurnal = median(mean_diurnal_draw),
      lower_90_hpdi  = HDInterval::hdi(mean_diurnal_draw, credMass = 0.90)[1],
      upper_90_hpdi  = HDInterval::hdi(mean_diurnal_draw, credMass = 0.90)[2],
      .groups = "drop"
    ) %>%
    mutate(
      species    = "AVERAGE (all species)",
      zone_label = ifelse(elephant_zone, "Elephant zone", "Non-elephant zone")
    )
  
  # Ensure both tables have identical columns before binding
  
  hpdi_by_species_clean <- hpdi_by_species %>%
    dplyr::select(species, elephant_zone, zone_label,
           mean_diurnal, median_diurnal, lower_90_hpdi, upper_90_hpdi)
  
  hpdi_average_clean <- hpdi_average %>%
    dplyr::select(species, elephant_zone, zone_label,
           mean_diurnal, median_diurnal, lower_90_hpdi, upper_90_hpdi)
  
  results <- bind_rows(hpdi_by_species_clean, hpdi_average_clean) %>%
    dplyr::select(zone_label, species, mean_diurnal, median_diurnal,
           lower_90_hpdi, upper_90_hpdi)
  
  mean_diurnal_elephant_zone_all_species <- round(results$mean_diurnal[results$species == "AVERAGE (all species)" & results$zone_label == "Elephant zone"],3)
  lower_90_hpdi_diurnal_elephant_zone_all_species <- round(results$lower_90_hpdi[results$species == "AVERAGE (all species)" & results$zone_label == "Elephant zone"],3)
  upper_90_hpdi_diurnal_elephant_zone_all_species <- round(results$upper_90_hpdi[results$species == "AVERAGE (all species)" & results$zone_label == "Elephant zone"],3)
  
  mean_diurnal_non_elephant_zone_all_species <- round(results$mean_diurnal[results$species == "AVERAGE (all species)" & results$zone_label == "Non-elephant zone"],3)
  lower_90_hpdi_diurnal_non_elephant_zone_all_species <- round(results$lower_90_hpdi[results$species == "AVERAGE (all species)" & results$zone_label == "Non-elephant zone"],3)
  upper_90_hpdi_diurnal_non_elephant_zone_all_species <- round(results$upper_90_hpdi[results$species == "AVERAGE (all species)" & results$zone_label == "Non-elephant zone"],3)
  
  row39 <- data.frame(
    `text reported result` = "mean_diurnal_elephant_zone_all_species",
    value = mean_diurnal_elephant_zone_all_species)
  
  row40 <- data.frame(
    `text reported result` = "lower_90_hpdi_diurnal_elephant_zone_all_species",
    value = lower_90_hpdi_diurnal_elephant_zone_all_species)
  
  row41 <- data.frame(
    `text reported result` = "upper_90_hpdi_diurnal_elephant_zone_all_species",
    value = upper_90_hpdi_diurnal_elephant_zone_all_species)
  
  row42 <- data.frame(
    `text reported result` = "mean_diurnal_non_elephant_zone_all_species",
    value = mean_diurnal_non_elephant_zone_all_species)
  
  row43 <- data.frame(
    `text reported result` = "lower_90_hpdi_diurnal_non_elephant_zone_all_species",
    value = lower_90_hpdi_diurnal_non_elephant_zone_all_species)
  
  row44 <- data.frame(
    `text reported result` = "upper_90_hpdi_diurnal_non_elephant_zone_all_species",
    value = upper_90_hpdi_diurnal_non_elephant_zone_all_species)
  
  # Assembling plot data
  plot_df <- data.frame(
    zone        = c("Elephant", "No elephant"),
    mean        = c(mean_diurnal_elephant_zone_all_species,
                    mean_diurnal_non_elephant_zone_all_species),
    lower       = c(lower_90_hpdi_diurnal_elephant_zone_all_species,
                    lower_90_hpdi_diurnal_non_elephant_zone_all_species),
    upper       = c(upper_90_hpdi_diurnal_elephant_zone_all_species,
                    upper_90_hpdi_diurnal_non_elephant_zone_all_species)
  )

  results_wide <- results %>%
    pivot_wider(
      names_from  = zone_label,
      values_from = c(mean_diurnal, median_diurnal, lower_90_hpdi, upper_90_hpdi),
      names_sep   = " | "
    )
   
  #Here calculating the posterior distribution of the difference in diurnality between zones
  
  diff_draws <- posterior_epred %>%
    group_by(elephant_zone, .draw) %>%
    summarise(mean_diurnal_draw = mean(.diurnal), .groups = "drop") %>%
    pivot_wider(names_from = elephant_zone, values_from = mean_diurnal_draw) %>%
    rename(no_elephant = `FALSE`, elephant = `TRUE`) %>%
    mutate(difference = elephant - no_elephant)
  
  # Compute summary stats
  mean_diff  <- round(mean(diff_draws$difference),2)
  hpdi_diff  <- HDInterval::hdi(diff_draws$difference, credMass = 0.90)
  pd         <- round(mean(diff_draws$difference > 0),2)
  
  
  row45 <- data.frame(
    `text reported result` = "probability_of_direction_elephant_diurnal_effect_all_species",
    value = pd)
  
  row46 <- data.frame(
    `text reported result` = "mean_difference_in_diurnality_between_zones_all_species",
    value = mean_diff)

  # 90% HPDI of the difference in P(diurnal): Elephant - No elephant ----
  row_diurnal_diff_hpdi_lower <- data.frame(
    `text reported result` = "lower_90_hpdi_difference_in_diurnality_between_zones_all_species",
    value = round(hpdi_diff[["lower"]], 3))

  row_diurnal_diff_hpdi_upper <- data.frame(
    `text reported result` = "upper_90_hpdi_difference_in_diurnality_between_zones_all_species",
    value = round(hpdi_diff[["upper"]], 3))


  n_camera_trap_detection_figure_4 <- nrow(figure_4_data)
  n_camera_trap_stations_figure_4 <- length(unique(figure_4_data$camera_id))
  n_species_figure_4 <- length(unique(figure_4_data$species))
  
  row47 <- data.frame(
    `text reported result` = "n_camera_trap_detection_figure_4",
    value = n_camera_trap_detection_figure_4)
  
  row48 <- data.frame(
    `text reported result` = "n_camera_trap_stations_figure_4",
    value = n_camera_trap_stations_figure_4)
  
  row49 <- data.frame(
    `text reported result` = "n_species_figure_4",
    value = n_species_figure_4)
  
  fit_m8_fig_S3  <- readRDS(paste0(fit_models_location, "model_8.rds"))
  fit_m10_fig_S3 <- readRDS(paste0(fit_models_location, "model_10.rds"))

  newdata_fig_S3           <- build_RAI_station_prediction_grid(data_location)
  elephant_cols_fig_S3     <- which(newdata_fig_S3$Elephant_Presence == "Elephant")
  no_elephant_cols_fig_S3  <- which(newdata_fig_S3$Elephant_Presence == "No elephant")

  get_diff_draws_fig_S3 <- function(fit) {
    epred <- posterior_epred(fit, newdata = newdata_fig_S3, re_formula = NA)
    rowMeans(epred[, elephant_cols_fig_S3]) - rowMeans(epred[, no_elephant_cols_fig_S3])
  }

  diff_m8_fig_S3  <- get_diff_draws_fig_S3(fit_m8_fig_S3)
  diff_m10_fig_S3 <- get_diff_draws_fig_S3(fit_m10_fig_S3)

  model_averaging_results_fig_S3 <- posterior_predict_elephant_presence_RAI_model_averaging_zi_species_varying(
    fit_models_location = fit_models_location,
    tables_location     = tables_location,
    data_location       = data_location,
    ci_level            = ci_level
  )
  
  diff_averaged_fig_S3 <- model_averaging_results_fig_S3$draws$difference

  mean_diff_RAI_fig_S3_M8       <- round(mean(diff_m8_fig_S3), 2)
  pd_RAI_fig_S3_M8              <- round(mean(diff_m8_fig_S3 > 0), 2)
  mean_diff_RAI_fig_S3_M10      <- round(mean(diff_m10_fig_S3), 2)
  pd_RAI_fig_S3_M10             <- round(mean(diff_m10_fig_S3 > 0), 2)
  mean_diff_RAI_fig_S3_averaged <- round(mean(diff_averaged_fig_S3), 2)
  pd_RAI_fig_S3_averaged        <- round(mean(diff_averaged_fig_S3 > 0), 2)

  row50 <- data.frame(
    `text reported result` = "mean_diff_RAI_fig_S3_M8",
    value = mean_diff_RAI_fig_S3_M8)

  row51 <- data.frame(
    `text reported result` = "pd_RAI_fig_S3_M8",
    value = pd_RAI_fig_S3_M8)

  row52 <- data.frame(
    `text reported result` = "mean_diff_RAI_fig_S3_M10",
    value = mean_diff_RAI_fig_S3_M10)

  row53 <- data.frame(
    `text reported result` = "pd_RAI_fig_S3_M10",
    value = pd_RAI_fig_S3_M10)

  row54 <- data.frame(
    `text reported result` = "mean_diff_RAI_fig_S3_averaged",
    value = mean_diff_RAI_fig_S3_averaged)

  row55 <- data.frame(
    `text reported result` = "pd_RAI_fig_S3_averaged",
    value = pd_RAI_fig_S3_averaged)

  #This caption is for the figure showing % increase in RAI for camera traps in the elephant zone versus outside it.
  save_caption_for_figure_S3(fit_models_location,
                             tables_location,
                             data_location,
                             figures_location,
                             ci_level = ci_level)

  row56 <- data.frame(
    `text reported result` = "n_free_listed_lion",
    value = n_free_listed_lion)

  row57 <- data.frame(
    `text reported result` = "n_free_listed_snake",
    value = n_free_listed_snake)

  row58 <- data.frame(
    `text reported result` = "n_free_listed_elephant",
    value = n_free_listed_elephant)

  row59 <- data.frame(
    `text reported result` = "n_free_listed_leopard",
    value = n_free_listed_leopard)

  text_reported_results <- bind_rows(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10,
                                     row11, row12, row13, row14, row15, row16, row17, row18, row19, row20,
                                     row21, row22, row23, row24, row25, row26, row27, row28, row29, row30,
                                     row31, row32, row33, row34, row35, row36, row37, row38, row39, row40,
                                     row41, row42, row43, row44, row45,
                                     row46, row47, row48, row49, row50,
                                     row51, row52, row53, row54, row55, row56,
                                     row57, row58, row59,
                                     row_diurnal_diff_hpdi_lower, row_diurnal_diff_hpdi_upper)
  
  output_file <- file.path(text_output_folder, "text_reported_results.csv")
  write.csv(text_reported_results, output_file, row.names = FALSE)
}


plot_and_save_figure_interview_results <- function(data_location, figures_location, height_in=3, width_in=9)
{
  a <- get_plot_average_danger_rank_by_species(data_location, figures_location)
  b <- get_plot_ever_encountered_elephant(data_location, figures_location)
  c <- get_plot_know_person_killed_by_elephant(data_location, figures_location)
  
  combined_plot <- (a + labs(tag = 'A')) | (b + labs(tag = 'B')) | (c + labs(tag = 'C'))
  
  combined_plot<- combined_plot + plot_layout(widths = c(2, 1, 1))
  
  ggsave(filename = file.path(figures_location, "interview_results_figure.tiff"), plot = combined_plot, width = width_in, height = height_in, units = "in", dpi = 300, compression = "lzw", device = "tiff")
}


plot_and_save_figure_interview_results_for_science <- function(data_location, figures_location, height_in=3, width_in=7.2)
{
  a <- get_plot_average_danger_rank_by_species_for_science(data_location, figures_location)
  b <- get_plot_ever_encountered_elephant(data_location, figures_location)
  c <- get_plot_know_person_killed_by_elephant(data_location, figures_location)

  combined_plot <- (a + labs(tag = 'A')) | (b + labs(tag = 'B')) | (c + labs(tag = 'C'))

  combined_plot<- combined_plot + plot_layout(widths = c(2, 1, 1))

  ggsave(filename = file.path(figures_location, "Fig_1_interview_results_figure_for_science.pdf"), plot = combined_plot, width = width_in, height = height_in, units = "in", device = "pdf")
}


get_plot_average_danger_rank_by_species <- function(data_location, figures_location)
{
  library(grid)
  interview_data_path <- file.path(data_location, "dangerous_animals_interview_data.csv")
  interview_data <- read.csv(interview_data_path, stringsAsFactors = F)
  species_data <- stack(interview_data[, c("dangerous_animal_1", "dangerous_animal_2", "dangerous_animal_3", "dangerous_animal_4", "dangerous_animal_5")])
  species_count <- table(species_data$values)
  species_count_df <- data.frame(species = names(species_count), count = as.numeric(species_count))
  species_count_df <- species_count_df[order(species_count_df$count, decreasing = TRUE), ]  
  species_count_df$species <- factor(species_count_df$species, levels = species_count_df$species)
  
  extended_data <- interview_data %>%
    tidyr::pivot_longer(cols = starts_with("dangerous_animal"), names_to = "rank", values_to = "species") %>%
    tidyr::complete(sex, species = unique(.$species), fill = list(rank = "dangerous_animal_6"))
  
  #compute average ranks by species
  rank_data <- extended_data %>%
    
    dplyr::mutate(
      rank_value = as.integer(factor(rank, levels = c(
        "dangerous_animal_1", "dangerous_animal_2", "dangerous_animal_3", 
        "dangerous_animal_4", "dangerous_animal_5", "dangerous_animal_6"
      )))
    ) %>%
    dplyr::group_by(species) %>%
    dplyr::summarise(
      avg_rank = mean(rank_value, na.rm = TRUE),
      # SE calculation
      se_rank  = sd(rank_value, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop" # This fixes the first warning message
    ) %>%
    # Handle cases where n=1 (SE should be NA, not 0 or NaN)
    dplyr::mutate(se_rank = ifelse(n > 1, se_rank, NA)) %>%
    dplyr::arrange(species)
  
  
  #new code that highlights elephant
  rank_data$species <- reorder(rank_data$species, rank_data$avg_rank)
  level_names <- levels(rank_data$species)
  
  fill_colors <- ifelse(level_names == "Elephant", "cornsilk4", "burlywood1")
  names(fill_colors) <- level_names
  
  font_faces <- ifelse(level_names == "Elephant", "bold", "plain")
  
  
  a <- ggplot(rank_data, aes(x = species, y = avg_rank, fill = species)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black", linewidth = 0.15) +
    geom_errorbar(aes(ymin = avg_rank - se_rank, ymax = avg_rank + se_rank),
                  color = "black", linewidth = 0.15, position = position_dodge(width = 0.7), width = 0.2) +
    scale_fill_manual(values = fill_colors) +
    labs(x = "", y = "Average rank (+/-SE)") +
    theme_classic(base_size = 6) +  
    theme(
      text = element_text(size = 6),
      axis.title = element_text(size = 6),
      axis.text = element_text(size = 6),
      axis.text.x = element_text(angle = 45, hjust = 1, face = font_faces),
      legend.position = "none",
      plot.margin = unit(c(1, 1, 1, 4), "lines") 
    ) +
    annotation_custom(
      grob = textGrob(label = "Higher\nDanger", gp = gpar(fontsize = 6, fontface = "bold")),
      xmin = -0.25, xmax = -0.25, ymin = 0, ymax = 0
    ) +
    annotation_custom(
      grob = textGrob(label = "Lower\nDanger", gp = gpar(fontsize = 6, fontface = "bold")),
      xmin = -0.25, xmax = -0.25, ymin = 4, ymax = 4
    ) +
    coord_cartesian(clip = "off")
  return(a)

}


get_plot_average_danger_rank_by_species_for_science <- function(data_location, figures_location)
{
  library(grid)
  interview_data_path <- file.path(data_location, "dangerous_animals_interview_data.csv")
  interview_data <- read.csv(interview_data_path, stringsAsFactors = F)


  species_data <- stack(interview_data[, c("dangerous_animal_1", "dangerous_animal_2", "dangerous_animal_3", "dangerous_animal_4", "dangerous_animal_5")])

  species_count <- table(species_data$values)

  species_count_df <- data.frame(species = names(species_count), count = as.numeric(species_count))
  species_count_df <- species_count_df[order(species_count_df$count, decreasing = TRUE), ]

  species_count_df$species <- factor(species_count_df$species, levels = species_count_df$species)

  extended_data <- interview_data %>%
    tidyr::pivot_longer(cols = starts_with("dangerous_animal"), names_to = "rank", values_to = "species") %>%
    tidyr::complete(sex, species = unique(.$species), fill = list(rank = "dangerous_animal_6"))

  rank_data <- extended_data %>%
    
    dplyr::mutate(
      rank_value = as.integer(factor(rank, levels = c(
        "dangerous_animal_1", "dangerous_animal_2", "dangerous_animal_3",
        "dangerous_animal_4", "dangerous_animal_5", "dangerous_animal_6"
      )))
    ) %>%
    
    dplyr::group_by(species) %>%
    dplyr::summarise(
      avg_rank = mean(rank_value, na.rm = TRUE),
      se_rank  = sd(rank_value, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop" # This fixes the first warning message
    ) %>%
    dplyr::mutate(se_rank = ifelse(n > 1, se_rank, NA)) %>%
    dplyr::arrange(species)


  
  rank_data$species <- reorder(rank_data$species, rank_data$avg_rank)
  level_names <- levels(rank_data$species)

  fill_colors <- ifelse(level_names == "Elephant", "cornsilk4", "burlywood1")
  names(fill_colors) <- level_names

  font_faces <- ifelse(level_names == "Elephant", "bold", "plain")


  a <- ggplot(rank_data, aes(x = species, y = avg_rank, fill = species)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black", linewidth = 0.15) +
    geom_errorbar(aes(ymin = avg_rank - se_rank, ymax = avg_rank + se_rank),
                  color = "black", linewidth = 0.15, position = position_dodge(width = 0.7), width = 0.2) +
    scale_fill_manual(values = fill_colors) +
    labs(x = "", y = "Average rank (+/-SE)") +
    theme_classic(base_size = 6) +
    theme(
      text = element_text(size = 6),
      axis.title = element_text(size = 6),
      axis.text = element_text(size = 6),
      axis.text.x = element_text(angle = 45, hjust = 1, face = font_faces),
      legend.position = "none",
      plot.margin = unit(c(1, 1, 1, 1.5), "lines")
    ) +
    annotation_custom(
      grob = textGrob(label = "Higher\nDanger", gp = gpar(fontsize = 6, fontface = "bold")),
      xmin = -0.25, xmax = -0.25, ymin = 0, ymax = 0
    ) +
    annotation_custom(
      grob = textGrob(label = "Lower\nDanger", gp = gpar(fontsize = 6, fontface = "bold")),
      xmin = -0.25, xmax = -0.25, ymin = 4, ymax = 4
    ) +
    coord_cartesian(clip = "off")
  return(a)

}


get_plot_ever_encountered_elephant <- function(data_location, figures_location)
{
  interview_data_path <- file.path(data_location, "dangerous_animals_interview_data.csv")
  interview_data <- read.csv(interview_data_path, stringsAsFactors = F)
  
  seen_elephant_data <- interview_data %>%
    dplyr::group_by(sex) %>%
    dplyr::summarise(prop = mean(seen_elephant == TRUE))
  
  b <- ggplot(seen_elephant_data, aes(x = sex, y = prop, fill = sex)) +
    geom_bar(stat = "identity", width = 0.5, color = "black", linewidth = 0.15) +
    scale_fill_manual(values = c("M" = "#377eb8", "F" = "#ff7f00")) +
    theme_classic(base_size = 6) +  
    theme(
      text = element_text(size = 6),
      axis.title = element_text(size = 6),
      axis.text = element_text(size = 6),
      legend.position = "none",
    ) +
    ylab("Proportion Who Have Seen Elephant") +
    xlab("Sex") +
    ylim(0, 1)
    
  return(b)
}


get_plot_know_person_killed_by_elephant <- function(data_location, figures_location)
{
  
  interview_data_path <- file.path(data_location, "dangerous_animals_interview_data.csv")
  interview_data <- read.csv(interview_data_path, stringsAsFactors = F)
  
  
  seen_person_data <- interview_data %>%
    dplyr::group_by(sex) %>%
    dplyr::summarise(prop = mean(seen_person_who_died_from_elephant == TRUE))
  
  c <- ggplot(seen_person_data, aes(x = sex, y = prop, fill = sex)) +
    geom_bar(stat = "identity", width = 0.5, color = "black", linewidth = 0.15) +
    scale_fill_manual(values = c("M" = "#377eb8", "F" = "#ff7f00")) +
    theme_classic(base_size = 6) +  
    theme(
      text = element_text(size = 6),
      axis.title = element_text(size = 6),
      axis.text = element_text(size = 6),
      legend.position = "none",
    ) +
    ylab("Proportion Know Someone Killed by Elephant") +
    xlab("Sex") +
    ylim(0, 1)
  return(c) 
}


# Panels A (camera grid / elephant-distance map), B (female tracks), and C (male tracks)
# are pre-rendered TIFFs in data_public/figure2_panels/, not
# built from source geometry here. This just reads and concatenates them.
plot_and_save_camera_grid_schema_and_GPS_tracks <- function(data_location, figures_location, height_in=3, width_in=9, figure_name="camera_grid_schema_and_GPS_tracks.tiff")
{
  panel_dir <- file.path(data_location, "figure2_panels")
  a <- magick::image_read(file.path(panel_dir, "figure2_panelA_camera_grid_map.tiff"))
  b <- magick::image_read(file.path(panel_dir, "figure2_panelB_female_tracks.tiff"))
  c <- magick::image_read(file.path(panel_dir, "figure2_panelC_male_tracks.tiff"))

  combined <- magick::image_append(c(a, b, c))
  combined <- magick::image_background(combined, "white")

  magick::image_write(combined, path = file.path(figures_location, figure_name),
                       format = "tiff", compression = "LZW")
}


save_table_S2_PSA_model_comparison_rotation <- function(fit_models_location, tables_location) {

  # Load posterior samples saved by run_path_selection_model (rotation method)

  #model 3 in the paper
  s_EE <- readRDS(paste0(fit_models_location, "s_Elephants_Environment_rot.rds"))
  
  #model 1 in the paper
  s_En <- readRDS(paste0(fit_models_location, "s_Environment_rot.rds"))
  
  #model 2 in the paper
  s_E  <- readRDS(paste0(fit_models_location, "s_Elephants_rot.rds"))
  
  # Compute LOO for each model
  loo_EE <- loo(s_EE$log_lik)
  loo_En <- loo(s_En$log_lik)
  loo_E  <- loo(s_E$log_lik)

  
  model_list <- list(
    M2 = loo_E,
    M1 = loo_En,
    M3 = loo_EE
  )

  comp <- loo_compare(model_list)

  # Pseudo-BMA and stacking weights
  pbma_weights  <- loo_model_weights(model_list, method = "pseudobma")
  stack_weights <- loo_model_weights(model_list, method = "stacking")

  
  predictor_map <- c(
    M2 = "Elephant Distance",
    M1 = "Ruggedness, Road Distance, River Distance",
    M3 = "Ruggedness, Elephant Distance, Road Distance, River Distance"
  )
  model_num_map <- c(M1 = "1", M2 = "2", M3 = "3")

  row_order <- rownames(comp)  # best-to-worst order

  table_df <- data.frame(
    `Model`                 = model_num_map[row_order],
    `Predictor Variables`   = predictor_map[row_order],
    `Delta ELPD`            = round(comp[row_order, "elpd_diff"], 1),
    `SE Delta ELPD`         = round(comp[row_order, "se_diff"], 1),
    `LOO Pseudo-BMA Weight` = round(as.numeric(pbma_weights[row_order]), 3),
    `LOO Stacking Weight`   = round(as.numeric(stack_weights[row_order]), 3),
    check.names             = FALSE,
    row.names               = NULL
  )

  write.csv(table_df,
            file      = file.path(tables_location, "Table_S2_PSA_model_comparison.csv"),
            row.names = FALSE)

  return(invisible(table_df))
}


build_RAI_station_prediction_grid <- function(data_location) {

  library(dplyr)
  library(tidyr)

  detections_data_with_locations <- read.csv(paste0(data_location, "RAI_analysis_data_with_locations.csv"))

  station_data <- detections_data_with_locations %>%
    dplyr::select(Trap.Station.Name, dist_from_camp_sc, canopy_cover_mean_1000m_buffer) %>%
    distinct(Trap.Station.Name, .keep_all = TRUE)

  station_data %>%
    tidyr::crossing(Elephant_Presence = c("Elephant", "No elephant")) %>%
    mutate(Nights = 100, log_Nights = log(100))
}


posterior_predict_elephant_presence_RAI_model_averaging_zi_species_varying <- function(fit_models_location,
                                                                      tables_location,
                                                                      data_location,
                                                                      ci_level = ci_level) {

  library(brms)
  library(dplyr)
  library(tidyr)

  fit_m8  <- readRDS(paste0(fit_models_location, "model_8.rds"))
  fit_m9  <- readRDS(paste0(fit_models_location, "model_9.rds"))
  fit_m10 <- readRDS(paste0(fit_models_location, "model_10.rds"))

  fit_list <- list(M8 = fit_m8, M9 = fit_m9, M10 = fit_m10)

  comp_table    <- read.csv(paste0(tables_location, "table_S3_RAI_model_comparison_zero_inflated_species_varying.csv"), check.names = FALSE)
  stack_weights <- setNames(comp_table[["LOO Stacking Weight"]], paste0("M", comp_table$Model))
  stack_weights <- stack_weights[names(fit_list)]  # reorder to M8, M9, M10; NA if a model is missing
  stopifnot(!anyNA(stack_weights))

  # ---- One row per real trap station, with its actual covariate values,
  # crossed with both Elephant_Presence scenarios, standardized to 100
  # trap-nights so predictions are RAI

  newdata <- build_RAI_station_prediction_grid(data_location)

  # Posterior expected predictions (response scale = expected detections
  # per 100 nights, i.e. RAI), at each station's real covariates, for "average
  # ungulates".
  epred_list <- lapply(fit_list, function(fit) {
    posterior_epred(fit, newdata = newdata, re_formula = NA)
  })

  n_draws <- unique(vapply(epred_list, nrow, integer(1)))
  stopifnot(length(n_draws) == 1)  

  # Model averaging via stacking
  set.seed(123)
  model_draw <- sample(names(fit_list), size = n_draws, replace = TRUE, prob = stack_weights)

  epred_stack <- matrix(NA_real_, nrow = n_draws, ncol = nrow(newdata))
  for (m in names(fit_list)) {
    rows <- which(model_draw == m)
    epred_stack[rows, ] <- epred_list[[m]][rows, , drop = FALSE]
  }

  elephant_cols    <- which(newdata$Elephant_Presence == "Elephant")
  no_elephant_cols <- which(newdata$Elephant_Presence == "No elephant")

  # Average across stations within each posterior draw first (correctly
  # propagates uncertainty and avoids Jensen's-inequality bias from
  # averaging after summarizing), giving one model-averaged RAI draw per
  # scenario, then compute the elephant-presence effect on both scales.
  draws_elephant    <- rowMeans(epred_stack[, elephant_cols])
  draws_no_elephant <- rowMeans(epred_stack[, no_elephant_cols])
  draws_difference  <- draws_elephant - draws_no_elephant
  draws_ratio       <- draws_elephant / draws_no_elephant

  # Probability of direction: share of model-averaged posterior draws in
  # which Elephant_Presence == "Elephant" predicts higher RAI than "No
  # elephant" (i.e. a positive effect on ungulate abundance). Computed
  # directly as P(difference > 0), equivalently P(ratio > 1) since both
  # are derived from the same draws and share sign/direction row-for-row.
  prob_direction_positive <- mean(draws_difference > 0)

  summary_df <- data.frame(
    quantity = c("RAI: Elephant", "RAI: No Elephant",
                 "Difference (Elephant - No Elephant)",
                 "Ratio (Elephant / No Elephant)"),
    mean  = c(mean(draws_elephant), mean(draws_no_elephant),
              mean(draws_difference), mean(draws_ratio)),
    lower = c(HDInterval::hdi(draws_elephant,    credMass = ci_level)["lower"],
              HDInterval::hdi(draws_no_elephant, credMass = ci_level)["lower"],
              HDInterval::hdi(draws_difference,  credMass = ci_level)["lower"],
              HDInterval::hdi(draws_ratio,       credMass = ci_level)["lower"]),
    upper = c(HDInterval::hdi(draws_elephant,    credMass = ci_level)["upper"],
              HDInterval::hdi(draws_no_elephant, credMass = ci_level)["upper"],
              HDInterval::hdi(draws_difference,  credMass = ci_level)["upper"],
              HDInterval::hdi(draws_ratio,       credMass = ci_level)["upper"]),
    prob_direction_positive = c(NA, NA, prob_direction_positive, prob_direction_positive),
    row.names = NULL
  )

  summary_list <- list(
    draws = list(
      elephant           = draws_elephant,
      no_elephant        = draws_no_elephant,
      difference         = draws_difference,
      ratio              = draws_ratio,
      model_draw_source  = model_draw
    ),
    stacking_weights         = stack_weights,
    prob_direction_positive  = prob_direction_positive,
    summary                  = summary_df
  )

  return(summary_list)
}


# Percent change in average detections per 100 nights (Elephant vs No Elephant),
# for one model's own posterior only (no stacking), averaged across stations within each
# posterior draw before taking the ratio. Shared building block behind each percent-change
# density panel below, and behind get_plot_RAI_pct_diff_model_10() (Figure 5 Panel C).
get_RAI_pct_diff_draws <- function(fit, data_location) {
  newdata <- build_RAI_station_prediction_grid(data_location)
  elephant_cols    <- which(newdata$Elephant_Presence == "Elephant")
  no_elephant_cols <- which(newdata$Elephant_Presence == "No elephant")

  epred <- posterior_epred(fit, newdata = newdata, re_formula = NA)
  (rowMeans(epred[, elephant_cols]) / rowMeans(epred[, no_elephant_cols]) - 1) * 100
}

# Panel builder, matching plot_and_save_fig_S3_RAI_diff_distribution
make_density_panel_pct_diff <- function(diff_draws, x_label, x_limits, x_breaks, x_break_labels) {

  mean_diff <- mean(diff_draws)
  dens      <- density(diff_draws, n = 2048)
  dens_df   <- data.frame(x = dens$x, y = dens$y)
  dens_df <- dens_df[dens_df$x >= x_limits[1] & dens_df$x <= x_limits[2], ]
  dens_pos <- dens_df %>% filter(x >= 0)
  dens_neg <- dens_df %>% filter(x <= 0)
  dens_pos <- bind_rows(data.frame(x = 0, y = 0), dens_pos, data.frame(x = x_limits[2], y = 0))
  dens_neg <- bind_rows(data.frame(x = x_limits[1], y = 0), dens_neg, data.frame(x = 0, y = 0))

  ggplot() +

    geom_polygon(
      data  = dens_neg,
      aes(x = x, y = y),
      fill  = "#B35A38",
      alpha = 0.5,
      color = NA
    ) +

    geom_polygon(
      data  = dens_pos,
      aes(x = x, y = y),
      fill  = "#31688E",
      alpha = 0.5,
      color = NA
    ) +

    geom_line(
      data      = dens_df,
      aes(x = x, y = y),
      color     = "#222222",
      linewidth = 0.35
    ) +

    geom_vline(xintercept = 0, linetype = "dashed", color = "#444444", linewidth = 0.35) +
    geom_vline(xintercept = mean_diff, linetype = "solid", color = "black", linewidth = 0.4) +

    scale_x_continuous(
      name   = x_label,
      breaks = x_breaks,
      labels = x_break_labels,
      limits = x_limits
    ) +
    scale_y_continuous(
      name   = "Posterior density",
      expand = expansion(mult = c(0, 0.05))
    ) +

    theme_classic(base_size = 7, base_family = "Helvetica") +
    theme(
      axis.title  = element_text(size = 7, face = "plain", color = "black"),
      axis.text   = element_text(size = 6, color = "black"),
      axis.line   = element_line(linewidth = 0.3, color = "black"),
      axis.ticks  = element_line(linewidth = 0.3, color = "black"),
      plot.margin = margin(5, 5, 5, 5, "pt")
    )
}

# Figure 5 Panel C: percent change in RAI (Elephant vs No Elephant) under Model 10 alone
# (Elephant Presence, Distance from Camp, Tree Canopy Cover). Same x-axis window/breaks as
# plot_and_save_fig_S3_RAI_percent_diff_distribution() by default so the panel matches the
# version shown there.
get_plot_RAI_pct_diff_model_10 <- function(data_location, fit_models_location,
                                            x_limits = c(-150, 1000),
                                            x_breaks = seq(-200, 1000, by = 100),
                                            x_break_labels = NULL) {

  if (is.null(x_break_labels)) {
    x_break_labels <- ifelse(x_breaks %in% c(-200, 0, 200, 400, 600, 800), x_breaks, "")
  }

  fit_m10 <- readRDS(paste0(fit_models_location, "model_10.rds"))
  pct_diff_m10 <- get_RAI_pct_diff_draws(fit_m10, data_location)

  make_density_panel_pct_diff(
    pct_diff_m10,
    "% Change in Average Detections per 100 nights:\nElephant vs No Elephant (Model 10)",
    x_limits, x_breaks, x_break_labels
  )
}

plot_and_save_fig_S3_RAI_percent_diff_distribution <- function(fit_models_location,
                                                                                  tables_location,
                                                                                  data_location,
                                                                                  figures_location,
                                                                                  ci_level    = ci_level,
                                                                                  figure_name = "figure_S3_percent_diff") {

  library(brms)
  library(dplyr)
  library(ggplot2)
  library(patchwork)

  # Model-averaged percent-change draws for Panel C
  model_averaging_results <- posterior_predict_elephant_presence_RAI_model_averaging_zi_species_varying(
    fit_models_location = fit_models_location,
    tables_location     = tables_location,
    data_location       = data_location,
    ci_level            = ci_level
  )
  pct_diff_model_averaged <- (model_averaging_results$draws$ratio - 1) * 100

  # Panels A and B: each model's own posterior only (no stacking)
  fit_m8  <- readRDS(paste0(fit_models_location, "model_8.rds"))
  fit_m10 <- readRDS(paste0(fit_models_location, "model_10.rds"))

  pct_diff_m8  <- get_RAI_pct_diff_draws(fit_m8,  data_location)
  pct_diff_m10 <- get_RAI_pct_diff_draws(fit_m10, data_location)

  # Shared x-axis range/breaks so the three panels are visually comparable
  x_limits       <- c(-150, 1000)
  x_breaks       <- seq(-200, 1000, by = 100)
  x_break_labels <- ifelse(x_breaks %in% c(-200, 0, 200, 400, 600, 800), x_breaks, "")

  p_A <- make_density_panel_pct_diff(pct_diff_m8,             "% Change in Average Detections per 100 nights:\nElephant vs No Elephant (Model 8)",             x_limits, x_breaks, x_break_labels)
  p_B <- make_density_panel_pct_diff(pct_diff_m10,            "% Change in Average Detections per 100 nights:\nElephant vs No Elephant (Model 10)",            x_limits, x_breaks, x_break_labels)
  p_C <- make_density_panel_pct_diff(pct_diff_model_averaged, "% Change in Average Detections per 100 nights:\nElephant vs No Elephant (Model Averaged)", x_limits, x_breaks, x_break_labels)

  combined_plot <- p_A + p_B + p_C +
    plot_layout(nrow = 1) +
    plot_annotation(tag_levels = "A")

  ggsave(
    filename = paste0(figures_location, figure_name, ".pdf"),
    plot     = combined_plot,
    width    = 7.5,
    height   = 2.2,
    units    = "in",
    device   = "pdf"
  )

  return(invisible(combined_plot))
}


save_caption_for_figure_S3 <- function(fit_models_location,
                                        tables_location,
                                        data_location,
                                        figures_location,
                                        ci_level = ci_level) {


  model_averaging_results <- posterior_predict_elephant_presence_RAI_model_averaging_zi_species_varying(
    fit_models_location = fit_models_location,
    tables_location     = tables_location,
    data_location       = data_location,
    ci_level            = ci_level
  )

  # Same transformation used for Panel C in
  # plot_and_save_fig_S3_RAI_percent_diff_distribution
  pct_diff_model_averaged <- (model_averaging_results$draws$ratio - 1) * 100

  mean_pct  <- round(mean(pct_diff_model_averaged), 1)
  hpdi_pct  <- HDInterval::hdi(pct_diff_model_averaged, credMass = ci_level)
  lower_pct <- round(hpdi_pct["lower"], 1)
  upper_pct <- round(hpdi_pct["upper"], 1)

  # P(percent change > 0) == P(ratio > 1) == P(difference > 0)
  pd <- round(model_averaging_results$prob_direction_positive, 2)

  ci_pct <- round(ci_level * 100)

  caption_text <- paste0(
    "Figure S3. Posterior distributions of the percent change in ungulate prey relative abundance index (RAI) ",
    "associated with elephant presence, expressed as percent change in average detections per 100 camera-trap ",
    "nights (Elephant zone vs. No elephant zone). Panel A shows Model 8 (Elephant Presence only). Panel B shows ",
    "Model 10 (Elephant Presence, Distance from Camp, and Tree Canopy Cover). Panel C shows the LOO-stacking-",
    "weighted model average across Models 8-10 (Table S3), representing the best overall estimate of the ",
    "elephant-presence effect. In each panel, the dashed vertical line marks zero change (no difference between ",
    "zones) and the solid vertical line marks the posterior mean. Blue-shaded posterior mass corresponds to a ",
    "positive percent change in RAI (higher relative abundance in the elephant zone), and reddish-shaded posterior ",
    "mass corresponds to a negative percent change.\n\n",
    "Based on the model-averaged posterior (Panel C), ungulate RAI was on average ", mean_pct, "% higher in the ",
    "elephant zone than in the non-elephant zone (", ci_pct, "% HPDI: ", lower_pct, "% to ", upper_pct, "%), with a ",
    "probability of direction of ", pd, " (i.e., ", round(pd * 100), "% of the posterior mass supported a positive ",
    "effect of elephant presence on RAI)."
  )

  writeLines(caption_text, file.path(figures_location, "Figure S3 Caption.txt"))

  return(invisible(caption_text))
}


# NHB submission change: Figure 4 gained a third panel (posterior difference distribution,
# also used standalone as Supplementary Figure S2). The original two-panel version is kept
# below, commented out, since other callers (e.g. the Science-formatted variant) still use
# the two-panel layout. save_figures() now calls
# plot_and_save_empirical_and_model_predicted_diurnality_NHB() instead of this one.
# plot_and_save_empirical_and_model_predicted_diurnality <- function(figures_location, data, model, ci_level, figure_name) {
#
#   p_empirical <- get_plot_empirical_diurnality_by_species_and_elephant_zone(
#     data  = data,
#     figures_location = figures_location
#   )
#
#   p_model <- get_plot_posterior_diurnality_predictions(
#     model=model,
#     figures_location = figures_location,
#     ci_level = ci_level
#   )
#
#   library(patchwork)
#
#   combined_plot <- p_empirical + p_model +
#     plot_layout(widths = c(2, 1)) +
#     plot_annotation(tag_levels = "A")
#
#   ggsave(
#     filename    = paste0(figures_location, paste0(figure_name, ".tiff")),
#     plot        = combined_plot,
#     compression = "lzw",
#     device      = "tiff",
#     width       = 6, height = 3, dpi = 300
#   )
# }

plot_and_save_empirical_and_model_predicted_diurnality_NHB <- function(figures_location, data, model, ci_level, figure_name) {

  p_empirical <- get_plot_empirical_diurnality_by_species_and_elephant_zone(
    data  = data,
    figures_location = figures_location
  )

  p_model <- get_plot_posterior_diurnality_predictions(
    model=model,
    figures_location = figures_location,
    ci_level = ci_level
  )

  # Panel C: posterior difference distribution, also used standalone as Supplementary Figure S2
  p_diff <- get_plot_diurnality_diff_distribution(model, ci_level)

  library(patchwork)

  combined_plot <- p_empirical + p_model + p_diff +
    plot_layout(widths = c(2, 1, 1.3)) +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(face = "bold"))

  ggsave(
    filename    = paste0(figures_location, paste0(figure_name, ".tiff")),
    plot        = combined_plot,
    compression = "lzw",
    device      = "tiff",
    width       = 9, height = 3, dpi = 300
  )
}

plot_and_save_empirical_and_model_predicted_diurnality_for_science <- function(figures_location, data, model, ci_level, figure_name) {

  p_empirical <- get_plot_empirical_diurnality_by_species_and_elephant_zone(
    data  = data,
    figures_location = figures_location
  )

  p_model <- get_plot_posterior_diurnality_predictions(
    model=model,
    figures_location = figures_location,
    ci_level = ci_level
  )

  library(patchwork)

  combined_plot <- p_empirical + p_model +
    plot_layout(widths = c(2, 1)) +
    plot_annotation(tag_levels = "A")

  ggsave(
    filename    = paste0(figures_location, paste0(figure_name, "_for_science.pdf")),
    plot        = combined_plot,
    device      = "pdf",
    width       = 6, height = 3
  )
}


get_plot_empirical_diurnality_by_species_and_elephant_zone <- function(data, figures_location) {
  
  library(dplyr)
  library(ggplot2)
  
  d <- data
  
  length(unique(d$species))
  # Coerce logicals to a labeled factor for plotting
  d <- d %>%
    mutate(
      elephant_zone = factor(
        ifelse(elephant_zone, "Elephant", "No elephant"),
        levels = c("Elephant", "No elephant")
      )
    )
  
  # Compute empirical probability of nocturnal detection per species × zone,
  # plus ±1 standard error of the proportion: SE = sqrt(p(1-p)/n)
  summary_df <- d %>%
    group_by(species, elephant_zone) %>%
    summarise(
      n_detections   = dplyr::n(),
      n_diurnal    = sum(diurnal),
      prob_diurnal = n_diurnal / n_detections,
      se             = sqrt(prob_diurnal * (1 - prob_diurnal) / n_detections),
      ci_lower       = pmax(0, prob_diurnal - se),
      ci_upper       = pmin(1, prob_diurnal + se),
      .groups = "drop"
    )
  
  # Order species by total detections (descending), as in the template
  species_order <- d %>%
    count(species, sort = TRUE) %>%
    pull(species)
  
  summary_df$species <- factor(summary_df$species, levels = species_order)
  
  dodge_offset <- 0.22   # half the spacing between dodged bars
  bar_width    <- 0.4    # must be ≤ 2 * dodge_offset to prevent overlap
  
  summary_df <- summary_df %>%
    mutate(
      x_num = as.numeric(species) +
        ifelse(elephant_zone == "Elephant", -dodge_offset, dodge_offset)
    )
  
  p <- ggplot(summary_df, aes(x = x_num,
                              y = prob_diurnal,
                              fill = elephant_zone)) +
    
    geom_col(width = bar_width, color = "darkgrey", linewidth = 0.15) +

    geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.1,
      linewidth = 0.3
    ) +
    
    scale_x_continuous(
      breaks = seq_along(levels(summary_df$species)),
      labels = levels(summary_df$species)
    ) +
    
    scale_fill_manual(values = c("Elephant" = "cornsilk4",
                                 "No elephant" = "burlywood1")) +
    
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25),
      expand = expansion(mult = c(0, 0.02))
    ) +
    
    labs(
      x = "Species",
      y = "Probability of daytime detection",
      fill = "Elephant Presence"
    ) +
    
    theme_classic(base_size = 6) +
    theme(
      text         = element_text(size = 6),
      axis.title   = element_text(size = 6, face = "bold", color = "black"),
      axis.text    = element_text(size = 6, face = "bold", color = "black"),
      axis.text.x  = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
      legend.position      = c(1, 1), # nudged ~1mm right from c(0.99, 0.95); nudged up slightly from y = 0.963
      legend.justification = c("right", "top"),
      legend.background    = element_blank(),
      legend.key           = element_blank(),
      legend.key.size      = unit(0.4, "cm"),
      legend.title         = element_text(size = 6, face = "bold", color = "black"),
      legend.text          = element_text(size = 6, face = "bold", color = "black")
    )

  return(p)
 
}


get_plot_posterior_diurnality_predictions <- function(model, figures_location, ci_level) {
  
  library(brms)
  library(tidybayes)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  fit_diurnal <- model
  
  species_list <- unique(fit_diurnal$data$species)
  
  # Build a prediction grid: all species x both elephant zones
  pred_grid <- expand.grid(
    species       = species_list,
    elephant_zone = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  
  # Draw posterior samples integrating over both fixed and species-level
  # varying effects (intercepts and slopes) for each species x zone combination
  posterior_epred <- add_epred_draws(
    newdata          = pred_grid,
    object           = fit_diurnal,
    allow_new_levels = FALSE
  )
  
  # Average P(diurnal) across species within each posterior draw, then
  # build a summary over the varying effects correctly and
  # avoids Jensen's inequality bias from averaging on the logit scale
  hpdi_average <- posterior_epred %>%
    group_by(elephant_zone, .draw) %>%
    summarise(mean_diurnal_draw = mean(.epred), .groups = "drop") %>% # Directly using .epred now
    group_by(elephant_zone) %>%
    summarise(
      Mean_Prob  = mean(mean_diurnal_draw),
      Lower_HPDI = HDInterval::hdi(mean_diurnal_draw, credMass = ci_level)["lower"],
      Upper_HPDI = HDInterval::hdi(mean_diurnal_draw, credMass = ci_level)["upper"],
      .groups    = "drop"
    ) %>%
    mutate(
      Zone  = factor(ifelse(elephant_zone, "Elephant", "No elephant"),
                      levels = c("Elephant", "No elephant")),
      x_val = ifelse(elephant_zone, 1, 2)
    )

  fig_predictions <- ggplot(
    hpdi_average,
    aes(x = x_val, y = Mean_Prob, color = Zone, fill = Zone)
  ) +
   
    geom_linerange(
      aes(ymin = Lower_HPDI, ymax = Upper_HPDI),
      color     = "black",
      linewidth = 1.4,
      lineend   = "round"
    ) +
    geom_linerange(
      aes(ymin = Lower_HPDI, ymax = Upper_HPDI),
      linewidth = 0.6,
      lineend   = "round"
    ) +
    geom_point(
      shape  = 21,
      size   = 3.6,
      stroke = 0.6,
      color  = "black"
    ) +
    scale_color_manual(values = c("Elephant" = "cornsilk4", "No elephant" = "burlywood1")) +
    scale_fill_manual(values = c("Elephant" = "cornsilk4", "No elephant" = "burlywood1")) +
    scale_x_continuous(
      name   = "Elephant Presence",
      breaks = c(1, 2),
      labels = c("Elephant", "No elephant"),
      limits = c(0.5, 2.5)
    ) +
    scale_y_continuous(
      name   = "Probability of daytime detection",
      limits = c(0.25, 0.75),
      breaks = seq(0, 1, 0.25)
    ) +
    theme_classic(base_size = 6) +
    theme(
      text            = element_text(size = 6),
      axis.title      = element_text(size = 6, face = "bold", color = "black"),
      axis.text       = element_text(size = 6, face = "bold", color = "black"),
      legend.position = "none"
    )

  return(fig_predictions)
}


# Builds the posterior difference-in-diurnality density panel. Used standalone as
# Supplementary Figure S2 (plot_and_save_supp_figure_of_diurnality_diff_distribution() below)
# and as Panel C of Figure 4 (plot_and_save_empirical_and_model_predicted_diurnality_NHB()).
get_plot_diurnality_diff_distribution <- function(model, ci_level) {

  library(brms)
  library(tidybayes)
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  # Build a prediction grid: all species x both elephant zones, then draw posterior
  # samples that integrate over both fixed and species-level varying effects. This is the
  # same approach used in get_plot_posterior_diurnality_predictions(), and in
  # the "difference in diurnality between zones" section of save_text_reported_results().
  species_list <- unique(model$data$species)
  pred_grid <- expand.grid(
    species       = species_list,
    elephant_zone = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  posterior_epred <- add_epred_draws(
    newdata          = pred_grid,
    object           = model,
    allow_new_levels = FALSE
  )

  # Average P(diurnal) across species within each posterior draw first. This marginalizes over the species-level varying effects,
  # then take the elephant-zone difference for that draw.
  diff_draws <- posterior_epred %>%
    group_by(elephant_zone, .draw) %>%
    summarise(mean_diurnal_draw = mean(.epred), .groups = "drop") %>%
    pivot_wider(names_from = elephant_zone, values_from = mean_diurnal_draw) %>%
    rename(no_elephant = `FALSE`, elephant = `TRUE`) %>%
    mutate(difference = elephant - no_elephant)

  mean_diff <- mean(diff_draws$difference)
  dens      <- density(diff_draws$difference, n = 2048)
  dens_df   <- data.frame(x = dens$x, y = dens$y)
  dens_pos  <- dens_df %>% filter(x >= 0)
  dens_neg  <- dens_df %>% filter(x <= 0)

  dens_pos <- bind_rows(data.frame(x = 0, y = 0), dens_pos, data.frame(x = max(dens_pos$x), y = 0))
  dens_neg <- bind_rows(data.frame(x = min(dens_neg$x), y = 0), dens_neg, data.frame(x = 0, y = 0))

  p <- ggplot() +

    geom_polygon(
      data  = dens_neg,
      aes(x = x, y = y),
      fill  = "#B35A38",
      alpha = 0.5,
      color = NA
    ) +

    geom_polygon(
      data  = dens_pos,
      aes(x = x, y = y),
      fill  = "#31688E",
      alpha = 0.5,
      color = NA
    ) +

    geom_line(
      data      = dens_df,
      aes(x = x, y = y),
      color     = "#222222",
      linewidth = 0.35
    ) +

    geom_vline(xintercept = 0, linetype = "dashed", color = "#444444", linewidth = 0.35) +
    geom_vline(xintercept = mean_diff, linetype = "solid", color = "black", linewidth = 0.4) +

    scale_x_continuous(name = "Difference in P(diurnal): Elephant - No elephant") +
    scale_y_continuous(
      name   = "Posterior density",
      expand = expansion(mult = c(0, 0.05))
    ) +

    theme_classic(base_size = 7, base_family = "Helvetica") +
    theme(
      axis.title  = element_text(size = 7, face = "plain", color = "black"),
      axis.text   = element_text(size = 6, color = "black"),
      axis.line   = element_line(linewidth = 0.3, color = "black"),
      axis.ticks  = element_line(linewidth = 0.3, color = "black"),
      plot.margin = margin(5, 5, 5, 5, "pt")
    )

  return(p)
}

plot_and_save_supp_figure_of_diurnality_diff_distribution <- function(figures_location, model, figure_name) {

  p <- get_plot_diurnality_diff_distribution(model, ci_level)

  ggsave(
    filename = paste0(figures_location, figure_name, ".pdf"),
    plot     = p,
    width    = 3.2,
    height   = 2.2,
    units    = "in",
    device   = "pdf"
  )

  return(invisible(p))
}


calc_and_save_summary_table_diurnality_analysis <- function(data_location, tables_location) {
  
  nocturnality_data_30_min_filter <- read.csv(file.path(data_location, "camera_trap_data_for_diurnality_analysis.csv"))
  
  summary_table <- nocturnality_data_30_min_filter %>%
    group_by(species, elephant_zone, nocturnal) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = nocturnal,
      values_from = n,
      names_prefix = "nocturnal_",
      values_fill = 0  # Fill missing values with 0
    ) %>%
    mutate(
      total = nocturnal_FALSE + nocturnal_TRUE,
      percent_diurnal = round(100 * nocturnal_FALSE / total, 1)  # Calculate and round percent diurnal
    ) %>%
    group_by(species) %>%
    mutate(species_total = sum(total)) %>%
    ungroup() %>%
    arrange(desc(species_total), species, elephant_zone) 

  
  #There were no Eland or Plains zebra detected in the condition elephant_zone==TRUE, and
  #I will now add those rows to the table to explicitly show this fact. 
  eland_row <- data.frame(species="Eland", elephant_zone=TRUE, nocturnal_FALSE=0, nocturnal_TRUE=0, total=0, percent_diurnal=NA, species_total=summary_table$species_total[summary_table$species=="Eland"])
  zebra_row <- data.frame(species="Plains zebra", elephant_zone=TRUE, nocturnal_FALSE=0, nocturnal_TRUE=0, total=0, percent_diurnal=NA, species_total=summary_table$species_total[summary_table$species=="Plains zebra"])
  summary_table <- rbind(summary_table, eland_row, zebra_row)
  summary_table <- summary_table %>% arrange(desc(species_total), species, elephant_zone)

  summary_table <- summary_table %>% rename(night_detections = nocturnal_TRUE)
  summary_table <- summary_table %>% rename(day_detections = nocturnal_FALSE)
  
  # formatting to recode elephant_zone to Inside/Outside and drop species_total
  summary_table <- summary_table %>% 
    mutate(elephant_zone = if_else(elephant_zone, "Inside", "Outside")) %>%
    dplyr::select(-species_total)
  
  write.csv(summary_table, paste0(tables_location, "table_S1_diurnality_analysis_summary_table.csv"), row.names = F)
}


# NHB submission change: Figure 5 gained a third panel (percent change in RAI under Model
# 10, also used standalone as Supplementary Figure S3 Panel B). The original two-panel
# version is kept below, commented out, since other callers (e.g. the Science-formatted
# variant) still use the two-panel layout. save_figures() now calls
# plot_and_save_RAI_by_species_and_elephant_zone_empirical_and_model_predictions_NHB()
# instead of this one.
# plot_and_save_RAI_by_species_and_elephant_zone_empirical_and_model_predictions <- function(figure_name, data_location, fit_models_location,
#                                                                                            figures_location,
#                                                                                            width_in, height_in,
#                                                                                            show_outliers)
# {
#   p_empirical <- get_plot_empirical_RAI_by_species_and_elephant_zone(data_location, show_outliers = show_outliers)
#   p_model_predictions <- get_plot_elephant_effects_m10(data_location, fit_models_location, ci_level)
#
#   library(patchwork)
#   rai_combined_plot <- (p_empirical + labs(tag = 'A')) | (p_model_predictions + labs(tag = 'B'))
#
#   rai_combined_plot<- rai_combined_plot + plot_layout(widths = c(2, 1))
#
#   ggsave(filename = file.path(figures_location, paste0(figure_name, ".tiff")), plot = rai_combined_plot, width = width_in, height = height_in, units = "in", dpi = 300, compression = "lzw", device = "tiff")
#
#
# }

plot_and_save_RAI_by_species_and_elephant_zone_empirical_and_model_predictions_NHB <- function(figure_name, data_location, fit_models_location,
                                                                                           figures_location,
                                                                                           width_in, height_in,
                                                                                           show_outliers)
{
  p_empirical <- get_plot_empirical_RAI_by_species_and_elephant_zone(data_location, show_outliers = show_outliers)
  p_model_predictions <- get_plot_elephant_effects_m10(data_location, fit_models_location, ci_level)

  # Panel C: percent change in RAI under Model 10, also used standalone as Supplementary
  # Figure S3 Panel B
  p_pct_diff <- get_plot_RAI_pct_diff_model_10(data_location, fit_models_location)

  library(patchwork)
  rai_combined_plot <- (p_empirical + labs(tag = 'A')) | (p_model_predictions + labs(tag = 'B')) | (p_pct_diff + labs(tag = 'C'))

  # Apply the layout, giving Panel C (longer axis title) a bit more width
  rai_combined_plot<- rai_combined_plot + plot_layout(widths = c(2, 1, 1.3)) &
    theme(plot.tag = element_text(face = "bold"))

  ggsave(filename = file.path(figures_location, paste0(figure_name, ".tiff")), plot = rai_combined_plot, width = width_in, height = height_in, units = "in", dpi = 300, compression = "lzw", device = "tiff")

}


plot_and_save_RAI_by_species_and_elephant_zone_empirical_and_model_predictions_for_science <- function(figure_name, data_location, fit_models_location,
                                                                                                         figures_location,
                                                                                                         width_in=6, height_in=3,
                                                                                                         show_outliers)
{
  p_empirical <- get_plot_empirical_RAI_by_species_and_elephant_zone(data_location, show_outliers = show_outliers)
  p_model_predictions <- get_plot_elephant_effects_m10(data_location, fit_models_location, ci_level)

  library(patchwork)
  rai_combined_plot <- (p_empirical + labs(tag = 'A')) | (p_model_predictions + labs(tag = 'B'))

  rai_combined_plot<- rai_combined_plot + plot_layout(widths = c(2, 1))

  ggsave(filename = file.path(figures_location, paste0(figure_name, ".pdf")), plot = rai_combined_plot, width = width_in, height = height_in, units = "in", device = "pdf")
}


get_plot_empirical_RAI_by_species_and_elephant_zone <- function(data_location, show_outliers) {
  
  detections_data <- read.csv(paste0(data_location, "RAI_analysis_data_with_locations.csv"))
  n_impala <- sum(detections_data$Total_Detections[detections_data$species_lower_case == "Impala"])
  
  if (show_outliers) {
    boxplot_layer <- geom_boxplot()
  } else {
    boxplot_layer <- geom_boxplot(outlier.shape = NA)
  }
  
  p <- ggplot(detections_data, aes(x = reorder(species_lower_case, Total_Detections, FUN = sum, decreasing = TRUE), 
                                   y = RAI, 
                                   fill = Elephant_Presence)) +
    boxplot_layer + 

    scale_fill_manual(values = c("Elephant" = "cornsilk4", "No elephant" = "burlywood1")) +
    
    labs(
      x = "Species",
      y = "Detections per 100 Nights",
      fill = "Elephant Presence"
    ) +
    
    theme_classic(base_size = 6) +
    theme(
      text = element_text(size = 6, face = "bold", color = "black"),
      axis.title = element_text(size = 6, face = "bold", color = "black"),
      axis.text = element_text(size = 6, face = "bold", color = "black"),

      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
      
      legend.position = c(1, 1),

      legend.justification = c("right", "top"),

      legend.background = element_rect(fill = alpha("white", 0.7), color = "white", linewidth = 0.2),
      legend.key.size = unit(0.4, "cm"), # Scale down the legend keys for the small font
      legend.title = element_text(size = 6, face = "bold", color = "black"),
      legend.text = element_text(size = 6, face = "bold", color = "black")
    )
  
 
  if (!show_outliers) {
    
    upper_whisker_limit <- 50
    p <- p + coord_cartesian(ylim = c(0, upper_whisker_limit * 1.1))
  }
  
  return(p)
}


get_plot_elephant_effects_m10 <- function(data_location, fit_models_location, ci_level) {
  
  library(brms)
  library(tidybayes)
  library(dplyr)
  library(ggplot2)
  
  fit_rai_m10 <- readRDS(paste0(fit_models_location, "model_10.rds"))

  # Canopy cover mean (unscaled), used to set covariate at its mean value so
  # the prediction reflects average habitat conditions. Taken directly from
  # the data the model was fit on.
  canopy_mean <- mean(fit_rai_m10$data$canopy_cover_mean_1000m_buffer)
  
  # Get species list from the model data
  species_list <- unique(fit_rai_m10$data$species_lower_case)
  
  # Build prediction grid: all species x both elephant zones, covariates
  # held at their mean values, offset set to log(100) for per-100-nights scale, i.e. RAI
  pred_grid <- expand.grid(
    species_lower_case                  = species_list,
    Elephant_Presence                   = c("Elephant", "No elephant"),
    dist_from_camp_sc                   = 0,
    canopy_cover_mean_1000m_buffer      = canopy_mean,
    Nights                              = 100,          
    Trap.Station.Name                   = NA,         
    stringsAsFactors                    = FALSE
  )
  
  pred_grid$log_Nights <- log(pred_grid$Nights)
  
  # add_epred_draws() integrates over both fixed effects and species-level
  # random effects 
  posterior_epred <- add_epred_draws(
    newdata      = pred_grid,
    object       = fit_rai_m10,
    re_formula   = ~ (1 + Elephant_Presence + canopy_cover_mean_1000m_buffer +
                        dist_from_camp_sc | species_lower_case),
    allow_new_levels = TRUE   # needed because Trap.Station.Name = NA is new
  )
  
  # Average predicted detections across species within each posterior draw,
  # then summarize to correctly propagate posterior uncertainty
  hpdi_average <- posterior_epred %>%
    group_by(Elephant_Presence, .draw) %>%
    summarise(mean_detections_draw = mean(.epred), .groups = "drop") %>%
    group_by(Elephant_Presence) %>%
    summarise(
      Mean_Detections = mean(mean_detections_draw),
      Lower_90_HPDI   = HDInterval::hdi(mean_detections_draw, credMass = ci_level)["lower"],
      Upper_90_HPDI   = HDInterval::hdi(mean_detections_draw, credMass = ci_level)["upper"],
      .groups         = "drop"
    ) %>%
    mutate(
      Zone  = ifelse(Elephant_Presence == "Elephant", "Inside", "Outside"),
      x_val = ifelse(Elephant_Presence == "Elephant", 1, 2)
    )
  
  fig_predictions <- ggplot(
    hpdi_average,
    aes(x = x_val, y = Mean_Detections, color = Elephant_Presence, fill = Elephant_Presence)
  ) +
    
    geom_linerange(
      aes(ymin = Lower_90_HPDI, ymax = Upper_90_HPDI),
      color     = "black",
      linewidth = 1.4,
      lineend   = "round"
    ) +
    geom_linerange(
      aes(ymin = Lower_90_HPDI, ymax = Upper_90_HPDI),
      linewidth = 0.6,
      lineend   = "round"
    ) +
    geom_point(
      shape  = 21,
      size   = 3.6,
      stroke = 0.6,
      color  = "black"
    ) +
    scale_color_manual(values = c("Elephant" = "cornsilk4", "No elephant" = "burlywood1")) +
    scale_fill_manual(values = c("Elephant" = "cornsilk4", "No elephant" = "burlywood1")) +
    scale_x_continuous(
      name   = "Elephant Presence",
      breaks = c(1, 2),
      labels = c("Elephant", "No Elephant"),
      limits = c(0.5, 2.5)
    ) +
    labs(y = "Detections per Species per 100 Nights") +
    theme_classic(base_size = 6) +
    theme(
      text            = element_text(size = 6, face = "bold", color = "black"),
      axis.title      = element_text(size = 6, face = "bold", color = "black"),
      axis.text       = element_text(size = 6, face = "bold", color = "black"),
      legend.position = "none"
    )

  return(fig_predictions)
}


get_or_compute_loo <- function(fit, cache_file) {
  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }
  loo_result <- suppressWarnings(loo(fit, moment_match = TRUE, reloo = TRUE))
  saveRDS(loo_result, file = cache_file)
  loo_result
}


save_table_S3_RAI_model_comparison <- function(fit_models_location, tables_location) {
  
  fit_m8  <- readRDS(paste0(fit_models_location, "model_8.rds"))
  fit_m9  <- readRDS(paste0(fit_models_location, "model_9.rds"))
  fit_m10 <- readRDS(paste0(fit_models_location, "model_10.rds"))
  
  # Compute LOO for each model (cached to disk; see get_or_compute_loo)
  loo_m8  <- get_or_compute_loo(fit_m8,  file.path(fit_models_location, "loo_8.rds"))
  loo_m9  <- get_or_compute_loo(fit_m9,  file.path(fit_models_location, "loo_9.rds"))
  loo_m10 <- get_or_compute_loo(fit_m10, file.path(fit_models_location, "loo_10.rds"))
  
  model_list <- list(M8 = loo_m8, M9 = loo_m9, M10 = loo_m10)
  
  comp <- loo_compare(model_list)
  
  # Pseudo-BMA and stacking weights; loo_model_weights returns a named vector in model_list order
  pbma_weights  <- loo_model_weights(model_list, method = "pseudobma")
  stack_weights <- loo_model_weights(model_list, method = "stacking")
  
  predictor_map <- c(
    M8  = "Elephant Presence",
    M9  = "Elephant Presence, Distance from Camp",
    M10 = "Elephant Presence, Distance from Camp, Tree Canopy Cover"
  )
  model_num_map <- c(M8 = "8", M9 = "9", M10 = "10")
  
  row_order <- rownames(comp)  # best-to-worst order
  
  table_df <- data.frame(
    `Model`                 = model_num_map[row_order],
    `Predictor Variables`   = predictor_map[row_order],
    `Delta ELPD`            = round(comp[row_order, "elpd_diff"], 1),
    `SE Delta ELPD`         = round(comp[row_order, "se_diff"], 1),
    `LOO Pseudo-BMA Weight` = round(as.numeric(pbma_weights[row_order]), 3),
    `LOO Stacking Weight`   = round(as.numeric(stack_weights[row_order]), 3),
    check.names             = FALSE,
    row.names               = NULL
  )
  
  write.csv(table_df,
            file      = file.path(tables_location, "Table_S3_RAI_model_comparison_zero_inflated_species_varying.csv"),
            row.names = FALSE)
  
  return(invisible(table_df))
}


plot_and_save_posterior_predictions_model_10 <- function(fit_models_location,
                                                                           data_location,
                                                                           figures_location,
                                                                           ci_level    = ci_level,
                                                                           figure_name = "figure_S4_posterior_predictions_model_10_RAI") {
  
  fit_m10 <- readRDS(paste0(fit_models_location, "model_10.rds"))
  
  detections_data_with_locations <- read.csv(paste0(data_location, "RAI_analysis_data_with_locations.csv"))
  
  # Reference values held constant when varying each focal predictor:
  # Elephant Presence held at "Elephant" (the reference level, absorbed into
  # the intercept), dist_from_camp_sc held at 0 (its mean, since it is
  # standardized), and canopy cover held at its observed (unscaled) mean
  canopy_mean <- mean(detections_data_with_locations$canopy_cover_mean_1000m_buffer, na.rm = TRUE)
  
  
  summarize_epred_hpdi <- function(epred) {
    data.frame(
      mean  = apply(epred, 2, mean),
      lower = apply(epred, 2, function(x) HDInterval::hdi(x, credMass = ci_level)["lower"]),
      upper = apply(epred, 2, function(x) HDInterval::hdi(x, credMass = ci_level)["upper"])
    )
  }
  
  panel_theme <- theme_classic(base_size = 7, base_family = "Helvetica") +
    theme(
      axis.title  = element_text(size = 7, face = "plain", color = "black"),
      axis.text   = element_text(size = 6, color = "black"),
      axis.line   = element_line(linewidth = 0.3, color = "black"),
      axis.ticks  = element_line(linewidth = 0.3, color = "black"),
      plot.margin = margin(5, 5, 5, 5, "pt")
    )
  
  y_lab <- "Average RAI\n(Detections per 100 Nights)"
  
  # ---- Panel A: Elephant Presence ----
  # Matches the approach used in get_plot_elephant_effects_m10 (Figure 5 panel
  # B), so the two panels report the same logical quantity: predictions are
  # generated for every observed species (re_formula includes the
  # species-level varying slopes/intercepts), then averaged across species
  # within each posterior draw before taking the HPDI.
  library(tidybayes)

  species_list_elephant <- unique(fit_m10$data$species_lower_case)

  pred_grid_elephant <- expand.grid(
    species_lower_case                  = species_list_elephant,
    Elephant_Presence                   = c("Elephant", "No elephant"),
    dist_from_camp_sc                   = 0,
    canopy_cover_mean_1000m_buffer      = canopy_mean,
    Nights                              = 100,
    Trap.Station.Name                   = NA,           # marginalize over camera random effect
    stringsAsFactors                    = FALSE
  )
  pred_grid_elephant$log_Nights <- log(pred_grid_elephant$Nights)

  posterior_epred_elephant <- add_epred_draws(
    newdata          = pred_grid_elephant,
    object           = fit_m10,
    re_formula       = ~ (1 + Elephant_Presence + canopy_cover_mean_1000m_buffer +
                             dist_from_camp_sc | species_lower_case),
    allow_new_levels = TRUE   # needed because Trap.Station.Name = NA is new
  )

  summary_elephant <- posterior_epred_elephant %>%
    group_by(Elephant_Presence, .draw) %>%
    summarise(mean_detections_draw = mean(.epred), .groups = "drop") %>%
    group_by(Elephant_Presence) %>%
    summarise(
      mean  = mean(mean_detections_draw),
      lower = HDInterval::hdi(mean_detections_draw, credMass = ci_level)["lower"],
      upper = HDInterval::hdi(mean_detections_draw, credMass = ci_level)["upper"],
      .groups = "drop"
    ) %>%
    mutate(Elephant_Presence = factor(Elephant_Presence, levels = c("Elephant", "No elephant")))

  p_elephant <- ggplot(summary_elephant, aes(x = Elephant_Presence, y = mean)) +
    geom_pointrange(aes(ymin = lower, ymax = upper), size = 0.6, fatten = 4) +
    labs(x = "Elephant Presence", y = y_lab) +
    panel_theme
  
  # ---- Panel B: Distance from Camp ----
  dist_seq <- seq(
    min(detections_data_with_locations$dist_from_camp_sc, na.rm = TRUE),
    max(detections_data_with_locations$dist_from_camp_sc, na.rm = TRUE),
    length.out = 50
  )
  newdata_dist <- data.frame(
    Elephant_Presence              = "Elephant",
    dist_from_camp_sc               = dist_seq,
    canopy_cover_mean_1000m_buffer  = canopy_mean,
    Nights                          = 100,
    log_Nights                      = log(100)
  )
  epred_dist   <- posterior_epred(fit_m10, newdata = newdata_dist, re_formula = NA)
  summary_dist <- summarize_epred_hpdi(epred_dist)
  summary_dist$dist_from_camp_sc <- dist_seq
  
  p_dist <- ggplot(summary_dist, aes(x = dist_from_camp_sc, y = mean)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#31688E", alpha = 0.3) +
    geom_line(color = "#31688E", linewidth = 0.5) +
    labs(x = "Distance from Camp (standardized)", y = y_lab) +
    panel_theme
  
  # Panel C: Tree Canopy Cover ----
  canopy_seq <- seq(
    min(detections_data_with_locations$canopy_cover_mean_1000m_buffer, na.rm = TRUE),
    max(detections_data_with_locations$canopy_cover_mean_1000m_buffer, na.rm = TRUE),
    length.out = 50
  )
  newdata_canopy <- data.frame(
    Elephant_Presence              = "Elephant",
    dist_from_camp_sc               = 0,
    canopy_cover_mean_1000m_buffer  = canopy_seq,
    Nights                          = 100,
    log_Nights                      = log(100)
  )
  epred_canopy   <- posterior_epred(fit_m10, newdata = newdata_canopy, re_formula = NA)
  summary_canopy <- summarize_epred_hpdi(epred_canopy)
  summary_canopy$canopy_cover_mean_1000m_buffer <- canopy_seq
  
  p_canopy <- ggplot(summary_canopy, aes(x = canopy_cover_mean_1000m_buffer, y = mean)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#31688E", alpha = 0.3) +
    geom_line(color = "#31688E", linewidth = 0.5) +
    labs(x = "Tree Canopy Cover (%)", y = y_lab) +
    panel_theme
  
  combined_plot <- p_elephant + p_dist + p_canopy +
    plot_layout(nrow = 1) +
    plot_annotation(tag_levels = "A")
  
  ggsave(
    filename = paste0(figures_location, figure_name, ".pdf"),
    plot     = combined_plot,
    width    = 7.5,
    height   = 2.4,
    units    = "in",
    device   = "pdf"
  )
  
  return(invisible(combined_plot))
}


get_manual_species_epred_draws <- function(fit, species_name, elephant_present, covariate_values = list()) {

  fe <- fixef(fit, summary = FALSE)
  re <- ranef(fit, summary = FALSE)$species_lower_case
  fe_names <- colnames(fe)

  dummy_no_elephant <- as.numeric(!elephant_present)

  mu_lin <- fe[, "Intercept"] + re[, species_name, "Intercept"] +
    (fe[, "Elephant_PresenceNoelephant"] + re[, species_name, "Elephant_PresenceNoelephant"]) * dummy_no_elephant

  # Model 8 has neither covariate below; Model 9 has dist_from_camp_sc only;
  # Model 10 has both. Terms absent from a given model's fixed effects are
  # skipped automatically, so this one function serves all three models.
  for (covariate_name in names(covariate_values)) {
    if (covariate_name %in% fe_names) {
      mu_lin <- mu_lin +
        (fe[, covariate_name] + re[, species_name, covariate_name]) * covariate_values[[covariate_name]]
    }
  }

  mu_lin <- mu_lin + log(100)  # offset: predictions on a per-100-trap-nights scale

  # Species-specific zero-inflation probability (see file-level comment)
  zi_lin <- fe[, "zi_Intercept"] + re[, species_name, "zi_Intercept"]

  # Zero-inflated model expectation: E[Y] = (1 - zero-inflation probability) * mu
  exp(mu_lin) * (1 - plogis(zi_lin))
}


get_RAI_model_draw_assignment <- function(tables_location, n_draws) {

  comp_table    <- read.csv(paste0(tables_location, "table_S3_RAI_model_comparison_zero_inflated_species_varying.csv"), check.names = FALSE)
  stack_weights <- setNames(comp_table[["LOO Stacking Weight"]], paste0("M", comp_table$Model))
  stack_weights <- stack_weights[c("M8", "M9", "M10")]
  stopifnot(!anyNA(stack_weights))

  set.seed(123)
  sample(names(stack_weights), size = n_draws, replace = TRUE, prob = stack_weights)
}


compute_table_S4_species_specific_RAI_differences <- function(fit_models_location,
                                                                tables_location,
                                                                data_location,
                                                                species_list = c("Kirk's dik-dik", "Greater kudu", "Impala", "Warthog"),
                                                                ci_level = ci_level) {

  fit_m8  <- readRDS(paste0(fit_models_location, "model_8.rds"))
  fit_m9  <- readRDS(paste0(fit_models_location, "model_9.rds"))
  fit_m10 <- readRDS(paste0(fit_models_location, "model_10.rds"))
  fit_list <- list(M8 = fit_m8, M9 = fit_m9, M10 = fit_m10)

  detections_data_with_locations <- read.csv(paste0(data_location, "RAI_analysis_data_with_locations.csv"))
  canopy_mean <- mean(detections_data_with_locations$canopy_cover_mean_1000m_buffer, na.rm = TRUE)
  reference_covariates <- list(canopy_cover_mean_1000m_buffer = canopy_mean, dist_from_camp_sc = 0)

  n_draws    <- nrow(as_draws_df(fit_m8))
  model_draw <- get_RAI_model_draw_assignment(tables_location, n_draws)

  summarize_diff <- function(elephant_draws, no_elephant_draws) {
    abs_diff_draws <- elephant_draws - no_elephant_draws
    pct_diff_draws <- (elephant_draws / no_elephant_draws - 1) * 100
    data.frame(
      rai_elephant_mean    = mean(elephant_draws),
      rai_no_elephant_mean = mean(no_elephant_draws),
      abs_diff_mean        = mean(abs_diff_draws),
      abs_diff_lower       = HDInterval::hdi(abs_diff_draws, credMass = ci_level)["lower"],
      abs_diff_upper       = HDInterval::hdi(abs_diff_draws, credMass = ci_level)["upper"],
      pct_diff_mean        = mean(pct_diff_draws),
      pct_diff_lower       = HDInterval::hdi(pct_diff_draws, credMass = ci_level)["lower"],
      pct_diff_upper       = HDInterval::hdi(pct_diff_draws, credMass = ci_level)["upper"],
      prob_direction       = mean(abs_diff_draws > 0)
    )
  }

  rows <- list()

  for (species_name in species_list) {

    species_epred <- lapply(fit_list, function(fit) {
      list(
        elephant    = get_manual_species_epred_draws(fit, species_name, elephant_present = TRUE,  covariate_values = reference_covariates),
        no_elephant = get_manual_species_epred_draws(fit, species_name, elephant_present = FALSE, covariate_values = reference_covariates)
      )
    })

    for (model_label in c("M8", "M10")) {
      s <- summarize_diff(species_epred[[model_label]]$elephant, species_epred[[model_label]]$no_elephant)
      rows[[length(rows) + 1]] <- data.frame(
        Species = species_name,
        Model   = c(M8 = "Model 8", M10 = "Model 10")[[model_label]],
        s,
        row.names = NULL
      )
    }

    # Model-averaged: mix using the same per-draw model assignment as the community-level average
    elephant_stack    <- numeric(n_draws)
    no_elephant_stack <- numeric(n_draws)
    for (m in names(fit_list)) {
      idx <- which(model_draw == m)
      elephant_stack[idx]    <- species_epred[[m]]$elephant[idx]
      no_elephant_stack[idx] <- species_epred[[m]]$no_elephant[idx]
    }

    s_avg <- summarize_diff(elephant_stack, no_elephant_stack)
    rows[[length(rows) + 1]] <- data.frame(
      Species = species_name,
      Model   = "Model-Averaged",
      s_avg,
      row.names = NULL
    )
  }

  table_df <- do.call(rbind, rows)

  numeric_cols <- setdiff(names(table_df), c("Species", "Model"))
  for (col in numeric_cols) {
    table_df[[col]] <- round(table_df[[col]], if (grepl("prob_direction", col)) 2 else 1)
  }

  names(table_df) <- c(
    "Species", "Model",
    "RAI: Elephant (mean)", "RAI: No Elephant (mean)",
    "Absolute Difference (mean)", "Absolute Difference (lower)", "Absolute Difference (upper)",
    "Percent Difference (mean)", "Percent Difference (lower)", "Percent Difference (upper)",
    "Probability of Direction"
  )
  
  columns_to_keep <- c("Species", "Model",
                       "RAI: Elephant (mean)", "RAI: No Elephant (mean)",
                       "Absolute Difference (mean)",
                       "Percent Difference (mean)",
                       "Probability of Direction")

  table_df <- table_df[,columns_to_keep]
  
  write.csv(table_df,
            file      = file.path(tables_location, "Table_S4_species_specific_RAI_differences.csv"),
            row.names = FALSE)

  return(invisible(table_df))
}


# Figures 1-5, Sized and Formatted for Science Submission ----
save_figures_for_science_submission <- function(data_location, figures_location_science="~/Dropbox/Manuscripts/Fear of Elephants/science submission/figures/", max_figure_dimensions) {

  #Figure 1
  plot_and_save_figure_interview_results_for_science(data_location, figures_location_science)

  #Figure 2
  plot_and_save_camera_grid_schema_and_GPS_tracks(data_location, figures_location_science, height_in = 3, width_in = 9, figure_name="Fig_2_map_and_GPS_tracks.tiff")

  #Figure 3
  save_figure_3_and_figure_S1_patchwork(data_location, figures_location=figures_location_science, sim_method="rot", run_all=FALSE)


  #Figure 4
  diurnality_data_30_min_filter <- read.csv(file.path(data_location, "camera_trap_data_for_diurnality_analysis.csv"))
  diurnality_model_30_min_filter <- readRDS(paste0(fit_models_location, "model_7.rds"))

  plot_and_save_empirical_and_model_predicted_diurnality_for_science(figures_location = figures_location_science,
                                                                      data=diurnality_data_30_min_filter,
                                                                      model=diurnality_model_30_min_filter,
                                                                      ci_level=ci_level,
                                                                      figure_name="Fig_4_empirical_and_modeled_diurnality_30_min_time_filter")

  #Figure 5
  plot_and_save_RAI_by_species_and_elephant_zone_empirical_and_model_predictions_for_science(figure_name="Figure_5_RAI",
                                                                                              data_location = data_location,
                                                                                              fit_models_location = fit_models_location,
                                                                                              figures_location=figures_location_science,
                                                                                              show_outliers=TRUE)

  #Figure S1
  save_figure_3_and_figure_S1_patchwork(data_location, figures_location=figures_location_science, sim_method="bcrw", run_all=FALSE)

  #figure S2
  plot_and_save_supp_figure_of_diurnality_diff_distribution(figures_location = figures_location_science, model=diurnality_model_30_min_filter, figure_name="figure_S2_posterior_distribution_of_difference_in_daytime_detection_probability")

  #Figure S3
  plot_and_save_fig_S3_RAI_percent_diff_distribution(fit_models_location = fit_models_location,
                                                                       tables_location     = tables_location,
                                                                       data_location       = data_location,
                                                                       figures_location    = figures_location_science,
                                                                       ci_level            = ci_level)
  #Figures S4
  plot_and_save_posterior_predictions_model_10(fit_models_location = fit_models_location, data_location = data_location, figures_location = figures_location_science, ci_level= ci_level)

}


# Model-fitting code for Models 7-10 (not run by reproduce_all()) ----
# reproduce_all() loads Models 7-10 pre-fit from fit_models_public/ rather than
# refitting them (see header). The functions below are the code actually used to
# produce those fits, included for transparency even though nothing here calls
# them. They will run against the data already in this package -- data_public/
# and fit_models_public/ are sufficient -- but MCMC sampling takes substantially
# longer than the rest of this script, and even with a fixed seed is not
# guaranteed to reproduce the shipped .rds files bit-for-bit across different
# R/Stan/brms versions or hardware. Load the shipped fits (as reproduce_all()
# does) for exact reproduction of every reported number.

# MCMC settings used for Models 8-10 ----
rai_mcmc_chains  <- 4
rai_mcmc_iter    <- 2000
rai_mcmc_warmup  <- 1000
rai_mcmc_cores   <- 4
rai_mcmc_seed    <- 123
rai_mcmc_control <- list(adapt_delta = 0.99, max_treedepth = 15)

# Weakly-Informative Priors for Models 8-10, Matched to PSA Models 1-6 ----
rai_priors <- function() {
  c(
    prior(normal(0, 1),   class = "b"),
    prior(normal(0, 10),  class = "Intercept"),
    prior(normal(0, 10),  class = "Intercept", dpar = "zi"),
    prior(exponential(1), class = "sd"),
    prior(exponential(1), class = "sd", dpar = "zi"),
    prior(lkj(4),         class = "cor"),
    prior(gamma(2, 0.1),  class = "shape")
  )
}

# Fit Model 7 (Diurnality) ----
fit_model_7 <- function(data_location, fit_models_location)
{
  d <- read.csv(paste0(data_location, "camera_trap_data_for_diurnality_analysis.csv"))

  my_priors <- c(
    prior(normal(0, 1),   class = "Intercept"),
    prior(normal(0, 1), class = "b", coef = "elephant_zoneTRUE"),
    prior(exponential(1), class = "sd"),
    prior(lkj(2),         class = "cor")
  )

  fit_diurnal <- brm(
    formula = diurnal ~ 1 + elephant_zone +
      (1 + elephant_zone | species),
    data    = d,
    family  = bernoulli(link = "logit"),
    prior  = my_priors,
    chains  = 4,
    iter    = 2000,
    warmup  = 1000,
    cores   = 4,
    seed    = 123,
    control = list(adapt_delta = 0.99),
    save_pars = save_pars(all = TRUE)
  )

  saveRDS(fit_diurnal, paste0(fit_models_location, "model_7.rds"))
}

# Fit RAI Model 8 (Zero-Inflated, Species-Varying ZI): Elephant Presence ----
fit_model_8 <- function(detections_data_with_locations, fit_models_location) {

  model_data <- detections_data_with_locations %>%
    mutate(log_Nights = log(Nights))

  message("--- Starting Zero-Inflated (Species-Varying ZI) RAI Model 8: Just Elephants ---")

  fit_m8 <- brm(
    formula = bf(
      Total_Detections ~ Elephant_Presence +
        (1 | Trap.Station.Name) +
        (1 + Elephant_Presence | species_lower_case) +
        offset(log_Nights),
      zi ~ (1 | species_lower_case)
    ),
    data      = model_data,
    family    = zero_inflated_negbinomial(),
    prior     = rai_priors(),
    chains    = rai_mcmc_chains, iter = rai_mcmc_iter, warmup = rai_mcmc_warmup,
    cores     = rai_mcmc_cores, seed = rai_mcmc_seed, control = rai_mcmc_control,
    save_pars = save_pars(all = TRUE)
  )
  saveRDS(fit_m8, file = file.path(fit_models_location, "model_8.rds"))
}

# Fit RAI Model 9 (Zero-Inflated, Species-Varying ZI): + Distance from Camp ----
fit_model_9 <- function(detections_data_with_locations, fit_models_location) {

  model_data <- detections_data_with_locations %>%
    mutate(log_Nights = log(Nights))

  message("--- Starting Zero-Inflated (Species-Varying ZI) RAI Model 9: Distance from camp and elephants ---")

  fit_m9 <- brm(
    formula = bf(
      Total_Detections ~ Elephant_Presence + dist_from_camp_sc +
        (1 | Trap.Station.Name) +
        (1 + Elephant_Presence + dist_from_camp_sc | species_lower_case) +
        offset(log_Nights),
      zi ~ (1 | species_lower_case)
    ),
    data      = model_data,
    family    = zero_inflated_negbinomial(),
    prior     = rai_priors(),
    chains    = rai_mcmc_chains, iter = rai_mcmc_iter, warmup = rai_mcmc_warmup,
    cores     = rai_mcmc_cores, seed = rai_mcmc_seed, control = rai_mcmc_control,
    save_pars = save_pars(all = TRUE)
  )
  saveRDS(fit_m9, file = file.path(fit_models_location, "model_9.rds"))
}

# Fit RAI Model 10 (Zero-Inflated, Species-Varying ZI): + Tree Canopy Cover ----
fit_model_10 <- function(detections_data_with_locations, fit_models_location) {

  model_data <- detections_data_with_locations %>%
    mutate(log_Nights = log(Nights))

  message("--- Starting Zero-Inflated (Species-Varying ZI) RAI Model 10: Distance from camp, tree canopy cover, and elephants ---")

  fit_m10 <- brm(
    formula = bf(
      Total_Detections ~ Elephant_Presence + canopy_cover_mean_1000m_buffer + dist_from_camp_sc +
        (1 | Trap.Station.Name) +
        (1 + Elephant_Presence + canopy_cover_mean_1000m_buffer + dist_from_camp_sc | species_lower_case) +
        offset(log_Nights),
      zi ~ (1 | species_lower_case)
    ),
    data      = model_data,
    family    = zero_inflated_negbinomial(),
    prior     = rai_priors(),
    chains    = rai_mcmc_chains, iter = rai_mcmc_iter, warmup = rai_mcmc_warmup,
    cores     = rai_mcmc_cores, seed = rai_mcmc_seed, control = rai_mcmc_control,
    save_pars = save_pars(all = TRUE)
  )
  saveRDS(fit_m10, file = file.path(fit_models_location, "model_10.rds"))
}

# Fit All Three RAI Models (8, 9, 10) ----
fit_RAI_models <- function(data_location, fit_models_location) {

  detections_data_with_locations <- read.csv(paste0(data_location, "RAI_analysis_data_with_locations.csv"))

  fit_model_8(detections_data_with_locations, fit_models_location)
  fit_model_9(detections_data_with_locations, fit_models_location)
  fit_model_10(detections_data_with_locations, fit_models_location)
}

# Fit Models 7-10 ----
fit_models <- function(data_location, fit_models_location)
{
  #this fits model 7
  fit_model_7(data_location, fit_models_location)

  #this fits models 8, 9, 10
  fit_RAI_models(data_location, fit_models_location)
}


# Reproduce All Function ----
reproduce_all <- function(data_location, figures_location, tables_location, max_figure_dimensions) {

  # Diurnality and RAI input data, and all models (diurnality, RAI, PSA), are loaded
  # pre-computed from data_public/ and fit_models_public/ rather than derived/fit here.
  save_tables(fit_models_location=fit_models_location, data_location = data_location, tables_location = tables_location)

  save_figures(data_location, figures_location, max_figure_dimensions)
  save_figures_for_science_submission(data_location, figures_location=file.path(figures_location, "science_submission/"), max_figure_dimensions)

  save_text_reported_results(data_location, fit_models_location, text_output_folder)

}

save_tables <- function(fit_models_location, data_location, tables_location) {

  #Table S1
  calc_and_save_summary_table_diurnality_analysis(data_location, tables_location)

  #Table S2
  save_table_S2_PSA_model_comparison_rotation(fit_models_location, tables_location)

  #Table S3
  save_table_S3_RAI_model_comparison(fit_models_location, tables_location)

  # calc_and_save_model_comparison_rai_analysis() reads model files that don't exist and
  # errors out; Table_S2_PSA_model_comparison.csv is already produced above.

  #Table S4
  compute_table_S4_species_specific_RAI_differences(fit_models_location,
                                                    tables_location,
                                                    data_location,
                                                    species_list = c("Kirk's dik-dik", "Greater kudu", "Impala", "Warthog"),
                                                    ci_level = ci_level)

}


# Figure 3 / Figure S1 ----
# panel_data holds one illustrative individual's chosen + candidate paths (anonymized;
# see data_public/README.md) plus the N_id/sex/id vectors Panel D needs.
save_figure_3_and_figure_S1_patchwork <- function(data_location, figures_location, sim_method, run_all, ci_level = 0.9) {

  panel_data <- readRDS(file.path(data_location, paste0("fig3_panelA_D_data_", sim_method, "_anon.rds")))

  if (sim_method == "rot") {
    s_Elephants_Environment <- readRDS(paste0(fit_models_location, "s_Elephants_Environment_rot.rds"))
    s_Environment           <- readRDS(paste0(fit_models_location, "s_Environment_rot.rds"))
    s_Elephants             <- readRDS(paste0(fit_models_location, "s_Elephants_rot.rds"))
  } else {
    s_Elephants_Environment <- readRDS(paste0(fit_models_location, "s_Elephants_Environment_bcrw.rds"))
    s_Environment           <- readRDS(paste0(fit_models_location, "s_Environment_bcrw.rds"))
    s_Elephants             <- readRDS(paste0(fit_models_location, "s_Elephants_bcrw.rds"))
  }

  s <- s_Elephants_Environment

  # ---- Panel A: path illustration ----
  alt_data    <- panel_data$alts
  chosen_data <- panel_data$chosen

  p_A <- ggplot() +
    geom_path(data = alt_data,    aes(x = x, y = y, group = cand, color = "Alternative"),
              linewidth = 0.4) +
    geom_path(data = chosen_data, aes(x = x, y = y,               color = "Chosen"),
              linewidth = 0.8) +
    scale_color_manual(name   = NULL,
                       values = c("Chosen" = "black", "Alternative" = "grey"),
                       breaks = c("Chosen", "Alternative")) +
    scale_x_continuous(expand = expansion(mult = 0.2)) +
    scale_y_continuous(expand = expansion(mult = 0.2)) +
    labs(x = "Longitude", y = "Latitude") +
    theme_classic() +
    theme(axis.text           = element_blank(),
          axis.ticks          = element_blank(),
          legend.position     = c(0, 1.05),
          legend.justification = c(0, 1),
          legend.background   = element_blank(),
          legend.key          = element_blank(),
          legend.text         = element_text(size = 9))

  # ---- Panel B: feature weights ----
  feat_names <- c("Ruggedness", "Elephant\nDistance", "Road\nDistance", "River\nDistance")

  feat_df <- do.call(rbind, lapply(1:4, function(i) {
    do.call(rbind, lapply(1:2, function(sex_i) {
      samp <- s$weights[, i, sex_i]
      data.frame(feature = feat_names[i],
                 sex     = ifelse(sex_i == 1, "Female", "Male"),
                 mean    = mean(samp),
                 lower   = HPDI(samp, ci_level)[1],
                 upper   = HPDI(samp, ci_level)[2])
    }))
  }))
  feat_df$feature <- factor(feat_df$feature, levels = rev(feat_names))
  feat_df$sex     <- factor(feat_df$sex, levels = c("Female", "Male"))

  p_B <- ggplot(feat_df, aes(x = mean, y = feature, color = sex, shape = sex)) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey60") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0,
                   position = position_dodge(width = 0.5), linewidth = 0.8) +
    geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
    scale_color_manual(name   = NULL,
                       values = c("Female" = "#ff7f00", "Male" = "#377eb8")) +
    scale_shape_manual(name   = NULL,
                       values = c("Female" = 17,        "Male" = 16)) +
    scale_x_continuous(limits = c(-3, 3)) +
    labs(x = "Feature Weight", y = NULL) +
    theme_classic() +
    theme(legend.position    = c(0.98, 0.98),
          legend.justification = c(1, 1),
          legend.background  = element_blank(),
          legend.key         = element_blank(),
          legend.text        = element_text(size = 9))

  # ---- Panel C: LOO model comparison ----
  loo_EE <- loo(s_Elephants_Environment$log_lik)
  loo_E  <- loo(s_Elephants$log_lik)
  loo_En <- loo(s_Environment$log_lik)

  comp     <- loo_compare(list(Elephants_Environment = loo_EE,
                               Elephants             = loo_E,
                               Environment           = loo_En))

  name_map <- NA
  if(sim_method == "rot") {
    name_map <- c("Elephants_Environment" = "Elephant + Environment (M3)",
                "Elephants"             = "Elephant (M2)",
                "Environment"           = "Environment (M1)")
  } else if(sim_method == "bcrw") {
    name_map <- c("Elephants_Environment" = "Elephant + Environment (M6)",
                  "Elephants"             = "Elephant (M5)",
                  "Environment"           = "Environment (M4)")
  }

  comp_df             <- as.data.frame(comp)
  comp_df$model_label <- name_map[rownames(comp_df)]
  comp_df$model_label <- factor(comp_df$model_label, levels = rev(comp_df$model_label))
  comp_df$non_ref     <- comp_df$se_diff > 0

  xlim_C <- if (sim_method == "rot") c(-20, 1) else c(-45, 1)

  p_C <- ggplot(comp_df, aes(x = elpd_diff, y = model_label)) +
    geom_col(fill = "grey30", color = "grey30") +
    geom_vline(xintercept = 0, linewidth = 0.75) +
    geom_errorbarh(data = subset(comp_df, non_ref),
                   aes(xmin = elpd_diff - 1.645 * se_diff,
                       xmax = elpd_diff + 1.645 * se_diff),
                   height = 0.25, linewidth = 0.8) +
    scale_x_continuous(limits = xlim_C) +
    labs(x = expression(Delta * " ELPD (rel. to best)"), y = NULL) +
    theme_classic()

  # ---- Panel D: individual elephant-distance effects ----
  sex <- panel_data$sex
  id  <- panel_data$id
  N_id <- panel_data$N_id

  n_samp     <- nrow(s$weights)
  weights_id <- array(NA, dim = c(N_id, 4, n_samp))
  for (id_i in unique(id)) {
    for (k in 1:4) {
      weights_id[id_i, k, ] <- s$weights[, k, sex[id_i]] + s$v_ID[, id_i, k]
    }
  }

  means <- apply(weights_id[, 2, ], 1, mean)
  HPDIs <- apply(weights_id[, 2, ], 1, function(x) HPDI(x, ci_level))

  ind_df <- data.frame(id    = 1:N_id,
                       sex   = factor(sex),
                       mean  = means,
                       lower = HPDIs[1, ],
                       upper = HPDIs[2, ])
  xlim_D <- if (sim_method == "rot") c(-2, 4) else c(-4, 8)

  p_D <- ggplot(ind_df, aes(x = mean, y = factor(id), color = sex, shape = sex)) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey60") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0, linewidth = 0.6) +
    geom_point(size = 1.8) +
    scale_color_manual(values = c("1" = "#ff7f00", "2" = "#377eb8")) +
    scale_shape_manual(values = c("1" = 17,        "2" = 16)) +
    scale_x_continuous(limits = xlim_D) +
    labs(x = "Feature Weight", y = "ID") +
    theme_classic() +
    theme(legend.position  = "none",
          axis.text.y      = element_blank(),
          axis.ticks.y     = element_blank())

  # ---- Combine with patchwork ----
  fig <- (p_A | p_B | p_C | p_D) +
    plot_layout(widths = c(1, 1, 1, 1)) +
    plot_annotation(tag_levels = 'A') &
    theme(plot.tag = element_text(size = 12, face = "plain"))

  out_file <- paste0(figures_location,
                     ifelse(sim_method == "rot", "Figure_3.pdf", "Figure_S1.pdf"))
  ggsave(out_file, plot = fig, height = 3.4, width = 13.5, units = "in", dpi = 400)
}
