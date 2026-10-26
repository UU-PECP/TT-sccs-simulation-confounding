if (!require("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library(pacman)
pacman::p_load(ggplot2, 
               dplyr,
               readr,
               tidyr,
               here,
               rio
               )
options(scipen = 999)

source(here("Codes", "SCCS_main_functions.R"))
source(here("Codes", "SCCS_sim_workflow_functions.R"))
source(here("Codes", "SCCS_helper_functions.R"))
source(here("Codes", "SCCS_sim_plot_functions.R"))


################################# 
# 1 - Table of all scenarios #### 
################################# 

scenarios <- scenario_table(p_C_vals = c(5e-4, 1e-3, 5e-3), 
                            C_on_E_vals = c(0.25, 1, 2, 5),
                            IRR_E_vals = c(1, 2), 
                            chosen_risk_window_C = c(10, 30, 60),
                            U_on_Y_vals = c(0.25, 1, 2, 15))

##############################
# 2 - List of methods #### 
##############################

# Need to be among 'ignore_C', 'exclude_C', 'censor_C', 'adjust_C', 'adjust_C_obs_at_exp', and 'adjust_calendar'
methods <- c("ignore_C",              # ignore C in model
             "exclude_C",             # exclude subjects with C
             "censor_C",              # censor observation time at first C occurrence
             "adjust_C")              # adjust for C in SCCS model

##############################
# 3 - Quantify bias and summarise results ####
##############################
bias_results <- summarise_simulation_results(method_scen = method_scen(scenario_table = scenarios),
                                             results_dir = here("Results"),
                                             summary_dir = file.path(here("Results"), "Summary"),
                                             summary_file_name = "Summary_all_scens_2026_05_01")

##############################
# 4 - Exploratory analysis ####
##############################



data_explor <- data_exploratory()
non_convergence <- data_explor %>% filter(gamma_1_hat <= -10)
data_explor_converge <- data_explor %>% filter(!is.na(gamma_1_hat) & gamma_1_hat > -10)

# Histogram of gamma_1, n_event and se_gamma_1 ---------------

plot_exploratory(data = data_explor_converge)

# Agreement between empirical se_gamma_1 and model-based se_gamma_1

se_agreement <- ggplot(data = bias_results, aes(x = mod_se_gamma_1, y = se_gamma_1)) +
  geom_point() + geom_abline(slope = 1, intercept = 0)
se_agreement

##############################
# 5 - Visualising results
##############################

#bias_results <- import(here("Results", "Summary", "Summary_all_scens_2025_12_02.xlsx"))

bias_results$methods <- factor(bias_results$methods, levels = c("ignore_C", "exclude_C", "censor_C", "adjust_C"))

# For plotting IRR
bias_results <- bias_results %>% 
  mutate(log_IRR_E_hat = log(IRR_E_hat),
         log_IRR_E_low_CI = log(IRR_E_hat_low_CI),
         log_IRR_E_up_CI = log(IRR_E_hat_up_CI),
         exp_gamma_1_hat = exp(gamma_1_hat),
         exp_gamma_1_hat_low_CI = exp(gamma_1_hat_low_CI),
         exp_gamma_1_hat_up_CI = exp(gamma_1_hat_up_CI),
         bias_exp_gamma_1_hat = exp(gamma_1_hat) - IRR_E,
         )

bias_results1 <- bias_results %>% mutate(across(c(p_C,ConE,IRR_E,risk_win_C, UonY), as.factor))

bias_results1 <- bias_results1 %>%
  mutate(
    p_C_num = as.numeric(p_C),
    method_offset = as.numeric(methods) * 0.15 - 0.30,   # Manual dodge: create offsets for each method
    risk_win_C_labs = factor(risk_win_C, 
                             levels = c("10", "30", "60"), 
                             labels = c("Risk window C: 10 days", "Risk window C: 30 days", "Risk window C: 60 days")),
    y_dodged = p_C_num + method_offset
  )

# 5.1. No Confounding effect of C and U ----------------------------------------------------------------

# Expectation: downward bias for ignore_C due to mediating effect of C.
# No bias for other methods

results_no_UC1 <- bias_results1 %>% filter(UonY == 1 & ConE == 1 & IRR_E == 1)
results_no_UC2 <- bias_results1 %>% filter(UonY == 1 & ConE == 1 & IRR_E == 2)

## Lollipop plot for bias/ estimated IRR -----------------------------

### Plot for bias gamma_1 -------------
lollipop_plot(data = results_no_UC1, aes_x ="bias_gamma_1", 
              aes_x_low_ci ="bias_gamma_1_low_CI", aes_x_up_ci = "bias_gamma_1_up_CI",
              mode = "gamma",
              refline = 0,
              x_break = seq(from = -0.18, to = 0.02, by = 0.02),
              x_limits = c(-0.18, 0.02),
              xlabel = expression("Bias of " * gamma[1] * " (" * log(IRR[E]) * ")"),
              plot_name = "bias_gamma1_no_UC_IRRE1")
lollipop_plot(data = results_no_UC2, aes_x ="bias_gamma_1", 
              aes_x_low_ci ="bias_gamma_1_low_CI", aes_x_up_ci = "bias_gamma_1_up_CI",
              mode = "gamma",
              refline = 0,
              x_break = seq(from = -0.18, to = 0.02, by = 0.02),
              x_limits = c(-0.18, 0.02),
              xlabel = expression("Bias of " * gamma[1] * " (" * log(IRR[E]) * ")"),
              plot_name = "bias_gamma1_no_UC_IRRE2")

### Plot for bias IRR_E -----
lollipop_plot(data = results_no_UC1, aes_x ="bias_IRR_E", 
              aes_x_low_ci ="bias_IRR_E_low_CI", aes_x_up_ci = "bias_IRR_E_up_CI",
              mode = "gamma",
              refline = 0,
              x_break = seq(from = -0.2, to = 0.12, by = 0.02),
              x_limits = c(-0.2, 0.12),
              xlabel  = expression("Bias of " * IRR[E]),
              plot_name = "bias_IRRE_no_UC_IRRE1")
lollipop_plot(data = results_no_UC2, aes_x ="bias_IRR_E", 
              aes_x_low_ci ="bias_IRR_E_low_CI", aes_x_up_ci = "bias_IRR_E_up_CI",
              mode = "gamma",
              refline = 0,
              x_break = seq(from = -0.2, to = 0.12, by = 0.02),
              x_limits = c(-0.2, 0.12),
              xlabel <- expression("Bias of " * IRR[E]),
              plot_name = "bias_IRRE_no_UC_IRRE2")

### Plot for bias exp(gamma_1_hat)
lollipop_plot_noerror(data = results_no_UC1, aes_x ="bias_exp_gamma_1_hat", 
              mode = "gamma",
              refline = 0,
              x_break = seq(from = -0.14, to = 0, by = 0.02),
              x_limits = c(-0.14, 0),
              xlabel = expression("Bias of " * hat(IRR[E])),
              plot_name = "bias_IRRE_expg1_no_UC_IRRE1")

lollipop_plot_noerror(data = results_no_UC2, aes_x ="bias_exp_gamma_1_hat", 
              mode = "gamma",
              refline = 0,
              x_break = seq(from = -0.2, to = 0, by = 0.02),
              x_limits = c(-0.2, 0),
              xlabel = expression("Bias of " * hat(IRR[E])),
              plot_name = "bias_IRRE_expg1_no_UC_IRRE2")

### Relative bias ----
lollipop_plot(data = results_no_UC2, aes_x ="relative_bias_g1", 
              aes_x_low_ci ="relative_bias_g1_low_CI", aes_x_up_ci = "relative_bias_g1_up_CI",
              mode = "gamma",
              refline = 0,
              x_break = seq(from = -0.18, to = 0.1, by = 0.02),
              x_limits = c(-0.19, 0.1),
              xlabel = expression("Relative bias of " * gamma[1]),
              plot_name = "relative_bias_g1_no_UC_IRRE2")

### Plot for IRR -----------------------------

lollipop_plot(
  data       = results_no_UC1,
  aes_x      = "log_IRR_E_hat",
  aes_x_low_ci = "log_IRR_E_low_CI",
  aes_x_up_ci  = "log_IRR_E_up_CI",
  mode       = "irr",
  refline    = 1,                     
  irr_breaks = seq(0.8, 1.12, by = 0.04),
  irr_limits = c(0.8, 1.12),
  xlabel     = expression("Estimated " * IRR[E]),
  plot_name  = "mean_IRRE_no_UC_IRRE1"
)

lollipop_plot(
  data       = results_no_UC2,
  aes_x      = "log_IRR_E_hat",
  aes_x_low_ci = "log_IRR_E_low_CI",
  aes_x_up_ci  = "log_IRR_E_up_CI",
  mode       = "irr",
  refline    = 2,                     
  irr_breaks = seq(1.8, 2.12, by = 0.04),
  irr_limits = c(1.8, 2.12),
  xlabel     = expression("Estimated " * IRR[E]),
  plot_name  = "mean_IRRE_no_UC_IRRE2"
)

# IRR corresponding to the average gamma_1
lollipop_plot(
  data       = results_no_UC1,
  aes_x      = "gamma_1_hat",
  aes_x_low_ci = "gamma_1_hat_low_CI",
  aes_x_up_ci  = "gamma_1_hat_up_CI",
  mode       = "irr",
  refline    = 1,                     
  irr_breaks = seq(0.8, 1.04, by = 0.04),
  irr_limits = c(0.8, 1.04),
  xlabel     = expression("Estimated " * IRR[E] *" corresponding to " * bar(gamma[1])),
  plot_name  = "mean_IRRE_expg1_no_UC_IRRE1"
)

lollipop_plot(
  data       = results_no_UC2,
  aes_x      = "gamma_1_hat",
  aes_x_low_ci = "gamma_1_hat_low_CI",
  aes_x_up_ci  = "gamma_1_hat_up_CI",
  mode       = "irr",
  refline    = 2,                     
  irr_breaks = seq(1.76, 2.04, by = 0.04),
  irr_limits = c(1.76, 2.04),
  xlabel     = expression("Estimated " * IRR[E] *" corresponding to " * bar(gamma[1])),
  plot_name  = "mean_IRRE_expg1_no_UC_IRRE2"
)


## Plot for variance ------------------------


lollipop_plot_var(data = results_no_UC1, aes_x ="se_gamma_1", 
              aes_x_low_ci ="se_gamma_1_low_CI", aes_x_up_ci = "se_gamma_1_up_CI",
              plot_name = "empirical_se_gamma_1_no_UC_IRRE1")
lollipop_plot_var(data = results_no_UC2, aes_x ="se_gamma_1", 
                  aes_x_low_ci ="se_gamma_1_low_CI", aes_x_up_ci = "se_gamma_1_up_CI",
                  plot_name = "empirical_se_gamma_1_no_UC_IRRE2")

## Plot for mean number of events ----------------------------


nr_event_plot(data = results_no_UC2, aes_x ="mean_nr_event", 
                  plot_name = "nr_event_no_UC_IRRE2")

### Coverage ----



# 5.2. No Confounding effect of U, confounding effect of C presents----------------------------------------

# Expectation: Bias when ignoring C: combination of confounding and mediating path E-C-Y
# No bias for other methods

# Reshape data to wide format
method_cols <- setdiff(
  names(bias_results),
  c("methods", "scen_name", "p_C", "ConE", "IRR_E", "risk_win_C",
    "UonY", "scen_id"))
bias_results_wide <- pivot_wider(data = bias_results, 
                                 names_from = methods, values_from = all_of(method_cols), 
                                 names_vary = "fastest")

# Filter data of scenarios U->Y = 1
results_noU1 <- bias_results_wide %>% filter(UonY == 1 & IRR_E == 1)
results_noU2 <- bias_results_wide %>% filter(UonY == 1 & IRR_E == 2)

## IRR_E = 1 -------------------------------
# Reorder the parameters for nested loop plot

results_noU1 <- results_noU1 %>%
  mutate(
         ConE_cat = as.numeric(factor(ConE, level = c(1, 0.25, 2, 5))), 
         p_C_cat = as.numeric(as.factor(p_C)),
         risk_win_C_cat = as.numeric(as.factor(risk_win_C)))

results_noU1_reorder <- nestedloop(results_noU1, 
                                               varnames = c("ConE_cat", "p_C_cat", "risk_win_C_cat"), #order of the loop
                                               varlabels = c(
                                                             "Effect of C on E (OR)", 
                                                             "Daily probability of C",  
                                                             "Risk window of C (days)"),
                                               sign = c(1, 1, 1, 1)) # Order of the levels of each parameter, 
# 1 = increasing, -1 = decreasing


# Create nested loop plot
parOrig <- par()

### Bias gamma
nested_loop_plot(
  data = results_noU1_reorder,
  yvars = c("bias_gamma_1_ignore_C",
            "bias_gamma_1_exclude_C",
            "bias_gamma_1_censor_C",
            "bias_gamma_1_adjust_C"),
  ylab = expression("Bias of " * gamma[1]*" (log "* IRR[E]*")"),
  loop_label = c("Effect C\u2192E \n(OR = 1, 0.25, 2, 5)",
                 "Daily prob. C \n(0.0005, 0.001, 0.005)", "Duration C\u2192Y \n(10, 30, 60 days)"),
  refline = 0,
  ylim = c(-0.7, 0.4),
  ymax_ref = -0.3,
  y_log = FALSE,
  ybreaks = seq(-0.2, 0.4, by = 0.05),
  y_tick_label = c(-0.2, 0, 0.2, 0.4),
  xlab = "4 × 3 × 3 = 36 ordered scenarios",
  outfile = here("Plots", "bias_gamma1_no_U_IRRE1_loop.png"),
  legend_inset = -0.25,
  plot_height = 5
)

### IRR
nested_loop_plot(
  data = results_noU1_reorder,
  yvars = c("IRR_E_hat_ignore_C",
            "IRR_E_hat_exclude_C",
            "IRR_E_hat_censor_C",
            "IRR_E_hat_adjust_C"),
  ylab = expression(hat(IRR[E])),
  refline = 1,
  ylim = c(0.4, 2),
  ymax_ref = 0.7,
  y_log = TRUE,
  ybreaks = seq(0.4, 2, by = 0.1),
  legend_pos = c(1, 2.2),
  outfile = here("Plots", "mean_IRRE_no_U_IRRE1_loop.png"),
  plot_height = 14
)

### IRR corresponding to average gamma_1

nested_loop_plot(
  data = results_noU1_reorder,
  yvars = c("exp_gamma_1_hat_ignore_C",
            "exp_gamma_1_hat_exclude_C",
            "exp_gamma_1_hat_censor_C",
            "exp_gamma_1_hat_adjust_C"),
  ylab = expression("Estimated " * IRR[E] *" corresponding to " * bar(gamma[1])),
  loop_label = c("Effect C\u2192E \n(OR = 1, 0.25, 2, 5)",
                 "Daily prob. C \n(0.0005, 0.001, 0.005)", "Duration C\u2192Y \n(10, 30, 60 days)"),
  refline = 1,
  ylim = c(0.3, 1.6),
  ymax_ref = 0.7,
  y_log = FALSE,
  ybreaks = seq(0.8, 1.6, by = 0.1),
  y_tick_label = seq(0.8, 1.6, by = 0.2),
  xlab = "4 × 3 × 3 = 36 ordered scenarios",
  outfile = here("Plots", "mean_IRRE_expg1_no_U_IRRE1_loop.png"),
  legend_inset = -0.25,
  plot_height = 5
)

### Variance 
nested_loop_plot(
  data = results_noU1_reorder,
  yvars = c("se_gamma_1_ignore_C",
            "se_gamma_1_exclude_C",
            "se_gamma_1_censor_C",
            "se_gamma_1_adjust_C"),
  ylab = expression("Empirical SE of "*hat(gamma[1])),
  refline = 0,
  ylim = c(-0.6, 0.8),
  ymax_ref = -0.1,
  y_log = FALSE,
  ybreaks = seq(0, 0.6, by = 0.1),
  legend_pos = c(1, 0.9),
  outfile = here("Plots", "empirical_se_gamma_1_no_U_IRRE1_loop.png"),
  plot_height = 14
)

## IRR_E = 2 ------------------------
results_noU2_reorder <- nestedloop(results_noU2, 
                                   varnames = c("ConE", "p_C", "risk_win_C"), #order of the loop
                                   varlabels = c("Effect of C on E (OR)", 
                                                 "Daily probability of C",  
                                                 "Risk window of C (days)"),
                                   sign = c(1, 1, 1)) # Order of the levels of each parameter, 
### Bias gamma
nested_loop_plot(
  data = results_noU2_reorder,
  yvars = c("bias_gamma_1_ignore_C",
            "bias_gamma_1_exclude_C",
            "bias_gamma_1_censor_C",
            "bias_gamma_1_adjust_C"),
  ylab = expression("Bias of " * gamma[1]),
  refline = 0,
  ylim = c(-0.7, 0.5),
  ymax_ref = -0.3,
  y_log = FALSE,
  ybreaks = seq(-0.6, 0.5, by = 0.05),
  legend_pos = c(1, 0.5),
  outfile = here("Plots", "bias_gamma1_no_U_IRRE2_loop.png"),
  plot_height = 14
)

### Relative bias
nested_loop_plot(
  data = results_noU2_reorder,
  yvars = c("relative_bias_g1_ignore_C",
            "relative_bias_g1_exclude_C",
            "relative_bias_g1_censor_C",
            "relative_bias_g1_adjust_C"),
  ylab = expression("Relative bias of " * gamma[1]),
  refline = 0,
  ylim = c(-0.9, 0.6),
  ymax_ref = -0.5,
  y_log = FALSE,
  ybreaks = seq(-0.6, 0.6, by = 0.05),
  legend_pos = c(1, 0.6),
  outfile = here("Plots", "relative_bias_gamma1_no_U_IRRE2_loop.png"),
  plot_height = 14
)

### IRR
nested_loop_plot(
  data = results_noU2_reorder,
  yvars = c("IRR_E_hat_ignore_C",
            "IRR_E_hat_exclude_C",
            "IRR_E_hat_censor_C",
            "IRR_E_hat_adjust_C"),
  ylab = expression(hat(IRR[E])),
  refline = 2,
  ylim = c(1.2, 2.6),
  ymax_ref = 1.5,
  y_log = TRUE,
  ybreaks = seq(1.4, 2.6, by = 0.1),
  legend_pos = c(1, 2.6),
  outfile = here("Plots", "mean_IRRE_no_U_IRRE2_loop.png"),
  plot_height = 14
)

### IRR corresponding to average gamma_1
nested_loop_plot(
  data = results_noU2_reorder,
  yvars = c("exp_gamma_1_hat_ignore_C",
            "exp_gamma_1_hat_exclude_C",
            "exp_gamma_1_hat_censor_C",
            "exp_gamma_1_hat_adjust_C"),
  ylab = expression("Estimated " * IRR[E] *" corresponding to " * bar(gamma[1])),
  refline = 2,
  ylim = c(1.2, 3),
  ymax_ref = 1.5,
  y_log = TRUE,
  ybreaks = seq(1.4, 3, by = 0.1),
  legend_pos = c(1, 3),
  outfile = here("Plots", "mean_IRRE_expg1_no_U_IRRE2_loop.png"),
  plot_height = 14
)

### Variance 
nested_loop_plot(
  data = results_noU2_reorder,
  yvars = c("se_gamma_1_ignore_C",
            "se_gamma_1_exclude_C",
            "se_gamma_1_censor_C",
            "se_gamma_1_adjust_C"),
  ylab = expression("Empirical SE of "*hat(gamma[1])),
  refline = 0,
  ylim = c(-0.6, 0.8),
  ymax_ref = -0.1,
  y_log = FALSE,
  ybreaks = seq(0, 0.6, by = 0.1),
  legend_pos = c(1, 0.9),
  outfile = here("Plots", "empirical_se_gamma_1_no_U_IRRE2_loop.png"),
  plot_height = 14
)

# 5.3. All scenarios ----------------------------------------------------------------------------

## Filter scenarios IRR_E = 1 and 2 
bias_results_wide_IRRE1 <-  bias_results_wide %>% filter(IRR_E == 1)
bias_results_wide_IRRE2 <-  bias_results_wide %>% filter(IRR_E == 2)

## Reorder the parameters for nested loop plot

bias_results_wide_IRRE1 <- bias_results_wide_IRRE1 %>%
  mutate(UonY_cat = as.numeric(factor(UonY, levels = c(1, 0.25, 2, 15))),
         ConE_cat = as.numeric(factor(ConE, level = c(1, 0.25, 2, 5))), 
         p_C_cat = as.numeric(as.factor(p_C)),
         risk_win_C_cat = as.numeric(as.factor(risk_win_C)))

bias_results_wide_IRRE1_reorder <- nestedloop(bias_results_wide_IRRE1, 
                                              varnames = c("UonY_cat", "ConE_cat", "p_C_cat", "risk_win_C_cat"), #order of the loop
                                              varlabels = c("Effect of U on Y (OR)",
                                                            "Effect of C on E (OR)", 
                                                            "Daily probability of C",  
                                                            "Risk window of C (days)"),
                                              sign = c(1, 1, 1, 1))

bias_results_wide_IRRE2 <- bias_results_wide_IRRE2 %>%
  mutate(UonY_cat = as.numeric(factor(UonY, levels = c(1, 0.25, 2, 15))),
         ConE_cat = as.numeric(factor(ConE, level = c(1, 0.25, 2, 5))), 
         p_C_cat = as.numeric(as.factor(p_C)),
         risk_win_C_cat = as.numeric(as.factor(risk_win_C)))

bias_results_wide_IRRE2_reorder <- nestedloop(bias_results_wide_IRRE2, 
                                               varnames = c("UonY_cat", "ConE_cat", "p_C_cat", "risk_win_C_cat"), #order of the loop
                                               varlabels = c("Effect of U on Y (OR)",
                                                             "Effect of C on E (OR)", 
                                                             "Daily probability of C",  
                                                             "Risk window of C (days)"),
                                               sign = c(1, 1, 1, 1))

## Create nested loop plot

### True IRR_E = 1 --------------------------
#### Bias gamma ----

nested_loop_plot(
  data = bias_results_wide_IRRE1_reorder,
  yvars = c("bias_gamma_1_ignore_C",
            "bias_gamma_1_exclude_C",
            "bias_gamma_1_censor_C",
            "bias_gamma_1_adjust_C"),
  ylab = expression("Bias of " * gamma[1]*" (log "* IRR[E]*")"),
  refline = 0,
  ylim = c(-1.2, 0.45),
  ymax_ref = -0.45,
  y_log = FALSE,
  ybreaks = seq(-0.5, 0.4, by = 0.1),
  y_tick_label = c(-0.4, -0.2, 0, 0.2, 0.4),
  xlab = "4 x 4 x 3 x 3 = 144 scenarios",
  legend_inset = -0.2,
  outfile = here("Plots", "bias_gamma1_all_scens_IRRE1_loop.png"),
  plot_height = 5
)

#### IRR
nested_loop_plot(
  data = bias_results_wide_IRRE1_reorder,
  yvars = c("IRR_E_hat_ignore_C",
            "IRR_E_hat_exclude_C",
            "IRR_E_hat_censor_C",
            "IRR_E_hat_adjust_C"),
  ylab = expression(hat(IRR[E])),
  refline = 1,
  ylim = c(0.4, 2),
  ymax_ref = 0.7,
  y_log = TRUE,
  ybreaks = seq(0.4, 2, by = 0.1),
  x_lab = "4 x 4 x 3 x 3 = 144 ordered scenarios",
  legend_pos = c(1, 2.2),
  outfile = here("Plots", "mean_IRRE_all_scens_IRRE1_loop.png"),
  plot_height = 12
)

### IRR corresponding to average gamma_1

nested_loop_plot(
  data = bias_results_wide_IRRE1_reorder,
  yvars = c("exp_gamma_1_hat_ignore_C",
            "exp_gamma_1_hat_exclude_C",
            "exp_gamma_1_hat_censor_C",
            "exp_gamma_1_hat_adjust_C"),
  ylab = expression(atop("Estimated " * IRR[E],
                         "corresponding to " * bar(gamma[1]))),
  refline = 1,
  ylim = c(0.2, 1.6),
  ymax_ref = 0.6,
  y_log = TRUE,
  ybreaks = seq(0.6, 1.6, by = 0.1),
  y_tick_label = seq(0.8, 1.6, by = 0.2),
  xlab = "4 x 4 x 3 x 3 = 144 ordered scenarios",
  legend_inset = -0.2,
  outfile = here("Plots", "mean_IRRE_expg1_all_scens_IRRE1_loop.png"),
  plot_height = 5
)

#### Mean squared error ----
nested_loop_plot(
  data = bias_results_wide_IRRE1_reorder,
  yvars = c("mse_g1_ignore_C",
            "mse_g1_exclude_C",
            "mse_g1_censor_C",
            "mse_g1_adjust_C"),
  ylab = expression("Mean squared error of "*gamma[1]),
  refline = 0,
  ylim = c(-0.6, 0.4),
  ymax_ref = -0.1,
  y_log = FALSE,
  ybreaks = seq(0, 0.4, by = 0.05),
  y_tick_label = seq(0, 0.4, by = 0.2),
  xlab = "4 x 4 x 3 x 3 = 144 ordered scenarios",
  legend_inset = -0.2,
  outfile = here("Plots", "mse_all_scens_IRRE1_loop.png"),
  plot_height = 5
)


#### Coverage ----

nested_loop_plot(
  data = bias_results_wide_IRRE1_reorder,
  yvars = c("coverage_irr_E_ignore_C",
            "coverage_irr_E_exclude_C",
            "coverage_irr_E_censor_C",
            "coverage_irr_E_adjust_C"),
  ylab = expression("Coverage of 95% CI of " * hat(IRR[E])),
  refline = 0.95,
  ylim = c(0, 1.2),
  ymax_ref = 0.5,
  y_log = FALSE,
  ybreaks = seq(0, 1, by = 0.1),
  x_lab = "4 x 4 x 3 x 3 = 144 ordered scenarios",
  legend_pos = c(1, 1.3),
  outfile = here("Plots", "coverage_all_scens_IRRE1_loop.png"),
  plot_height = 13
)

 

### True IRR_E = 2 --------------------------
#### Bias gamma ----

nested_loop_plot(
  data = bias_results_wide_IRRE2_reorder,
  yvars = c("bias_gamma_1_ignore_C",
            "bias_gamma_1_exclude_C",
            "bias_gamma_1_censor_C",
            "bias_gamma_1_adjust_C"),
  ylab = expression("Bias of " * gamma[1]*" (log "* IRR[E]*")"),
  refline = 0,
  ylim = c(-1.1, 0.45),
  ymax_ref = -0.45,
  y_log = FALSE,
  ybreaks = seq(-0.4, 0.4, by = 0.1),
  y_tick_label = c(-0.4, -0.2, 0, 0.2, 0.4),
  xlab = "4 x 4 x 3 x 3 = 144 scenarios",
  outfile = here("Plots", "bias_gamma1_all_scens_IRRE2_loop.png")
)

#### Relative bias ----
nested_loop_plot(
  data = bias_results_wide_IRRE2_reorder,
  yvars = c("relative_bias_g1_ignore_C",
            "relative_bias_g1_exclude_C",
            "relative_bias_g1_censor_C",
            "relative_bias_g1_adjust_C"),
  ylab = expression("Relative bias of " * gamma[1]),
  refline = 0,
  ylim = c(-1.5, 0.7),
  ymax_ref = -0.6,
  y_log = FALSE,
  ybreaks = seq(-1, 0.7, by = 0.05),
  x_lab = "4 x 4 x 3 x 3 = 144 ordered scenarios",
  legend_pos = c(1, 0.8),
  outfile = here("Plots", "relative_bias_gamma1_all_scens_IRRE2_loop.png"),
  plot_height = 14
)


#### IRR ----
nested_loop_plot(
  data = bias_results_wide_IRRE2_reorder,
  yvars = c("IRR_E_hat_ignore_C",
            "IRR_E_hat_exclude_C",
            "IRR_E_hat_censor_C",
            "IRR_E_hat_adjust_C"),
  ylab = expression(hat(IRR[E])),
  refline = 2,
  ylim = c(1, 3.2),
  ymax_ref = 1.5,
  y_log = TRUE,
  ybreaks = seq(1, 3.2, by = 0.1),
  x_lab = "4 x 4 x 3 x 3 = 144 ordered scenarios",
  legend_pos = c(1, 3.4),
  outfile = here("Plots", "mean_IRRE_all_scens_IRRE2_loop.png"),
  plot_height = 14
)

### IRR corresponding to average gamma_1 ----

nested_loop_plot(
  data = bias_results_wide_IRRE2_reorder,
  yvars = c("exp_gamma_1_hat_ignore_C",
            "exp_gamma_1_hat_exclude_C",
            "exp_gamma_1_hat_censor_C",
            "exp_gamma_1_hat_adjust_C"),
  ylab = expression(atop("Estimated " * IRR[E],
                     "corresponding to " * bar(gamma[1]))),
  refline = 2,
  ylim = c(0.8, 2.9),
  ymax_ref = 1.2,
  y_log = TRUE,
  ybreaks = seq(1.3, 3.1, by = 0.1),
  y_tick_label = seq(1.4, 3.0, by = 0.2),
  xlab = "4 x 4 x 3 x 3 = 144 ordered scenarios",
  outfile = here("Plots", "mean_IRRE_expg1_all_scens_IRRE2_loop.png"),
)

#### Mean squared error ----
nested_loop_plot(
  data = bias_results_wide_IRRE2_reorder,
  yvars = c("mse_g1_ignore_C",
            "mse_g1_exclude_C",
            "mse_g1_censor_C",
            "mse_g1_adjust_C"),
  ylab = expression("Mean squared error of "*gamma[1]),
  refline = 0,
  ylim = c(-0.6, 0.3),
  ymax_ref = -0.1,
  y_log = FALSE,
  ybreaks = seq(0, 0.3, by = 0.05),
  y_tick_label = seq(0, 0.3, by = 0.1),
  xlab = "4 x 4 x 3 x 3 = 144 ordered scenarios",
  xlab_line   = 0.5,
  legend_inset = -0.15,
  outfile = here("Plots", "mse_all_scens_IRRE2_loop.png"),
  plot_height = 5
)


#### Coverage ----

nested_loop_plot(
  data = bias_results_wide_IRRE2_reorder,
  yvars = c("coverage_irr_E_ignore_C",
            "coverage_irr_E_exclude_C",
            "coverage_irr_E_censor_C",
            "coverage_irr_E_adjust_C"),
  ylab = expression("Coverage of 95% CI of " * hat(IRR[E])),
  refline = 0.95,
  ylim = c(0, 1.2),
  ymax_ref = 0.5,
  y_log = FALSE,
  ybreaks = seq(0, 1, by = 0.1),
  x_lab = "4 x 4 x 3 x 3 = 144 ordered scenarios",
  legend_pos = c(1, 1.3),
  outfile = here("Plots", "coverage_all_scens_IRRE2_loop.png"),
  plot_height = 13
)


### Number of events ----------------------------

nested_loop_plot(
  data = bias_results_wide_IRRE2_reorder,
  yvars = c("mse_g1_ignore_C",
            "mse_g1_exclude_C",
            "mse_g1_censor_C",
            "mse_g1_adjust_C"),
  ylab = expression("Mean squared error of "*gamma[1]),
  refline = 0,
  ylim = c(-0.6, 0.3),
  ymax_ref = -0.1,
  y_log = FALSE,
  ybreaks = seq(0, 0.3, by = 0.05),
  y_tick_label = seq(0, 0.3, by = 0.1),
  xlab = "4 x 4 x 3 x 3 = 144 ordered scenarios",
  legend_inset = -0.2,
  outfile = here("Plots", "mse_all_scens_IRRE2_loop.png"),
  plot_height = 5
)

nested_loop_plot(
  data = bias_results_wide_IRRE1_reorder,
  yvars = c("mean_nr_event_ignore_C",
            "mean_nr_event_exclude_C",
            "mean_nr_event_censor_C",
            "mean_nr_event_adjust_C"),
  ylab = "Mean number of events",
  refline = NULL,
  ylim = c(-2000, 5600),
  ymax_ref = -100,
  y_log = FALSE,
  ybreaks = seq(0, 5600, by = 500),
  x_lab = "4 x 4 x 3 x 3 = 144 ordered scenarios",
  legend_pos = c(1, 5600),
  outfile = here("Plots", "nr_event_all_scens_IRRE1_loop.png"),
  plot_height = 15,
  plot_width = 25
)

nested_loop_plot(
  data = bias_results_wide_IRRE2_reorder,
  yvars = c("mean_nr_event_ignore_C",
            "mean_nr_event_exclude_C",
            "mean_nr_event_censor_C"),
  ylab = "Mean number of events",
  refline = NULL,
  ylim = c(-3000, 5600),
  ymax_ref = -100,
  y_log = FALSE,
  ybreaks = seq(0, 5600, by = 200),
  y_tick_label = seq(0, 5400, by = 600),
  xlab = "4 x 4 x 3 x 3 = 144 ordered scenarios",
  legend_labels = c("Ignore C/ Adjust for C", 
                    "Exclude C",
                    "Censor C"),
  xlab_line   = 0.5,
  legend_inset = -0.15,
  outfile = here("Plots", "nr_event_all_scens_IRRE2_loop.png"),
  plot_height = 5
)


# 5.4. Zoom in UonY = 15 --------------------------------------------------
bias_results_wide_UY1_15_IRRE1_reorder <- bias_results_wide_IRRE1_reorder %>% 
  filter (UonY %in% c(1, 15))

bias_results_wide_UY1_15_IRRE2_reorder <- bias_results_wide_IRRE2_reorder %>% 
  filter (UonY %in% c(1, 15))

## IRR
nested_loop_plot(
  data = bias_results_wide_UY1_15_IRRE1_reorder,
  yvars = c("IRR_E_hat_exclude_C",
            "IRR_E_hat_censor_C",
            "IRR_E_hat_adjust_C"),
  ylab = expression(hat(IRR[E])),
  refline = 1,
  ylim = c(0.4, 1.8),
  ymax_ref = 0.8,
  y_log = TRUE,
  ybreaks = seq(0.4, 1.8, by = 0.1),
  x_lab = "2x 4 x 3 x 3 = 36 ordered scenarios",
  legend_labels = c("Exclude C", "Censor C", "Adjust for C"),
  colours = c("#2f357c", "#e69b00", "#999999"),
  legend_pos = c(1, 2),
  outfile = here("Plots", "mean_IRRE_UY1_15_IRRE1_loop.png"),
  plot_height = 14
)

nested_loop_plot(
  data = bias_results_wide_UY1_15_IRRE2_reorder,
  yvars = c("IRR_E_hat_exclude_C",
            "IRR_E_hat_censor_C",
            "IRR_E_hat_adjust_C"),
  ylab = expression(hat(IRR[E])),
  refline = 2,
  ylim = c(1.2, 2.8),
  ymax_ref = 1.8,
  y_log = TRUE,
  ybreaks = seq(1.2, 2.8, by = 0.1),
  x_lab = "2x 4 x 3 x 3 = 36 ordered scenarios",
  legend_labels = c("Exclude C", "Censor C", "Adjust for C"),
  colours = c("#2f357c", "#e69b00", "#999999"),
  legend_pos = c(1, 2.9),
  outfile = here("Plots", "mean_IRRE_UY1_15_IRRE2_loop.png"),
  plot_height = 14
)

## Coverage
nested_loop_plot(
  data = bias_results_wide_UY1_15_IRRE1_reorder,
  yvars = c("coverage_irr_E_exclude_C",
            "coverage_irr_E_censor_C",
            "coverage_irr_E_adjust_C"),
  ylab = expression("Coverage of 95% CI of " * hat(IRR[E])),
  refline = 0.95,
  ylim = c(0.4, 1.2),
  ymax_ref = 0.8,
  y_log = FALSE,
  ybreaks = seq(0.4, 1, by = 0.1),
  x_lab = "2x 4 x 3 x 3 = 36 ordered scenarios",
  legend_labels = c("Exclude C", "Censor C", "Adjust for C"),
  colours = c("#2f357c", "#e69b00", "#999999"),  
  legend_pos = c(1, 1.2),
  outfile = here("Plots", "coverage_UY1_15_IRRE1_loop.png"),
  plot_height = 13
)

nested_loop_plot(
  data = bias_results_wide_UY1_15_IRRE2_reorder,
  yvars = c("coverage_irr_E_exclude_C",
            "coverage_irr_E_censor_C",
            "coverage_irr_E_adjust_C"),
  ylab = expression("Coverage of 95% CI of " * hat(IRR[E])),
  refline = 0.95,
  ylim = c(0.4, 1.2),
  ymax_ref = 0.8,
  y_log = FALSE,
  ybreaks = seq(0.4, 1, by = 0.1),
  x_lab = "2x 4 x 3 x 3 = 36 ordered scenarios",
  legend_labels = c("Exclude C", "Censor C", "Adjust for C"),
  colours = c("#2f357c", "#e69b00", "#999999"), 
  legend_pos = c(1, 1.2),
  outfile = here("Plots", "coverage_UY1_15_IRRE2_loop.png"),
  plot_height = 13
)
