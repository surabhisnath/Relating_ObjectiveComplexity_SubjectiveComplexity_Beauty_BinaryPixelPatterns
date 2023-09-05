## Author: Surabhi S Nath
## Description: This script implements mixed effects models.
## on complexity and beauty ratings.
## Uses process.R which should be running in the background
## Helper functions and code for plotting in utils/Utils.R
## Things printed on the terminal can be largely ignored.
## Important things are saved in plots/ and model_fits/:
## tables written to model_fits/
## plots saved in plots/

# Imports
{
  library(lme4)
  library(ggplot2)
  library(interactions)
  library(dplyr)
  library(mediation)
  source("utils/Utils.R")

  # Set seed 
  set.seed(20) # Seed set randomly for reproducibility
}

# Setup
{
  # Read data
  data <- read.csv("utils/stat_analysis.csv")

  # Remove people who failed all attention checks
  all_attention_failed <- c("ls1hg9ya", "jjs7ytws")
  data <- data[!data$subject %in% all_attention_failed, ]

  # Scale all variables in the data
  my_scale <- function(x) {
    as.numeric(scale(x))
  }

  data$rtime <- data$rtime / 1000
  data$subject <- factor(data$subject)
  data$set <- factor(data$set, levels = c(1, 2, 3, 4))
  data$pattern <- factor(data$pattern)
  data$trial <- my_scale(data$trial)
  data$complexity_rating <- my_scale(data$complexity_rating)
  data$previous_complexity_rating <- my_scale(data$previous_complexity_rating)
  data$complexity_rating_sq <- my_scale(data$complexity_rating^2)
  data$beauty_rating <- my_scale(data$beauty_rating)
  data$previous_beauty_rating <- my_scale(data$previous_beauty_rating)
  data$LSCsq <- my_scale(data$LSC^2)
  data$LSC <- my_scale(data$LSC)
  data$intricacy_4sq <- my_scale(data$intricacy_4^2)
  data$intricacy_8sq <- my_scale(data$intricacy_8^2)
  data$intricacy_4 <- my_scale(data$intricacy_4)
  data$intricacy_8 <- my_scale(data$intricacy_8)
  data$quadtree <- my_scale(data$quadtree)
  data$local_asymm <- my_scale(data$local_asymm)
  data$Hasymm <- my_scale(data$Hasymm)
  data$Vasymm <- my_scale(data$Vasymm)
  data$asymm <- my_scale((data$Hasymm + data$Vasymm) / 2)
  data$entropy <- my_scale(data$entropy)
  data$mean_entropy <- my_scale(data$mean_entropy)
  data$densitysq <- my_scale(data$density^2)
  data$density <- my_scale(data$density)
  data$neighbourhood_size <- factor(data$neighbourhood_size)
  data$tot_outertot <- factor(data$tot_outertot)
  data$IC <- factor(data$IC)
  data$num_active_rules <- my_scale(data$num_active_rules)
}

# Stratified data sampling to create 3 folds
{
  num_folds <- 3

  # Create fold 1
  data_test_fold1 <- data %>%
    group_by(subject) %>%
    sample_n(size = 20)
  data_train_fold1 <- setdiff(data, data_test_fold1)

  # Subtract all data from fold 1 to get fold 2 and 3
  data_test_fold23 <- setdiff(data, data_test_fold1)

  # Create fold 2
  data_test_fold2 <- data_test_fold23 %>%
    group_by(subject) %>%
    sample_n(size = 20)
  data_train_fold2 <- setdiff(data, data_test_fold2)

  # Create fold 3
  data_test_fold3 <- setdiff(data_test_fold23, data_test_fold2)
  data_train_fold3 <- setdiff(data, data_test_fold3)
}

# Notes:
# participant in paper is subject here.
# intricacy from paper is intricacy_4 here.
# multiscale_entropy from paper is mean_entropy here.

############### COMPLEXITY MODELS ##############

# List of complexity models reported in the paper
models_in_paper_complexity <- list(
  "1 + (1 | subject)",
  "1 + (1 | subject) + (1 | set)",
  "LSC + (1 | subject)",
  "LSC + (LSC | subject)",
  "KC + (1 | subject)",
  "LSC + density + (1 | subject)",
  "LSC + entropy + (1 | subject)",
  "LSC + mean_entropy + (1 | subject)",
  "LSC + asymm + (1 | subject)",
  "LSC + intricacy_4 + (1 | subject)",
  "LSC * intricacy_4 + (1 | subject)",
  "LSC + intricacy_4 + (LSC | subject)",
  "LSC + intricacy_4 + (intricacy_4 | subject)",
  "LSC + intricacy_4 + ((LSC + intricacy_4) | subject)",
  "LSC * intricacy_4 + ((LSC + intricacy_4) | subject)",
  "LSCsq + intricacy_4sq + (1 | subject)",
  "LSC + LSCsq + intricacy_4 + intricacy_4sq + (1 | subject)"
)

# List of all models of complexity - Table AIII.1
all_models_complexity <- list(
  "1 + (1 | subject)",
  "1 + (1 | subject) + (1 | set)",
  "density + (1 | subject)",
  "entropy + (1 | subject)",
  "mean_entropy + (1 | subject)",
  "H_2 + (1 | subject)",
  "H_mean + (1 | subject)",
  "LSC + (1 | subject)",
  "KC + (1 | subject)",
  "asymm + (1 | subject)",
  "intricacy_4 + (1 | subject)",
  "intricacy_8 + (1 | subject)",
  "quadtree + (1 | subject)",
  "LSC + density + (1 | subject)",
  "LSC + entropy + (1 | subject)",
  "LSC + mean_entropy + (1 | subject)",
  "LSC + H_2 + (1 | subject)",
  "LSC + asymm + (1 | subject)",
  "LSC + intricacy_4 + (1 | subject)",
  "LSC + intricacy_8 + (1 | subject)",
  "LSC + quadtree + (1 | subject)",
  "LSC + H_2 + intricacy_4 + (1 | subject)",
  "LSC + intricacy_4 + intricacy_8 + (1 | subject)",
  "LSC * intricacy_4 + (1 | subject)",
  "LSC + intricacy_4 + (LSC | subject)",
  "LSC + intricacy_4 + (intricacy_4 | subject)",
  "LSC + intricacy_4 + ((LSC + intricacy_4) | subject)",
  "LSC * intricacy_4 + ((LSC + intricacy_4) | subject)",
  "H_2 + intricacy_4 + (1 | subject)",
  "H_2 + intricacy_4 + ((H_2 + intricacy_4) | subject)",
  "H_2 * intricacy_4 + ((H_2 + intricacy_4) | subject)",
  "quadtree + intricacy_4 + (1 | subject)",
  "quadtree + intricacy_4 + ((quadtree + intricacy_4) | subject)",
  "quadtree * intricacy_4 + ((quadtree + intricacy_4) | subject)",
  "LSCsq + intricacy_4sq + (1 | subject)",
  "LSC + LSCsq + intricacy_4 + intricacy_4sq + (1 | subject)",
  "neighbourhood_size + tot_outertot + IC + (1 | subject)",
  "trial + (1 | subject)",
  "LSC + intricacy_4 + trial + ((LSC + intricacy_4) | subject)",
  "previous_complexity_rating + (1 | subject)",
  "LSC + intricacy_4 + previous_complexity_rating + 
  ((LSC + intricacy_4) | subject)"
)

# Analyze a subset of the complexity models - Table 1
# Save all results to model_fits/Table_1A_models_complexity.csv
{
  df <- data.frame(matrix(ncol = 13, nrow = 0, dimnames =
  list(NULL, c("Id", "model", "AIC", "BIC", "AIC/BIC Var",
  "Rsq train mean", "Rsq train var", "Rsq test mean", "Rsq test var",
  "RMSE train mean", "RMSE train var", "RMSE test mean", "RMSE test var"))))

  id <- 0
  for (formula in models_in_paper_complexity) {
    # can also run for all_models instead
    # by replacing models_in_paper_complexity by all_models_complexity

    id <- id + 1
    fullformula <- paste("complexity_rating ~", formula)
    f1 <- lmer(fullformula, data = data_train_fold1,
    control = lmerControl(optimizer = "bobyqa"))
    f2 <- lmer(fullformula, data = data_train_fold2,
    control = lmerControl(optimizer = "bobyqa"))
    f3 <- lmer(fullformula, data = data_train_fold3,
    control = lmerControl(optimizer = "bobyqa"))

    metrics <- modelanalysis("complexity", num_folds,
    list(f1, f2, f3), list(data_train_fold1, data_train_fold2,
    data_train_fold3), list(data_test_fold1, data_test_fold2, data_test_fold3),
    FALSE, FALSE, fullformula) # set second last param to TRUE for printing
    df[nrow(df) + 1, ] <- c(id, noquote(fullformula), metrics)
  }

  write.csv(df, "model_fits/Table_1A_models_complexity.csv", row.names = FALSE)
}

# Print performance of best model and make plots
# for train, test and random effects
# Figure 7, Figure AIII.7(A)
# Plots saved to ./plots/
{
  bestformula <- "LSC + intricacy_4 + ((LSC + intricacy_4) | subject)"
  bestfullformula <- paste("complexity_rating ~", bestformula)
  f1 <- lmer(bestfullformula, data = data_train_fold1,
  control = lmerControl(optimizer = "bobyqa"))
  f2 <- lmer(bestfullformula, data = data_train_fold2,
  control = lmerControl(optimizer = "bobyqa"))
  f3 <- lmer(bestfullformula, data = data_train_fold3,
  control = lmerControl(optimizer = "bobyqa"))
  f <- lmer(bestfullformula, data = data_train_fold1,
  control = lmerControl(optimizer = "bobyqa"))

  # Plots data vs predictions on train and test data - Figure 66
  metrics <- modelanalysis("complexity", num_folds,
  list(f1, f2, f3), list(data_train_fold1, data_train_fold2,
  data_train_fold3), list(data_test_fold1, data_test_fold2, data_test_fold3),
  FALSE, TRUE)

  # Plots random effects - Figure AIII.7(a)
  ggCaterpillar(ranef(f, condVar = TRUE))
  ggsave("plots/Figure_AIII_7A_random_effects_complexity.pdf")
}

# Save fixed effects - Table 1B
{
  write.csv(fixef(f), file = "model_fits/Table_1B_fixed_effects_complexity.csv")
}

# Make plots for only LSC and only intricacy - Figure AIII.2
# Plots saved to ./plots/
{
  formula_lsc <- "LSC + (1 | subject)"
  fullformula_lsc <- paste("complexity_rating ~", formula_lsc)
  model_lsc <- lmer(fullformula_lsc, data = data_train_fold1)
  plot_model(model_lsc, "Figure_AIII_2A_complexity_train_test_LSC.pdf",
  data_train_fold1, data_test_fold1)

  formula_intricacy <- "intricacy_4 + (1 | subject)"
  fullformula_intricacy <- paste("complexity_rating ~", formula_intricacy)
  model_intricacy <- lmer(fullformula_intricacy, data = data_train_fold1)
  plot_model(model_intricacy, "Figure_AIII_2B_complexity_train_test_intricacy.pdf",
  data_train_fold1, data_test_fold1)

  formula_both <- "LSC + intricacy_4 + (1 | subject)"
  fullformula_both <- paste("complexity_rating ~", formula_both)
  model_both <- lmer(fullformula_both, data = data_train_fold1)
  plot_model(model_both, "Figure_AIII_2C_complexity_train_test_both.pdf",
  data_train_fold1, data_test_fold1)
}

# Write objective complexity, with and without random intercepts/slopes as columns onto data for mediation analysis (performed below)
{
  data$obj_comp <- (fixef(f)["LSC"] * data$LSC) +
  (fixef(f)["intricacy_4"] * data$intricacy_4) + fixef(f)["(Intercept)"]
  ranef <- ranef(f)
  ranefintercept <- ranef$subject[, "(Intercept)"]
  ranef_lsc <- ranef$subject[, "LSC"]
  ranefintricacy <- ranef$subject[, "intricacy_4"]
  data$randint <- rep(ranefintercept, each = 60)
  data$randslope_lsc <- rep(ranef_lsc, each = 60)
  data$randslope_intricacy <- rep(ranefintricacy, each = 60)
  data$obj_comp_withrandint <- data$obj_comp + data$randint
  data$obj_comp_withrandint_withrandslopes <- ((fixef(f)["LSC"] +
  data$randslope_lsc) * data$LSC) + ((fixef(f)["intricacy_4"] +
  data$randslope_intricacy) * data$intricacy_4) +
  fixef(f)["(Intercept)"] + data$randint
}

# Get pattern numbers with maximum and minimum discrepency
# visualised in DescriptiveAnalysis.ipynb
{
  data$discrepancy <- my_scale(data$obj_comp_withrandint_withrandslopes
  - data$complexity_rating)
  mean_pattern_disc <- aggregate(data$discrepancy, list(data$pattern), mean)
  inc_disc <- mean_pattern_disc[order(mean_pattern_disc$x), ]
  print("Max Discrepancy Patterns:")
  print(tail(inc_disc, 8)$Group.1) # pattern numbers where
                                    # complexity is overestimated,
                                    # ie, discrepancy is the largest
  print(tail(inc_disc, 8)$x)
  print("Min Discrepancy Patterns:")
  print(head(inc_disc, 8)$Group.1)  # patterns numbers where
                                    # complexity is underestimated,
                                    # ie, discrepancy is the smallest
  print(head(inc_disc, 8)$x)
}

############### BEAUTY MODELS ###############

# Create the disorder variable and recreate the folds
{
  formula <- "complexity_rating + asymm + entropy + (1 | subject)"
  fullformula <- paste("beauty_rating ~", formula)
  f <- lmer(fullformula, data = data_train_fold1)
  # add disorder (asymm + entropy) to data
  data$disorder <- my_scale((abs(fixef(f)["asymm"]) *
  data$asymm) + (abs(fixef(f)["entropy"]) * data$entropy))

  formula <- "complexity_rating + asymm + mean_entropy + (1 | subject)"
  fullformula <- paste("beauty_rating ~", formula)
  f <- lmer(fullformula, data = data_train_fold1)
  data$disorder_2 <- my_scale((abs(fixef(f)["asymm"]) *
  data$asymm) + (abs(fixef(f)["mean_entropy"]) * data$mean_entropy)) # disorder_2 uses mean_entropy 
  
  formula <- "complexity_rating + local_asymm + entropy + (1 | subject)"
  fullformula <- paste("beauty_rating ~", formula)
  f <- lmer(fullformula, data = data_train_fold1)
  data$disorder_3 <- my_scale((abs(fixef(f)["local_asymm"]) *
  data$local_asymm) + (abs(fixef(f)["entropy"]) * data$entropy)) # disorder_3 uses local_asymm
  
  formula <- "complexity_rating + local_asymm + mean_entropy + (1 | subject)"
  fullformula <- paste("beauty_rating ~", formula)
  f <- lmer(fullformula, data = data_train_fold1)
  data$disorder_4 <- my_scale((abs(fixef(f)["local_asymm"]) *
  data$local_asymm) + (abs(fixef(f)["mean_entropy"]) * data$mean_entropy)) # disorder_4 uses both mean_entropy and local_asymm

  # recreate the folds
  data_test_fold1 <- data %>%
    group_by(subject) %>%
    sample_n(size = 20)
  data_train_fold1 <- setdiff(data, data_test_fold1)

  data_test_fold23 <- setdiff(data, data_test_fold1)

  data_test_fold2 <- data_test_fold23 %>%
    group_by(subject) %>%
    sample_n(size = 20)
  data_train_fold2 <- setdiff(data, data_test_fold2)

  data_test_fold3 <- setdiff(data_test_fold23, data_test_fold2)
  data_train_fold3 <- setdiff(data, data_test_fold3)
}

{
  # List of beauty models reported in the paper
  models_in_paper_beauty <- list(
    "complexity_rating + (1 | subject)",
    "complexity_rating + disorder + (1 | subject)",
    "complexity_rating + disorder + (disorder | subject)",
    "complexity_rating + disorder + (complexity_rating | subject)",
    "complexity_rating + disorder + ((disorder + complexity_rating) | subject)",
    "complexity_rating * disorder + (1 | subject)",
    "complexity_rating * disorder + ((complexity_rating + disorder) | subject)",
    "complexity_rating_sq + disorder + (1 | subject)",
    "complexity_rating + complexity_rating_sq + disorder + (1 | subject)",
    "obj_comp + disorder + (1 | subject)",
    "obj_comp + disorder + ((obj_comp + disorder) | subject)",
    "obj_comp * disorder + ((obj_comp + disorder) | subject)"
  )

  # List of all models of beauty - Table AIII.2
  all_models_beauty <- list(
    "complexity_rating + (1 | subject)",
    "complexity_rating + asymm + entropy + (1 | subject)",
    "complexity_rating + asymm + mean_entropy + (1 | subject)",
    "complexity_rating + disorder + (1 | subject)",
    "complexity_rating + disorder + (disorder | subject)",
    "complexity_rating + disorder + (complexity_rating | subject)",
    "complexity_rating + disorder + ((complexity_rating + disorder) | subject)",
    "complexity_rating * disorder + (1 | subject)",
    "complexity_rating * disorder + (disorder | subject)",
    "complexity_rating * disorder + (complexity_rating | subject)",
    "complexity_rating * disorder + ((complexity_rating + disorder) | subject)",
    "complexity_rating * disorder_2 + ((complexity_rating + disorder_2) | subject)",
    "complexity_rating * disorder_3 + ((complexity_rating + disorder_3) | subject)",
    "complexity_rating * disorder_4 + ((complexity_rating + disorder_4) | subject)",
    "complexity_rating_sq + disorder + (1 | subject)",
    "complexity_rating + complexity_rating_sq + disorder + (1 | subject)",
    "LSC + intricacy_4 + asymm + entropy + (1 | subject)",
    "LSC + intricacy_4 + disorder + (1 | subject)",
    "obj_comp + asymm + entropy + (1 | subject)",
    "obj_comp + disorder + (1 | subject)",
    "obj_comp + disorder + (obj_comp | subject)",
    "obj_comp + disorder + (disorder | subject)",
    "obj_comp + disorder + ((obj_comp + disorder) | subject)",
    "obj_comp * disorder + ((obj_comp + disorder) | subject)",
    "trial + (1 | subject)",
    "complexity_rating * disorder + trial + ((complexity_rating + disorder) | subject)",
    "previous_beauty_rating + (1 | subject)",
    "complexity_rating * disorder + previous_beauty_rating + ((complexity_rating + disorder) | subject)",
    "complexity_rating * trial + (1 | subject)"
  )
}

{
  df <- data.frame(matrix(ncol = 13, nrow = 0, dimnames = 
  list(NULL, c("Id", "model", "AIC", "BIC", "AIC/BIC Var", "Rsq train mean",
  "Rsq train var", "Rsq test mean", "Rsq test var",
  "RMSE train mean", "RMSE train var", "RMSE test mean", "RMSE test var"))))

  id <- 0
  # Analyze a subset of the beauty models - Table 2
  # Save all results to model_fits/Table_2A_models_beauty.csv

  for (formula in models_in_paper_beauty) {
    # can also run for all_models instead
    # by replacing models_in_paper_beauty by all_models_beauty
    id <- id + 1
    fullformula <- paste("beauty_rating ~", formula)
    f1 <- lmer(fullformula, data = data_train_fold1,
    control = lmerControl(optimizer = "bobyqa"))
    f2 <- lmer(fullformula, data = data_train_fold2,
    control = lmerControl(optimizer = "bobyqa"))
    f3 <- lmer(fullformula, data = data_train_fold3,
    control = lmerControl(optimizer = "bobyqa"))
    metrics <- modelanalysis("beauty", num_folds,
    list(f1, f2, f3), list(data_train_fold1, data_train_fold2,
    data_train_fold3), list(data_test_fold1, data_test_fold2, data_test_fold3),
    FALSE, FALSE, fullformula) # set sencond last param to TRUE for printing
    df[nrow(df) + 1, ] <- c(id, noquote(fullformula), c(metrics))
  }

  write.csv(df, "model_fits/Table_2A_models_beauty.csv", row.names = FALSE)
}

# Print performance of best model and make plots
# of train, test and random effects
# Figure 9, Figure AIII.7(B)
# Plots saved to ./plots/
{
  bestformula <- "complexity_rating * disorder + ((complexity_rating + disorder) | subject)"
  bestfullformula <- paste("beauty_rating ~", bestformula)
  f1 <- lmer(bestfullformula, data = data_train_fold1)
  f2 <- lmer(bestfullformula, data = data_train_fold2)
  f3 <- lmer(bestfullformula, data = data_train_fold3)
  f <- lmer(bestfullformula, data = data_train_fold1)

  # Plots data vs predictions on train and test data - Figure 8
  metrics <- modelanalysis("beauty", num_folds,
  list(f1, f2, f3), list(data_train_fold1, data_train_fold2,
  data_train_fold3), list(data_test_fold1, data_test_fold2, data_test_fold3),
  FALSE, TRUE)

  # Plots random effects - Figure AIII.7(b)
  ggCaterpillar(ranef(f, condVar = TRUE))
  ggsave("plots/Figure_AIII_7B_random_effects_beauty.pdf")
}

# Save fixed effects - Table 2B
{
  write.csv(fixef(f), file = "model_fits/Table_2B_fixed_effects_beauty.csv")
}

# Create interaction plot
# Figure 10
# Plot saved to ./plots/
{
  p <- interact_plot(f,
      pred = disorder, modx = complexity_rating, interval = TRUE,
      x.label = "Disorder", y.label = "Beauty Ratings",
      main.title = "Interaction Plot for Disorder x Complexity Ratings",
      legend.main = "Complexity Ratings", colors = "seagreen"
  ) + theme(
      plot.title = element_text(family = "serif", size = 16),
      axis.title = element_text(family = "serif", size = 20),
      axis.text = element_text(family = "serif", size = 14),
      legend.text = element_text(family = "serif", size = 15),
      legend.title = element_text(family = "serif", size = 18),
      strip.text = element_text(family = "serif")
  )

  # Save the plot
  ggsave(filename = "plots/Figure_10_complexity_order_interaction_for_beauty.pdf", plot = p, width = 10, height = 10, units = "in")
}


# Get patterns at interaction extremes for Figure 10
{
  mean_pattern <- aggregate(data, list(data$pattern), mean)
  mean_pattern_bydis <- mean_pattern[order(mean_pattern$disorder), ]

  # top 10% elements of mean_pattern_bydisorder
  num_dis <- 22
  high_dis <- tail(mean_pattern_bydis, num_dis)
  # bottom 15 elements of mean_pattern_dis
  low_dis <- head(mean_pattern_bydis, num_dis)
  
  high_dis_by_comp <- high_dis[order(high_dis$complexity_rating), ]
  low_dis_by_comp <- low_dis[order(low_dis$complexity_rating), ]
  mid_start <- (num_dis - 2) %/% 2 + 1
  mid_end <- mid_start + 2
  high_dis_low_comp <- head(high_dis_by_comp, 3)
  high_dis_mid_comp <- high_dis_by_comp[mid_start:mid_end,]
  high_dis_high_comp <- tail(high_dis_by_comp, 3)
  low_dis_low_comp <- head(low_dis_by_comp, 3)
  low_dis_mid_comp <- low_dis_by_comp[mid_start:mid_end,]
  low_dis_high_comp <- tail(low_dis_by_comp, 3)

  print(high_dis_low_comp$Group.1)
  print(high_dis_mid_comp$Group.1)
  print(high_dis_high_comp$Group.1)
  print(low_dis_low_comp$Group.1)
  print(low_dis_mid_comp$Group.1)
  print(low_dis_high_comp$Group.1)
}


############ MEDIATION ANALYSIS #############

# Run porcess.R
# Requires process.R from Process running in the background
{
  process(data = data_train_fold2, y = "beauty_rating",
  x = "obj_comp_withrandint_withrandslopes", m = "complexity_rating",
  w = "disorder", model = 15, center = 2, moments = 1, modelbt = 1,
  boot = 10000, seed = 20, jn = 1)
}

# Mediation analysis - Table 4
{
  model.0 <- lmer(beauty_rating ~ obj_comp +
  disorder + ((obj_comp + disorder) | subject), data = data_test_fold1)
  model.M <- lmer(complexity_rating ~ obj_comp +
  (obj_comp | subject), data = data_test_fold1)
  model.Y <- lmer(beauty_rating ~ complexity_rating + obj_comp +
  disorder + ((complexity_rating + obj_comp + disorder) | subject), data = data_test_fold1)

  results <- mediate(model.M, model.Y,
    treat = "obj_comp", mediator = "complexity_rating",
    boot = FALSE, sims = 500
  )

  capture.output(summary(results), file = "model_fits/Table_4_mediationanalysis.txt")
}

# Get model significances - Table 3
{
  library(lmerTest)
  model.0 <- lmer(beauty_rating ~ obj_comp +
  disorder + ((obj_comp + disorder) | subject), data = data_test_fold1)
  model.M <- lmer(complexity_rating ~ obj_comp +
  (obj_comp | subject), data = data_test_fold1)
  model.Y <- lmer(beauty_rating ~ complexity_rating + obj_comp +
  disorder + ((complexity_rating + obj_comp + disorder) | subject), data = data_test_fold1)

  capture.output(summary(model.0), file = "model_fits/Table_3_mediationanalysis.txt")
  capture.output(summary(model.M), file = "model_fits/Table_3_mediationanalysis.txt", append = TRUE)
  capture.output(summary(model.Y), file = "model_fits/Table_3_mediationanalysis.txt", append = TRUE)
}
