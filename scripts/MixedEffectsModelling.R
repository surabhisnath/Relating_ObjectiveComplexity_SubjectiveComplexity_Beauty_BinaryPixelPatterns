## Author: Surabhi S Nath
## Description: This script implements mixed effects models on complexity and beauty ratings.

# Imports
{
  library(lme4)
  library(ggplot2)
  library(interactions)
  library(dplyr)
  library(mediation)

  # Set seed
  set.seed(20)
}

# Catterpillar plot for random effects
# Code borrowed from: https://stackoverflow.com/questions/13847936/plot-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot-how-to-mak, answer by caracal
ggCaterpillar <- function(re, QQ = FALSE, likeDotplot = TRUE, detailedFacetLabs = TRUE) {
  f <- function(x, nm = "ranef plot") {
    pv <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each = nrow(x))
    pDf <- data.frame(
      y = unlist(x)[ord],
      ci = 1.96 * se[ord],
      nQQ = rep(stats::qnorm(stats::ppoints(nrow(x))), ncol(x)),
      ID = factor(rep(rownames(x), ncol(x))[ord], levels = rownames(x)[ord]),
      ind = gl(ncol(x), nrow(x), labels = names(x))
    )

    if (detailedFacetLabs) {
      pDf$ind <- ifelse(grepl("(Intercept)", pDf$ind), "intercept adjustment", paste0("slope adj: ", pDf$ind))
    }

    if (QQ) { ## normal QQ-plot
      p <- ggplot(pDf, aes_string(x = "nQQ", y = "y"))
      p <- p + facet_wrap(~ind, scales = "free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else { ## caterpillar dotplot
      p <- ggplot(pDf, aes_string(x = "ID", y = "y")) +
        coord_flip()
      if (likeDotplot) { ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ind)
      } else { ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales = "free_y")
      }
      p <- p + xlab(nm) + ylab("Random effects")
      scale <- 12 - log(length(levels(pDf$ID)), 2)
      p <- p + theme(axis.text.y = element_text(size = scale))
    }

    p <- p + theme(legend.position = "none")
    p <- p + geom_hline(yintercept = 0, lwd = I(7 / 12), colour = I(grDevices::hsv(0 / 12, 7 / 12, 7 / 12)), alpha = I(5 / 12))
    p <- p + geom_errorbar(aes_string(ymin = "y - ci", ymax = "y + ci"), width = 0, colour = "black")
    p <- p + geom_point(aes())
    return(p)
  }

  lapply(seq_along(re), function(y, n, i) {
    f(y[[i]], n[[i]])
  }, y = re, n = names(re)) # adds plot names
}

# Analysis function
modelanalysis <- function(dependent_var, num_folds, models, data_train_folds, data_test_folds, vif, print, plot) {
  # This function performs all analyses on the model and returns all performance metrics
  # Arguements:
  # dependent_var: string: either "complexity" or "beauty"
  # num_folds: number of CV folds
  # models - array of lmer models on each fold
  # data_train_folds - ground truth train data for each fold
  # data_test_folds - ground truth test data for each fold
  # vif - logical - indicates if variance inflation factors need to be printed (TRUE means yes) - if more than 2 variables, vif = TRUE
  # print - logical - indicates if metrics need to be printed (TRUE means yes)
  # plot - logical - indicates if plots should be made (TRUE means yes)
  VIFs <- numeric(num_folds)
  AICs <- numeric(num_folds)
  BICs <- numeric(num_folds)
  rsqtrains <- numeric(num_folds)
  rsqtests <- numeric(num_folds)
  RMSEtrains <- numeric(num_folds)
  RMSEtests <- numeric(num_folds)

  for (x in 1:num_folds)
  {
    # if (vif)
    # {
    #   VIFs[x] <- vif(models[x])
    # }
    AICs[x] <- AIC(models[[x]])
    BICs[x] <- BIC(models[[x]])

    if (dependent_var == "complexity") {
      rsqtrains[x] <- cor(predict(models[[x]], data_train_folds[[x]]), data_train_folds[[x]]$complexity_rating)^2
      rsqtests[x] <- cor(predict(models[[x]], data_test_folds[[x]]), data_test_folds[[x]]$complexity_rating)^2
      RMSEtrains[x] <- sqrt(mean(residuals(models[[x]])^2))
      RMSEtests[x] <- sqrt(mean((predict(models[[x]], data_test_folds[[x]]) - data_test_folds[[x]]$complexity_rating)^2))
    } else if (dependent_var == "beauty") {
      rsqtrains[x] <- cor(predict(models[[x]], data_train_folds[[x]]), data_train_folds[[x]]$beauty_rating)^2
      rsqtests[x] <- cor(predict(models[[x]], data_test_folds[[x]]), data_test_folds[[x]]$beauty_rating)^2
      RMSEtrains[x] <- sqrt(mean(residuals(models[[x]])^2))
      RMSEtests[x] <- sqrt(mean((predict(models[[x]], data_test_folds[[x]]) - data_test_folds[[x]]$beauty_rating)^2))
    }
  }

  meanAIC <- mean(AICs)
  meanBIC <- mean(BICs)
  varAICBIC <- var(AICs)
  meanrsqtrain <- mean(rsqtrains)
  varrsqtrain <- var(rsqtrains)
  meanrsqtest <- mean(rsqtests)
  varrsqtest <- var(rsqtests)
  meanRMSEtrain <- mean(RMSEtrains)
  varRMSEtrain <- var(RMSEtrains)
  meanRMSEtest <- mean(RMSEtests)
  varRMSEtest <- var(RMSEtests)

  if (print) {
    if (vif) {
      for (x in 1:num_folds)
      {
        print(VIFs[x])
      }
    }

    print(noquote(paste("Mean AIC =", meanAIC)))
    print(noquote(paste("Mean BIC =", meanBIC)))
    print(noquote(paste("Var AIC, BIC =", varAICBIC)))

    print(noquote(paste("Mean R^2 train =", meanrsqtrain)))
    print(noquote(paste("Var R^2 train =", varrsqtrain)))
    print(noquote(paste("Mean R^2 test =", meanrsqtest)))
    print(noquote(paste("Var R^2 test =", varrsqtest)))

    print(noquote(paste("Mean train RMSE =", meanRMSEtrain)))
    print(noquote(paste("Var train RMSE =", varRMSEtrain)))
    print(noquote(paste("Mean test RMSE =", meanRMSEtest)))
    print(noquote(paste("Var test RMSE =", varRMSEtest)))
  }

  if (plot) # Make plot if plot is TRUE
    {
      make_plots(dependent_var, models[[1]], data_train_folds[[1]], data_test_folds[[1]])
    }

  metrics1 <- c(meanAIC, meanBIC, varAICBIC)
  metrics1 <- round(metrics1, digits = 1)
  metrics2 <- c(meanrsqtrain, varrsqtrain, meanrsqtest, varrsqtest, meanRMSEtrain, varRMSEtrain, meanRMSEtest, varRMSEtest)
  metrics2 <- signif(metrics2, digits = 2)

  # returns all metrics
  return(c(metrics1, metrics2))
}

make_plots <- function(dependent_var, model, data_train_fold, data_test_fold) {
  # Make and save plots

  if (dependent_var == "complexity") {
    # Save plot of complexity ratings vs predictions (on both train and test data)

    pdf(file = "plots/complexity_train.pdf", width = 10, height = 10, family = "Times")
    par(mar = c(5, 6, 4, 1) + .1)
    p1 <- plot(predict(model, data_train_fold), data_train_fold$complexity_rating, xlab = "Predictions on training data", ylab = "Complexity ratings", cex.axis = 3, cex.lab = 3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), pch = 16, axes = F)
    axis(1, cex.axis = 2)
    axis(2, cex.axis = 2)
    p1 + theme(
      plot.title = element_text(family = "serif", size = 16),
      axis.title = element_text(family = "serif", size = 20),
      axis.text = element_text(family = "serif", size = 14)
    )
    abline(a = 0, b = 1, col = "blue", lwd = 3, lty = 2)
    dev.off()

    pdf(file = "plots/complexity_test.pdf", width = 10, height = 10, family = "Times")
    par(mar = c(5, 6, 4, 1) + .1)
    p2 <- plot(predict(model, data_test_fold), data_test_fold$complexity_rating, xlab = "Predictions on test data", ylab = "Complexity ratings", cex.axis = 3, cex.lab = 3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), pch = 16, axes = F)
    axis(1, cex.axis = 2)
    axis(2, cex.axis = 2)
    p2 + theme(
      plot.title = element_text(family = "serif", size = 16),
      axis.title = element_text(family = "serif", size = 20),
      axis.text = element_text(family = "serif", size = 14)
    )
    abline(a = 0, b = 1, col = "blue", lwd = 3, lty = 2)
    dev.off()
  } else if (dependent_var == "beauty") {
    # Save plot of beauty ratings vs predictions (on both train and test data)

    pdf(file = "plots/beauty_train.pdf", width = 10, height = 10, family = "Times")
    par(mar = c(5, 6, 4, 1) + .1)
    p1 <- plot(predict(model, data_train_fold), data_train_fold$beauty_rating, xlab = "Predictions on training data", ylab = "Beauty ratings", cex.axis = 3, cex.lab = 3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), pch = 16, axes = F)
    axis(1, cex.axis = 2)
    axis(2, cex.axis = 2)
    p1 + theme(
      plot.title = element_text(family = "serif", size = 16),
      axis.title = element_text(family = "serif", size = 20),
      axis.text = element_text(family = "serif", size = 14)
    ) + scale_x_continuous(n.breaks = 10) + scale_y_continuous(n.breaks = 10)
    abline(a = 0, b = 1, col = "blue", lwd = 3, lty = 2)
    dev.off()

    pdf(file = "plots/beauty_test.pdf", width = 10, height = 10, family = "Times")
    par(mar = c(5, 6, 4, 1) + .1)
    p2 <- plot(predict(model, data_test_fold), data_test_fold$beauty_rating, xlab = "Predictions on test data", ylab = "Beauty ratings", cex.axis = 3, cex.lab = 3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), pch = 16, axes = F)
    axis(1, cex.axis = 2)
    axis(2, cex.axis = 2)
    p2 + theme(
      plot.title = element_text(family = "serif", size = 16),
      axis.title = element_text(family = "serif", size = 20),
      axis.text = element_text(family = "serif", size = 14)
    )
    abline(a = 0, b = 1, col = "blue", lwd = 3, lty = 2)
    dev.off()
  }
}

# Only for Figure AIII.2
plot_model <- function(model, path) {
  # Convert data to data frames
  train_df <- data.frame(predictions = predict(model, data_train_fold1), complexity_rating = data_train_fold1$complexity_rating)
  test_df <- data.frame(predictions = predict(model, data_test_fold1), complexity_rating = data_test_fold1$complexity_rating)

  # Create the plot with training and test data
  # Create the plot with training and test data
  ggplot(mapping = aes(x = predictions, y = complexity_rating)) +
    geom_point(data = train_df, aes(color = "Training"), alpha = 0.5, size = 4) +
    geom_point(data = test_df, aes(color = "Test"), alpha = 0.5, size = 4) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = 2, size = 3) +
    xlab("Predictions") +
    ylab("Complexity ratings") +
    theme_classic() +
    theme(
      axis.title = element_text(family = "serif", size = 32),
      axis.text = element_text(family = "serif", size = 25),
      legend.position = "top",
      legend.title = element_text(family = "serif", size = 28),
      legend.text = element_text(family = "serif", size = 28)
    ) +
    # scale_color_manual(name = "", values = c("Training" = "darkblue", "Test" = "cyan2"))
    # scale_color_manual(name = "", values = c("Training" = "darkgreen", "Test" = "darkolivegreen2"))
    scale_color_manual(name = "", values = c("Training" = "darkred", "Test" = "darksalmon"))
  # Save the plot to a PDF file
  ggsave(paste("plots/", path), width = 10, height = 10)
}

# Setup
{
  # Read data
  data <- read.csv("utils/stat_analysis.csv")

  # Remove people who failed all attention checks
  all_attemntion_failed <- c("ls1hg9ya", "jjs7ytws")
  data <- data[!data$subject %in% all_attemntion_failed, ]

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
  data$asymm <- my_scale((data$local_asymm + data$Hasymm + data$Vasymm) / 3)
  data$entropy <- my_scale(data$entropy)
  data$mean_entropy <- my_scale(data$mean_entropy)
  data$entropy_of_means <- my_scale(data$entropy_of_means)
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

# View data
View(data)

# Notes:
# participant in paper is subject here.
# intricacy from paper is intricacy_4 here.

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
  "LSC + entropy_of_means + (1 | subject)",
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
  "entropy_of_means + (1 | subject)",
  "LSC + (1 | subject)",
  "KC + (1 | subject)",
  "asymm + (1 | subject)",
  "intricacy_4 + (1 | subject)",
  "intricacy_8 + (1 | subject)",
  "quadtree + (1 | subject)",
  "LSC + density + (1 | subject)",
  "LSC + entropy + (1 | subject)",
  "LSC + mean_entropy + (1 | subject)",
  "LSC + entropy_of_means + (1 | subject)",
  "LSC + asymm + (1 | subject)",
  "LSC + intricacy_4 + (1 | subject)",
  "LSC + intricacy_8 + (1 | subject)",
  "LSC + quadtree + (1 | subject)",
  "LSC + intricacy_4 + intricacy_8 + (1 | subject)",
  "LSC + entropy_of_means + density + (1 | subject)",
  "LSC + intricacy_4 + LSC:intricacy_4 + (1 | subject)",
  "LSC + intricacy_4 + (LSC | subject)",
  "LSC + intricacy_4 + (intricacy_4 | subject)",
  "LSC + intricacy_4 + ((LSC + intricacy_4) | subject)",
  "LSC * intricacy_4 + ((LSC + intricacy_4) | subject)",
  "quadtree + intricacy_4 + (1 | subject)",
  "quadtree + intricacy_4 + (quadtree | subject)",
  "quadtree + intricacy_4 + ((intricacy_4 + intricacy_4) | subject)",
  "LSCsq + intricacy_4sq + (1 | subject)",
  "LSC + LSCsq + intricacy_4 + intricacy_4sq + (1 | subject)",
  "neighbourhood_size + tot_outertot + IC + (1 | subject)",
  "trial + (1 | subject)",
  "LSC + intricacy_4 + trial + ((LSC + intricacy_4) | subject)",
  "previous_complexity_rating + (trial | subject)",
  "LSC + intricacy_4 + previous_complexity_rating + ((LSC + intricacy_4) | subject)"
)

# Analyze a subset of the complexity models - Table 1
# Save all results to model_fits/models_complexity.csv
{
  df <- data.frame(matrix(ncol = 13, nrow = 0, dimnames = list(NULL, c("Id", "model", "AIC", "BIC", "AIC/BIC Var", "Rsq train mean", "Rsq train var", "Rsq test mean", "Rsq test var", "RMSE train mean", "RMSE train var", "RMSE test mean", "RMSE test var"))))

  id <- 0
  for (formula in models_in_paper_complexity) { # can also run for all_models instead (replace models_in_paper_complexity by all_models_complexity)
    id <- id + 1
    fullformula <- paste("complexity_rating ~", formula)
    f1 <- lmer(fullformula, data = data_train_fold1, control = lmerControl(optimizer = "bobyqa"))
    f2 <- lmer(fullformula, data = data_train_fold2, control = lmerControl(optimizer = "bobyqa"))
    f3 <- lmer(fullformula, data = data_train_fold3, control = lmerControl(optimizer = "bobyqa"))
    print(noquote(fullformula))

    metrics <- modelanalysis("complexity", num_folds, list(f1, f2, f3), list(data_train_fold1, data_train_fold2, data_train_fold3), list(data_test_fold1, data_test_fold2, data_test_fold3), FALSE, TRUE, FALSE)
    df[nrow(df) + 1, ] <- c(id, noquote(fullformula), metrics)
  }

  write.csv(df, "model_fits/models_complexity.csv", row.names = FALSE)
}


# Print performance of best model and make plots (train, test and random effects)
# Figure 7, Figure AIII.7(A)
# Plots saved to ./plots/
{
  bestformula <- "LSC + intricacy_4 + ((LSC + intricacy_4) | subject)"
  bestfullformula <- paste("complexity_rating ~", bestformula)
  f1 <- lmer(bestfullformula, data = data_train_fold1, control = lmerControl(optimizer = "bobyqa"))
  f2 <- lmer(bestfullformula, data = data_train_fold2, control = lmerControl(optimizer = "bobyqa"))
  f3 <- lmer(bestfullformula, data = data_train_fold3, control = lmerControl(optimizer = "bobyqa"))
  f <- lmer(bestfullformula, data = data_train_fold1, control = lmerControl(optimizer = "bobyqa"))

  # Plots data vs predictions on train and test data - Figure 66
  metrics <- modelanalysis("complexity", num_folds, list(f1, f2, f3), list(data_train_fold1, data_train_fold2, data_train_fold3), list(data_test_fold1, data_test_fold2, data_test_fold3), FALSE, FALSE, TRUE)

  # Plots random effects - Figure AIII.5(a)
  ggCaterpillar(ranef(f, condVar = TRUE))
  ggsave("plots/random_effects_complexity.pdf")
}

# Make plots for only LSC and only intricacy - Figure AIII.2
# Plots saved to ./plots/
{
  formula_LSC <- "LSC + (1 | subject)"
  fullformula_LSC <- paste("complexity_rating ~", formula_LSC)
  model_LSC <- lmer(fullformula_LSC, data = data_train_fold1)
  plot_model(model_LSC, "complexity_train_test_LSC.pdf")

  formula_intricacy <- "intricacy_4 + (1 | subject)"
  fullformula_intricacy <- paste("complexity_rating ~", formula_intricacy)
  model_intricacy <- lmer(fullformula_intricacy, data = data_train_fold1)
  plot_model(model_intricacy, "complexity_train_test_intricacy.pdf")

  formula_both <- "LSC + intricacy_4 + (1 | subject)"
  fullformula_both <- paste("complexity_rating ~", formula_both)
  model_both <- lmer(fullformula_both, data = data_train_fold1)
  plot_model(model_both, "complexity_train_test_both.pdf")
}

# Print fixed effects - Table 1B
{
  fixef(f)
}

# Write objective complexity column onto data for mediation analysis
{
  data$obj_comp <- (fixef(f)["LSC"] * data$LSC) + (fixef(f)["intricacy_4"] * data$intricacy_4) + fixef(f)["(Intercept)"]
  ranef <- ranef(f)
  ranefintercept <- ranef$subject[, "(Intercept)"]
  ranefLSC <- ranef$subject[, "LSC"]
  ranefintricacy <- ranef$subject[, "intricacy_4"]
  data$randint <- rep(ranefintercept, each = 60)
  data$randslope_LSC <- rep(ranefLSC, each = 60)
  data$randslope_intricacy <- rep(ranefintricacy, each = 60)
  data$obj_comp_withrandint <- data$obj_comp + data$randint
  data$obj_comp_withrandint_withrandslopes <- ((fixef(f)["LSC"] + data$randslope_LSC) * data$LSC) + ((fixef(f)["intricacy_4"] + data$randslope_intricacy) * data$intricacy_4) + fixef(f)["(Intercept)"] + data$randint
}

# Get pattern numbers with maximum and minimum discrepency - visualised in DescriptiveAnalysis.ipynb
{
  data$discrepancy <- my_scale(data$obj_comp_withrandint_withrandslopes - data$complexity_rating)
  mean_pattern <- aggregate(data, list(data$pattern), mean)
  inc_disc <- mean_pattern[order(mean_pattern$discrepancy), ]
  print(tail(inc_disc, 10)$Group.1) # overestimation of complexity
  print(head(inc_disc, 8)$Group.1) # underestimation of complexity
}

############### BEAUTY MODELS ###############

# Create the disorder variable and recreate the folds
{
  formula <- "complexity_rating + asymm + entropy + (1 | subject)"
  fullformula <- paste("beauty_rating ~", formula)
  f1 <- lmer(fullformula, data = data_train_fold1)
  f2 <- lmer(fullformula, data = data_train_fold2)
  f3 <- lmer(fullformula, data = data_train_fold3)
  f <- lmer(fullformula, data = data_train_fold1)
  # add disorder to data
  data$disorder_1 <- my_scale((abs(fixef(f)["asymm"]) * data$asymm) + (abs(fixef(f)["entropy"]) * data$entropy))

  formula <- "complexity_rating + asymm + mean_entropy + (1 | subject)"
  fullformula <- paste("beauty_rating ~", formula)
  f1 <- lmer(fullformula, data = data_train_fold1)
  f2 <- lmer(fullformula, data = data_train_fold2)
  f3 <- lmer(fullformula, data = data_train_fold3)
  f <- lmer(fullformula, data = data_train_fold1)
  data$disorder_2 <- my_scale((abs(fixef(f)["asymm"]) * data$asymm) + (abs(fixef(f)["mean_entropy"]) * data$mean_entropy))

  formula <- "complexity_rating + asymm + entropy_of_means + (1 | subject)"
  fullformula <- paste("beauty_rating ~", formula)
  f1 <- lmer(fullformula, data = data_train_fold1)
  f2 <- lmer(fullformula, data = data_train_fold2)
  f3 <- lmer(fullformula, data = data_train_fold3)
  f <- lmer(fullformula, data = data_train_fold1)
  data$disorder <- my_scale((abs(fixef(f)["asymm"]) * data$asymm) + (abs(fixef(f)["entropy_of_means"]) * data$entropy_of_means))

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

# List of beauty models reported in the paper
models_in_paper_beauty <- list(
  "complexity_rating + (1 | subject)",
  "complexity_rating + disorder + (1 | subject)",
  "complexity_rating + disorder + (disorder | subject)",
  "complexity_rating * disorder + (disorder | subject)",
  "complexity_rating_sq + disorder + (1 | subject)",
  "complexity_rating + complexity_rating_sq + disorder + (1 | subject)",
  "LSC + intricacy_4 + disorder + (1 | subject)",
  "LSC + intricacy_4 + disorder + (disorder | subject)",
  "(LSC + intricacy_4) * disorder + (disorder | subject)"
)

# List of all models of beauty - Table AIII.2
all_models_beauty <- list(
  "complexity_rating + (1 | subject)",
  "complexity_rating + asymm + entropy + (1 | subject)",
  "complexity_rating + asymm + entropy + (complexity_rating | subject)",
  "complexity_rating + asymm + entropy + (asymm | subject)",
  "complexity_rating + asymm + entropy + (entropy | subject)",
  "complexity_rating + disorder + (1 | subject)",
  "complexity_rating + disorder + (disorder | subject)",
  "complexity_rating * disorder + (disorder | subject)",
  "complexity_rating * disorder_1 + (disorder_1 | subject)",
  "complexity_rating * disorder_2 + (disorder_2 | subject)",
  "complexity_rating_sq + disorder + (1 | subject)",
  "complexity_rating + complexity_rating_sq + disorder + (1 | subject)",
  "LSC + intricacy_4 + asymm + entropy + (1 | subject)",
  "LSC + intricacy_4 + disorder + (1 | subject)",
  "LSC + intricacy_4 + disorder + (disorder | subject)",
  "LSC + intricacy_4 + disorder + (LSC + intricacy_4):disorder + (disorder | subject)",
  "trial + (1 | subject)",
  "complexity_rating * disorder + trial + (disorder | subject)",
  "previous_beauty_rating + (trial | subject)",
  "complexity_rating * disorder + previous_beauty_rating + (disorder | subject)",
  "complexity_rating * trial + (1 | subject)"
)

{
  df <- data.frame(matrix(ncol = 13, nrow = 0, dimnames = list(NULL, c("Id", "model", "AIC", "BIC", "AIC/BIC Var", "Rsq train mean", "Rsq train var", "Rsq test mean", "Rsq test var", "RMSE train mean", "RMSE train var", "RMSE test mean", "RMSE test var"))))

  id <- 0
  # Analyze a subset of the beauty models - Table 2
  for (formula in models_in_paper_beauty) { # can also run for all_models instead (replace models_in_paper_beauty by all_models_beauty)
    id <- id + 1
    fullformula <- paste("beauty_rating ~", formula)
    f1 <- lmer(fullformula, data = data_train_fold1, control = lmerControl(optimizer = "bobyqa"))
    f2 <- lmer(fullformula, data = data_train_fold2, control = lmerControl(optimizer = "bobyqa"))
    f3 <- lmer(fullformula, data = data_train_fold3, control = lmerControl(optimizer = "bobyqa"))
    # Uncomment the next 3 lines to print summaries
    # print(summary(f1))
    # print(summary(f2))
    # print(summary(f3))
    print(noquote(fullformula))

    metrics <- modelanalysis("beauty", num_folds, list(f1, f2, f3), list(data_train_fold1, data_train_fold2, data_train_fold3), list(data_test_fold1, data_test_fold2, data_test_fold3), FALSE, TRUE, FALSE)
    df[nrow(df) + 1, ] <- c(id, noquote(fullformula), c(metrics))
  }

  write.csv(df, "model_fits/models_beauty.csv", row.names = FALSE)
}

# Print performance of best model and make plots (train, test and random effects)
# Figure 9, Figure AIII.7(B)
# Plots saved to ./plots/
{
  bestformula <- "complexity_rating * disorder + (disorder | subject)"
  bestfullformula <- paste("beauty_rating ~", bestformula)
  f1 <- lmer(bestfullformula, data = data_train_fold1)
  f2 <- lmer(bestfullformula, data = data_train_fold2)
  f3 <- lmer(bestfullformula, data = data_train_fold3)
  f <- lmer(bestfullformula, data = data_train_fold1)

  # Plots data vs predictions on train and test data - Figure 8
  metrics <- modelanalysis("beauty", num_folds, list(f1, f2, f3), list(data_train_fold1, data_train_fold2, data_train_fold3), list(data_test_fold1, data_test_fold2, data_test_fold3), FALSE, FALSE, TRUE)

  # Plots random effects - Figure AIII.5(b)
  ggCaterpillar(ranef(f, condVar = TRUE))
  ggsave("plots/random_effects_beauty.pdf")
}

# Print fixed effects - Table 2B
{
  fixef(f)
}

# Create interaction plot
# Figure 10
# Plot saved to ./plots/
{
  pdf(file = "plots/complexity_order_interaction_for_beauty.pdf", width = 10, height = 10)
  par(mar = c(5, 6, 4, 1) + .1)
  interact_plot(f,
    pred = disorder, modx = complexity_rating, interval = TRUE, x.label = "Disorder", y.label = "Beauty Ratings",
    main.title = "Interaction Plot for Disorder x Complexity Ratings", legend.main = "Complexity Ratings",
    colors = "seagreen"
  ) + theme(
    plot.title = element_text(family = "serif", size = 16),
    axis.title = element_text(family = "serif", size = 20),
    axis.text = element_text(family = "serif", size = 14),
    legend.text = element_text(family = "serif", size = 15),
    legend.title = element_text(family = "serif", size = 18),
    strip.text = element_text(family = "serif")
  )
}

# Save interaction plot
{
  dev.off()
}

# Get patterns at interaction extremes for Figure 10
{
  data$complexity_rating_bin <- findInterval(data$complexity_rating, c(-1, 0, 1, 2))
  # bins: 0, 1, 2, 3, 4
  data$beauty_rating_bin <- findInterval(data$beauty_rating, c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2))
  data$disorder_bin <- findInterval(data$disorder, c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5))

  mean_pattern <- aggregate(data, list(data$pattern), mean)
  mean_pattern_bydisorder <- mean_pattern[order(-mean_pattern$disorder), ]

  highest_disorder <- mean_pattern_bydisorder[mean_pattern_bydisorder$disorder_bin >= 8, ]
  # max disorder patterns: 120, 109, 58

  mean_pattern_highdisorder_bycomplexity <- highest_disorder[order(highest_disorder$complexity_rating), ]

  print(head(mean_pattern_highdisorder_bycomplexity, 3)$Group.1) # high disorder, low CR
  cat(mean_pattern_highdisorder_bycomplexity[round(nrow(mean_pattern_highdisorder_bycomplexity) / 2) - 1, ]$Group.1, mean_pattern_highdisorder_bycomplexity[round(nrow(mean_pattern_highdisorder_bycomplexity) / 2), ]$Group.1, mean_pattern_highdisorder_bycomplexity[round(nrow(mean_pattern_highdisorder_bycomplexity) / 2) + 1, ]$Group.1) # low disorder, medium CR
  print(tail(mean_pattern_highdisorder_bycomplexity, 3)$Group.1) # high disorder, high CR
  # high disorder, low CR -     1, 167, 58
  # high disorder, medium CR -  8, 70, 67
  # high disorder, high CR -    131, 15, 195

  lowest_disorder <- mean_pattern_bydisorder[mean_pattern_bydisorder$disorder_bin <= 2, ]
  # min disorder patterns: 163 55 178 4 23 87 174 89 158

  mean_pattern_lowdisorder_bycomplexity <- lowest_disorder[order(lowest_disorder$complexity_rating), ]

  print(head(mean_pattern_lowdisorder_bycomplexity, 3)$Group.1) # low disorder, low CR
  cat(mean_pattern_lowdisorder_bycomplexity[round(nrow(mean_pattern_lowdisorder_bycomplexity) / 2) - 1, ]$Group.1, mean_pattern_lowdisorder_bycomplexity[round(nrow(mean_pattern_lowdisorder_bycomplexity) / 2), ]$Group.1, mean_pattern_lowdisorder_bycomplexity[round(nrow(mean_pattern_lowdisorder_bycomplexity) / 2) + 1, ]$Group.1) # low disorder, medium CR
  print(tail(mean_pattern_lowdisorder_bycomplexity, 3)$Group.1) # low disorder, high CR
  # low disorder, low CR -     163, 55, 4
  # low disorder, medium CR -  199, 132, 133
  # low disorder, high CR -    89, 87, 159
}

############ MEDIATION ANALYSIS #############

# Run porcess.R
# Requires process.R from Process running in the background
{
  process(data = data_train_fold2, y = "beauty_rating", x = "obj_comp_withrandint_withrandslopes", m = "complexity_rating", w = "disorder", model = 15, center = 2, moments = 1, modelbt = 1, boot = 10000, seed = 20, jn = 1)
}

# Mediation analysis - Table 4
{
  model.0 <- lmer(beauty_rating ~ obj_comp + disorder + (disorder | subject), data = data_test_fold1)
  model.M <- lmer(complexity_rating ~ obj_comp + (intricacy_4 | subject), data = data_test_fold1)
  model.Y <- lmer(beauty_rating ~ complexity_rating + obj_comp + disorder + (disorder | subject), data = data_test_fold1)

  results <- mediate(model.M, model.Y,
    treat = "obj_comp", mediator = "complexity_rating",
    boot = FALSE, sims = 500
  )
  print(summary(results))
}

# Get model significances - Table 3
{
  library(lmerTest)
  model.0 <- lmer(beauty_rating ~ obj_comp + disorder + (disorder | subject), data = data_test_fold1)
  model.M <- lmer(complexity_rating ~ obj_comp + (intricacy_4 | subject), data = data_test_fold1)
  model.Y <- lmer(beauty_rating ~ complexity_rating + obj_comp + disorder + (disorder | subject), data = data_test_fold1)
  print(summary(model.0))
  print(summary(model.M))
  print(summary(model.Y))
}
