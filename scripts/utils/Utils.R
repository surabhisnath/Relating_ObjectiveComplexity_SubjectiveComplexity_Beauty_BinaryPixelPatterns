library(lme4)
library(ggplot2)
library(interactions)
library(dplyr)
library(mediation)

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


