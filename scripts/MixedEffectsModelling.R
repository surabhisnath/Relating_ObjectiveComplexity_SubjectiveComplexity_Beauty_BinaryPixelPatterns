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
ggCaterpillar <- function(re, QQ=FALSE, likeDotplot=TRUE, detailedFacetLabs = TRUE) {
  f <- function(x, nm = "ranef plot") {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(stats::qnorm(stats::ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))

    if(detailedFacetLabs){
      pDf$ind <- ifelse(grepl("(Intercept)", pDf$ind), "intercept adjustment", paste0("slope adj: ", pDf$ind))
    }

    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes_string(x="nQQ", y="y"))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes_string(x="ID", y="y")) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab(nm) + ylab("Random effects")
      scale <- 12-log(length(levels(pDf$ID)),2)
      p <- p + theme(axis.text.y = element_text(size=scale))
    }

    p <- p + theme(legend.position="none")
    p <- p + geom_hline(yintercept=0, lwd = I(7/12), colour = I(grDevices::hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
    p <- p + geom_errorbar(aes_string(ymin="y - ci", ymax="y + ci"), width=0, colour="black")
    p <- p + geom_point(aes())
    return(p)
  }

  lapply(seq_along(re), function(y, n, i) { f(y[[i]], n[[i]]) }, y=re, n=names(re)) # adds plot names
}

# Analysis function
modelanalysis <- function(complexity_or_beauty, model1, model2, model3, vif, model4)  {
  # complexity_or_beauty - 1 for complexity, 2 for beauty

  # if more than 2 variables, print variation inflation factors
  if (!missing(vif))
  {
    tryCatch(
      {
        print(vif(model1))
        print(vif(model2))
        print(vif(model3))
      },

      error = function(cond)
      {
      }
    )
  }

  # AIC
  print(noquote(paste("Mean AIC =", (AIC(model1) + AIC(model2) + AIC(model3))/3)))
  
  # BIC
  print(noquote(paste("Mean BIC =", (BIC(model1) + BIC(model2) + BIC(model3))/3)))

  # R^2 test and RMSEs
  if (complexity_or_beauty == 1)
  {
    print(noquote(paste("Mean R^2 test =", (cor(predict(model1, data_test_fold1), data_test_fold1$complexity_rating) ^ 2 + cor(predict(model2, data_test_fold2), data_test_fold2$complexity_rating) ^ 2 + cor(predict(model3, data_test_fold3), data_test_fold3$complexity_rating) ^ 2)/3) ))
    print(noquote(paste("Mean train RMSE =",( sqrt(mean(residuals(model1)^2)) + sqrt(mean(residuals(model2)^2)) + sqrt(mean(residuals(model3)^2)))/3))) # nolint
    print(noquote(paste("Mean test RMSE =", ( sqrt(mean((predict(model1, data_test_fold1) - data_test_fold1$complexity_rating)^2)) + sqrt(mean((predict(model2, data_test_fold2) - data_test_fold2$complexity_rating)^2)) + sqrt(mean((predict(model3, data_test_fold3) - data_test_fold3$complexity_rating)^2)) ) / 3 ) ))
  }

  else if (complexity_or_beauty == 2)
  {
    print(noquote(paste("Mean R^2 test =", (cor(predict(model1, data_test_fold1), data_test_fold1$beauty_rating) ^ 2 + cor(predict(model2, data_test_fold2), data_test_fold2$beauty_rating) ^ 2 + cor(predict(model3, data_test_fold3), data_test_fold3$beauty_rating) ^ 2)/3) ))
    print(noquote(paste("Mean train RMSE =",( sqrt(mean(residuals(model1)^2)) + sqrt(mean(residuals(model2)^2)) + sqrt(mean(residuals(model3)^2)))/3))) # nolint
    print(noquote(paste("Mean test RMSE =", ( sqrt(mean((predict(model1, data_test_fold1) - data_test_fold1$beauty_rating)^2)) + sqrt(mean((predict(model2, data_test_fold2) - data_test_fold2$beauty_rating)^2)) + sqrt(mean((predict(model3, data_test_fold3) - data_test_fold3$beauty_rating)^2)) ) / 3 ) ))
  }

  # Plot
  if(!missing(model4))
  {
    if (complexity_or_beauty == 1)
    {
      pdf(file="plots/complexity_train.pdf", width = 10, height = 10, family = "Times")
      par(mar=c(5,6,4,1)+.1)
      p1 <- plot(predict(model4, data_train_fold1), data_train_fold1$complexity_rating, xlab="Predictions on training data", ylab="Complexity ratings", cex.axis = 3, cex.lab = 3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), pch = 16, axes=F)      
      axis(1, cex.axis=2)
      axis(2, cex.axis=2)
      p1 + theme(plot.title = element_text(family = "serif", size=16),
        axis.title = element_text(family = "serif", size=20),
        axis.text = element_text(family = "serif", size=14))
      abline(a=0, b=1, col="blue", lwd=3, lty=2)
      dev.off()

      pdf(file="plots/complexity_test.pdf", width = 10, height = 10, family = "Times")
      par(mar=c(5,6,4,1)+.1)
      p2 <- plot(predict(model4, data_test_fold1), data_test_fold1$complexity_rating, xlab="Predictions on test data", ylab="Complexity ratings", cex.axis = 3, cex.lab = 3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), pch = 16, axes=F)
      axis(1, cex.axis=2)
      axis(2, cex.axis=2)
      p2 + theme(plot.title = element_text(family = "serif", size=16),
        axis.title = element_text(family = "serif", size=20),
        axis.text = element_text(family = "serif", size=14))
      abline(a=0, b=1, col="blue", lwd=3, lty=2)
      dev.off()
    }

    else if (complexity_or_beauty == 2)
    {
      pdf(file="plots/beauty_train.pdf", width = 10, height = 10, family = "Times")
      par(mar=c(5,6,4,1)+.1)
      p1 <- plot(predict(model4, data_train_fold1), data_train_fold1$beauty_rating, xlab="Predictions on training data", ylab="Beauty ratings", cex.axis = 3, cex.lab = 3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), pch = 16, axes=F)
      axis(1, cex.axis=2)
      axis(2, cex.axis=2)
      p1 + theme(plot.title = element_text(family = "serif", size=16),
        axis.title = element_text(family = "serif", size=20),
        axis.text = element_text(family = "serif", size=14)) + scale_x_continuous(n.breaks = 10) + scale_y_continuous(n.breaks = 10)
      abline(a=0, b=1, col="blue", lwd=3, lty=2)
      dev.off()

      pdf(file="plots/beauty_test.pdf", width = 10, height = 10, family = "Times")
      par(mar=c(5,6,4,1)+.1)
      p2 <- plot(predict(model4, data_test_fold1), data_test_fold1$beauty_rating, xlab="Predictions on test data", ylab="Beauty ratings", cex.axis = 3, cex.lab = 3, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), pch = 16, axes=F)
      axis(1, cex.axis=2)
      axis(2, cex.axis=2)
      p2 + theme(plot.title = element_text(family = "serif", size=16),
        axis.title = element_text(family = "serif", size=20),
        axis.text = element_text(family = "serif", size=14))
      abline(a=0, b=1, col="blue", lwd=3, lty=2)
      dev.off()
    }
  }
}

# Setup
{
# Read data
data <- read.csv("utils/stat_analysis.csv")

# Remove people who failed all attention checks
all_attemntion_failed <- c('ls1hg9ya', 'jjs7ytws')
data <- data[! data$subject %in% all_attemntion_failed,]

# Scale data
my_scale<-function(x){as.numeric(scale(x))}
data$rtime <- data$rtime/1000
data$subject <- factor(data$subject)
data$set <- factor(data$set, levels = c(1,2,3,4))
data$pattern <- factor(data$pattern)
data$trial <- my_scale(data$trial)
data$complexity_rating <- my_scale(data$complexity_rating)
data$previous_complexity_rating <- my_scale(data$previous_complexity_rating)
data$complexity_rating_sq <- data$complexity_rating^2
data$beauty_rating <- my_scale(data$beauty_rating)
data$previous_beauty_rating <- my_scale(data$previous_beauty_rating)
data$SC <- my_scale(data$SC)
data$SCsq <- data$SC^2
data$intricacy_4 <- my_scale(data$intricacy_4)
data$intricacy_8 <- my_scale(data$intricacy_8)
data$intricacy_4sq <- data$intricacy_4^2
data$intricacy_8sq <- data$intricacy_8^2
data$quadtree <- my_scale(data$quadtree)
data$local_asymm <- my_scale(data$local_asymm)
data$Hasymm <- my_scale(data$Hasymm)
data$Vasymm <- my_scale(data$Vasymm)
data$asymm <- (data$local_asymm + data$Hasymm + data$Vasymm)/3
data$entropy <- my_scale(data$entropy)
data$density <- my_scale(data$density)
data$neighbourhood_size <- factor(data$neighbourhood_size)
data$tot_outertot <- factor(data$tot_outertot)
data$IC <- factor(data$IC)
data$num_active_rules <- my_scale(data$num_active_rules)
}

# Stratified data sampling to create 3 folds
{
  data_test_fold1 <- data %>% group_by(subject) %>% sample_n(size=20)
  data_train_fold1 <- setdiff(data, data_test_fold1)

  data_test_fold23 <- setdiff(data, data_test_fold1)

  data_test_fold2 <- data_test_fold23 %>% group_by(subject) %>% sample_n(size=20)
  data_train_fold2 <- setdiff(data, data_test_fold2)

  data_test_fold3 <- setdiff(data_test_fold23, data_test_fold2)
  data_train_fold3 <- setdiff(data, data_test_fold3)
}

# View data
View(data)

############### COMPLEXITY MODELS ##############

# List of all models of complexity
formulae_complexity <- list("1 + (1 | subject)",
          "1 + (1 | subject) + (1 | set)",
          "SC + (1 | subject)",
          "quadtree + (1 | subject)",
          "quadtree + (quadtree | subject)",
          "SC + (SC | subject)",
          "KC + (1 | subject)",
          "intricacy_4 + (1 | subject)",
          "intricacy_8 + (1 | subject)",
          "SC + asymm + (1 | subject)",
          "SC + entropy + (1 | subject)",
          "SC + density + (1 | subject)",
          "SC + entropy + density + (1 | subject)",
          "SC + intricacy_4 + (1 | subject)",
          "SC + intricacy_8 + (1 | subject)",
          "SC + intricacy_4 + intricacy_8 + (1 | subject)",
          "SC + quadtree + (1 | subject)",
          "SC + intricacy_4 + SC:intricacy_4 + (1 | subject)",
          "SC + intricacy_4 + (SC | subject)",
          "SC + intricacy_4 + (intricacy_4 | subject)",
          "SC * intricacy_4 + (intricacy_4 | subject)",
          "quadtree + intricacy_4 + (1 | subject)",
          "quadtree + intricacy_4 + (quadtree | subject)",
          "quadtree + intricacy_4 + (intricacy_4 | subject)",
          "SCsq + intricacy_4sq + (1 | subject)",
          "SC + SCsq + intricacy_4 + intricacy_4sq + (1 | subject)",
          "SC + intricacy_4 + tot_outertot + (1 | subject)",
          "neighbourhood_size + tot_outertot + IC + (1 | subject)",
          "trial + (1 | subject)",
          "SC + intricacy_4 + trial + (intricacy_4 | subject)",
          "previous_complexity_rating + (trial | subject)",
          "SC + intricacy_4 + previous_complexity_rating + (intricacy_4 | subject)"
          )

# Analyze each model - Table 1
for (formula in formulae_complexity) {
  fullformula = paste("complexity_rating ~", formula)
  f1 = lmer(fullformula, data = data_train_fold1)
  f2 = lmer(fullformula, data = data_train_fold2)
  f3 = lmer(fullformula, data = data_train_fold3)
  # Can also print summaries if desired
  # print(summary(f1))
  # print(summary(f2))
  # print(summary(f3))
  print(noquote(fullformula))
  modelanalysis(1, f1, f2, f3, 1)
}

# Make plots for best model
# Figure 6 and Figure AIII.5(a)
{
  bestformula = "SC + intricacy_4 + (intricacy_4 | subject)"
  bestfullformula = paste("complexity_rating ~", bestformula)
  f1 = lmer(bestfullformula, data = data_train_fold1)
  f2 = lmer(bestfullformula, data = data_train_fold2)
  f3 = lmer(bestfullformula, data = data_train_fold3)
  f = lmer(bestfullformula, data = data_train_fold1)
  
  # Plots data vs predictions on train and test data - Figure 66
  modelanalysis(1, f1, f2, f3, 1, f)

  # Plots random effects - Figure AIII.5(a)
  ggCaterpillar(ranef(f, condVar=TRUE))
  ggsave("plots/random_effects_complexity.pdf")
}

# Write objective complexity column onto data for mediation analysis
{
  data$obj_comp <- (fixef(f)["SC"] * data$SC) + (fixef(f)["intricacy_4"] * data$intricacy_4) + fixef(f)["(Intercept)"]
  ranef <- ranef(f)
  raneflist1 <- ranef$subject[,"(Intercept)"]
  raneflist2 <- ranef$subject[,"intricacy_4"]
  data$randint <- rep(raneflist1, each=60)
  data$randslope <- rep(raneflist2, each=60)
  data$obj_comp_withrandint <- data$obj_comp + data$randint
  data$obj_comp_withrandint_withrandslope <- (fixef(f)["SC"] * data$SC) + ((fixef(f)["intricacy_4"] + data$randslope) * data$intricacy_4) + fixef(f)["(Intercept)"] + data$randint
}

# Get pattern numbers with maximum and minimum discrepency
{
  data$discrepancy <- my_scale(data$obj_comp_withrandint_withrandslope - data$complexity_rating)
  mean_pattern <- aggregate(data, list(data$pattern), mean)
  inc_disc <- mean_pattern[order(mean_pattern$discrepancy), ]
  print(head(inc_disc, 7)$Group.1)
  print(tail(inc_disc, 7)$Group.1)
}

############### BEAUTY MODELS ###############

# Create the disorder variable and recreate the folds
{
  formula = "complexity_rating + asymm + entropy + (asymm | subject)"
  fullformula = paste("beauty_rating ~", formula)
  f1 = lmer(fullformula, data = data_train_fold1)
  f2 = lmer(fullformula, data = data_train_fold2)
  f3 = lmer(fullformula, data = data_train_fold3)
  f = lmer(fullformula, data = data_train_fold1)

  # add disorder to data
  data$disorder <- my_scale((abs(fixef(f)["asymm"]) * data$asymm) + (abs(fixef(f)["entropy"]) * data$entropy))

  # recreate the folds
  data_test_fold1 <- data %>%
    group_by(subject) %>%
    sample_n(size=20)
  data_train_fold1 <- setdiff(data, data_test_fold1)

  data_test_fold23 <- setdiff(data, data_test_fold1)

  data_test_fold2 <- data_test_fold23 %>%
    group_by(subject) %>%
    sample_n(size=20)
  data_train_fold2 <- setdiff(data, data_test_fold2)

  data_test_fold3 <- setdiff(data_test_fold23, data_test_fold2)
  data_train_fold3 <- setdiff(data, data_test_fold3)
}

# List of all models of beauty
formulae_beauty <- list("complexity_rating + (1 | subject)",
          "complexity_rating + asymm + entropy + (1 | subject)",
          "complexity_rating + asymm + entropy + (complexity_rating | subject)",
          "complexity_rating + asymm + entropy + (asymm | subject)",
          "complexity_rating + asymm + entropy + (entropy | subject)",
          "complexity_rating + disorder + (1 | subject)",
          "complexity_rating + disorder + (disorder | subject)",
          "complexity_rating * disorder + (disorder | subject)",
          "complexity_rating_sq + disorder + (1 | subject)",
          "complexity_rating + complexity_rating_sq + disorder + (1 | subject)",
          "SC + intricacy_4 + asymm + entropy + (1 | subject)",
          "SC + intricacy_4 + disorder + (1 | subject)",
          "SC + intricacy_4 + disorder + (disorder | subject)",
          "SC + intricacy_4 + disorder + (SC + intricacy_4):disorder + (disorder | subject)",
          "trial + (1 | subject)",
          "complexity_rating * disorder + trial + (disorder | subject)",
          "previous_beauty_rating + (trial | subject)",
          "complexity_rating * disorder + previous_beauty_rating + (disorder | subject)",
          "complexity_rating * trial + (1 | subject)"
          )

# Analyze each model - Table 2
for (formula in formulae_beauty) {
  fullformula = paste("beauty_rating ~", formula)
  f1 = lmer(fullformula, data = data_train_fold1)
  f2 = lmer(fullformula, data = data_train_fold2)
  f3 = lmer(fullformula, data = data_train_fold3)
  # Can also print summaries if desired
  # print(summary(f1))
  # print(summary(f2))
  # print(summary(f3))
  print(noquote(fullformula))
  modelanalysis(2, f1, f2, f3, 1)
}

# Make plots for best model
# Figure 8 and Figure AIII.5(b)
{
  bestformula = "complexity_rating * disorder + (disorder | subject)"
  bestfullformula = paste("beauty_rating ~", bestformula)
  f1 = lmer(bestfullformula, data = data_train_fold1)
  f2 = lmer(bestfullformula, data = data_train_fold2)
  f3 = lmer(bestfullformula, data = data_train_fold3)
  f = lmer(bestfullformula, data = data_train_fold1)

  # Plots data vs predictions on train and test data - Figure 8
  modelanalysis(2, f1, f2, f3, 1, f)

  # Plots random effects - Figure AIII.5(b)
  ggCaterpillar(ranef(f, condVar=TRUE))
  ggsave("plots/random_effects_beauty.pdf")
}

# Create interaction plot
# Figure 7
{
  pdf(file="plots/complexity_order_interaction_for_beauty.pdf", width = 10, height = 10)
  par(mar=c(5,6,4,1)+.1)
  interact_plot(f, pred = disorder, modx = complexity_rating, interval = TRUE, x.label = "Disorder", y.label = "Beauty Ratings",
                main.title = "Interaction Plot for Disorder x Complexity Ratings",  legend.main = "Complexity Ratings",
                colors = "seagreen") + theme(plot.title = element_text(family = "serif", size=16),
              axis.title = element_text(family = "serif", size=20),
              axis.text = element_text(family = "serif", size=14),
            legend.text = element_text(family = "serif", size=15),
            legend.title = element_text(family = "serif", size=18),
            strip.text = element_text(family = "serif"))
}

# Save interaction plot
{
  dev.off()
}

# Get patterns at interaction extremes for Figure 7
{
  data$complexity_rating_bin <- findInterval(data$complexity_rating, c(-1, 0, 1, 2))
  # bins: 0, 1, 2, 3, 4
  data$beauty_rating_bin <- findInterval(data$beauty_rating, c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2))
  data$disorder_bin <- findInterval(data$disorder, c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5))

  mean_pattern <- aggregate(data, list(data$pattern), mean)
  mean_pattern_bydisorder <- mean_pattern[order(-mean_pattern$disorder), ]
  # max disorder patterns: 120, 109, 58
  lowest_disorder <- mean_pattern_bydisorder[mean_pattern_bydisorder$disorder_bin <= 2,]
  # min disorder patterns: 163 55 178 4 23 87 174 89 158 

  mean_pattern_bydisorder_bycomplexity_bybeauty <- lowest_disorder[order(lowest_disorder$complexity_rating, lowest_disorder$beauty_rating),]

  print(head(mean_pattern_bydisorder_bycomplexity_bybeauty, 3)$Group.1)
  print(tail(mean_pattern_bydisorder_bycomplexity_bybeauty, 3)$Group.1)
  # low disorder, high CR, high BR -      89, 87, 159
  # low disorder, medium CR, medium BR -  199, 132, 133
  # low disorder, low CR, low BR -        163, 55, 4
}

############ MEDIATION ANALYSIS #############

# Run porcess
# Requires process.R from Process running in the background
{
  process(data = data_train_fold2, y = "beauty_rating", x = "obj_comp_withrandint_withrandslope", m = "complexity_rating", w = "disorder", model = 15, center = 2, moments = 1, modelbt = 1, boot = 10000, seed = 20, jn = 1)
}

# Mediation analysis - Table 4
{
  model.0 <- lmer(beauty_rating ~ obj_comp + disorder + (disorder | subject), data = data_test_fold1)
  model.M <- lmer(complexity_rating ~ obj_comp + (intricacy_4 | subject), data = data_test_fold1)
  model.Y <- lmer(beauty_rating ~ complexity_rating + obj_comp + disorder + (disorder | subject), data = data_test_fold1)

  results <- mediate(model.M, model.Y, treat='obj_comp', mediator='complexity_rating',
                    boot=FALSE, sims=500)
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
