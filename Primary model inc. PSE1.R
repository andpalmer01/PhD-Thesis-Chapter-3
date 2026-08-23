## Primary model including PSE1 as a predictor variable

# Load packages
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(janitor)
library(tidyr)
library(purrr)
library(tibble)
library(psych)
library(corrplot)
library(car)
library(mice)
library(mfp2)
library(MASS)
library(sjPlot)
library(mitools)
library(broom)
library(ggpubr)
library(boot)
library(howManyImputations)
library(Metrics)  # for rmse()
library(caret)    # for R²
library(ResourceSelection)  # optional for calibration
library(pROC)
library(scales)
library(patchwork)  # For combining plots
library(hexbin)
library(parallel)
library(lsr)
library(vcd)

# Specify the path to Excel file
PracticeFile <- "Practice Registry 2025.xlsx"

# Read the Excel file into R
PracticeData <- read_excel(PracticeFile)

# Filter data so that only the rows representing participants who completed the programme are left
CompletedData<-filter(PracticeData, Completed==1, PSE1!=999, PSE1!=2222,
                      PSE2!=999, PSE2!=2222, PSE2!=72)

# View the first few rows of the imported data
# head(CompletedData)
# table(CompletedData$PSE2)

# Convert all 2222, 999 & 1111 values to NA and view summary of columns
CompletedData[CompletedData==2222] <- NA
CompletedData[CompletedData==999] <- NA
CompletedData[CompletedData==1111] <- NA
# summary(CompletedData)

# Remove outliers, errors and irrelevant data
CompletedData$gender[CompletedData$gender==3] <- NA
CompletedData$SCSTotalMeanScore[CompletedData$SCSTotalMeanScore==6] <- NA
CompletedData$RMQ1[CompletedData$RMQ1==31] <- NA
CompletedData$TSK1[CompletedData$TSK1==13] <- NA
CompletedData$Performance1[CompletedData$Performance1==308] <- NA
CompletedData$program[CompletedData$program==3] <- NA
CompletedData$program[CompletedData$program==7] <- NA
CompletedData$Litigation[CompletedData$Litigation==5] <- NA
CompletedData$Litigation[CompletedData$Litigation==6] <- NA
CompletedData$Occup1[CompletedData$Occup1==11] <- NA

# Create change variable for PSE to be used as primary outcome
CompletedData$PSEchange <- CompletedData$PSE2-CompletedData$PSE1

# Count usable data in each column and make tibble
non_na_counts <- colSums(!is.na(CompletedData))
non_na_counts <- stack(non_na_counts)
non_na_counts <- tibble(non_na_counts)
# print(non_na_counts)
# view(non_na_counts)

na_counts <- colSums(is.na(CompletedData))
na_counts <- stack(na_counts)
na_counts <- tibble(na_counts)
# print(na_counts)
# view(na_counts)

# glimpse(CompletedData)
# class(CompletedData$PSEchange)
# table(SubsetData$PSEchange)

### data preparation ###
# change to numeric
CompletedData$duration <- as.numeric(CompletedData$duration)
CompletedData$Satisfaction1 <- as.numeric(CompletedData$Satisfaction1)
CompletedData$Performance1 <- as.numeric(CompletedData$Performance1)
CompletedData$IMDDecile <- as.numeric(CompletedData$IMDDecile)
CompletedData$PSFS1 <- as.numeric(CompletedData$PSFS1)

# change to factor
CompletedData$Ethnicity <- factor(CompletedData$Ethnicity)
CompletedData$GPCode <- factor(CompletedData$GPCode)
CompletedData$program <- factor(CompletedData$program)
CompletedData$programmeType <- factor(CompletedData$programmeType)
CompletedData$PrimaryDiagnosisGroup <- factor(CompletedData$PrimaryDiagnosisGroup)
CompletedData$MaritalStatus <- factor(CompletedData$MaritalStatus)
CompletedData$gender <- factor(CompletedData$gender)
CompletedData$Online <- factor(CompletedData$Online)
CompletedData$NoPrep <- factor(CompletedData$NoPrep)
CompletedData$Occup1 <- factor(CompletedData$Occup1)
CompletedData$Litigation <- factor(CompletedData$Litigation)
CompletedData$Employment <- factor(CompletedData$Employment)

# subset dataset with all predictor variables of potential use
SubsetData <- CompletedData%>%dplyr::select(age,PSE1,duration,Intensity1,Distress1,
                                            BDI1,RMQ1,PCS1,CPAQtot1,
                                            sitstan1,Walk5min1,Performance1,Satisfaction1,
                                            gender,Online,NoPrep,
                                            PrimaryDiagnosisGroup,Employment,Litigation,PSEchange)

# head(SubsetData)
# table(SubsetData$PSEchange)

# Count usable data in each column and make tibble
non_na_counts_Subset <- colSums(!is.na(SubsetData))
non_na_counts_Subset <- stack(non_na_counts_Subset)
non_na_counts_Subset <- tibble(non_na_counts_Subset)
## print(non_na_counts_Subset)
## view(non_na_counts_Subset)

# num_df <- CompletedData[sapply(CompletedData, is.numeric)]
# col_means <- colMeans(num_df, na.rm = TRUE)
# col_means <- stack(col_means)
# col_means <- tibble(col_means)
# print(col_means)
# view(col_means)

# apply(CompletedData, 2, sd, na.rm = TRUE)

# table(CompletedData$PrimaryDiagnosisGroup)
# table(CompletedData$gender)
# table(CompletedData$programmeType)
# table(CompletedData$Online)
# table(CompletedData$NoPrep)
# table(CompletedData$Employment)
# table(CompletedData$Litigation)

# Check amount of missing data
mean(is.na(SubsetData))

### Multiple imputation for missing values in subset
# md.pattern(SubsetData)
# Specify imputation methods for continuous and categorical variables
methods <- c(age = "pmm", 
             PSE1 = "pmm",
             duration = "pmm", 
             Intensity1 = "pmm", 
             Distress1 = "pmm",
             BDI1 = "pmm", 
             RMQ1 = "pmm", 
             PCS1 = "pmm", 
             CPAQtot1 = "pmm", 
             sitstan1 = "pmm", 
             Walk5min1 = "pmm", 
             Performance1 = "pmm", 
             Satisfaction1 = "pmm", 
             gender = "logreg",             # Binary categorical variable
             Online = "logreg",             # Binary categorical variable
             NoPrep = "logreg",             # Binary categorical variable
             PrimaryDiagnosisGroup = "polyreg", # Multinomial categorical variable
             Employment = "polyreg",        # Multinomial categorical variable
             Litigation = "logreg",         # Binary categorical variable
             PSEchange = "pmm")   
ImputedData <- mice(SubsetData, m = 20, method = methods, seed = 1)
summary(ImputedData)

# Variable names by type
continuous_vars <- c("age", "duration", "Intensity1", "Distress1", "BDI1", 
                     "RMQ1", "PCS1", "CPAQtot1", "sitstan1", "Walk5min1", 
                     "Performance1", "Satisfaction1", "PSEchange")

categorical_vars <- c("gender", "Online", "NoPrep", 
                      "PrimaryDiagnosisGroup", "Employment", "Litigation")

ImputedData1 <- complete(ImputedData, 1)

# Select only continuous variables
cont_data <- ImputedData1[, continuous_vars]

# Compute Pearson correlations (use method = "spearman" if non-normal)
cor_matrix <- cor(cont_data, use = "pairwise.complete.obs", method = "pearson")

# Display numeric matrix
print(round(cor_matrix, 2))

# Visualization
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.cex = 0.8, order = "hclust")

# Separate binary and multicategory
binary_vars <- c("gender", "Online", "NoPrep", "Litigation")
multi_vars <- c("PrimaryDiagnosisGroup", "Employment")

# Point-biserial correlation (binary vs continuous)
pb_results <- sapply(binary_vars, function(var) {
  sapply(continuous_vars, function(cont) {
    cor(ImputedData1[[cont]], as.numeric(ImputedData1[[var]]), use = "pairwise.complete.obs")
  })
})
pb_results <- round(pb_results, 2)
print(pb_results)

# Eta-squared (multi-category vs continuous)
eta_results <- lapply(multi_vars, function(var) {
  sapply(continuous_vars, function(cont) {
    model <- aov(ImputedData1[[cont]] ~ ImputedData1[[var]])
    etaSquared(model)[1, "eta.sq"]
  })
})
names(eta_results) <- multi_vars
eta_results

cramersV_results <- matrix(NA, nrow = length(categorical_vars), ncol = length(categorical_vars),
                           dimnames = list(categorical_vars, categorical_vars))

for (i in seq_along(categorical_vars)) {
  for (j in seq_along(categorical_vars)) {
    if (i < j) {
      tbl <- table(ImputedData1[[categorical_vars[i]]], ImputedData1[[categorical_vars[j]]])
      cramersV_results[i, j] <- assocstats(tbl)$cramer
    }
  }
}
round(cramersV_results, 2)

# Continuous predictors
cor_with_PSEchange <- sapply(continuous_vars[-length(continuous_vars)], function(x)
  cor(ImputedData1[[x]], ImputedData1$PSEchange, use = "pairwise.complete.obs"))
round(sort(cor_with_PSEchange, decreasing = TRUE), 2)

# Categorical predictors – use ANOVA effect size
eta_PSEchange <- sapply(categorical_vars, function(x) {
  model <- aov(ImputedData1$PSEchange ~ ImputedData1[[x]])
  etaSquared(model)[1, "eta.sq"]
})
round(sort(eta_PSEchange, decreasing = TRUE), 2)


set.seed(1)
mfp2_results_list <- list()
mfp2_selected_vars_list <- list()

for (i in 1:20) {
  imputed_dataset <- complete(ImputedData, i)
  cat("Running Imputation", i, "\n")
  
  ## Fit MFP2 Model with AIC selection
  mfp2_model <- mfp2(
    PSEchange ~ fp(age) + fp(PSE1) + fp(duration) + fp(Intensity1) + fp(Distress1) + 
      fp(BDI1) + fp(RMQ1) + fp(PCS1) + fp(CPAQtot1) + 
      fp(sitstan1) + fp(Walk5min1) + fp(Performance1) + 
      fp(Satisfaction1) + gender + Online + 
      NoPrep + PrimaryDiagnosisGroup + Employment + Litigation,
    family = "gaussian",
    data = imputed_dataset,
    criterion = "aic",
    verbose = FALSE
  )
  
  ## Extract selected variables from fp_terms
  fp_terms_df <- mfp2_model$fp_terms
  selected_rows <- rownames(fp_terms_df)[fp_terms_df$selected == TRUE]
  
  # Map back to base variable names (strip factor level suffixes)
  base_vars <- unique(gsub("[0-9]+$", "", selected_rows))
  
  ## Get fitted values for RMSE (no predict() needed)
  fitted_vals <- fitted(mfp2_model)
  mfp2_RMSE <- sqrt(mean((imputed_dataset$PSEchange - fitted_vals)^2))
  
  ## Store results
  mfp2_results_list[[paste0("Imp", i)]] <- list(
    Model = mfp2_model,
    Formula = formula(mfp2_model),
    AIC = AIC(mfp2_model),
    Coefficients = coef(mfp2_model),
    StdErrors = summary(mfp2_model)$coefficients[, "Std. Error"],
    RMSE = mfp2_RMSE,
    FP_terms = mfp2_model$fp_terms,
    Converged = mfp2_model$convergence_mfp
  )
  
  # Store selected variables
  mfp2_selected_vars_list[[paste0("Imp", i)]] <- base_vars
}

# Pooled AIC
mfp2_mean_AIC <- mean(sapply(mfp2_results_list, function(x) x$AIC))
cat("Pooled AIC (MFP2):", mfp2_mean_AIC, "\n")

mfp2_sd_AIC <- sd(sapply(mfp2_results_list, function(x) x$AIC))
cat("Pooled SD (MFP2):", mfp2_sd_AIC, "\n")

mfp2_mean_RMSE <- mean(sapply(mfp2_results_list, function(x) x$RMSE))
cat("Pooled RMSE (MFP2):", mfp2_mean_RMSE, "\n")

# Check coefficient variability
mfp2_coefs_df <- bind_rows(lapply(mfp2_results_list, function(x) {
  as.data.frame(t(x$Coefficients))
}))
view(mfp2_coefs_df)

# Count usable data in each column
non_na_counts_mfp2 <- colSums(!is.na(mfp2_coefs_df))
non_na_counts_mfp2 <- stack(non_na_counts_mfp2)
non_na_counts_mfp2 <- tibble(non_na_counts_mfp2)
print(non_na_counts_mfp2)
view(non_na_counts_mfp2)

# Tally variable frequency
mfp2_var_freq <- table(unlist(mfp2_selected_vars_list))
mfp2_selected_final_vars <- names(mfp2_var_freq[mfp2_var_freq >= 0.5 * length(mfp2_selected_vars_list)])
mfp2_selected_final_vars

## MFP2 predictor stability
mfp2_predictor_counts <- table(unlist(mfp2_selected_vars_list))

# Total number of imputations
mfp2_n_models <- length(mfp2_results_list)

# Keep predictors present in >= 50% of models
mfp2_stable_predictors <- names(mfp2_predictor_counts[mfp2_predictor_counts / mfp2_n_models >= 0.5])

# View result
print(mfp2_stable_predictors)

mfp2_predictor_freqs <- mfp2_predictor_counts / mfp2_n_models
mfp2_predictor_freqs[mfp2_predictor_freqs >= 0.5]


# Part 1: Coefficient Pooling using mice (Method 1) ----
cat("========================================\n")
cat("PART 1: COEFFICIENT POOLING\n")
cat("========================================\n\n")

# Step 1: Calculate the standard deviations from the original dataset
# This is crucial for accurate standardisation.
sd_data <- data.frame(
  term = c("PSEchange", "PSE1", "RMQ1", "CPAQtot1", "PCS1", "Intensity1", "Distress1", "Satisfaction1", "sitstan1"),
  sd = c(
    sd(SubsetData$PSEchange, na.rm = TRUE),
    sd(SubsetData$PSE1, na.rm = TRUE),
    sd(SubsetData$RMQ1, na.rm = TRUE),
    sd(SubsetData$CPAQtot1, na.rm = TRUE),
    sd(SubsetData$PCS1, na.rm = TRUE),
    sd(SubsetData$Intensity1, na.rm = TRUE),
    sd(SubsetData$Distress1, na.rm = TRUE),
    sd(SubsetData$Satisfaction1, na.rm = TRUE),
    sd(SubsetData$sitstan1, na.rm = TRUE)
  )
)
sd_data$term <- as.character(sd_data$term)

# Fit the model across all imputed datasets
glm_models_mice <- with(ImputedData,
                        glm(PSEchange ~ PSE1 + RMQ1 + CPAQtot1 + PCS1 + Intensity1 +
                              Distress1 + Satisfaction1 + sitstan1 + Online + NoPrep +
                              PrimaryDiagnosisGroup + Employment))

# Extract each fitted model from the with() output
model_list <- glm_models_mice$analyses

# Compute VIF for each imputed model
vif_list <- lapply(model_list, function(m) {
  # car::vif can fail if a factor has only one level after imputation,
  # so we use tryCatch for robustness
  tryCatch({
    vif_values <- car::vif(m)
    return(vif_values)
  }, error = function(e) {
    warning(paste("VIF failed for one model:", e$message))
    return(NA)
  })
})

# Combine VIFs across imputations (mean and max)
vif_summary <- do.call(cbind, vif_list)
vif_mean <- apply(vif_summary, 1, mean, na.rm = TRUE)
vif_max <- apply(vif_summary, 1, max, na.rm = TRUE)

# Create a neat summary table
vif_table <- data.frame(
  Variable = names(vif_mean),
  Mean_VIF = round(vif_mean, 2),
  Max_VIF = round(vif_max, 2)
)

cat("\n========================================\n")
cat("VIF SUMMARY ACROSS IMPUTATIONS\n")
cat("========================================\n")
print(vif_table)

# Pool the results using Rubin's rules
pooled_results <- pool(glm_models_mice)

# Get summary with pooled estimates
pooled_summary <- summary(pooled_results, conf.int = TRUE, conf.level = 0.95)

# Step 2: Create a named vector of standard deviations for continuous independent variables
sd_independent_vars <- c(
  "PSE1" = sd_data$sd[sd_data$term == "PSE1"],
  "RMQ1" = sd_data$sd[sd_data$term == "RMQ1"],
  "CPAQtot1" = sd_data$sd[sd_data$term == "CPAQtot1"],
  "PCS1" = sd_data$sd[sd_data$term == "PCS1"],
  "Intensity1" = sd_data$sd[sd_data$term == "Intensity1"],
  "Distress1" = sd_data$sd[sd_data$term == "Distress1"],
  "Satisfaction1" = sd_data$sd[sd_data$term == "Satisfaction1"],
  "sitstan1" = sd_data$sd[sd_data$term == "sitstan1"]
)

# Step 3: Calculate standardised betas using a vectorised approach
# Use `mapply` to apply a function to the coefficients and their names
pooled_summary$term <- as.character(pooled_summary$term)

# Step 3: Create the summary table with standardized betas AND their CIs
standardised_betas <- mapply(function(term, beta) {
  if (term %in% names(sd_independent_vars)) {
    sd_dep <- sd_data$sd[sd_data$term == "PSEchange"]
    sd_indep <- sd_independent_vars[term]
    return(beta * (sd_indep / sd_dep))
  } else {
    return(NA)
  }
}, pooled_summary$term, pooled_summary$estimate)

# Calculate standardized CI bounds
standardised_ci_lower <- mapply(function(term, ci_lower) {
  if (term %in% names(sd_independent_vars)) {
    sd_dep <- sd_data$sd[sd_data$term == "PSEchange"]
    sd_indep <- sd_independent_vars[term]
    return(ci_lower * (sd_indep / sd_dep))
  } else {
    return(NA)
  }
}, pooled_summary$term, pooled_summary$`2.5 %`)

standardised_ci_upper <- mapply(function(term, ci_upper) {
  if (term %in% names(sd_independent_vars)) {
    sd_dep <- sd_data$sd[sd_data$term == "PSEchange"]
    sd_indep <- sd_independent_vars[term]
    return(ci_upper * (sd_indep / sd_dep))
  } else {
    return(NA)
  }
}, pooled_summary$term, pooled_summary$`97.5 %`)

# Step 4: Create the summary table with the new column
results_table <- data.frame(
  Variable = pooled_summary$term,
  Coefficient = round(pooled_summary$estimate, 4),
  `Standardised Beta` = round(standardised_betas, 4),
  `Std Beta CI Lower` = round(standardised_ci_lower, 4),
  `Std Beta CI Upper` = round(standardised_ci_upper, 4),
  SE = round(pooled_summary$std.error, 4),
  `t-statistic` = round(pooled_summary$statistic, 3),
  df = round(pooled_summary$df, 1),
  `P-value` = format.pval(pooled_summary$p.value, digits = 3),
  `CI_Lower` = round(pooled_summary$`2.5 %`, 4),
  `CI_Upper` = round(pooled_summary$`97.5 %`, 4),
  check.names = FALSE
)

# Print the final table
print(results_table)

# # Step 5: Get partial eta-squared values for each predictor variable
# # Function to get overall partial eta-squared for categorical predictors
# get_overall_partial_eta2 <- function(imputed_data, predictor) {
#   # Fit full model (same as original)
#   full_models <- with(imputed_data,
#                       glm(PSEchange ~ RMQ1 + CPAQtot1 + PCS1 + Intensity1 +
#                             Distress1 + Satisfaction1 + sitstan1 + 
#                             Online + NoPrep +
#                             PrimaryDiagnosisGroup + Employment))
#   
#   # Create reduced formula by building it from remaining predictors
#   all_predictors <- c("RMQ1", "CPAQtot1", "PCS1", "Intensity1", 
#                       "Distress1", "Satisfaction1", "sitstan1",
#                       "Online", "NoPrep",
#                       "PrimaryDiagnosisGroup", "Employment")
#   
#   # Remove the predictor of interest
#   remaining_predictors <- all_predictors[all_predictors != predictor]
#   
#   # Build reduced formula string
#   reduced_formula_str <- paste("PSEchange ~", paste(remaining_predictors, collapse = " + "))
#   
#   # Fit reduced model
#   reduced_models <- with(imputed_data,
#                          glm(as.formula(reduced_formula_str)))
#   
#   # Calculate partial eta-squared for each imputation
#   partial_eta2_list <- sapply(1:length(full_models$analyses), function(i) {
#     full_model <- full_models$analyses[[i]]
#     reduced_model <- reduced_models$analyses[[i]]
#     
#     # Residual sum of squares
#     rss_full <- sum(residuals(full_model)^2)
#     rss_reduced <- sum(residuals(reduced_model)^2)
#     
#     # Partial eta-squared = (RSS_reduced - RSS_full) / RSS_reduced
#     partial_eta2 <- (rss_reduced - rss_full) / rss_reduced
#     return(partial_eta2)
#   })
#   
#   return(mean(partial_eta2_list))
# }
# 
# # List continuous and categorical predictors
# continuous_predictors <- c("RMQ1", "CPAQtot1", "PCS1", "Intensity1", 
#                            "Distress1", "Satisfaction1", "sitstan1")
# categorical_predictors <- c("Online", "NoPrep", 
#                             "PrimaryDiagnosisGroup", "Employment")
# 
# # Continuous predictor partial eta-squared (from t-statistics in pooled_summary)
# continuous_eta2 <- data.frame(
#   Variable = continuous_predictors,
#   `Partial η²` = sapply(continuous_predictors, function(var) {
#     t_stat <- pooled_summary$statistic[pooled_summary$term == var]
#     df <- pooled_summary$df[pooled_summary$term == var]
#     return(t_stat^2 / (t_stat^2 + df))
#   }),
#   check.names = FALSE
# )
# 
# # Categorical predictor partial eta-squared (overall effect)
# categorical_eta2 <- data.frame(
#   Variable = categorical_predictors,
#   `Partial η²` = sapply(categorical_predictors, function(pred) {
#     get_overall_partial_eta2(ImputedData, pred)
#   }),
#   check.names = FALSE
# )
# 
# # Combine
# all_eta2 <- rbind(continuous_eta2, categorical_eta2)
# all_eta2$`Partial η²` <- round(all_eta2$`Partial η²`, 4)
# 
# print(all_eta2)

## Part 2: Performance Metrics with Proper Pooling ----
cat("\n========================================\n")
cat("PART 2: PERFORMANCE METRICS POOLING\n")
cat("========================================\n\n")

# Initialize storage for performance metrics
m <- 20  # number of imputations
set.seed(1)
apparent_rmses <- numeric(m)
r2_values <- numeric(m)
cal_slopes <- numeric(m)
cal_intercepts <- numeric(m)
cal_slope_se <- numeric(m)
cal_intercept_se <- numeric(m)
eo_ratios <- numeric(m)
all_preds_obs <- data.frame()

# Calculate metrics for each imputation
for (i in 1:m) {
  cat("Processing Imputation", i, "\n")
  data_i <- complete(ImputedData, i)
  
  # Fit model
  model_i <- glm(PSEchange ~ PSE1 + BDI1 + CPAQtot1 + Litigation + Intensity1 +
                   Distress1 + Performance1 + PCS1 + Online + NoPrep + Walk5min1 +
                   PrimaryDiagnosisGroup + Employment,
                 data = data_i)
  
  # Predictions
  preds <- predict(model_i, newdata = data_i)
  
  # Store predictions and observations
  all_preds_obs <- rbind(all_preds_obs, 
                         data.frame(
                           Imputation = i,
                           Predicted = preds,
                           Observed = data_i$PSEchange
                         ))
  
  # RMSE
  apparent_rmses[i] <- sqrt(mean((data_i$PSEchange - preds)^2))
  
  # R-squared
  ss_res <- sum((data_i$PSEchange - preds)^2)
  ss_tot <- sum((data_i$PSEchange - mean(data_i$PSEchange))^2)
  r2_values[i] <- 1 - ss_res/ss_tot
  
  # E/O Ratio
  eo_ratios[i] <- mean(preds) / mean(data_i$PSEchange)
  
  # Calibration slope and intercept with SEs
  cal_model <- lm(data_i$PSEchange ~ preds)
  cal_intercepts[i] <- coef(cal_model)[1]
  cal_slopes[i] <- coef(cal_model)[2]
  cal_intercept_se[i] <- summary(cal_model)$coefficients[1, 2]
  cal_slope_se[i] <- summary(cal_model)$coefficients[2, 2]
}

## Pool performance metrics using appropriate methods

# RMSE: Pool using squared values then take square root
rmse_squared <- apparent_rmses^2
pooled_rmse_squared <- mean(rmse_squared)
within_var_rmse <- var(rmse_squared) / m
between_var_rmse <- var(rmse_squared)
total_var_rmse <- within_var_rmse + (1 + 1/m) * between_var_rmse
pooled_rmse <- sqrt(pooled_rmse_squared)
pooled_rmse_se <- sqrt(total_var_rmse) / (2 * pooled_rmse)  # Delta method

# R-squared: Pool using Fisher's z-transformation
z_transform <- atanh(sqrt(r2_values))  # Fisher's z-transformation
pooled_z <- mean(z_transform)
within_var_z <- mean((1/(nrow(complete(ImputedData, 1)) - 3)))  # Approximate within-variance
between_var_z <- var(z_transform)
total_var_z <- within_var_z + (1 + 1/m) * between_var_z
pooled_r2 <- tanh(pooled_z)^2
pooled_r2_se <- sqrt(total_var_z) * 2 * tanh(pooled_z) * (1 - tanh(pooled_z)^2)  # Delta method

# Calibration slope and intercept: Pool using Rubin's rules
# Calibration slope
pooled_cal_slope <- mean(cal_slopes)
within_var_slope <- mean(cal_slope_se^2)
between_var_slope <- var(cal_slopes)
total_var_slope <- within_var_slope + (1 + 1/m) * between_var_slope
pooled_cal_slope_se <- sqrt(total_var_slope)
df_slope <- (m - 1) * (1 + within_var_slope/((1 + 1/m) * between_var_slope))^2
ci_slope <- pooled_cal_slope + c(-1, 1) * qt(0.975, df_slope) * pooled_cal_slope_se

# Calibration intercept
pooled_cal_intercept <- mean(cal_intercepts)
within_var_intercept <- mean(cal_intercept_se^2)
between_var_intercept <- var(cal_intercepts)
total_var_intercept <- within_var_intercept + (1 + 1/m) * between_var_intercept
pooled_cal_intercept_se <- sqrt(total_var_intercept)
df_intercept <- (m - 1) * (1 + within_var_intercept/((1 + 1/m) * between_var_intercept))^2
ci_intercept <- pooled_cal_intercept + c(-1, 1) * qt(0.975, df_intercept) * pooled_cal_intercept_se

# E/O Ratio: Simple pooling (as it's a ratio of means)
pooled_eo <- mean(eo_ratios)
pooled_eo_se <- sd(eo_ratios) / sqrt(m)
ci_eo <- pooled_eo + c(-1, 1) * qt(0.975, m-1) * pooled_eo_se

# Print pooled performance metrics
cat("\nPooled Performance Metrics:\n")
cat("---------------------------\n")
cat(sprintf("Apparent RMSE: %.3f (SE: %.3f)\n", pooled_rmse, pooled_rmse_se))
cat(sprintf("R-squared: %.3f (SE: %.3f)\n", pooled_r2, pooled_r2_se))
cat(sprintf("Calibration Intercept: %.3f (SE: %.3f, 95%% CI: [%.3f, %.3f])\n", 
            pooled_cal_intercept, pooled_cal_intercept_se, ci_intercept[1], ci_intercept[2]))
cat(sprintf("Calibration Slope: %.3f (SE: %.3f, 95%% CI: [%.3f, %.3f])\n", 
            pooled_cal_slope, pooled_cal_slope_se, ci_slope[1], ci_slope[2]))
cat(sprintf("E/O Ratio: %.3f (SE: %.3f, 95%% CI: [%.3f, %.3f])\n", 
            pooled_eo, pooled_eo_se, ci_eo[1], ci_eo[2]))

# Create summary table for performance metrics
performance_table <- data.frame(
  Metric = c("Apparent RMSE", "R-squared", "Calibration Intercept", 
             "Calibration Slope", "E/O Ratio"),
  Estimate = round(c(pooled_rmse, pooled_r2, pooled_cal_intercept, 
                     pooled_cal_slope, pooled_eo), 3),
  SE = round(c(pooled_rmse_se, pooled_r2_se, pooled_cal_intercept_se, 
               pooled_cal_slope_se, pooled_eo_se), 3),
  `CI_Lower` = round(c(NA, NA, ci_intercept[1], ci_slope[1], ci_eo[1]), 3),
  `CI_Upper` = round(c(NA, NA, ci_intercept[2], ci_slope[2], ci_eo[2]), 3),
  check.names = FALSE
)

print(performance_table)
