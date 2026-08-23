## T2-T0 model for PCS

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
CompletedData<-filter(PracticeData, Completed==1, AttendedFollowUp==1, 
                      PCS1!=999, PCS1!=2222, PCS3!=999, PCS3!=2222, PCS3!=55)

# View the first few rows of the imported data
# head(CompletedData)
# table(CompletedData$PCS2)

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

# Create change variable for PCS to be used as primary outcome
CompletedData$PCSchange <- CompletedData$PCS3-CompletedData$PCS1

# Count usable data in each column and make tibble
non_na_counts <- colSums(!is.na(CompletedData))
non_na_counts <- stack(non_na_counts)
non_na_counts <- tibble(non_na_counts)
# print(non_na_counts)
# view(non_na_counts)

# glimpse(CompletedData)
# class(CompletedData$PCSchange)
# table(SubsetData$PCSchange)

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

# subset dataset with all predictor variables of potential use (no PCS1)
SubsetData <- CompletedData%>%dplyr::select(age,duration,Intensity1,Distress1,
                                            BDI1,RMQ1,PSE1,CPAQtot1,
                                            sitstan1,Walk5min1,Performance1,Satisfaction1,
                                            gender,Online,NoPrep,
                                            PrimaryDiagnosisGroup,Employment,Litigation,PCSchange)

# head(SubsetData)
# table(SubsetData$PCSchange)

# Count usable data in each column and make tibble
non_na_counts_Subset <- colSums(!is.na(SubsetData))
non_na_counts_Subset <- stack(non_na_counts_Subset)
non_na_counts_Subset <- tibble(non_na_counts_Subset)
## print(non_na_counts_Subset)
## view(non_na_counts_Subset)

# Check amount of missing data
mean(is.na(SubsetData))

### Multiple imputation for missing values in subset
# md.pattern(SubsetData)
# Specify imputation methods for continuous and categorical variables
methods <- c(age = "pmm", 
             duration = "pmm", 
             Intensity1 = "pmm", 
             Distress1 = "pmm",
             BDI1 = "pmm", 
             RMQ1 = "pmm", 
             PSE1 = "pmm", 
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
             PCSchange = "pmm")   
ImputedData <- mice(SubsetData, m = 20, method = methods, seed = 1)
summary(ImputedData)

# Variable names by type
continuous_vars <- c("age", "duration", "Intensity1", "Distress1", "BDI1", 
                     "RMQ1", "PSE1", "CPAQtot1", "sitstan1", "Walk5min1", 
                     "Performance1", "Satisfaction1", "PCSchange")

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
cor_with_PCSchange <- sapply(continuous_vars[-length(continuous_vars)], function(x)
  cor(ImputedData1[[x]], ImputedData1$PCSchange, use = "pairwise.complete.obs"))
round(sort(cor_with_PCSchange, decreasing = TRUE), 2)

# Categorical predictors – use ANOVA effect size
eta_PCSchange <- sapply(categorical_vars, function(x) {
  model <- aov(ImputedData1$PCSchange ~ ImputedData1[[x]])
  etaSquared(model)[1, "eta.sq"]
})
round(sort(eta_PCSchange, decreasing = TRUE), 2)


set.seed(1)
mfp2_results_list <- list()
mfp2_selected_vars_list <- list()

for (i in 1:20) {
  imputed_dataset <- complete(ImputedData, i)
  cat("Running Imputation", i, "\n")
  
  ## Fit MFP2 Model with AIC selection
  mfp2_model <- mfp2(
    PCSchange ~ fp(age) + fp(duration) + fp(Intensity1) + fp(Distress1) + 
      fp(BDI1) + fp(RMQ1) + fp(PSE1) + fp(CPAQtot1) + 
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
  mfp2_RMSE <- sqrt(mean((imputed_dataset$PCSchange - fitted_vals)^2))
  
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
  term = c("PCSchange", "age", "BDI1", "RMQ1", "CPAQtot1", "sitstan1"),
  sd = c(
    sd(SubsetData$PCSchange, na.rm = TRUE),
    sd(SubsetData$age, na.rm = TRUE),
    sd(SubsetData$BDI1, na.rm = TRUE),
    sd(SubsetData$RMQ1, na.rm = TRUE),
    sd(SubsetData$CPAQtot1, na.rm = TRUE),
    sd(SubsetData$sitstan1, na.rm = TRUE)
  )
)
sd_data$term <- as.character(sd_data$term)

# Fit the model across all imputed datasets
glm_models_mice <- with(ImputedData,
                        glm(PCSchange ~ age + BDI1 + RMQ1 + CPAQtot1 +
                              sitstan1 + gender + NoPrep + 
                              PrimaryDiagnosisGroup + Employment + Litigation))

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

# Create a summary table
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
  "age" = sd_data$sd[sd_data$term == "age"],
  "BDI1" = sd_data$sd[sd_data$term == "BDI1"],
  "RMQ1" = sd_data$sd[sd_data$term == "RMQ1"],
  "CPAQtot1" = sd_data$sd[sd_data$term == "CPAQtot1"],
  "sitstan1" = sd_data$sd[sd_data$term == "sitstan1"]
)

# Step 3: Calculate standardised betas using a vectorised approach
# Use `mapply` to apply a function to the coefficients and their names
pooled_summary$term <- as.character(pooled_summary$term)

# Step 3: Create the summary table with standardized betas AND their CIs
standardised_betas <- mapply(function(term, beta) {
  if (term %in% names(sd_independent_vars)) {
    sd_dep <- sd_data$sd[sd_data$term == "PCSchange"]
    sd_indep <- sd_independent_vars[term]
    return(beta * (sd_indep / sd_dep))
  } else {
    return(NA)
  }
}, pooled_summary$term, pooled_summary$estimate)

# Calculate standardized CI bounds
standardised_ci_lower <- mapply(function(term, ci_lower) {
  if (term %in% names(sd_independent_vars)) {
    sd_dep <- sd_data$sd[sd_data$term == "PCSchange"]
    sd_indep <- sd_independent_vars[term]
    return(ci_lower * (sd_indep / sd_dep))
  } else {
    return(NA)
  }
}, pooled_summary$term, pooled_summary$`2.5 %`)

standardised_ci_upper <- mapply(function(term, ci_upper) {
  if (term %in% names(sd_independent_vars)) {
    sd_dep <- sd_data$sd[sd_data$term == "PCSchange"]
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
#                       glm(PCSchange ~ age + BDI1 + RMQ1 + CPAQtot1 +
# sitstan1 + gender + NoPrep + 
#   PrimaryDiagnosisGroup + Employment + Litigation))
#   
#   # Create reduced formula by building it from remaining predictors
#   all_predictors <- c("age", "BDI1", "RMQ1", "CPAQtot1",
#                       "sitstan1", "gender", "NoPrep", "PrimaryDiagnosisGroup",
#                       "Employment", "Litigation")
#   
#   # Remove the predictor of interest
#   remaining_predictors <- all_predictors[all_predictors != predictor]
#   
#   # Build reduced formula string
#   reduced_formula_str <- paste("PCSchange ~", paste(remaining_predictors, collapse = " + "))
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
# continuous_predictors <- c("age", "BDI1", "RMQ1", "CPAQtot1", "sitstan1")
# categorical_predictors <- c("gender", "NoPrep", "PrimaryDiagnosisGroup",
#                             "Employment", "Litigation")
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



## Part 2: Performance Metrics with Pooling ----
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
  model_i <- glm(PCSchange ~ age + BDI1 + RMQ1 + CPAQtot1 +
                   sitstan1 + gender + NoPrep + 
                   PrimaryDiagnosisGroup + Employment + Litigation,
                 data = data_i)
  
  # Predictions
  preds <- predict(model_i, newdata = data_i)
  
  # Store predictions and observations
  all_preds_obs <- rbind(all_preds_obs, 
                         data.frame(
                           Imputation = i,
                           Predicted = preds,
                           Observed = data_i$PCSchange
                         ))
  
  # RMSE
  apparent_rmses[i] <- sqrt(mean((data_i$PCSchange - preds)^2))
  
  # R-squared
  ss_res <- sum((data_i$PCSchange - preds)^2)
  ss_tot <- sum((data_i$PCSchange - mean(data_i$PCSchange))^2)
  r2_values[i] <- 1 - ss_res/ss_tot
  
  # E/O Ratio
  eo_ratios[i] <- mean(preds) / mean(data_i$PCSchange)
  
  # Calibration slope and intercept with SEs
  cal_model <- lm(data_i$PCSchange ~ preds)
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

## Part 3: Calibration Plots ----
cat("\n========================================\n")
cat("PART 3: CALIBRATION PLOTS\n")
cat("========================================\n\n")

# Overall calibration plot
cal_plot_overall <- ggplot(all_preds_obs, aes(x = Predicted, y = Observed)) +
  geom_hex(bins = 30, alpha = 0.8) +  # Hexbins for better visualization with many points
  geom_smooth(method = "loess", color = "blue", se = TRUE, alpha = 0.3) +  # Flexible calibration curve
  geom_smooth(method = "lm", color = "darkblue", se = FALSE, linetype = "dotted") +  # Linear fit
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +  # Perfect calibration
  geom_rug(alpha = 0.02, length = unit(0.02, "npc")) +  # Distribution indicators
  labs(
    title = "Pooled Calibration Plot Across All Imputations",
    subtitle = sprintf("Calibration: Intercept = %.3f (%.3f, %.3f), Slope = %.3f (%.3f, %.3f)",
                       pooled_cal_intercept, ci_intercept[1], ci_intercept[2],
                       pooled_cal_slope, ci_slope[1], ci_slope[2]),
    x = "Predicted PCS Change",
    y = "Observed PCS Change"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 12)
  ) +
  scale_fill_viridis_c(name = "Count")

print(cal_plot_overall)

# Calibration plot by imputation (faceted)
cal_plot_faceted <- ggplot(all_preds_obs, aes(x = Predicted, y = Observed)) +
  geom_point(alpha = 0.3, size = 0.5, color = "darkgrey") +
  geom_smooth(method = "lm", color = "blue", se = TRUE, alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ Imputation, ncol = 5, scales = "fixed") +
  labs(
    title = "Calibration Plots by Imputation",
    x = "Predicted PCS Change",
    y = "Observed PCS Change"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 7)
  )

print(cal_plot_faceted)

## Part 4: Decile Calibration Plot ----
cat("\n========================================\n")
cat("PART 4: DECILE CALIBRATION PLOT\n")
cat("========================================\n\n")

cal_data_deciles <- all_preds_obs %>%
  mutate(Decile = ntile(Predicted, 10)) %>%
  group_by(Decile) %>%
  summarise(
    MeanPred = mean(Predicted),
    MeanObs = mean(Observed),
    SDPred = sd(Predicted),
    SDObs = sd(Observed),
    N = n(),
    SE = SDObs / sqrt(N),
    CI_lower = MeanObs - 1.96 * SE,
    CI_upper = MeanObs + 1.96 * SE,
    .groups = 'drop'
  )

# Decile calibration plot
decile_plot <- ggplot(cal_data_deciles, aes(x = MeanPred, y = MeanObs)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                width = diff(range(cal_data_deciles$MeanPred)) * 0.02,
                color = "darkblue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue", 
              alpha = 0.3, linetype = "solid", size = 0.5) +
  geom_text(aes(label = Decile), vjust = -1, hjust = 0.5, size = 3, color = "darkgrey") +
  labs(
    title = "Calibration Plot by Deciles of Predicted Risk",
    subtitle = sprintf("Based on %d observations across %d imputations", 
                       nrow(all_preds_obs), m),
    x = "Mean Predicted PCS Change",
    y = "Mean Observed PCS Change",
    caption = "Error bars represent 95% confidence intervals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8, face = "italic"),
    axis.title = element_text(size = 12)
  ) +
  coord_fixed()

print(decile_plot)

# Print decile summary table
cat("\nDecile Calibration Summary:\n")
print(cal_data_deciles %>%
        mutate(across(where(is.numeric), ~round(., 3))) %>%
        dplyr::select(Decile, N, MeanPred, MeanObs, CI_lower, CI_upper))

## Part 5: Distribution plots ----
cat("\n========================================\n")
cat("PART 5: DISTRIBUTION PLOTS\n")
cat("========================================\n\n")

# Distribution of predictions vs observations
dist_plot <- all_preds_obs %>%
  pivot_longer(cols = c(Predicted, Observed), 
               names_to = "Type", 
               values_to = "PCSchange") %>%
  ggplot(aes(x = PCSchange, fill = Type)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Imputation, ncol = 5, scales = "fixed") +
  labs(
    title = "Distribution of Predicted vs Observed Values by Imputation",
    x = "PCS Change",
    y = "Density"
  ) +
  scale_fill_manual(values = c("Predicted" = "blue", "Observed" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

print(dist_plot)

# Residual plots
residual_data <- all_preds_obs %>%
  mutate(Residual = Observed - Predicted)

residual_plot <- ggplot(residual_data, aes(x = Predicted, y = Residual)) +
  geom_hex(bins = 30, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", color = "blue", se = TRUE, alpha = 0.3) +
  labs(
    title = "Residual Plot",
    subtitle = sprintf("Mean residual: %.3f, SD: %.3f", 
                       mean(residual_data$Residual), 
                       sd(residual_data$Residual)),
    x = "Predicted PCS Change",
    y = "Residual (Observed - Predicted)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  ) +
  scale_fill_viridis_c(name = "Count")

print(residual_plot)

## Part 6: Combined visualization ----
cat("\n========================================\n")
cat("PART 6: COMBINED VISUALIZATION\n")
cat("========================================\n\n")

# Combine key plots using patchwork
combined_plot <- (cal_plot_overall | decile_plot) / residual_plot +
  plot_annotation(
    title = "Model Calibration Assessment",
    subtitle = sprintf("Based on %d imputations with Rubin's rules pooling", m),
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
  )

print(combined_plot)

## Part 7: Export results ----
cat("\n========================================\n")
cat("PART 7: EXPORTING RESULTS\n")
cat("========================================\n\n")

# Save plots
ggsave("calibration_plot_overall.png", cal_plot_overall, width = 8, height = 8, dpi = 300)
ggsave("calibration_plot_deciles.png", decile_plot, width = 8, height = 8, dpi = 300)
ggsave("calibration_plot_faceted.png", cal_plot_faceted, width = 12, height = 6, dpi = 300)
ggsave("residual_plot.png", residual_plot, width = 8, height = 6, dpi = 300)
ggsave("combined_calibration.png", combined_plot, width = 14, height = 10, dpi = 300)

# Save tables
write.csv(results_table, "pooled_coefficients.csv", row.names = FALSE)
write.csv(performance_table, "pooled_performance_metrics.csv", row.names = FALSE)
write.csv(cal_data_deciles, "decile_calibration.csv", row.names = FALSE)

cat("All results have been exported successfully!\n")
cat("Files saved: \n")
cat("- pooled_coefficients.csv\n")
cat("- pooled_performance_metrics.csv\n")
cat("- decile_calibration.csv\n")
cat("- calibration_plot_overall.png\n")
cat("- calibration_plot_deciles.png\n")
cat("- calibration_plot_faceted.png\n")
cat("- residual_plot.png\n")
cat("- combined_calibration.png\n")

# Calculate shrunk coefficients and standardized betas
shrinkage_factor <- 0.770

results_table_shrunk <- results_table %>%
  dplyr::mutate(
    `Shrunk Coefficient` = Coefficient * shrinkage_factor,
    `Shrunk Std Beta` = `Standardised Beta` * shrinkage_factor
  )

# Create a factor with the original order of variables (excluding intercept)
original_order <- results_table_shrunk %>%
  dplyr::filter(Variable != "(Intercept)") %>%
  dplyr::pull(Variable)

# Prepare long format data with significance indicator
plot_long <- results_table_shrunk %>%
  dplyr::filter(Variable != "(Intercept)") %>%
  # Add significance indicator based on p-value
  dplyr::mutate(
    # Convert P-value to numeric (handling "<" signs)
    p_numeric = as.numeric(gsub("<", "", `P-value`)),
    significant = ifelse(p_numeric < 0.05, "Significant (p < 0.05)", "Not Significant")
  ) %>%
  dplyr::select(Variable, Coefficient, `Standardised Beta`, 
                CI_Lower, CI_Upper, 
                `Std Beta CI Lower`, `Std Beta CI Upper`, 
                significant) %>%
  tidyr::pivot_longer(cols = c(Coefficient, `Standardised Beta`),
                      names_to = "Type",
                      values_to = "Value") %>%
  dplyr::filter(!is.na(Value)) %>%
  dplyr::mutate(
    # Convert Variable to factor with original order
    Variable = factor(Variable, levels = rev(original_order)),
    # Update labels for facets
    Type = case_when(
      Type == "Coefficient" ~ "Unstandardised Coefficients",
      Type == "Standardised Beta" ~ "Standardised Beta",
      TRUE ~ Type
    ),
    # Assign appropriate CIs based on Type
    CI_Lower_adj = ifelse(Type == "Standardised Beta", 
                          `Std Beta CI Lower`, 
                          CI_Lower),
    CI_Upper_adj = ifelse(Type == "Standardised Beta", 
                          `Std Beta CI Upper`, 
                          CI_Upper),
    estimate_type = "Original"
  )

# Create shrunk estimates data (for BOTH panels)
plot_shrunk <- results_table_shrunk %>%
  dplyr::filter(Variable != "(Intercept)") %>%
  dplyr::mutate(
    p_numeric = as.numeric(gsub("<", "", `P-value`)),
    significant = ifelse(p_numeric < 0.05, "Significant (p < 0.05)", "Not Significant")
  ) %>%
  dplyr::select(Variable, `Shrunk Coefficient`, `Shrunk Std Beta`, significant) %>%
  tidyr::pivot_longer(cols = c(`Shrunk Coefficient`, `Shrunk Std Beta`),
                      names_to = "Type",
                      values_to = "Value") %>%
  dplyr::filter(!is.na(Value)) %>%
  dplyr::mutate(
    Variable = factor(Variable, levels = rev(original_order)),
    Type = case_when(
      Type == "Shrunk Coefficient" ~ "Unstandardised Coefficients",
      Type == "Shrunk Std Beta" ~ "Standardised Beta",
      TRUE ~ Type
    ),
    CI_Lower_adj = NA,
    CI_Upper_adj = NA,
    estimate_type = "Shrunk (optimism-corrected)"
  ) %>%
  dplyr::select(Variable, Type, Value, CI_Lower_adj, CI_Upper_adj, significant, estimate_type)

# Combine original and shrunk data
plot_combined <- bind_rows(plot_long, plot_shrunk)

# ── Shared plot function ───────────────────────────────────────────────────────
make_forest_plot <- function(data, title_label, x_label) {
  ggplot(data, aes(x = Value, y = Variable,
                   color = significant,
                   shape = estimate_type,
                   alpha = estimate_type)) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.4) +
    geom_errorbarh(data = filter(data, estimate_type == "Original"),
                   aes(xmin = CI_Lower_adj, xmax = CI_Upper_adj),
                   height = 0, size = 0.6, na.rm = TRUE) +
    geom_point(size = 3.5, position = position_dodge(width = 0.3)) +
    scale_color_manual(values = c("Significant (p < 0.05)" = "#D55E00",
                                  "Not Significant" = "#999999"),
                       name = "Significance") +
    scale_shape_manual(values = c("Original" = 18,
                                  "Shrunk (optimism-corrected)" = 17),
                       name = "Estimate Type") +
    scale_alpha_manual(values = c("Original" = 1.0,
                                  "Shrunk (optimism-corrected)" = 0.7),
                       name = "Estimate Type") +
    labs(title = title_label,
         x = x_label,
         y = NULL) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.major.y = element_line(color = "gray85", size = 0.3),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", size = 0.3),
      panel.border = element_rect(color = "black", size = 0.8),
      strip.background = element_rect(fill = "gray95", color = "black", size = 0.8),
      strip.text = element_text(face = "bold", size = 12),
      axis.text.y = element_text(size = 10, color = "black", hjust = 0),
      axis.text.x = element_text(size = 10, color = "black"),
      axis.title.x = element_text(size = 11, face = "bold"),
      plot.title = element_text(size = 13, face = "bold"),
      axis.ticks = element_line(size = 0.4),
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10, face = "bold"),
      legend.box = "vertical",
      plot.margin = margin(10, 10, 10, 10)
    ) +
    guides(
      color = guide_legend(order = 1),
      shape = guide_legend(order = 2),
      alpha = guide_legend(order = 2)
    )
}

# ── Figure 1: Unstandardised Coefficients ─────────────────────────────────────
p_unstd <- make_forest_plot(
  data        = filter(plot_combined, Type == "Unstandardised Coefficients"),
  title_label = "Unstandardised Coefficients",
  x_label     = "\nCoefficient (95% CI for original estimates only)"
)

# ── Figure 2: Standardised Beta ───────────────────────────────────────────────
p_std <- make_forest_plot(
  data        = filter(plot_combined, Type == "Standardised Beta"),
  title_label = "Standardised Beta",
  x_label     = "\nStandardised Beta (95% CI for original estimates only)"
)

print(p_unstd)
print(p_std)

# ── Save both figures ──────────────────────────────────────────────────────────
for (suffix in c("pdf", "png")) {
  ggsave(paste0("forest_plot_unstandardised.", suffix), plot = p_unstd,
         width = 8, height = 7, dpi = ifelse(suffix == "png", 600, 300),
         bg = "white")
  ggsave(paste0("forest_plot_standardised.", suffix),  plot = p_std,
         width = 8, height = 7, dpi = ifelse(suffix == "png", 600, 300),
         bg = "white")
}







# Create publication-ready faceted forest plot with shrunk estimates
p_with_shrinkage <- ggplot(plot_combined, 
                           aes(x = Value, y = Variable, 
                               color = significant,
                               shape = estimate_type,
                               alpha = estimate_type)) +
  # Add reference line at zero
  geom_vline(xintercept = 0, 
             linetype = "solid", 
             color = "black", 
             size = 0.4) +
  
  # Add error bars (only for original estimates)
  geom_errorbarh(data = filter(plot_combined, estimate_type == "Original"),
                 aes(xmin = CI_Lower_adj, xmax = CI_Upper_adj), 
                 height = 0, 
                 size = 0.6,
                 na.rm = TRUE) +
  
  # Add point estimates
  geom_point(size = 3.5,
             position = position_dodge(width = 0.3)) +
  
  # Color scale for significance
  scale_color_manual(values = c("Significant (p < 0.05)" = "#D55E00",
                                "Not Significant" = "#999999"),
                     name = "Significance") +
  
  # Shape scale for estimate type
  scale_shape_manual(values = c("Original" = 18,  # Diamond
                                "Shrunk (optimism-corrected)" = 17),  # Triangle
                     name = "Estimate Type") +
  
  # Alpha scale for estimate type
  scale_alpha_manual(values = c("Original" = 1.0,
                                "Shrunk (optimism-corrected)" = 0.7),
                     name = "Estimate Type") +
  
  # Facet by coefficient type
  facet_wrap(~ factor(Type, levels = c("Unstandardised Coefficients", "Standardised Beta")), 
             scales = "free_x",
             nrow = 1) +
  
  # Labels
  labs(x = "\nEstimate (95% CI for original estimates only)",
       y = NULL) +
  
  # Professional theme
  theme_bw(base_size = 11) +
  theme(
    # Panel formatting
    panel.grid.major.y = element_line(color = "gray85", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", size = 0.3),
    panel.border = element_rect(color = "black", size = 0.8),
    
    # Facet strip formatting
    strip.background = element_rect(fill = "gray95", color = "black", size = 0.8),
    strip.text = element_text(face = "bold", size = 12),
    
    # Axis formatting
    axis.text.y = element_text(size = 10, color = "black", hjust = 0),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.ticks = element_line(size = 0.4),
    
    # Legend at bottom
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold"),
    legend.box = "vertical",
    
    # General
    plot.margin = margin(10, 10, 10, 10)
  ) +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2),
    alpha = guide_legend(order = 2)
  )

print(p_with_shrinkage)

# Save the plot
ggsave("forest_plot_with_shrinkage.pdf", 
       plot = p_with_shrinkage,
       width = 11, 
       height = 7,
       dpi = 300)

ggsave("forest_plot_with_shrinkage.png", 
       plot = p_with_shrinkage,
       width = 11, 
       height = 7, 
       dpi = 600,
       bg = "white")






# Create the shrunk results table
results_table_shrunk <- results_table

# Apply shrinkage to coefficients (excluding intercept)
is_intercept <- results_table_shrunk$Variable == "(Intercept)"

# Shrink coefficients
results_table_shrunk$Coefficient[!is_intercept] <- 
  results_table_shrunk$Coefficient[!is_intercept] * shrinkage_factor

# Shrink confidence intervals
results_table_shrunk$CI_Lower[!is_intercept] <- 
  results_table_shrunk$CI_Lower[!is_intercept] * shrinkage_factor
results_table_shrunk$CI_Upper[!is_intercept] <- 
  results_table_shrunk$CI_Upper[!is_intercept] * shrinkage_factor

# Shrink standardised betas (only for continuous variables)
if("Standardised Beta" %in% names(results_table_shrunk)) {
  not_na_beta <- !is.na(results_table_shrunk$`Standardised Beta`)
  results_table_shrunk$`Standardised Beta`[not_na_beta] <- 
    results_table_shrunk$`Standardised Beta`[not_na_beta] * shrinkage_factor
}

# Round for display
results_table_shrunk <- data.frame(
  Variable = results_table_shrunk$Variable,
  Coefficient = round(results_table_shrunk$Coefficient, 4),
  `Standardised Beta` = round(results_table_shrunk$`Standardised Beta`, 4),
  SE = round(results_table_shrunk$SE, 4),  # SE stays the same
  `t-statistic` = round(results_table_shrunk$`t-statistic`, 3),  # t-stat stays the same
  df = round(results_table_shrunk$df, 1),  # df stays the same
  `P-value` = results_table_shrunk$`P-value`,  # P-value stays the same
  `CI_Lower` = round(results_table_shrunk$CI_Lower, 4),
  `CI_Upper` = round(results_table_shrunk$CI_Upper, 4),
  check.names = FALSE
)

# Print the shrunk table
cat("\n========================================\n")
cat("SHRUNKEN COEFFICIENTS (Calibration Slope = 0.770)\n")
cat("========================================\n")
print(results_table_shrunk)
write.csv(results_table_shrunk, "pooled_coefficients_shrunk.csv", row.names = FALSE)

# Create comparison table
comparison_table <- data.frame(
  Variable = results_table$Variable,
  Original_Coef = round(results_table$Coefficient, 4),
  Shrunk_Coef = round(results_table_shrunk$Coefficient, 4),
  Reduction = ifelse(results_table$Variable != "(Intercept)", 
                     paste0(round((1 - shrinkage_factor) * 100, 1), "%"),
                     "None"),
  Original_StdBeta = round(results_table$`Standardised Beta`, 4),
  Shrunk_StdBeta = round(results_table_shrunk$`Standardised Beta`, 4),
  P_value = results_table$`P-value`,
  check.names = FALSE
)

cat("\n========================================\n")
cat("COMPARISON: Original vs Shrunk Coefficients\n")
cat("Shrinkage Factor (Calibration Slope): 0.770\n")
cat("========================================\n")
print(comparison_table)
write.csv(comparison_table, "pooled_coefficients_comparison.csv", row.names = FALSE)

# Get partial eta-squared values for each predictor variable with shrinkage
# Function to get overall partial eta-squared for categorical predictors
get_overall_partial_eta2_shrunk <- function(imputed_data, predictor, shrinkage_factor) {
  
  # Fit full model
  full_models <- with(imputed_data,
                      glm(PCSchange ~ age + BDI1 + RMQ1 + CPAQtot1 +
                            sitstan1 + gender + NoPrep + 
                            PrimaryDiagnosisGroup + Employment + Litigation))
  
  # All predictors
  all_predictors <- c("age", "BDI1", "RMQ1", "CPAQtot1", "sitstan1", 
                      "gender", "NoPrep",
                      "PrimaryDiagnosisGroup", "Employment", "Litigation")
  
  # Remove the predictor of interest
  remaining_predictors <- all_predictors[all_predictors != predictor]
  reduced_formula_str <- paste("PCSchange ~", paste(remaining_predictors, collapse = " + "))
  
  # Fit reduced model
  reduced_models <- with(imputed_data,
                         glm(as.formula(reduced_formula_str)))
  
  # Calculate partial eta-squared for each imputation using shrunk coefficients
  partial_eta2_list <- sapply(1:length(full_models$analyses), function(i) {
    full_model <- full_models$analyses[[i]]
    reduced_model <- reduced_models$analyses[[i]]
    
    # Get model matrices
    X_full <- model.matrix(full_model)
    X_reduced <- model.matrix(reduced_model)
    y <- full_model$y
    
    # Apply shrinkage to coefficients (excluding intercept)
    coefs_full <- coef(full_model)
    coefs_full[-1] <- coefs_full[-1] * shrinkage_factor  # Don't shrink intercept
    
    coefs_reduced <- coef(reduced_model)
    coefs_reduced[-1] <- coefs_reduced[-1] * shrinkage_factor
    
    # Calculate fitted values using shrunk coefficients
    fitted_full <- X_full %*% coefs_full
    fitted_reduced <- X_reduced %*% coefs_reduced
    
    # Calculate RSS using shrunk fitted values
    rss_full <- sum((y - fitted_full)^2)
    rss_reduced <- sum((y - fitted_reduced)^2)
    
    # Partial eta-squared
    partial_eta2 <- (rss_reduced - rss_full) / rss_reduced
    return(partial_eta2)
  })
  
  return(mean(partial_eta2_list))
}

# Recalculate categorical partial eta-squared with shrinkage
shrinkage_factor <- 0.770


# List continuous and categorical predictors
continuous_predictors <- c("age", "BDI1", "RMQ1", "CPAQtot1", "sitstan1")
categorical_predictors <- c("gender", "NoPrep", "PrimaryDiagnosisGroup", "Employment", "Litigation")

categorical_eta2_shrunk <- data.frame(
  Variable = categorical_predictors,
  `Partial η²` = sapply(categorical_predictors, function(pred) {
    get_overall_partial_eta2_shrunk(ImputedData, pred, shrinkage_factor)
  }),
  check.names = FALSE
)

# For continuous predictors, adjust t-statistic approach
# Partial eta2 from shrunk standardised betas
continuous_eta2_shrunk <- data.frame(
  Variable = continuous_predictors,
  `Partial η²` = sapply(continuous_predictors, function(var) {
    t_stat <- pooled_summary$statistic[pooled_summary$term == var] * shrinkage_factor
    df <- pooled_summary$df[pooled_summary$term == var]
    return(t_stat^2 / (t_stat^2 + df))
  }),
  check.names = FALSE
)

# Combine
all_eta2_shrunk <- rbind(continuous_eta2_shrunk, categorical_eta2_shrunk)
all_eta2_shrunk$`Partial η²` <- round(all_eta2_shrunk$`Partial η²`, 4)

print(all_eta2_shrunk)


## Relative importance of predictors plot (eta2 shrunk)
all_eta2_shrunk_plot <- all_eta2_shrunk %>%
  arrange(`Partial η²`) %>%
  mutate(
    Variable = factor(Variable, levels = Variable),
    Effect_Size = case_when(
      `Partial η²` >= 0.14 ~ "Large (≥0.14)",
      `Partial η²` >= 0.06 ~ "Medium (0.06-0.14)",
      `Partial η²` >= 0.01 ~ "Small (0.01-0.06)",
      TRUE ~ "Negligible (<0.01)"
    ),
    Effect_Size = factor(Effect_Size, 
                         levels = c("Negligible (<0.01)", "Small (0.01-0.06)", 
                                    "Medium (0.06-0.14)", "Large (≥0.14)"))
  )

p_eta_shrunk_colored <- ggplot(all_eta2_shrunk_plot, 
                               aes(x = `Partial η²`, y = Variable, fill = Effect_Size)) +
  geom_col(alpha = 0.9) +
  scale_fill_manual(values = c("Negligible (<0.01)" = "#999999",
                               "Small (0.01-0.06)" = "#E69F00",
                               "Medium (0.06-0.14)" = "#56B4E9",
                               "Large (≥0.14)" = "#009E73")) +
  labs(x = "Partial η² (Proportion of unique variance explained)",
       y = NULL,
       fill = "Effect Size",
       title = "Relative importance of predictors") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10, color = "black"),
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )

print(p_eta_shrunk_colored)

# Save as PDF
ggsave("partial_eta_squared_comparison.pdf", 
       plot = p_eta_shrunk_colored, 
       width = 8, 
       height = 6, 
       dpi = 300)

# Save as high-resolution PNG
ggsave("partial_eta_squared_comparison.png", 
       plot = p_eta_shrunk_colored, 
       width = 8, 
       height = 6, 
       dpi = 600,
       bg = "white")

library(openxlsx)
write.xlsx(all_eta2_shrunk, "partial_eta_squared_shrunk.xlsx", rowNames = FALSE)