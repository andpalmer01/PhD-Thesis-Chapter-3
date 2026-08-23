#=============================================================================
# BOOTSTRAP INTERNAL VALIDATION WITH MULTIPLE IMPUTATION AND MFP2
# PARALLELISED VERSION
# Using median for optimism calculations
#=============================================================================

library(mice)
library(mfp2)
library(dplyr)
library(parallel)

#-----------------------------------------------------------------------------
# CONFIGURATION
#-----------------------------------------------------------------------------

outcome_var <- "PSEchange"

fp_vars <- c("age", "duration", "Intensity1", "Distress1", "BDI1", "RMQ1", 
             "PCS1", "CPAQtot1", "sitstan1", "Walk5min1", "Performance1", 
             "Satisfaction1")

linear_vars <- c("gender", "Online", "NoPrep", 
                 "PrimaryDiagnosisGroup", "Employment", "Litigation")

# Detect cores
n_cores <- detectCores() - 1
cat("Detected", detectCores(), "cores. Using", n_cores, "for parallel processing.\n")

#-----------------------------------------------------------------------------
# HELPER FUNCTIONS
#-----------------------------------------------------------------------------

calc_rmse <- function(observed, predicted) {
  sqrt(mean((observed - predicted)^2, na.rm = TRUE))
}

calc_r2 <- function(observed, predicted) {
  ss_res <- sum((observed - predicted)^2, na.rm = TRUE)
  ss_tot <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
  1 - ss_res / ss_tot
}

calc_calibration <- function(observed, predicted) {
  cal_fit <- lm(observed ~ predicted)
  list(
    intercept = unname(coef(cal_fit)[1]),
    slope = unname(coef(cal_fit)[2])
  )
}

calc_eo_ratio <- function(observed, predicted) {
  mean(predicted, na.rm = TRUE) / mean(observed, na.rm = TRUE)
}

build_mfp_formula <- function(outcome, fp_vars, linear_vars) {
  fp_terms <- paste0("fp(", fp_vars, ")", collapse = " + ")
  linear_terms <- paste(linear_vars, collapse = " + ")
  formula_str <- paste(outcome, "~", fp_terms, "+", linear_terms)
  as.formula(formula_str)
}

get_selected_vars <- function(mfp_fit, fp_vars, linear_vars) {
  fp_terms_df <- mfp_fit$fp_terms
  selected_rows <- rownames(fp_terms_df)[fp_terms_df$selected == TRUE]
  base_names <- unique(gsub("\\..*$", "", gsub("[0-9]+$", "", selected_rows)))
  all_vars <- c(fp_vars, linear_vars)
  selected_original <- character(0)
  for (v in all_vars) {
    v_base <- gsub("[0-9]+$", "", v)
    if (any(grepl(paste0("^", v_base), base_names, ignore.case = TRUE)) ||
        v %in% selected_rows ||
        v_base %in% base_names) {
      selected_original <- c(selected_original, v)
    }
  }
  unique(selected_original)
}

build_glm_formula <- function(outcome, selected_vars) {
  if (length(selected_vars) == 0) {
    return(as.formula(paste(outcome, "~ 1")))
  }
  as.formula(paste(outcome, "~", paste(selected_vars, collapse = " + ")))
}

#-----------------------------------------------------------------------------
# SINGLE BOOTSTRAP ITERATION (to be run in parallel)
#-----------------------------------------------------------------------------

run_single_bootstrap <- function(b, data, n, y, mfp_formula, outcome_var, 
                                 fp_vars, linear_vars) {
  
  # Draw bootstrap sample
  boot_idx <- sample(1:n, n, replace = TRUE)
  boot_data <- data[boot_idx, ]
  y_boot <- boot_data[[outcome_var]]
  
  # Fit MFP2 on bootstrap sample
  fit_boot_mfp <- tryCatch({
    mfp2(mfp_formula,
         data = boot_data,
         family = "gaussian",
         criterion = "aic",
         verbose = FALSE)
  }, error = function(e) NULL)
  
  if (is.null(fit_boot_mfp)) {
    return(list(
      optimism = c(rmse = NA, r2 = NA, cal_slope = NA, cal_int = NA, eo_ratio = NA),
      selected_vars = character(0)
    ))
  }
  
  # Get selected variables
  boot_selected <- get_selected_vars(fit_boot_mfp, fp_vars, linear_vars)
  
  if (length(boot_selected) == 0) {
    return(list(
      optimism = c(rmse = NA, r2 = NA, cal_slope = NA, cal_int = NA, eo_ratio = NA),
      selected_vars = character(0)
    ))
  }
  
  # Fit glm with selected variables
  glm_formula_boot <- build_glm_formula(outcome_var, boot_selected)
  
  fit_boot_glm <- tryCatch({
    glm(glm_formula_boot, data = boot_data, family = gaussian())
  }, error = function(e) NULL)
  
  if (is.null(fit_boot_glm)) {
    return(list(
      optimism = c(rmse = NA, r2 = NA, cal_slope = NA, cal_int = NA, eo_ratio = NA),
      selected_vars = boot_selected
    ))
  }
  
  # TRAINING performance
  pred_boot_train <- predict(fit_boot_glm, newdata = boot_data, type = "response")
  
  train_rmse <- calc_rmse(y_boot, pred_boot_train)
  train_r2 <- calc_r2(y_boot, pred_boot_train)
  train_cal <- calc_calibration(y_boot, pred_boot_train)
  train_eo <- calc_eo_ratio(y_boot, pred_boot_train)
  
  # TEST performance (on ORIGINAL data)
  pred_boot_test <- tryCatch({
    predict(fit_boot_glm, newdata = data, type = "response")
  }, error = function(e) rep(NA, n))
  
  if (any(is.na(pred_boot_test))) {
    return(list(
      optimism = c(rmse = NA, r2 = NA, cal_slope = NA, cal_int = NA, eo_ratio = NA),
      selected_vars = boot_selected
    ))
  }
  
  test_rmse <- calc_rmse(y, pred_boot_test)
  test_r2 <- calc_r2(y, pred_boot_test)
  test_cal <- calc_calibration(y, pred_boot_test)
  test_eo <- calc_eo_ratio(y, pred_boot_test)
  
  # OPTIMISM
  list(
    optimism = c(
      rmse = test_rmse - train_rmse,
      r2 = train_r2 - test_r2,
      cal_slope = train_cal$slope - test_cal$slope,
      cal_int = train_cal$intercept - test_cal$intercept,
      eo_ratio = train_eo - test_eo
    ),
    selected_vars = boot_selected
  )
}

#-----------------------------------------------------------------------------
# MAIN FUNCTION: Bootstrap validation within single imputed dataset
#-----------------------------------------------------------------------------

bootstrap_validate_single_imp <- function(data, 
                                          outcome_var,
                                          fp_vars,
                                          linear_vars,
                                          n_boot = 500,
                                          seed = NULL,
                                          n_cores = 1,
                                          verbose = FALSE) {
  
  n <- nrow(data)
  y <- data[[outcome_var]]
  mfp_formula <- build_mfp_formula(outcome_var, fp_vars, linear_vars)
  
  #--- Step 1: Fit apparent model ---
  fit_apparent <- tryCatch({
    mfp2(mfp_formula,
         data = data,
         family = "gaussian",
         criterion = "aic",
         verbose = FALSE)
  }, error = function(e) {
    warning("Apparent model fitting failed: ", e$message)
    return(NULL)
  })
  
  if (is.null(fit_apparent)) return(NULL)
  
  apparent_selected <- get_selected_vars(fit_apparent, fp_vars, linear_vars)
  
  if (length(apparent_selected) == 0) {
    warning("No variables selected in apparent model")
    return(NULL)
  }
  
  glm_formula_apparent <- build_glm_formula(outcome_var, apparent_selected)
  glm_apparent <- glm(glm_formula_apparent, data = data, family = gaussian())
  pred_apparent <- predict(glm_apparent, type = "response")
  
  apparent_metrics <- list(
    rmse = calc_rmse(y, pred_apparent),
    r2 = calc_r2(y, pred_apparent),
    cal_slope = 1,
    cal_int = 0,
    eo_ratio = calc_eo_ratio(y, pred_apparent)
  )
  
  #--- Step 2: Parallel bootstrap loop ---
  if (verbose) cat("  Starting parallel bootstrap (", n_cores, " cores)...\n")
  
  # Create cluster
  cl <- makeCluster(n_cores)
  
  # Set seed for reproducibility
  if (!is.null(seed)) {
    clusterSetRNGStream(cl, seed)
  }
  
  # Export necessary objects and functions to cluster
  clusterExport(cl, c(
    "data", "n", "y", "mfp_formula", "outcome_var", "fp_vars", "linear_vars",
    "run_single_bootstrap", "get_selected_vars", "build_glm_formula",
    "calc_rmse", "calc_r2", "calc_calibration", "calc_eo_ratio"
  ), envir = environment())
  
  # Load libraries on each worker
  clusterEvalQ(cl, {
    library(mfp2)
  })
  
  # Run parallel bootstrap
  boot_results <- tryCatch({
    parLapply(cl, 1:n_boot, function(b) {
      run_single_bootstrap(b, data, n, y, mfp_formula, outcome_var, 
                           fp_vars, linear_vars)
    })
  }, error = function(e) {
    stopCluster(cl)
    stop("Parallel bootstrap failed: ", e$message)
  })
  
  # Stop cluster
  stopCluster(cl)
  
  #--- Step 3: Aggregate results ---
  optimism_matrix <- do.call(rbind, lapply(boot_results, function(x) x$optimism))
  selected_vars_tracker <- lapply(boot_results, function(x) x$selected_vars)
  
  n_successful <- sum(!is.na(optimism_matrix[, "rmse"]))
  
  if (n_successful == 0) {
    warning("No successful bootstrap samples")
    return(NULL)
  }
  
  median_optimism <- apply(optimism_matrix, 2, median, na.rm = TRUE)
  iqr_optimism <- apply(optimism_matrix, 2, IQR, na.rm = TRUE)
  optimism_percentiles <- apply(optimism_matrix, 2, quantile, 
                                probs = c(0.025, 0.25, 0.75, 0.975), na.rm = TRUE)
  
  #--- Step 4: Optimism-corrected metrics ---
  corrected_metrics <- list(
    rmse = apparent_metrics$rmse + median_optimism["rmse"],
    r2 = apparent_metrics$r2 - median_optimism["r2"],
    cal_slope = apparent_metrics$cal_slope - median_optimism["cal_slope"],
    cal_int = apparent_metrics$cal_int - median_optimism["cal_int"],
    eo_ratio = apparent_metrics$eo_ratio - median_optimism["eo_ratio"]
  )
  
  #--- Step 5: Bootstrap Inclusion Fractions ---
  all_candidate_vars <- c(fp_vars, linear_vars)
  valid_selections <- Filter(function(x) length(x) > 0, selected_vars_tracker)
  n_valid_boots <- length(valid_selections)
  
  if (n_valid_boots > 0) {
    bif <- sapply(all_candidate_vars, function(v) {
      sum(sapply(valid_selections, function(sel) v %in% sel)) / n_valid_boots
    })
  } else {
    bif <- setNames(rep(0, length(all_candidate_vars)), all_candidate_vars)
  }
  
  list(
    apparent_selected_vars = apparent_selected,
    apparent_metrics = apparent_metrics,
    median_optimism = median_optimism,
    iqr_optimism = iqr_optimism,
    optimism_percentiles = optimism_percentiles,
    corrected_metrics = corrected_metrics,
    bif = sort(bif, decreasing = TRUE),
    optimism_matrix = optimism_matrix,
    n_successful_boots = n_successful,
    n_total_boots = n_boot
  )
}

#-----------------------------------------------------------------------------
# WRAPPER: Run validation across all imputed datasets
#-----------------------------------------------------------------------------

validate_across_imputations <- function(mice_obj,
                                        outcome_var,
                                        fp_vars,
                                        linear_vars,
                                        n_boot = 500,
                                        seed = 1,
                                        n_cores = 1,
                                        verbose = TRUE) {
  
  m <- mice_obj$m
  results_list <- vector("list", m)
  
  for (i in seq_len(m)) {
    if (verbose) cat("Processing imputation", i, "of", m, "\n")
    
    dat_i <- complete(mice_obj, i)
    
    # Use different seed per imputation for reproducibility
    imp_seed <- if (!is.null(seed)) seed + i * 1000 else NULL
    
    results_list[[i]] <- bootstrap_validate_single_imp(
      data = dat_i,
      outcome_var = outcome_var,
      fp_vars = fp_vars,
      linear_vars = linear_vars,
      n_boot = n_boot,
      seed = imp_seed,
      n_cores = n_cores,
      verbose = verbose
    )
    
    if (!is.null(results_list[[i]])) {
      if (verbose) cat("  -> ", results_list[[i]]$n_successful_boots, "/", 
                       n_boot, " successful\n")
    } else {
      if (verbose) cat("  -> FAILED\n")
    }
  }
  
  valid_results <- Filter(Negate(is.null), results_list)
  m_valid <- length(valid_results)
  
  if (m_valid == 0) stop("All imputation validations failed!")
  if (m_valid < m) warning(m - m_valid, " imputation(s) failed")
  
  # Pool optimism
  all_optimism <- do.call(rbind, lapply(valid_results, function(x) x$optimism_matrix))
  pooled_median_optimism <- apply(all_optimism, 2, median, na.rm = TRUE)
  pooled_percentiles <- apply(all_optimism, 2, quantile, 
                              probs = c(0.025, 0.25, 0.75, 0.975), na.rm = TRUE)
  
  per_imp_medians <- do.call(rbind, lapply(valid_results, function(x) x$median_optimism))
  sd_between_imputations <- apply(per_imp_medians, 2, sd, na.rm = TRUE)
  
  # Pool apparent metrics
  apparent_pooled <- list(
    rmse = mean(sapply(valid_results, function(x) x$apparent_metrics$rmse)),
    r2 = mean(sapply(valid_results, function(x) x$apparent_metrics$r2)),
    cal_slope = 1,
    cal_int = 0,
    eo_ratio = mean(sapply(valid_results, function(x) x$apparent_metrics$eo_ratio))
  )
  
  # Final corrected
  final_corrected <- list(
    rmse = apparent_pooled$rmse + pooled_median_optimism["rmse"],
    r2 = apparent_pooled$r2 - pooled_median_optimism["r2"],
    cal_slope = apparent_pooled$cal_slope - pooled_median_optimism["cal_slope"],
    cal_int = apparent_pooled$cal_int - pooled_median_optimism["cal_int"],
    eo_ratio = apparent_pooled$eo_ratio - pooled_median_optimism["eo_ratio"]
  )
  
  # Pool BIF
  bif_matrix <- do.call(rbind, lapply(valid_results, function(x) x$bif))
  pooled_bif <- apply(bif_matrix, 2, mean, na.rm = TRUE)
  
  # Variable selection across imputations
  all_selected_vars <- unlist(lapply(valid_results, function(x) x$apparent_selected_vars))
  var_selection_freq <- table(all_selected_vars)
  var_selection_prop <- var_selection_freq / m_valid
  stable_vars <- names(var_selection_prop[var_selection_prop >= 0.5])
  
  list(
    apparent_pooled = apparent_pooled,
    optimism_pooled = pooled_median_optimism,
    corrected_pooled = final_corrected,
    optimism_ci = pooled_percentiles,
    sd_between_imputations = sd_between_imputations,
    stable_vars_across_imputations = stable_vars,
    var_selection_frequency = sort(var_selection_prop, decreasing = TRUE),
    pooled_bif = sort(pooled_bif, decreasing = TRUE),
    per_imputation_results = valid_results,
    n_imputations = m,
    n_valid_imputations = m_valid,
    n_boot_per_imp = n_boot
  )
}

#=============================================================================
# RUN THE VALIDATION
#=============================================================================

set.seed(1)

cat("\n========================================\n")
cat("STARTING PARALLELISED BOOTSTRAP VALIDATION\n")
cat("========================================\n\n")

start_time <- Sys.time()

validation_results <- validate_across_imputations(
  mice_obj = ImputedData,
  outcome_var = "PSEchange",
  fp_vars = c("age", "duration", "Intensity1", "Distress1", "BDI1", "RMQ1", 
              "PCS1", "CPAQtot1", "sitstan1", "Walk5min1", "Performance1", 
              "Satisfaction1"),
  linear_vars = c("gender", "Online", "NoPrep", 
                  "PrimaryDiagnosisGroup", "Employment", "Litigation"),
  n_boot = 500,
  seed = 1,
  n_cores = n_cores,
  verbose = TRUE
)

end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "mins")
cat("\nTotal time:", round(as.numeric(elapsed), 1), "minutes\n")

#=============================================================================
# DISPLAY RESULTS
#=============================================================================

cat("\n========================================\n")
cat("BOOTSTRAP INTERNAL VALIDATION RESULTS\n")
cat("(Median optimism)\n")
cat("========================================\n\n")

cat("--- APPARENT PERFORMANCE ---\n")
cat(sprintf("RMSE:              %.3f\n", validation_results$apparent_pooled$rmse))
cat(sprintf("R-squared:         %.3f\n", validation_results$apparent_pooled$r2))
cat(sprintf("Calibration slope: %.3f\n", validation_results$apparent_pooled$cal_slope))
cat(sprintf("Calibration int:   %.3f\n", validation_results$apparent_pooled$cal_int))
cat(sprintf("E/O ratio:         %.3f\n", validation_results$apparent_pooled$eo_ratio))

cat("\n--- MEDIAN OPTIMISM ---\n")
cat(sprintf("RMSE:              %.4f\n", validation_results$optimism_pooled["rmse"]))
cat(sprintf("R-squared:         %.4f\n", validation_results$optimism_pooled["r2"]))
cat(sprintf("Calibration slope: %.4f\n", validation_results$optimism_pooled["cal_slope"]))
cat(sprintf("Calibration int:   %.4f\n", validation_results$optimism_pooled["cal_int"]))
cat(sprintf("E/O ratio:         %.4f\n", validation_results$optimism_pooled["eo_ratio"]))

cat("\n--- OPTIMISM-CORRECTED ---\n")
cat(sprintf("RMSE:              %.3f\n", validation_results$corrected_pooled$rmse))
cat(sprintf("R-squared:         %.3f\n", validation_results$corrected_pooled$r2))
cat(sprintf("Calibration slope: %.3f\n", validation_results$corrected_pooled$cal_slope))
cat(sprintf("Calibration int:   %.3f\n", validation_results$corrected_pooled$cal_int))
cat(sprintf("E/O ratio:         %.3f\n", validation_results$corrected_pooled$eo_ratio))

cat("\n--- OPTIMISM 95% CI ---\n")
cat(sprintf("RMSE:      [%.4f, %.4f]\n", 
            validation_results$optimism_ci["2.5%", "rmse"],
            validation_results$optimism_ci["97.5%", "rmse"]))
cat(sprintf("R-squared: [%.4f, %.4f]\n", 
            validation_results$optimism_ci["2.5%", "r2"],
            validation_results$optimism_ci["97.5%", "r2"]))
cat(sprintf("Cal slope: [%.4f, %.4f]\n", 
            validation_results$optimism_ci["2.5%", "cal_slope"],
            validation_results$optimism_ci["97.5%", "cal_slope"]))

cat("\n--- VARIABLE SELECTION ---\n")
cat("Stable (>=50% imputations):\n")
print(validation_results$stable_vars_across_imputations)
cat("\nFrequency:\n")
print(round(validation_results$var_selection_frequency, 2))
cat("\nBootstrap Inclusion Fractions:\n")
print(round(validation_results$pooled_bif, 3))

# Summary table
summary_table <- data.frame(
  Metric = c("RMSE", "R-squared", "Cal Slope", "Cal Intercept", "E/O Ratio"),
  Apparent = round(c(validation_results$apparent_pooled$rmse,
                     validation_results$apparent_pooled$r2,
                     validation_results$apparent_pooled$cal_slope,
                     validation_results$apparent_pooled$cal_int,
                     validation_results$apparent_pooled$eo_ratio), 3),
  Optimism = round(c(validation_results$optimism_pooled["rmse"],
                     validation_results$optimism_pooled["r2"],
                     validation_results$optimism_pooled["cal_slope"],
                     validation_results$optimism_pooled["cal_int"],
                     validation_results$optimism_pooled["eo_ratio"]), 4),
  Corrected = round(c(validation_results$corrected_pooled$rmse,
                      validation_results$corrected_pooled$r2,
                      validation_results$corrected_pooled$cal_slope,
                      validation_results$corrected_pooled$cal_int,
                      validation_results$corrected_pooled$eo_ratio), 3)
)

cat("\n========================================\n")
cat("SUMMARY TABLE\n")
cat("========================================\n")
print(summary_table, row.names = FALSE)

# Save tables
write.csv(summary_table, "optimism.csv", row.names = FALSE)

