## Follow-up attendance analyses

# Specify the path to Excel file
PracticeFile <- "Practice Registry 2025.xlsx"

# Read the Excel file into R
PracticeData <- read_excel(PracticeFile)

CompletedData<-filter(PracticeData, Completed==1)

# Filter data so that only the rows representing participants who completed the programme are left
CompletedDataFU<-filter(SubsetData, AttendedFollowUp==1)
CompletedDataNFU<-filter(SubsetData, AttendedFollowUp %in% c(NA, 2))

# Combine into one dataframe with a group indicator
CompletedDataFU$group <- "Completer"
CompletedDataNFU$group <- "Non-completer"
df <- bind_rows(CompletedDataFU, CompletedDataNFU)

# --- Run tests per variable ---
baseline_vars <- setdiff(colnames(df), "group")  # all columns except 'group'

results <- data.frame(
  variable = character(),
  test_used = character(),
  statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (var in baseline_vars) {
  
  comp <- df$group == "Completer"
  
  if (is.numeric(df[[var]])) {
    # Skip if variance is zero in either group
    if (var(df[[var]][comp], na.rm = TRUE) == 0 || var(df[[var]][!comp], na.rm = TRUE) == 0) {
      next
    }
    test <- wilcox.test(df[[var]][comp], df[[var]][!comp])
    results <- rbind(results, data.frame(
      variable   = var,
      test_used  = "Wilcox test",
      statistic  = test$statistic,
      p_value    = test$p.value
    ))
    
  } else if (is.factor(df[[var]]) || is.character(df[[var]])) {
    # Categorical: chi-squared test
    contingency <- table(df[[var]], df$group)
    test <- chisq.test(contingency)
    results <- rbind(results, data.frame(
      variable   = var,
      test_used  = "Chi-squared",
      statistic  = test$statistic,
      p_value    = test$p.value
    ))
  }
}

# --- Output ---
results$significant <- ifelse(results$p_value < 0.05, "Yes", "No")
print(results)

library(effectsize)

for (var in baseline_vars[sapply(df[baseline_vars], is.numeric)]) {
  x <- df[[var]][!is.na(df[[var]])]
  g <- df$group[!is.na(df[[var]])]
  es <- rank_biserial(x ~ g)
  cat(var, ": r =", es$r_rank_biserial, "\n")
}

for (var in baseline_vars[sapply(df[baseline_vars], function(x) is.factor(x) || is.character(x))]) {
  contingency <- table(df[[var]], df$group)
  es <- cramers_v(contingency)
  cat(var, ": V =", es$Cramers_v_adjusted, "\n")
}







# Count usable data in each column and make tibble
non_na_counts <- colSums(!is.na(CompletedDataNFU))
non_na_counts <- stack(non_na_counts)
non_na_countsNFU <- tibble(non_na_counts)
# print(non_na_counts)
# view(non_na_counts)

na_counts <- colSums(is.na(SubsetData))
na_counts <- stack(na_counts)
na_counts <- tibble(na_counts)
# print(na_counts)
# view(na_counts)


num_df <- SubsetData[sapply(SubsetData, is.numeric)]
col_means <- colMeans(num_df, na.rm = TRUE)
col_means <- stack(col_means)
col_means <- tibble(col_means)
print(col_means)
view(col_means)

num_dfNFU <- CompletedDataNFU[sapply(CompletedDataNFU, is.numeric)]
col_meansNFU <- colMeans(num_dfNFU, na.rm = TRUE)
col_meansNFU <- stack(col_meansNFU)
col_meansNFU <- tibble(col_meansNFU)
print(col_meansNFU)
view(col_meansNFU)

apply(CompletedData, 2, sd, na.rm = TRUE)

apply(CompletedData, 2, median, na.rm = TRUE)
median(CompletedData$PSE1)
median(CompletedData$PCS1)

apply(CompletedData, 2, IQR, na.rm = TRUE)
IQR(CompletedData$PSE1)
IQR(CompletedData$PCS1)

dplyr_df<-CompletedData |>
  dplyr::select(where(is.numeric)) |>
  summarise(across(
    everything(),
    list(
      mean   = ~mean(.x, na.rm = TRUE),
      sd     = ~sd(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      IQR    = ~IQR(.x, na.rm = TRUE)
    )
  ))

view(dplyr_df)

library(matrixStats)

x <- as.matrix(num_df)

cbind(
  mean   = matrixStats::colMeans2(x, na.rm = TRUE),
  sd     = matrixStats::colSds(x, na.rm = TRUE),
  median = matrixStats::colMedians(x, na.rm = TRUE),
  IQR    = matrixStats::colIQRs(x, na.rm = TRUE)
)

table(CompletedData$PrimaryDiagnosisGroup)
table(CompletedData$gender)
table(CompletedData$programmeType)
table(CompletedData$Online)
table(CompletedData$NoPrep)
table(CompletedData$Employment)
table(CompletedData$Litigation)