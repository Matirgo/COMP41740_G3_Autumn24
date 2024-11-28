# ===== Setup for statistical analysis of experiment results. =====

# If not already installed, install necessary libraries.
# Uncomment the code below if installation is required.
# install.packages("dplyr")

# Load necessary libraries.
library(dplyr)

# Read in the .csv file and store it as "Experiment_Results".
Experiment_Results <- read.csv("Experiment_Results.csv")

# View a summary of the dataset.
summary(Experiment_Results)

# ===== Shapiro-Wilk Test, Concise VS. Detailed Explanations =====

# Calculate the differences between the paired scores.
paired_differences_topic_scores <- 
  Experiment_Results$Concise_Topic_Score - 
  Experiment_Results$Detailed_Topic_Score

# Perform the Shapiro-Wilk test.
shapiro_test_topic_scores <- 
  shapiro.test(paired_differences_topic_scores)

# Print the Shapiro-Wilk test result.
cat("Shapiro-Wilk Test for Normality:\n")
print(shapiro_test_topic_scores)

# Interpretation of the p-value.
if (shapiro_test_topic_scores$p.value >= 0.05) {
  cat("The paired differences are likely normally distributed (p =", shapiro_test_topic_scores$p.value, ").
      \n Paired t-test recommended.")
} else {
  cat("The paired differences are not normally distributed (p =", shapiro_test_topic_scores$p.value, ").
      \n Wilcoxon-signed rank test recommended.")
}

# ===== Paired t-test, Concise VS. Detailed Explanations =====

# Run a paired sample t-test to compare test scores
# following concise and detailed explanations.
t_test_result_concise_vs_detailed_answers <- t.test(
  Experiment_Results$Concise_Topic_Score,
  Experiment_Results$Detailed_Topic_Score,
  paired = TRUE
)

# Calculate standard deviations.
concise_topic_score_sd <- sd(Experiment_Results$Concise_Topic_Score, na.rm = TRUE)
detailed_topic_score_sd <- sd(Experiment_Results$Detailed_Topic_Score, na.rm = TRUE)

# Print the results of the t-test.
print(t_test_result_concise_vs_detailed_answers)

# Extract useful information into a DataFrame.
t_test_summary_consice_vs_detailed_scores <- data.frame(
  T_Statistic = t_test_result_concise_vs_detailed_answers$statistic,
  Degrees_of_Freedom = t_test_result_concise_vs_detailed_answers$parameter,
  P_Value = t_test_result_concise_vs_detailed_answers$p.value,
  Confidence_Interval_Lower = t_test_result_concise_vs_detailed_answers$conf.int[1],
  Confidence_Interval_Upper = t_test_result_concise_vs_detailed_answers$conf.int[2],
  Mean_Difference = t_test_result_concise_vs_detailed_answers$estimate,
  Concise_SD = concise_topic_score_sd,
  Detailed_SD = detailed_topic_score_sd,
  Shapiro_Wilk_Normality = shapiro_test_topic_scores$p.value
)

# Save the summary to a .csv file.
write.csv(t_test_summary_consice_vs_detailed_scores,
          "t_test_summary_consice_vs_detailed_scores.csv",
          row.names = FALSE)

# Print confirmation.
cat("t-test summary saved to 't_test_summary_consice_vs_detailed_scores.csv'\n")

# Save the plot as a PNG file.
png(filename = "boxplot_concise_vs_detailed_scores.png", width = 800, height = 600)

# Display the results as side-by-side box plots.
boxplot(Experiment_Results$Concise_Topic_Score,
        Experiment_Results$Detailed_Topic_Score,
        main = "Participants' Test Scores Following \n Concisve VS. Detailed Explanations",
        at = c(1, 2),
        names = c("Concise Explanations", "Detailed Explanations"),
        ylab = "Number of correctly answered questions",
        col = "white",
        border = "black",
        horizontal = FALSE,
        notch = FALSE,
        yaxt = "n")

# Set y-axis to increment by one within the number of correctly
# answered questions. (Removes decimal points.)
axis(2, at = 
       seq(0, max(c(Experiment_Results$Concise_Topic_Score,
                    Experiment_Results$Detailed_Topic_Score)),
           by = 1), las = 2)

# Display the p-value on the plot.
p_value <- t_test_result_concise_vs_detailed_answers$p.value
text(x = 1.5, y = max(c(Experiment_Results$Concise_Topic_Score,
                        Experiment_Results$Detailed_Topic_Score)), 
     labels = paste("p-value =", round(p_value, 3)), cex = 0.8)

# Finalise and save the plot.
dev.off()

# ===== Shapiro-Wilk Test, Concise VS. Detailed Understanding =====

# Calculate the differences between the paired scores.
paired_differences_topic_understanding <- 
  Experiment_Results$Concise_Topic_Understanding - 
  Experiment_Results$Detailed_Topic_Understanding

# Perform the Shapiro-Wilk test.
shapiro_test_topic_understanding <- 
  shapiro.test(paired_differences_topic_understanding)

# Print the Shapiro-Wilk test result.
cat("Shapiro-Wilk Test for Normality:\n")
print(shapiro_test_topic_understanding)

# Interpretation of the p-value.
if (shapiro_test_topic_understanding$p.value >= 0.05) {
  cat("The paired differences are likely normally distributed (p =", shapiro_test_topic_understanding$p.value, ").
      \n Paired t-test recommended.")
} else {
  cat("The paired differences are not normally distributed (p =", shapiro_test_topic_understanding$p.value, ").
      \n Wilcoxon-signed rank test recommended.")
}

# ===== Paired t-test, Concise VS. Detailed Understanding =====

# Run a paired sample t-test to compare
# understanding after concise and detailed explanations.
t_test_result_concise_vs_detailed_understanding <- t.test(
  Experiment_Results$Concise_Topic_Understanding,
  Experiment_Results$Detailed_Topic_Understanding,
  paired = TRUE
)

# Calculate standard deviations.
concise_topic_understanding_sd <- sd(Experiment_Results$Concise_Topic_Understanding, na.rm = TRUE)
detailed_topic_understanding_sd <- sd(Experiment_Results$Detailed_Topic_Understanding, na.rm = TRUE)

# Print the results of the t-test.
print(t_test_result_concise_vs_detailed_understanding)

# Extract useful information into a DataFrame.
t_test_summary_consice_vs_detailed_understanding <- data.frame(
  T_Statistic = t_test_result_concise_vs_detailed_understanding$statistic,
  Degrees_of_Freedom = t_test_result_concise_vs_detailed_understanding$parameter,
  P_Value = t_test_result_concise_vs_detailed_understanding$p.value,
  Confidence_Interval_Lower = t_test_result_concise_vs_detailed_understanding$conf.int[1],
  Confidence_Interval_Upper = t_test_result_concise_vs_detailed_understanding$conf.int[2],
  Mean_Difference = t_test_result_concise_vs_detailed_understanding$estimate,
  Concise_SD = concise_topic_understanding_sd,
  Detailed_SD = detailed_topic_understanding_sd,
  Shapiro_Wilk_Normality = shapiro_test_topic_understanding$p.value
)

# Save the summary to a .csv file.
write.csv(t_test_summary_consice_vs_detailed_understanding,
          "t_test_summary_consice_vs_detailed_understanding.csv",
          row.names = FALSE)

# Print confirmation.
cat("t-test summary saved to 't_test_summary_consice_vs_detailed_understanding.csv'\n")

# Save the plot as a PNG file.
png(filename = "boxplot_concise_vs_detailed_understanding.png", width = 800, height = 600)

# Display the results as side-by-side box plots.
boxplot(Experiment_Results$Concise_Topic_Understanding,
        Experiment_Results$Detailed_Topic_Understanding,
        main = "Participants' Post-Experiment \n Understanding of Topics",
        at = c(1, 2),
        names = c("Concise Explanations", "Detailed Explanations"),
        ylab = "Level of understanding",
        col = "white",
        border = "black",
        horizontal = FALSE,
        notch = FALSE,
        yaxt = "n")

# Add custom y-axis with descriptive labels
axis(2, 
     at = 0:3,
     labels = c("None", "Basic", "Med", "High"),
     las = 2)

# Display the p-value on the plot.
p_value <- t_test_result_concise_vs_detailed_understanding$p.value
text(x = 1.5, y = max(c(Experiment_Results$Concise_Topic_Understanding,
                        Experiment_Results$Detailed_Topic_Understanding)), 
     labels = paste("p-value =", round(p_value, 3)), cex = 0.8)

# Finalise and save the plot.
dev.off()

# ===== Shapiro-Wilk Test, Concise VS. Detailed Satisfaction & Trustworthiness =====

# Satisfaction.
# Calculate the differences between the paired scores.
paired_differences_satisfaction <- 
  Experiment_Results$Concise_Satisfaction - 
  Experiment_Results$Detailed_Satisfaction

# Perform the Shapiro-Wilk test.
shapiro_test_satisfaction <- 
  shapiro.test(paired_differences_satisfaction)

# Print the Shapiro-Wilk test result.
cat("Shapiro-Wilk Test for Normality:\n")
print(shapiro_test_satisfaction)

# Interpretation of the p-value.
if (shapiro_test_satisfaction$p.value >= 0.05) {
  cat("The paired differences are likely normally distributed (p =", shapiro_test_satisfaction$p.value, ").
      \n Paired t-test recommended.")
} else {
  cat("The paired differences are not normally distributed (p =", shapiro_test_satisfaction$p.value, ").
      \n Wilcoxon-signed rank test recommended.")
}

# Trustworthiness.
# Calculate the differences between the paired scores.
paired_differences_trustworthiness <- 
  Experiment_Results$Concise_Trustworthiness - 
  Experiment_Results$Detailed_Trustworthiness

# Perform the Shapiro-Wilk test.
shapiro_test_trustworthiness <- 
  shapiro.test(paired_differences_trustworthiness)

# Print the Shapiro-Wilk test result.
cat("Shapiro-Wilk Test for Normality:\n")
print(shapiro_test_trustworthiness)

# Interpretation of the p-value.
if (shapiro_test_trustworthiness$p.value >= 0.05) {
  cat("The paired differences are likely normally distributed (p =", shapiro_test_trustworthiness$p.value, ").
      \n Paired t-test recommended.")
} else {
  cat("The paired differences are not normally distributed (p =", shapiro_test_trustworthiness$p.value, ").
      \n Wilcoxon-signed rank test recommended.")
}

# ===== Paired t-test, Concise VS. Detailed Satisfaction & Trustworthiness =====
# Perform paired t-tests.
t_test_satisfaction_test <- t.test(
  Experiment_Results$Concise_Satisfaction,
  Experiment_Results$Detailed_Satisfaction,
  paired = TRUE
)

t_test_trustworthiness_test <- t.test(
  Experiment_Results$Concise_Trustworthiness,
  Experiment_Results$Detailed_Trustworthiness,
  paired = TRUE
)

# Calculate standard deviations.
concise_topic_satisfaction_sd <- sd(Experiment_Results$Concise_Satisfaction, na.rm = TRUE)
detailed_topic_satisfaction_sd <- sd(Experiment_Results$Detailed_Satisfaction, na.rm = TRUE)
concise_topic_trustworthiness_sd <- sd(Experiment_Results$Concise_Trustworthiness, na.rm = TRUE)
detailed_topic_trustworthiness_sd <- sd(Experiment_Results$Detailed_Trustworthiness, na.rm = TRUE)

# Extract p-values and print to check.
cat("Satisfaction p-value: ", t_test_satisfaction_test$p.value, "\n")
cat("Trustworthiness p-value: ", t_test_trustworthiness_test$p.value, "\n")

# Extract useful satisfaction information into a DataFrame.
t_test_summary_satisfaction_test <- data.frame(
  T_Statistic = t_test_satisfaction_test$statistic,
  Degrees_of_Freedom = t_test_satisfaction_test$parameter,
  P_Value = t_test_satisfaction_test$p.value,
  Confidence_Interval_Lower = t_test_satisfaction_test$conf.int[1],
  Confidence_Interval_Upper = t_test_satisfaction_test$conf.int[2],
  Mean_Difference = t_test_satisfaction_test$estimate,
  Concise_SD = concise_topic_satisfaction_sd,
  Detailed_SD = detailed_topic_satisfaction_sd,
  Shapiro_Wilk_Normality = shapiro_test_satisfaction$p.value
)

# Save the summary to a .csv file.
write.csv(t_test_summary_satisfaction_test,
          "t_test_summary_satisfaction_test.csv",
          row.names = FALSE)

# Print confirmation.
cat("t-test summary saved to 't_test_summary_satisfaction_test.csv'\n")

# Extract useful satisfaction information into a DataFrame.
t_test_summary_trustworthiness_test <- data.frame(
  T_Statistic = t_test_trustworthiness_test$statistic,
  Degrees_of_Freedom = t_test_trustworthiness_test$parameter,
  P_Value = t_test_trustworthiness_test$p.value,
  Confidence_Interval_Lower = t_test_trustworthiness_test$conf.int[1],
  Confidence_Interval_Upper = t_test_trustworthiness_test$conf.int[2],
  Mean_Difference = t_test_trustworthiness_test$estimate,
  Concise_SD = concise_topic_trustworthiness_sd,
  Detailed_SD = detailed_topic_trustworthiness_sd,
  Shapiro_Wilk_Normality = shapiro_test_trustworthiness$p.value
)

# Save the summary to a .csv file.
write.csv(t_test_summary_trustworthiness_test,
          "t_test_summary_trustworthiness_test.csv",
          row.names = FALSE)

# Print confirmation.
cat("t-test summary saved to 't_test_summary_trustworthiness_test.csv'\n")

# Save the plot as a PNG file.
png(filename = "boxplot_concise_vs_detailed_satisfaction_and_trustworthiness.png", width = 800, height = 600)

# Combine data for plotting.
boxplot_data <- list(
  "Detailed\nTrustworthiness" = Experiment_Results$Detailed_Trustworthiness,
  "Concise\nTrustworthiness" = Experiment_Results$Concise_Trustworthiness,
  "Detailed\nSatisfaction" = Experiment_Results$Detailed_Satisfaction,
  "Concise\nSatisfaction" = Experiment_Results$Concise_Satisfaction
)

# Adjust margins for better label visibility.
par(mar = c(5, 6, 4, 2))

# Create horizontal box plots.
boxplot(boxplot_data,
        horizontal = TRUE,
        col = c("orange", "lightblue", "orange", "lightblue"),
        main = "Satisfaction and Trustworthiness Scores",
        xlab = "Likert Scale (1-7)",
        las = 1,
        cex.axis = 0.8,
        frame = FALSE)

# Draw lines connecting the axes at the bottom-left corner.
segments(x0 = par("usr")[1], y0 = par("usr")[3],  # Bottom-left corner.
         x1 = par("usr")[1], y1 = par("usr")[4],  # Left edge (y-axis).
         col = "black")
segments(x0 = par("usr")[1], y0 = par("usr")[3],  # Bottom-left corner.
         x1 = par("usr")[2], y1 = par("usr")[3],  # Bottom edge (x-axis).
         col = "black")

# Annotate p-values.
text(x = 6.85, y = 1.45, labels = paste("p =", signif(t_test_trustworthiness_test$p.value, 3)), cex = 0.8)
text(x = 6.85, y = 3.55, labels = paste("p =", signif(t_test_satisfaction_test$p.value, 3)), cex = 0.8)

# Add a legend.
legend("topright", legend = c("Concise", "Detailed"), fill = c("lightblue", "orange"), cex = 0.8)

# Finalise and save the plot.
dev.off()

# Reset margins to default.
par(mar = c(5, 4, 4, 2))

# ===== Descriptive Statistics, AI Familiarity =====

# Descriptive statistics.
summary_stats <- Experiment_Results %>%
  group_by(AI_Familiarity) %>%
  summarize(across(c(Concise_Topic_Score, Detailed_Topic_Score, 
                     Concise_Topic_Understanding, Detailed_Topic_Understanding,
                     Concise_Satisfaction, Detailed_Satisfaction,
                     Concise_Trustworthiness, Detailed_Trustworthiness), 
                   list(mean = mean, sd = sd, median = median), 
                   .names = "{.col}_{.fn}"))

# Print the summary statistics.
print(summary_stats)

# Save the summary statistics to a .csv file.
write.csv(summary_stats,
          "ai_familiarity_summary_statistics.csv",
          row.names = FALSE)

# Print the confirmation message.
cat("Summary statistics saved to 'ai_familiarity_summary_statistics.csv'\n")

# ===== Descriptive Statistics, Concise Topic Knowledge =====

# Descriptive statistics.
summary_stats_knowledge <- Experiment_Results %>%
  group_by(Concise_Topic_Knowledge) %>%
  summarize(across(c(Concise_Topic_Score, Detailed_Topic_Score, 
                     Concise_Topic_Understanding, Detailed_Topic_Understanding,
                     Concise_Satisfaction, Detailed_Satisfaction,
                     Concise_Trustworthiness, Detailed_Trustworthiness), 
                   list(mean = mean, sd = sd, median = median), 
                   .names = "{.col}_{.fn}"))

# Print the summary statistics.
print(summary_stats_knowledge)

# Save the summary statistics to a .csv file.
write.csv(summary_stats_knowledge,
          "concise_topic_knowledge_summary_statistics.csv",
          row.names = FALSE)

# Print the confirmation message.
cat("Summary statistics saved to 'concise_topic_knowledge_summary_statistics.csv'\n")

# ===== Descriptive Statistics, Detailed Topic Knowledge =====

# Descriptive statistics.
summary_stats_detailed_knowledge <- Experiment_Results %>%
  group_by(Detailed_Topic_Knowledge) %>%
  summarize(across(c(Concise_Topic_Score, Detailed_Topic_Score, 
                     Concise_Topic_Understanding, Detailed_Topic_Understanding,
                     Concise_Satisfaction, Detailed_Satisfaction,
                     Concise_Trustworthiness, Detailed_Trustworthiness), 
                   list(mean = mean, sd = sd, median = median), 
                   .names = "{.col}_{.fn}"))

# Print the summary statistics.
print(summary_stats_detailed_knowledge)

# Save the summary statistics to a .csv file.
write.csv(summary_stats_detailed_knowledge,
          "detailed_topic_knowledge_summary_statistics.csv",
          row.names = FALSE)

# Print the confirmation message.
cat("Summary statistics saved to 'detailed_topic_knowledge_summary_statistics.csv'\n")