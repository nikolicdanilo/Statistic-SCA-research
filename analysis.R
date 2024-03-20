# Importing libraries.
require("ggplot2")
require("tidyr")
require("dplyr")
require("MASS")
require("blorr")
require("ModelMetrics")
require("pdp")

library("ggplot2")
library("tidyr")
library("dplyr")
library("MASS")
library("blorr")
library("ModelMetrics")
library("pdp")

# Reading a dataset.
path <- file.choose()
frame <- read.csv(path)
frame[,c("X","index")] <- NULL
frame$exam_attempts <- as.integer(frame$exam_attempts)
frame <- frame %>%
  group_by(language) %>%
  mutate(language = n()) %>%
  ungroup()
names(frame)[names(frame) == "duplications..."] <- "duplications_percentage"
names(frame)[names(frame) == "code.smells"] <- "code_smells"
names(frame)[names(frame) == "security.hotspots"] <- "security_hotspots"
frame$exam_attempts[is.na(frame$exam_attempts)] <- 0


#Exploratory Data Analysis.
summary(frame)

df_long <- pivot_longer(frame, cols = c("grade","exam_attempts","bugs","duplications_percentage","vulnerabilities","code_smells",
                                        "security_hotspots","academic_year","number_of_code_lines"), names_to = "Variable", values_to = "Value")
bin_widths <- df_long %>%
  group_by(Variable) %>%
  summarize(BinWidth = 2 * IQR(Value) * length(Value)^(-1/3)) %>%
  .$BinWidth

rep_bin_width <- median(bin_widths)

ggplot(df_long, aes(x = Value)) + 
  geom_histogram(binwidth = rep_bin_width, fill = "purple") + 
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(x = NULL, y = "Count", title = "Distribution of Non-Binary Inputs Data (Freedman-Diaconis based bins)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

frame_long <- pivot_longer(frame, cols = c("female","static_code_analysis","versioning","online",
                                           "team","pass_quality_gate"), 
                           names_to = "Variable", 
                           values_to = "Outcome")

ggplot(frame_long, aes(x = as.factor(Outcome))) + 
  geom_bar(fill = "purple") + 
  facet_wrap(~ Variable, scales = "free_y") +
  labs(x = "Labels", y = "Count", title = "Binary Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Splitting the dataset.
X <- frame[, c("static_code_analysis", "versioning", "number_of_code_lines", 
               "academic_year", "online", "team", "language")]
y <- frame[["pass_quality_gate"]]

# Standardization.
X_std <- scale(X)
X_std <- as.data.frame(scale(X))

# Fitting Logistic Regression.
y <- factor(y)
model_logistic <- glm(y ~ ., data = X_std, family = binomial)

# Model interpretation.
summary_logistic <- summary(model_logistic)
coefficients_summary <- summary_logistic$coefficients
odds_ratio <- exp(coefficients_summary[, "Estimate"])
conf_int <- confint(model_logistic)
combined_stats <- cbind(coefficients_summary, `2.5 %` = conf_int[,1], `97.5 %` = conf_int[,2], 
                        `Odds Ratio` = odds_ratio)
results_table <- round(combined_stats, 4)
print(results_table)

# Inputs importance (Permutation Importance and Cumulative Importance).
set.seed(123)
selected_features <- c("static_code_analysis", "versioning", "number_of_code_lines", 
                       "academic_year", "online", "team", "language")
num_shuffles <- 100
baseline_preds <- predict(model_logistic, type = "response")
baseline_perf <- auc(actual = frame$pass_quality_gate, predicted = baseline_preds)

perm_importances <- matrix(0, nrow = num_shuffles, ncol = length(selected_features),
                           dimnames = list(NULL, selected_features))

for (i in 1:num_shuffles) {
  for (feature in selected_features) {
    df_shuffled <- frame
    df_shuffled[[feature]] <- sample(frame[[feature]])
    
    shuffled_preds <- predict(model_logistic, newdata = df_shuffled, type = "response")
    
    shuffled_perf <- auc(actual = df_shuffled$pass_quality_gate, predicted = shuffled_preds)
    
    perm_importances[i, feature] <- baseline_perf - shuffled_perf
  }
}

feature_importance_means <- apply(perm_importances, 2, mean)
feature_importance_sds <- apply(perm_importances, 2, sd)

feature_importance_df <- data.frame(
  MeanImportance = feature_importance_means,
  StdDev = feature_importance_sds
)

feature_importance_df <- feature_importance_df[order(feature_importance_df$MeanImportance, decreasing = TRUE), ]

print(feature_importance_df)

# Hypothesis Testing: Impact of Quality of Code on Students Success (grade and attempts).
test_grade <- wilcox.test(grade ~ pass_quality_gate, data = frame)
test_attempts <- wilcox.test(exam_attempts ~ pass_quality_gate, data = frame)
print(test_grade)
print(test_attempts)