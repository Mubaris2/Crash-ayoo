# ----------------------------------------
# EVENT MINING SCRIPT
# IoT Vehicle Crash Data Warehouse Project
# ----------------------------------------

library(DBI)
library(RSQLite)
library(dplyr)
library(arules)
library(arulesViz)
library(ggplot2)

# ----------------------------------------
# 1. Connect to Data Warehouse
# ----------------------------------------

con <- dbConnect(RSQLite::SQLite(), "warehouse/vehicle_dw.sqlite")

fact_data <- dbReadTable(con, "fact_vehicle_events")

dbDisconnect(con)

# Inspect data
print(head(fact_data))

# ----------------------------------------
# 2. Event Frequency Analysis
# ----------------------------------------

event_counts <- fact_data %>%
  group_by(event_type) %>%
  summarise(count = n())

print(event_counts)

# Plot event distribution
ggplot(event_counts, aes(x = event_type, y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Vehicle Event Distribution",
    x = "Event Type",
    y = "Frequency"
  )

ggsave("visualization/event_distribution.png")

# ----------------------------------------
# 3. Prepare Data for Data Mining
# ----------------------------------------

mining_data <- fact_data %>%
  mutate(
    speed_level = ifelse(speed > 60, "high_speed", "normal_speed"),
    accel_state = ifelse(acceleration < -6, "hard_deceleration", "normal_accel"),
    vibration_state = ifelse(vibration > 4, "high_vibration", "normal_vibration")
  ) %>%
  select(speed_level, accel_state, vibration_state, event_type)

# ----------------------------------------
# 4. Convert Data to Transactions
# ----------------------------------------

transactions <- as(mining_data, "transactions")

summary(transactions)

# ----------------------------------------
# 5. Association Rule Mining
# ----------------------------------------

rules <- apriori(
  transactions,
  parameter = list(
    supp = 0.01,
    conf = 0.6,
    minlen = 2
  )
)

# ----------------------------------------
# 6. Filter Crash Related Rules
# ----------------------------------------

# Sort rules by confidence
rules_sorted <- sort(rules, by = "confidence", decreasing = TRUE)

crash_rules <- subset(rules, rhs %in% "event_type=crash")

inspect(crash_rules)

# ----------------------------------------
# 7. Visualize Association Rules
# ----------------------------------------

cat("\nGenerating visualizations...\n")

# Extract rule quality metrics for plotting
rules_df <- as(rules_sorted, "data.frame")

# Scatter plot using base R (always works)
png("visualization/association_rules_scatter.png", width = 800, height = 600)
plot(rules_df$support, rules_df$confidence, 
     col = rgb(0, 0, 1, alpha = 0.5),
     pch = 19, cex = 1.5,
     xlab = "Support", ylab = "Confidence",
     main = "Association Rules: Support vs Confidence")
grid()
dev.off()
cat("✓ Association rules scatter plot saved\n")

# Crash rules scatter plot
if (length(crash_rules) > 0) {
  crash_df <- as(crash_rules, "data.frame")
  png("visualization/crash_rules_scatter.png", width = 800, height = 600)
  plot(crash_df$support, crash_df$confidence,
       col = rgb(1, 0, 0, alpha = 0.6),
       pch = 19, cex = 2,
       xlab = "Support", ylab = "Confidence",
       main = "Crash Rules: Support vs Confidence")
  grid()
  dev.off()
  cat("✓ Crash rules scatter plot saved\n")
} else {
  cat("No crash rules to visualize (support too low)\n")
}

# ----------------------------------------
# 8. Crash Event Analysis
# ----------------------------------------

crash_events <- fact_data %>%
  filter(event_type == "crash")

print(summary(crash_events))

# Speed distribution during crashes
ggplot(crash_events, aes(speed)) +
  geom_histogram(bins = 20) +
  theme_minimal() +
  labs(
    title = "Speed Distribution During Crash Events",
    x = "Speed",
    y = "Frequency"
  )

ggsave("visualization/crash_speed_distribution.png")

# ----------------------------------------
# 9. Save Mining Results
# ----------------------------------------

write.csv(event_counts, "results/event_counts.csv", row.names = FALSE)

capture.output(
  inspect(rules_sorted),
  file = "results/association_rules.txt"
)

print("Event Mining Completed Successfully")

# ----------------------------------------
# 10. Rule Validation (Train/Test Split)
# ----------------------------------------

set.seed(42)
n <- nrow(mining_data)
train_idx <- sample(seq_len(n), size = floor(0.8 * n))

train_data <- mining_data[train_idx, ]
test_data  <- mining_data[-train_idx, ]

# Mine rules on training set only
train_transactions <- as(train_data, "transactions")

train_rules <- apriori(
  train_transactions,
  parameter = list(supp = 0.01, conf = 0.6, minlen = 2)
)

# Filter to rules that predict event_type on RHS
event_rules <- subset(train_rules, rhs %pin% "event_type")
event_rules <- sort(event_rules, by = "confidence", decreasing = TRUE)

cat("\nRules used for validation (predicting event_type):\n")
inspect(event_rules)

# ----------------------------------------
# 11. Apply Rules to Test Set & Confusion Matrix
# ----------------------------------------

# Helper: apply highest-confidence matching rule to a single row
# Pre-build rule lookup from arules S4 object
build_rule_lookup <- function(rules) {
  rules_df   <- as(rules, "data.frame")          # lhs, rhs, support, confidence...
  lhs_labels <- labels(lhs(rules))               # e.g. "{speed_level=high_speed,accel_state=...}"
  rhs_labels <- labels(rhs(rules))               # e.g. "{event_type=crash}"

  # Parse LHS into character vectors of items
  lhs_parsed <- lapply(lhs_labels, function(x) {
    x <- gsub("\\{|\\}", "", x)
    trimws(strsplit(x, ",")[[1]])
  })

  # Extract event_type value from RHS
  rhs_event <- regmatches(
    rhs_labels,
    regexpr("(?<=event_type=)\\w+", rhs_labels, perl = TRUE)
  )

  list(
    lhs      = lhs_parsed,
    rhs      = rhs_event,
    conf     = rules_df$confidence
  )
}

predict_event <- function(row, lookup) {
  row_items <- paste0(names(row), "=", unlist(row))

  for (i in seq_along(lookup$lhs)) {
    if (length(lookup$rhs[i]) == 0 || lookup$rhs[i] == "") next
    if (all(lookup$lhs[[i]] %in% row_items)) {
      return(lookup$rhs[i])
    }
  }
  return("unknown")
}

# Build feature columns for test set (exclude event_type for prediction)
test_features <- test_data %>% select(-event_type)

# Build lookup once (efficient — avoids re-parsing per row)
lookup <- build_rule_lookup(event_rules)

predictions <- sapply(seq_len(nrow(test_features)), function(i) {
  predict_event(test_features[i, ], lookup)
})

actual <- test_data$event_type

# Confusion matrix
all_labels <- union(unique(actual), unique(predictions))
all_labels <- all_labels[all_labels != "unknown"]

conf_matrix <- table(
  Predicted = factor(predictions, levels = c(all_labels, "unknown")),
  Actual    = factor(actual,      levels = all_labels)
)

cat("\n--- Confusion Matrix ---\n")
print(conf_matrix)

# Per-class precision, recall, F1
cat("\n--- Per-Class Metrics ---\n")
metrics <- lapply(all_labels, function(cls) {
  tp <- sum(predictions == cls & actual == cls)
  fp <- sum(predictions == cls & actual != cls)
  fn <- sum(predictions != cls & actual == cls)

  precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA
  recall    <- if ((tp + fn) > 0) tp / (tp + fn) else NA
  f1        <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0)
                 2 * precision * recall / (precision + recall) else NA

  cat(sprintf("%-15s  Precision: %.3f  Recall: %.3f  F1: %.3f\n",
              cls,
              ifelse(is.na(precision), 0, precision),
              ifelse(is.na(recall),    0, recall),
              ifelse(is.na(f1),        0, f1)))
})

# Overall accuracy (excluding unknowns)
known_mask <- predictions != "unknown"
accuracy <- sum(predictions[known_mask] == actual[known_mask]) / sum(known_mask)
coverage <- mean(known_mask)

cat(sprintf("\nOverall Accuracy (on covered rows): %.1f%%\n", accuracy * 100))
cat(sprintf("Rule Coverage: %.1f%% of test rows matched a rule\n", coverage * 100))

# Save validation summary
capture.output({
  cat("=== Rule Validation Summary ===\n\n")
  cat("Train size:", nrow(train_data), "\n")
  cat("Test size: ", nrow(test_data),  "\n\n")
  cat("Confusion Matrix:\n")
  print(conf_matrix)
  cat(sprintf("\nOverall Accuracy (covered): %.1f%%\n", accuracy * 100))
  cat(sprintf("Rule Coverage:              %.1f%%\n",  coverage * 100))
}, file = "results/validation_summary.txt")

print("Validation Completed")