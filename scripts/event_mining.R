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

png("visualization/association_rules_graph.png")
plot(rules_sorted, method = "graph")
dev.off()

png("visualization/crash_rules_graph.png")
plot(rules_sorted, method = "graph")
dev.off()

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