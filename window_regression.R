# window_regression.R
# Window-level regression with predictors available for every window row.
# Output: final_analysis_dataset_window.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
})

stopifnot(file.exists("polymarket_all_meetings.csv"))
stopifnot(file.exists("fedwatch_option_probs.csv"))

pm_all <- read_csv("polymarket_all_meetings.csv", show_col_types = FALSE)

# 1) Polymarket window rows
pm_win <- pm_all %>%
  filter(!is.na(window)) %>%
  mutate(
    timestamp = ymd_hms(timestamp, tz = "UTC"),
    Date = as.Date(Date),
    meeting_file = paste0(pm_meeting_key, "-probabilities.csv")
  ) %>%
  select(Date, timestamp, pm_meeting_key, meeting_file, option_type, p_poly, window)

stopifnot(nrow(pm_win) >= 1000)

# 2) Meeting dates for days_to_meeting
meeting_dates <- tibble::tribble(
  ~pm_meeting_key, ~meeting_date,
  "sept2025",  as.Date("2025-09-17"),
  "oct2025",   as.Date("2025-10-29"),
  "dec2025",   as.Date("2025-12-17"),
  "jan2026",   as.Date("2026-01-28"),
  "march2026", as.Date("2026-03-18"),
  "april2026", as.Date("2026-04-29"),
  "june2026",  as.Date("2026-06-17")
)

pm_win <- pm_win %>%
  left_join(meeting_dates, by = "pm_meeting_key") %>%
  filter(!is.na(meeting_date)) %>%
  mutate(days_to_meeting = as.numeric(difftime(meeting_date, as.Date(timestamp), units = "days"))) %>%
  filter(days_to_meeting >= 0)

# 3) Window-derived predictors
pm_win <- pm_win %>%
  arrange(pm_meeting_key, option_type, timestamp) %>%
  group_by(pm_meeting_key, option_type) %>%
  mutate(
    p_lag1 = lag(p_poly, 1),
    p_lag5 = lag(p_poly, 5),
    dp1 = p_poly - lag(p_poly, 1),
    dp5 = p_poly - lag(p_poly, 5),
    abs_dp1 = abs(dp1)
  ) %>%
  ungroup()

# 4) FedWatch benchmark: latest Date per meeting_file and option_type
fw_snap <- read_csv("fedwatch_option_probs.csv", show_col_types = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(meeting_file, option_type) %>%
  slice_max(Date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(meeting_file, option_type, p_fed, fw_date = Date)

# 5) Merge and response
df <- pm_win %>%
  left_join(fw_snap, by = c("meeting_file", "option_type")) %>%
  filter(!is.na(p_fed)) %>%
  mutate(
    error = p_poly - p_fed,
    abs_error = abs(error)
  )

message("Rows after FedWatch merge: ", nrow(df))
stopifnot(nrow(df) >= 1000)

# 6) Model-ready: drop rows lacking lag predictors
need_vars <- c("days_to_meeting", "p_lag1", "p_lag5", "dp1", "dp5", "abs_dp1")
df_model <- df %>%
  filter(if_all(all_of(need_vars), ~ !is.na(.x))) %>%
  mutate(
    pm_meeting_key = factor(pm_meeting_key),
    option_type = factor(option_type)
  )

message("Rows after predictor filter: ", nrow(df_model))
stopifnot(nrow(df_model) >= 1000)

# 7) Write cleaned dataset used in regression
write_csv(df_model, "final_analysis_dataset_window.csv")
message("Wrote final_analysis_dataset_window.csv")

# 8) Regression
# dp5 is collinear with p_lag5 (dp5 = p_poly - p_lag5), so dp5 will be dropped if included
model_1 <- lm(
  error ~ days_to_meeting + p_lag1 + p_lag5 + dp1 + abs_dp1 + option_type + pm_meeting_key,
  data = df_model
)

print(summary(model_1))

