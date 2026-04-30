library(tidyverse)

# Read the wide file created by fedwatch-append.r
fed_all <- read.csv("fedwatch_all_meetings_wide.csv", check.names = FALSE)

# Standardize Date
library(tidyverse)
library(lubridate)

# Read the combined wide file created by fedwatch-append.r
fed_all <- read.csv("fedwatch_all_meetings_wide.csv", check.names = FALSE)

# Identify the meeting file column (your append script added this)
stopifnot("meeting_file" %in% names(fed_all))

# Long format: one row per (meeting_file, Date, rate_bin)
fed_long <- fed_all %>%
  pivot_longer(
    cols = -c(Date, meeting_file),
    names_to = "rate_bin",
    values_to = "p_fed_raw"
  ) %>%
  mutate(
    Date = mdy(Date),
    p_fed = as.numeric(p_fed_raw)
  ) %>%
  filter(!is.na(Date), !is.na(p_fed))

# Convert percent to probability when needed, per meeting_file
scale_tbl <- fed_long %>%
  group_by(meeting_file) %>%
  summarise(max_p = max(p_fed, na.rm = TRUE), .groups = "drop") %>%
  mutate(scale = if_else(max_p > 1.5, 100, 1))

fed_long <- fed_long %>%
  left_join(scale_tbl %>% select(meeting_file, scale), by = "meeting_file") %>%
  mutate(p_fed = p_fed / scale) %>%
  select(Date, meeting_file, rate_bin, p_fed)

# Sanity check: within each meeting_file-date, bins sum to ~1
check <- fed_long %>%
  group_by(meeting_file, Date) %>%
  summarise(total = sum(p_fed), .groups = "drop")

stopifnot(all(abs(check$total - 1) < 1e-3))

write.csv(fed_long, "fedwatch_long.csv", row.names = FALSE)

