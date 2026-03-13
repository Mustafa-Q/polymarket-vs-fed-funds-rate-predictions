# fedwatch-transform.R
# Transform FedWatch long-format probabilities into Polymarket-aligned option buckets.
# Output: fedwatch_option_probs.csv (meeting_file, Date, option_type, p_fed)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(stringr)
})

# 1) Load
fed_long <- read.csv("fedwatch_long.csv", stringsAsFactors = FALSE)
stopifnot(all(c("Date", "meeting_file", "rate_bin", "p_fed") %in% names(fed_long)))

# 2) Parse Date robustly
fed_long <- fed_long %>%
  mutate(
    Date_chr = as.character(Date),
    Date = suppressWarnings(as.Date(mdy(Date_chr))),
    Date = if_else(is.na(Date), suppressWarnings(as.Date(ymd(Date_chr))), Date)
  ) %>%
  select(-Date_chr) %>%
  filter(!is.na(Date))

# 3) Ensure probabilities numeric; scale if in percent
fed_long <- fed_long %>%
  mutate(p_fed = as.numeric(p_fed)) %>%
  filter(!is.na(p_fed))

scale_tbl <- fed_long %>%
  group_by(meeting_file) %>%
  summarise(max_p = max(p_fed, na.rm = TRUE), .groups = "drop") %>%
  mutate(scale = if_else(max_p > 1.5, 100, 1))

fed_long <- fed_long %>%
  left_join(scale_tbl %>% select(meeting_file, scale), by = "meeting_file") %>%
  mutate(p_fed = p_fed / scale) %>%
  select(-scale)

# 4) Parse rate_bin into low/high bps and midpoint rate
extract_low_high_bps <- function(x) {
  nums <- str_extract_all(x, "\\d+")[[1]]
  if (length(nums) < 2) return(c(NA_real_, NA_real_))
  c(as.numeric(nums[1]), as.numeric(nums[2]))
}

lh <- t(vapply(fed_long$rate_bin, extract_low_high_bps, numeric(2)))
fed_long$low_bps  <- lh[, 1]
fed_long$high_bps <- lh[, 2]

fed_long <- fed_long %>%
  mutate(rate_mid = ((low_bps + high_bps) / 2) / 100)

bad_mid <- fed_long %>%
  filter(is.na(rate_mid) | !is.finite(rate_mid)) %>%
  distinct(meeting_file, rate_bin, low_bps, high_bps) %>%
  head(50)

if (nrow(bad_mid) > 0) {
  print(bad_mid)
  stop("rate_bin parsing failed for some rows.")
}

# 5) Meeting-level current target upper bound (edit if needed)
meeting_targets <- tribble(
  ~meeting_file,                 ~target_upper,
  "sept2025-probabilities.csv",   4.33,
  "oct2025-probabilities.csv",    4.33,
  "dec2025-probabilities.csv",    4.33,
  "jan2026-probabilities.csv",    4.33,
  "march2026-probabilities.csv",  4.33,
  "april2026-probabilities.csv",  4.33,
  "june2026-probabilities.csv",   4.33
)

# 6) Map rate bins -> Polymarket buckets and aggregate to option probabilities
fed_mapped <- fed_long %>%
  left_join(meeting_targets, by = "meeting_file") %>%
  filter(!is.na(target_upper)) %>%
  mutate(
    option_type = case_when(
      rate_mid <= target_upper - 0.50 ~ "cut_50plus",
      rate_mid <= target_upper - 0.25 ~ "cut_25",
      rate_mid <= target_upper        ~ "no_change",
      TRUE                            ~ "hike_25plus"
    )
  ) %>%
  group_by(meeting_file, Date, option_type) %>%
  summarise(p_fed = sum(p_fed, na.rm = TRUE), .groups = "drop") %>%
  mutate(p_fed = pmax(p_fed, 0)) %>%
  group_by(meeting_file, Date) %>%
  mutate(p_fed = p_fed / sum(p_fed)) %>%
  ungroup()

# 7) Sanity checks
eps <- 1e-6

bad <- fed_mapped %>%
  filter(
    is.na(option_type) |
      is.na(p_fed) |
      !is.finite(p_fed) |
      p_fed < -eps |
      p_fed > 1 + eps
  )

if (nrow(bad) > 0) {
  print(bad %>% arrange(meeting_file, Date, option_type) %>% head(50))
  stop("Invalid p_fed values after mapping.")
}

totals <- fed_mapped %>%
  group_by(meeting_file, Date) %>%
  summarise(total = sum(p_fed), .groups = "drop") %>%
  filter(abs(total - 1) > 0.02)

if (nrow(totals) > 0) {
  print(totals %>% arrange(desc(abs(total - 1))) %>% head(50))
  stop("Totals not near 1 for some meeting_file/Date rows.")
}

# 8) Write output
write.csv(fed_mapped, "fedwatch_option_probs.csv", row.names = FALSE)

message("Wrote fedwatch_option_probs.csv")
print(head(fed_mapped, 10))

