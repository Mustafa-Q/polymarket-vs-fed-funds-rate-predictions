library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)

# --- helpers ----

parse_fedwatch_cols <- function(colname) {
  # Input like: "X.350.375."
  x <- colname %>% str_replace("^X\\.", "") %>% str_replace("\\.$", "")
  parts <- str_split(x, "\\.", simplify = TRUE)
  tibble(
    lower = as.numeric(parts[, 1]) / 100,
    upper = as.numeric(parts[, 2]) / 100
  )
}

pick_snapshot_date <- function(dates, meeting_date, rule = c("last_before")) {
  rule <- match.arg(rule)
  if (rule == "last_before") {
    max(dates[dates < meeting_date], na.rm = TRUE)
  }
}

# --- main extractor for one meeting ----

fedwatch_one_meeting <- function(
    csv_path,
    meeting_id,
    meeting_date,
    current_upper,
    snapshot_rule = "last_before"
) {
  fw <- read.csv(csv_path, check.names = FALSE)
  
  fw$Date <- as.Date(fw$Date, format = "%m/%d/%Y")
  snap_date <- pick_snapshot_date(fw$Date, meeting_date, snapshot_rule)
  
  row <- fw %>% filter(Date == snap_date)
  
  long <- row %>%
    pivot_longer(-Date, names_to = "rate_bin", values_to = "prob") %>%
    filter(!is.na(prob), prob > 0)
  
  parsed <- parse_fedwatch_cols(long$rate_bin)
  
  long2 <- bind_cols(long %>% select(Date, prob), parsed) %>%
    mutate(
      mid = (lower + upper) / 2,
      delta_bps = (mid - current_upper) * 100,
      option_type = case_when(
        delta_bps <= -50 ~ "cut_50plus",
        delta_bps > -50 & delta_bps <= -25 ~ "cut_25",
        delta_bps > -25 & delta_bps < 25 ~ "no_change",
        delta_bps >= 25 ~ "hike_25plus",
        TRUE ~ NA_character_
      )
    )
  
  out <- long2 %>%
    group_by(option_type) %>%
    summarise(p_fedwatch = sum(prob), .groups = "drop") %>%
    mutate(
      meeting_id = meeting_id,
      meeting_date = meeting_date,
      snapshot_date = snap_date,
      current_upper = current_upper
    ) %>%
    select(meeting_id, meeting_date, snapshot_date, current_upper, option_type, p_fedwatch) %>%
    arrange(meeting_id, option_type)
  
  stopifnot(nrow(out) == 4)
  stopifnot(abs(sum(out$p_fedwatch) - 1) < 1e-6)
  
  out
}

# --- batch builder across many meetings ----
# meta_csv must have columns:
# meeting_id, meeting_date, current_upper, csv_path
build_fedwatch_all <- function(meta_csv) {
  meta <- read.csv(meta_csv, stringsAsFactors = FALSE) %>%
    mutate(
      meeting_date = as.Date(meeting_date),
      current_upper = as.numeric(current_upper)
    )
  
  res <- pmap_dfr(
    list(meta$csv_path, meta$meeting_id, meta$meeting_date, meta$current_upper),
    ~ fedwatch_one_meeting(
      csv_path = ..1,
      meeting_id = ..2,
      meeting_date = ..3,
      current_upper = ..4
    )
  )
  
  write.csv(res, "fedwatch_all_meetings.csv", row.names = FALSE)
  res
}