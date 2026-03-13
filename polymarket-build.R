library(jsonlite)
library(dplyr)
library(lubridate)
library(purrr)
library(stringr)
library(tibble)

# ------------------------------------------------------------------
# Mapping Polymarket meetings to FedWatch files
# ------------------------------------------------------------------
meeting_map <- tribble(
  ~pm_meeting_key, ~meeting_file,
  "sept2025",  "sept2025-probabilities.csv",
  "oct2025",   "oct2025-probabilities.csv",
  "dec2025",   "dec2025-probabilities.csv",
  "jan2026",   "jan2026-probabilities.csv",
  "march2026", "march2026-probabilities.csv",
  "april2026", "april2026-probabilities.csv",
  "june2026",  "june2026-probabilities.csv"
)

# ------------------------------------------------------------------
# SNAPSHOT FILE PARSER (YYYY-MM-DD_key.json)
# ------------------------------------------------------------------
parse_snapshot_file <- function(path) {
  fname <- basename(path)
  
  snap_date <- as.Date(str_extract(fname, "^\\d{4}-\\d{2}-\\d{2}"))
  pm_key <- str_match(fname, "^\\d{4}-\\d{2}-\\d{2}_(.+)\\.json$")[,2]
  
  resp <- fromJSON(path, simplifyVector = FALSE)
  event <- resp[[1]]
  markets <- bind_rows(event$markets)
  
  markets %>%
    mutate(
      Date = snap_date,
      pm_meeting_key = pm_key,
      p_poly = map_dbl(outcomePrices, ~ as.numeric(parse_json(.x)[[1]])),
      option_type = case_when(
        groupItemTitle == "50+ bps decrease" ~ "cut_50plus",
        groupItemTitle == "25 bps decrease"  ~ "cut_25",
        groupItemTitle == "No change"        ~ "no_change",
        groupItemTitle == "25+ bps increase" ~ "hike_25plus",
        TRUE ~ NA_character_
      ),
      days_to_meeting = as.numeric(
        difftime(ymd_hms(event$endDate, tz = "UTC"),
                 as.POSIXct(paste0(Date, " 12:00:00"), tz = "UTC"),
                 units = "days")
      ),
      market_age_days = as.numeric(
        difftime(as.POSIXct(paste0(Date, " 12:00:00"), tz = "UTC"),
                 ymd_hms(createdAt, tz = "UTC"),
                 units = "days")
      ),
      log_volume_1wk = log1p(as.numeric(volume1wk)),
      log_volume_1mo = log1p(as.numeric(volume1mo)),
      log_liquidity  = log1p(as.numeric(liquidityNum)),
      spread = as.numeric(spread),
      order_min_size = as.numeric(orderMinSize),
      neg_risk = as.integer(negRisk),
      competitive = as.numeric(competitive),
      comment_count_event = as.numeric(event$commentCount),
      event_liquidity = as.numeric(event$liquidity)
    ) %>%
    filter(!is.na(option_type)) %>%
    select(
      Date, pm_meeting_key, option_type, p_poly,
      days_to_meeting, market_age_days,
      log_volume_1wk, log_volume_1mo, log_liquidity,
      spread, order_min_size, neg_risk, competitive,
      comment_count_event, event_liquidity
    )
}

# ------------------------------------------------------------------
# WINDOW FILE PARSER (oct2025-window1-cut_25.json)
# ------------------------------------------------------------------
parse_window_file <- function(path) {
  fname <- basename(path)
  
  m <- str_match(
    fname,
    "^(sept2025|oct2025|dec2025|jan2026|march2026|april2026|june2026)-(window\\d+)-(cut_25|cut_50plus|no_change|hike_25plus)\\.json$"
  )
  
  pm_meeting_key <- m[,2]
  window_id <- m[,3]
  option_type <- m[,4]
  
  raw <- fromJSON(path)
  
  tibble(
    pm_meeting_key = pm_meeting_key,
    option_type = option_type,
    window = window_id,
    timestamp = as.POSIXct(raw$history$t, origin = "1970-01-01", tz = "UTC"),
    Date = as.Date(timestamp),
    p_poly = raw$history$p
  )
}

# ------------------------------------------------------------------
# LOAD FILES
# ------------------------------------------------------------------
all_files <- list.files("json", pattern = "\\.json$", full.names = TRUE)

snapshot_files <- all_files[str_detect(basename(all_files), "^\\d{4}-\\d{2}-\\d{2}_")]
window_files   <- setdiff(all_files, snapshot_files)

# ------------------------------------------------------------------
# BUILD DATASET
# ------------------------------------------------------------------
pm_snapshots <- map_dfr(snapshot_files, parse_snapshot_file)
pm_windows   <- map_dfr(window_files, parse_window_file)

pm_all <- bind_rows(pm_snapshots, pm_windows) %>%
  left_join(meeting_map, by = "pm_meeting_key") %>%
  filter(!is.na(meeting_file)) %>%
  filter(p_poly >= 0, p_poly <= 1)

# ------------------------------------------------------------------
# WRITE OUTPUT
# ------------------------------------------------------------------
write.csv(pm_all, "polymarket_all_meetings.csv", row.names = FALSE)

# ------------------------------------------------------------------
# SANITY CHECKS
# ------------------------------------------------------------------
print(table(pm_all$pm_meeting_key))
print(nrow(pm_all))

