source("extract-event.R")

snapshot_time <- ymd_hms("2026-02-05 21:27:30", tz = "UTC")

files <- list.files("json/", pattern = "\\.json$", full.names = TRUE)

all_pm <- bind_rows(lapply(files, extract_one_event, snapshot_time_utc = snapshot_time))

write.csv(all_pm, "polymarket_all_meetings.csv", row.names = FALSE)

