library(tidyverse)

files <- list.files(
  "fedwatch-probabilities",
  pattern = "\\.csv$",
  full.names = TRUE
)

stopifnot(length(files) > 0)

fed_all <- map_dfr(files, function(f) {
  df <- read.csv(f, check.names = FALSE)
  df$meeting_file <- basename(f)     # keeps which meeting CSV this came from
  df
})

stopifnot(nrow(fed_all) > 0)

write.csv(fed_all, "fedwatch_all_meetings_wide.csv", row.names = FALSE)
