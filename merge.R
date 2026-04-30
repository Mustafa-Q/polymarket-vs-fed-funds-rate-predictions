library(tidyverse)
library(lubridate)

pm <- read.csv("polymarket_all_meetings.csv", stringsAsFactors = FALSE)
fw <- read.csv("fedwatch_option_probs.csv", stringsAsFactors = FALSE)

pm$Date <- as.Date(pm$Date)
fw$Date <- as.Date(fw$Date)

merged <- pm %>%
  inner_join(fw, by = c("meeting_file", "Date", "option_type")) %>%
  mutate(
    error = p_poly - p_fed,
    abs_error = abs(error)
  )

write.csv(merged, "final_analysis_dataset.csv", row.names = FALSE)

print(dim(merged))
print(head(merged, 12))

