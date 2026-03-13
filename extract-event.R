library(jsonlite)
library(dplyr)
library(lubridate)
library(purrr)

get_num <- function(df, col) {
  if (col %in% names(df)) as.numeric(df[[col]]) else rep(NA_real_, nrow(df))
}

get_int <- function(df, col) {
  if (col %in% names(df)) as.integer(df[[col]]) else rep(NA_integer_, nrow(df))
}

parse_first_price <- function(x) {
  if (is.null(x)) return(NA_real_)
  
  if (is.character(x)) {
    parsed <- tryCatch(parse_json(x), error = function(e) NULL)
    if (is.null(parsed) || length(parsed) < 1) return(NA_real_)
    return(as.numeric(parsed[[1]]))
  }
  
  if (is.list(x) || is.atomic(x)) {
    if (length(x) < 1) return(NA_real_)
    return(as.numeric(x[[1]]))
  }
  
  NA_real_
}

extract_one_event <- function(json_path, snapshot_time_utc) {
  resp <- fromJSON(json_path, simplifyVector = FALSE)
  
  event <- resp[[1]]
  
  event_id <- as.character(event$id)
  event_slug <- as.character(event$slug)
  event_endDate <- as.character(event$endDate)
  
  markets_raw <- event$markets
  markets <- bind_rows(markets_raw)

  markets <- markets %>%
    mutate(
      event_id = event_id,
      event_slug = event_slug,
      event_endDate = event_endDate,
      snapshot_time = as.character(snapshot_time_utc)
    )
  
  markets <- markets %>%
    mutate(
      liquidity_any = coalesce(
        get_num(., "liquidity"),
        get_num(., "liquidityClob"),
        get_num(., "liquidityNum")
      ),
      volume1wk_any = coalesce(
        get_num(., "volume1wk"),
        get_num(., "volume1wkClob")
      ),
      spread_any = get_num(., "spread"),
      orderMinSize_any = get_num(., "orderMinSize"),
      negRisk_any = get_int(., "negRisk")
    )
  
  out <- markets %>%
    mutate(
      p_poly = map_dbl(outcomePrices, parse_first_price),
    
      option_type = case_when(
        groupItemTitle == "50+ bps decrease" ~ "cut_50plus",
        groupItemTitle == "25 bps decrease" ~ "cut_25",
        groupItemTitle == "No change" ~ "no_change",
        groupItemTitle == "25+ bps increase" ~ "hike_25plus",
        TRUE ~ NA_character_
      ),
      
      days_to_meeting = as.numeric(difftime(
        ymd_hms(event_endDate, tz = "UTC"),
        snapshot_time_utc,
        units = "days"
      )),
      market_age_days = as.numeric(difftime(
        snapshot_time_utc,
        ymd_hms(createdAt, tz = "UTC"),
        units = "days"
      )),
      
      log_volume_1wk = log1p(volume1wk_any),
      log_liquidity = log1p(liquidity_any),
      spread = spread_any,
      order_min_size = orderMinSize_any,
      neg_risk = negRisk_any
    ) %>%
    select(
      event_id, event_slug, event_endDate, snapshot_time,
      option_type, p_poly,
      days_to_meeting, market_age_days,
      log_volume_1wk, log_liquidity,
      spread, order_min_size, neg_risk
    )
  
  out
}