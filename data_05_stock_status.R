landings_proportions_RDBES <- read.csv("./output/data_03_shareLandingsInEcoregion.csv")

mac <- catch_current %>% 
    filter(StockKeyLabel == "mac.27.nea")

mac_RDBES <- landings_proportions_RDBES %>% 
    filter(fishstock == "mac.27.nea")

names(landings_proportions_RDBES)
names(catch_current)
unique(catch_current$Year)
landings_proportions_RDBES_inside <- landings_proportions_RDBES %>% 
    filter(CS == "inside")

unique(landings_proportions_RDBES_inside$fishstock)
unique(catch_current$StockKeyLabel)

### which stocks are inside catch_current that are not in landings_proportions_RDBES_inside
setdiff(unique(catch_current$StockKeyLabel), unique(landings_proportions_RDBES_inside$fishstock))
### which stocks are in landings_proportions_RDBES_inside that are not in catch_current
setdiff(unique(landings_proportions_RDBES_inside$fishstock), unique(catch_current$StockKeyLabel))


# ------------------------------------------------------------------
# Adjust catch and landings using the RDBES share for the matching
# stock-year combination. This is a placeholder until a complete
# stock-name matching table is available.
# ------------------------------------------------------------------

# temporary mapping for obvious naming mismatches between ICES stocks and
# the RDBES stock names used in the landings table
stock_name_map <- tibble::tribble(
  ~StockKeyLabel, ~match_stock,
  "ane.27.9aS", "ane.27.9a",
  "ane.27.9aW", "ane.27.9a",
  "hom.27.2a3a4a5b6a7a-ce-k8", "hom.27.2a4a5b6a7a-ce-k8"
)

# identify the RDBES stock names represented in catch_current
catch_stocks <- catch_current %>%
  transmute(
    StockKeyLabel = trimws(as.character(StockKeyLabel))
  ) %>%
  left_join(stock_name_map, by = "StockKeyLabel") %>%
  transmute(fishstock = coalesce(match_stock, StockKeyLabel)) %>%
  distinct(fishstock)

# keep only RDBES stocks represented in catch_current, then use the latest
# available inside-share row for each stock
recent_stock_share <- landings_proportions_RDBES %>%
  mutate(
    fishstock = trimws(as.character(fishstock)),
    CLyear = as.integer(CLyear)
  ) %>%
  filter(CS == "inside") %>%
  semi_join(catch_stocks, by = "fishstock") %>%
  group_by(fishstock) %>%
  slice_max(CLyear, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    RDBESStockKeyLabel = fishstock,
    LastRDBESYear = CLyear,
    ShareLandingsInEcoregion = as.numeric(ShareLandingsInEcoregion)
  )

catch_current_adj <- catch_current %>%
  mutate(
    StockKeyLabel = trimws(as.character(StockKeyLabel))
  ) %>%
  left_join(stock_name_map, by = "StockKeyLabel") %>%
  mutate(
    RDBESStockKeyLabel = coalesce(match_stock, StockKeyLabel),
    Year = as.integer(Year)
  ) %>%
  left_join(recent_stock_share, by = "RDBESStockKeyLabel") %>%
  mutate(
    ShareLandingsInEcoregion = if_else(!is.na(ShareLandingsInEcoregion),
                                      ShareLandingsInEcoregion / 100,
                                      NA_real_),
    Catches_in_ecoregion = if_else(!is.na(ShareLandingsInEcoregion),
                                  Catches * ShareLandingsInEcoregion,
                                  Catches),
    Landings_in_ecoregion = if_else(!is.na(ShareLandingsInEcoregion),
                                   Landings * ShareLandingsInEcoregion,
                                   Landings)
  )

# check which stocks still have no matching RDBES share
catch_current_adj %>%
  filter(is.na(ShareLandingsInEcoregion)) %>%
  distinct(StockKeyLabel) %>%
  arrange(StockKeyLabel)
names(catch_current_adj)
# example: compare one stock
test_mac <- catch_current_adj %>%
  filter(StockKeyLabel == "mac.27.nea") %>%
  select(StockKeyLabel, Year, RDBESStockKeyLabel,
         ShareLandingsInEcoregion,
         Catches, Catches_in_ecoregion, Landings, Landings_in_ecoregion)
test_whb <- catch_current_adj %>%
  filter(StockKeyLabel == "whb.27.1-91214") %>%
  select(StockKeyLabel, Year, RDBESStockKeyLabel,
         ShareLandingsInEcoregion,
         Catches, Catches_in_ecoregion, Landings, Landings_in_ecoregion)
head(test_mac)
head(test_whb)
test_mac$Catches
test_mac$Catches_in_ecoregion/test_mac$Catches
test_mac$Landings
test_mac$Landings_in_ecoregion

catch_current_adj$ShareLandingsInEcoregion
    
