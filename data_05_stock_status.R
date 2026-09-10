landings_proportions_RDBES <- read.csv("./output/data_03_shareLandingsInEcoregion.csv")

mac <- catch_current %>% 
    filter(StockKeyLabel == "mac.27.nea")

mac_RDBES <- landings_proportions_RDBES %>% 
    filter(fishstock == "mac.27.nea")
157955469/715121305
157955469/1000
22.08793+77.91207
names(landings_proportions_RDBES)
landings_proportions_RDBES_inside <- landings_proportions_RDBES %>% 
    filter(CS == "inside")

unique(landings_proportions_RDBES_inside$fishstock)
unique(catch_current$StockKeyLabel)

### which stocks are inside catch_current that are not in landings_proportions_RDBES_inside
setdiff(unique(catch_current$StockKeyLabel), unique(landings_proportions_RDBES_inside$fishstock))
### which stocks are in landings_proportions_RDBES_inside that are not in catch_current
setdiff(unique(landings_proportions_RDBES_inside$fishstock), unique(catch_current$StockKeyLabel))


# ------------------------------------------------------------------
# Temporary stock-by-stock adjustment using the most recent RDBES share
# for each stock. This is a placeholder until a more complete stock-match
# table is built.
# ------------------------------------------------------------------

# keep only the latest inside-share value per stock
recent_stock_share <- landings_proportions_RDBES %>%
  filter(CS == "inside") %>%
  mutate(
    fishstock = trimws(as.character(fishstock)),
    ShareLandingsInEcoregion = as.numeric(ShareLandingsInEcoregion),
    CLyear = as.integer(CLyear)
  ) %>%
  group_by(fishstock) %>%
  slice_max(CLyear, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    StockKeyLabel = fishstock,
    ShareLandingsInEcoregion,
    LastRDBESYear = CLyear
  )

# temporary mapping for obvious naming mismatches between ICES stocks and
# the RDBES stock names used in the landings table
stock_name_map <- tibble::tribble(
  ~StockKeyLabel, ~match_stock,
  "ane.27.9aS", "ane.27.9a",
  "ane.27.9aW", "ane.27.9a",
  "hom.27.2a3a4a5b6a7a-ce-k8", "hom.27.2a4a5b6a7a-ce-k8"
)

catch_current_adj <- catch_current %>%
  mutate(
    StockKeyLabel = trimws(as.character(StockKeyLabel))
  ) %>%
  left_join(stock_name_map, by = "StockKeyLabel") %>%
  mutate(
    StockKeyLabel = coalesce(match_stock, StockKeyLabel)
  ) %>%
  left_join(recent_stock_share, by = "StockKeyLabel") %>%
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

# check which stocks still have no recent RDBES share available
catch_current_adj %>%
  filter(is.na(ShareLandingsInEcoregion)) %>%
  distinct(StockKeyLabel) %>%
  arrange(StockKeyLabel)

# example: compare one stock
catch_current_adj %>%
  filter(StockKeyLabel == "mac.27.nea") %>%
  select(StockKeyLabel, LastRDBESYear, ShareLandingsInEcoregion,
         Catches, Catches_in_ecoregion, Landings, Landings_in_ecoregion)
head(catch_current_adj)

catch_current_adj$ShareLandingsInEcoregion
    
