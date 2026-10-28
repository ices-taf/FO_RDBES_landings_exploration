library(dplyr)

source("utilities_discards.R")

sag <- getSAG_ecoregion_new("Bay of Biscay and the Iberian Coast")

sid <- getSID(2026, "Bay of Biscay and the Iberian Coast")




discard_base_data <- CLD_trends(format_sag(sag, sid))

head(discard_base_data)

### find Discards for pelagic stocks and sum
pelagic_discards <- discard_base_data %>%
        filter(FisheriesGuild == "pelagic") %>%
        filter(Year > 2010) %>%
        select(Year, StockKeyLabel, Discards)
        

head(pelagic_discards)
sum(pelagic_discards$Discards, na.rm = TRUE)

### do the same for demersal stocks
demersal_discards <- discard_base_data %>%
        filter(FisheriesGuild == "demersal") %>%
        filter(Year > 2010) %>%
        select(Year, StockKeyLabel, Discards)

head(demersal_discards)
sum(demersal_discards$Discards, na.rm = TRUE)

### do the same for elasmobranch stocks
elasmobranch_discards <- discard_base_data %>%
        filter(FisheriesGuild == "elasmobranch") %>%
        filter(Year > 2010) %>%
        select(Year, StockKeyLabel, Discards)

head(elasmobranch_discards)
sum(elasmobranch_discards$Discards, na.rm = TRUE)





unique(discard_base_data$FisheriesGuild)
plot_discard_trends_app_plotly(discard_base_data, 2026)

plot_discard_trends_app_plotly(
  discard_base_data,
  2026,
  return_data = TRUE
) %>%
  filter(FisheriesGuild == "pelagic") %>%
  select(Year, guildLandings, guildDiscards, guildRate)





library(dplyr)
head(discard_base_data)


share_lookup <- catch_current_adj %>%
  filter(
    !is.na(ShareLandingsInEcoregion),
    between(ShareLandingsInEcoregion, 0, 1)
  ) %>%
  select(
    StockKeyLabel,
    share = ShareLandingsInEcoregion,
    LastRDBESYear
  )

discard_estimated <- discard_base_data %>%
  left_join(
    share_lookup,
    by = "StockKeyLabel",
    relationship = "many-to-one"
  ) %>%
  mutate(
    Catches_ecoregion = Catches * share,
    Landings_ecoregion = Landings * share,
    Discards_ecoregion = Discards * share
  )


head(share_lookup) %>% print(width = Inf)
print(n = 55,share_lookup, width = Inf)
head(discard_estimated) %>% print(width = Inf)



p1 <- plot_discard_trends_app_plotly_v3(x = discard_estimated, year = 2026, ecoregion = "BI", complete_pairs = TRUE)
filename <- "Discards trends adjusted with RDBES - New method"
htmlwidgets::saveWidget(
        widget = p1,
        file = file.path("./output/kobe_interactive/Bref_Fref-v2/", paste0(filename,"-", Sys.Date(), ".html")),
        selfcontained = TRUE
      )

head(discard_estimated) 
write.csv(discard_estimated, "output/discard_estimated.csv", row.names = FALSE)

######################## testing new methid
library(dplyr)

# 1. Read the annual stock shares.
new_proportions <- read.csv(
  "output/stock_matching_rectangle_check/Bay_of_Biscay_and_the_Iberian_Coast/20261028_011823/stock_share_lookup_with_anchovy_proxy.csv",
  stringsAsFactors = FALSE
)

# 2. Prepare one share per stock and RDBES year.
# Keep NA shares: an unavailable estimate is not a zero share.
share_lookup <- new_proportions %>%
  transmute(
    StockKeyLabel = as.character(fishstock),
    RDBES_share_year = as.integer(CLyear),
    share = ShareLandingsInEcoregion / 100
  )

# Check that the lookup has unique stock-year keys and valid proportions.
stopifnot(
  !anyDuplicated(
    share_lookup[c("StockKeyLabel", "RDBES_share_year")]
  ),
  all(
    is.na(share_lookup$share) |
      between(share_lookup$share, 0, 1)
  )
)

# 3. Choose the share year, join by stock AND year, and apply the share.
discard_estimated <- discard_base_data %>%
  mutate(
    StockKeyLabel = as.character(StockKeyLabel),
    Year = as.integer(as.character(Year)),

    # Earlier years use 2021; all later years retain their own year.
    RDBES_share_year = pmax(Year, 2021L)
  ) %>%
  left_join(
    share_lookup,
    by = c("StockKeyLabel", "RDBES_share_year"),
    relationship = "many-to-one",
    na_matches = "never"
  ) %>%
  mutate(
    share_status = case_when(
      is.na(share) ~ "share unavailable",
      Year < 2021L ~ "2021 share applied to earlier year",
      TRUE ~ "same-year share"
    ),

    Catches_ecoregion  = Catches  * share,
    Landings_ecoregion = Landings * share,
    Discards_ecoregion = Discards * share
  )
