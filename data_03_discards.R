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
filename <- "Discards trends adjusted with RDBES"
htmlwidgets::saveWidget(
        widget = p1,
        file = file.path("./output/kobe_interactive/Bref_Fref-v2/", paste0(filename,"-", Sys.Date(), ".html")),
        selfcontained = TRUE
      )
