library(dplyr)

source("utilities_discards.R")

sag <- getSAG_ecoregion_new("Bay of Biscay and the Iberian Coast")



sid <- getSID(2025, "Bay of Biscay and the Iberian Coast")




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
