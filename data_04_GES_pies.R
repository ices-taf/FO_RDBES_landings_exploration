source("utilities_GES_pies.R")
source("utilities.R")
source("utilities_discards.R")

SAG_Settings <- getSAG_SettingsEcoregion("Bay of Biscay and the Iberian Coast")

sid <- getSID(2026, "Bay of Biscay and the Iberian Coast")
sag <- getSAG_ecoregion_new("Bay of Biscay and the Iberian Coast")
catch_current <- stockstatus_CLD_current_proxy(add_proxyRefPoints(format_sag(sag, sid),  sag_settings = SAG_Settings))
catch_current_v2 <- stockstatus_CLD_current_proxy(add_proxyRefPoints_v2(format_sag(sag, sid),  sag_settings = SAG_Settings))
head(catch_current) %>% print(width = Inf)
head(catch_current_v2) %>% print(width = Inf)
source("data_05_stock_status.R")

head(catch_current_adj) %>% print(width = Inf)
unique(discard_base_data$Year)


clean_status <- format_sag_status_new(getStatusWebService("Bay of Biscay and the Iberian Coast", sid), sag)
test <- getStatusWebService("Bay of Biscay and the Iberian Coast", sid)
head(clean_status)
head(test)
unique(test$stockSizeStatus)
unique(test$fishingPressureStatus)
