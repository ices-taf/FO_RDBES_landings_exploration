source("utilities_GES_pies.R")
source("utilities.R")

SAG_Settings <- getSAG_SettingsEcoregion("Bay of Biscay and the Iberian Coast")


catch_current <- stockstatus_CLD_current_proxy(add_proxyRefPoints(format_sag(sag, sid),  sag_settings = SAG_Settings))

source("data_05_stock_status.R")

clean_status <- format_sag_status_new(getStatusWebService("Bay of Biscay and the Iberian Coast", sid), sag)



