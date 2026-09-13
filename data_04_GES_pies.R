source("utilities_GES_pies.R")
source("utilities.R")

SAG_Settings <- getSAG_SettingsEcoregion("Bay of Biscay and the Iberian Coast")


catch_current <- stockstatus_CLD_current_proxy(add_proxyRefPoints(format_sag(sag, sid),  sag_settings = SAG_Settings))











clean_status <- format_sag_status_new(getStatusWebService("Bay of Biscay and the Iberian Coast", sid), sag)


plot_GES_pies(clean_status, catch_current_adj)
p4 <- plot_GES_pies(clean_status, catch_current_adj)

test <- catch_current_adj %>% filter(StockKeyLabel == "mac.27.nea")
p <- plot_GES_pies_plotly(clean_status, catch_current_adj)
file_name <- "GES_pies_catches_adjusted_with_plotly_RDBES_BI"
htmlwidgets::saveWidget(
        widget = p,
        file = file.path("./output", paste0(file_name, ".html")),
        selfcontained = TRUE
      )
head(clean_status)
test_pil <- catch_current %>% filter(StockKeyLabel == "pil.27.8c9a")

test_pil2 <- catch_current_adj %>% filter(StockKeyLabel == "pil.27.8c9a")
test_pli3 <- clean_status %>% filter(StockKeyLabel == "pil.27.8c9a")
print(test_pil, width = Inf)
print(test_pil2, width = Inf)
print(test_pli3, width = Inf)

head(catch_current)

p <- plot_GES_pies(clean_status, catch_current_adj)
p2 <- plot_GES_pies_plotly2(clean_status, catch_current_adj)



p3 <- plot_CLD_bar_app(catch_current_adj, "Pelagic")
head(catch_current_adj)
