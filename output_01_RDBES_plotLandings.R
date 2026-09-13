### libraries
library(plotly)
library(htmlwidgets)
library(dplyr)

source("utilities.R")
dir.create("./output/landings_RDBES_html_new", recursive = TRUE, showWarnings = FALSE)

### load data
BI_RDBES <- read.csv("./data/RDBES_landings_BI.csv")


guilds <- unique(BI_RDBES$GUILD) %>%
  na.omit() %>%
  sort()

save_catch_trends_html(
  data = BI_RDBES,
  guilds = guilds,
  types = c("Common name", "Country", "Fisheries guild"),
  ecoregion = "Bay of Biscay and the Iberian Coast",
  output_dir = "./output/landings_RDBES_html_new",
  line_count = 10,
  dataUpdated = "Data updated: 2024-06-01"
)

