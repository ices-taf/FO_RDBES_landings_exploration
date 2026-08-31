#### explore the data
names(cl.all2)
unique(cl.all2$CLconfidentialityFlag)
table(cl.all2$CLconfidentialityFlag, cl.all2$CLvesselFlagCountry)
table(cl.all2$CLconfidentialityFlag, cl.all2$CLyear)
table(cl.all2$CLarea, cl.all2$CLvesselFlagCountry)



names(cl.allArea)
unique(cl.allArea$CLarea)
table(cl.allArea$CLarea, cl.allArea$CLvesselFlagCountry)
areaBI <- cl.allArea %>% filter(CS=="inside" & IntraCS =="outside")

table(areaBI$CLarea, areaBI$CLvesselFlagCountry)
names(areaBI)
head(areaBI$CLscientificWeight)


fish_guilds_from_sid <- sid %>%
    mutate(
      Alpha3_Code = substr(StockKeyLabel, 1, 3),
      Alpha3_Code = clean_code(Alpha3_Code)
    ) %>%
    select(Alpha3_Code, FisheriesGuild) %>%
    distinct()
### refiltering using the areas we alrady use for the landings

# unique(areaAll$ices_area)
Ecoregion = "Bay of Biscay and the Iberian Coast"
BI_areas_landings <- c("27.8.a", "27.8.b", "27.8.c", "27.8.d.2", "27.8.e.2", "27.9.a", "27.9.b.2")

cl.allArea <- cl.allArea %>%
      mutate(IntraBI = case_when(
        CLarea %in% BI_areas_landings ~ "inside",
        .default = "outside"
      ))
areaBI <- filter(cl.allArea, IntraBI =="inside")
names(areaBI)
fao_species_lookup <- read.csv(file = "./Boot/ASFIS_sp_2025.csv", quote = "")
head(fao_species_lookup)

### add english name from fao_species_lookup to areaBI using fao_species_lookup$Alpha3_Code == areaBI$CLspeciesFaoCode
areaBI <- areaBI %>%
  left_join(
    fao_species_lookup %>%
      select(Alpha3_Code, English_name, Scientific_Name),
    by = c("CLspeciesFaoCode" = "Alpha3_Code")
  )

### add fisheries guild from sid to areaBI using fish_guilds_from_sid$Alpha3_Code == areaBI$CLspeciesFaoCode
areaBI <- areaBI %>%
  left_join(
    fish_guilds_from_sid,
    by = c("CLspeciesFaoCode" = "Alpha3_Code")
  )
names(areaBI)
table(areaBI$FisheriesGuild, areaBI$CLspeciesFaoCode)
unique(areaBI$FisheriesGuild)
### turn FisheriesGuild == NA into "Undefined"
areaBI <- areaBI %>%
  mutate(FisheriesGuild = ifelse(is.na(FisheriesGuild), "Undefined", FisheriesGuild))

table(areaBI$FisheriesGuild)



### landings x country
landings_country <- areaBI %>%
#   filter(
#     IntraBI == "inside",
#     CLspeciesFaoCode != "NULL"
#   ) %>%
  group_by(
    CLyear,
    CLvesselFlagCountry
  ) %>%
  summarise(
    landings = sum(CLscientificWeight_perVessel, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    landings_thousand_tonnes = landings / 1e6
  )

dim(landings_country)

RDBES_landings_country <- plot_ly(
  landings_country,
  x = ~CLyear,
  y = ~landings_thousand_tonnes,
  color = ~CLvesselFlagCountry,
  type = "scatter",
  mode = "lines+markers",
  text = ~paste0(
    "<b>", CLvesselFlagCountry, "</b>",
    "<br>Year: ", CLyear,
    "<br>Landings: ", scales::comma(landings_thousand_tonnes), " thousand tonnes"
  ),
  hoverinfo = "text"
) %>%
  layout(
    yaxis = list(title = "Landings (thousand tonnes)")
  )

#saving plot
htmlwidgets::saveWidget(
        widget = RDBES_landings_country,
        file = file.path("./output", paste0("RDBES_landings_country", ".html")),
        selfcontained = TRUE
      )

### landings x species
landings_species <- areaBI %>%
#   filter(
#     IntraBI == "inside",
#     CLspeciesFaoCode != "NULL"
#   ) %>%
  group_by(
    CLyear,
    English_name
  ) %>%
  summarise(
    landings = sum(CLscientificWeight_perVessel, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    landings_thousand_tonnes = landings / 1e6
  )

dim(landings_species)

RDBES_landings_species <- plot_ly(
  landings_species,
  x = ~CLyear,
  y = ~landings_thousand_tonnes,
  color = ~English_name,
  type = "scatter",
  mode = "lines+markers",
  text = ~paste0(
    "<b>", English_name, "</b>",
    "<br>Year: ", CLyear,
    "<br>Landings: ", scales::comma(landings_thousand_tonnes), " thousand tonnes"
  ),
  hoverinfo = "text"
) %>%
  layout(
    yaxis = list(title = "Landings (thousand tonnes)")
  )

#saving plot
htmlwidgets::saveWidget(
        widget = RDBES_landings_species,
        file = file.path("./output", paste0("RDBES_landings_species", ".html")),
        selfcontained = TRUE
      )


### landings by species and guilds. Keep the top 11 species by landings, and group the rest into "other"
landings_species_guild <- areaBI %>%
  filter(
    FisheriesGuild == "Undefined",
  ) %>%
  group_by(
    CLyear,
    English_name
  ) %>%
  summarise(
    landings = sum(CLscientificWeight_perVessel, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    landings_thousand_tonnes = landings / 1e6
  )

  RDBES_landings_species_Undefined <- plot_ly(
  landings_species_guild,    
  x = ~CLyear,
  y = ~landings_thousand_tonnes,
  color = ~English_name,
  type = "scatter",
  mode = "lines+markers",
  text = ~paste0(
    "<b>", English_name, "</b>",
    "<br>Year: ", CLyear,
    "<br>Landings: ", scales::comma(landings_thousand_tonnes), " thousand tonnes"
  ),
  hoverinfo = "text"
) %>%
  layout(
    yaxis = list(title = "Landings (thousand tonnes)")
  )

#saving plot
htmlwidgets::saveWidget(
        widget = RDBES_landings_species_Undefined,
        file = file.path("./output", paste0("RDBES_landings_species_Undefined", ".html")),
        selfcontained = TRUE
      )

### landings by fisheries guilds
landings_guild <- areaBI %>%
#   filter(
#     FisheriesGuild == "Pelagic",
#   ) %>%
  group_by(
    CLyear,
    FisheriesGuild
  ) %>%
  summarise(
    landings = sum(CLscientificWeight_perVessel, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    landings_thousand_tonnes = landings / 1e6
  )

  RDBES_landings_guild <- plot_ly(
  landings_guild,    
  x = ~CLyear,
  y = ~landings_thousand_tonnes,
  color = ~FisheriesGuild,
  type = "scatter",
  mode = "lines+markers",
  text = ~paste0(
    "<b>", FisheriesGuild, "</b>",
    "<br>Year: ", CLyear,
    "<br>Landings: ", scales::comma(landings_thousand_tonnes), " thousand tonnes"
  ),
  hoverinfo = "text"
) %>%
  layout(
    yaxis = list(title = "Landings (thousand tonnes)")
  )

#saving plot
htmlwidgets::saveWidget(
        widget = RDBES_landings_guild,
        file = file.path("./output", paste0("RDBES_landings_guild", ".html")),
        selfcontained = TRUE
      )
