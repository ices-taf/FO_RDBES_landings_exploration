##### input needed
## RDBES data = RDBES
## list of ices rectangles from the eco region ICES_Rect
## percentages of landings over which should be aggregated to OTH

# output WD
output.wd <- "./output/"

# local variables
ecoregionList <- c(#"Celtic Seas"#, 
                   "Bay of Biscay and the Iberian Coast"#, 
                   ##"Azores", 
                   #"Baltic Sea", 
                   ##"Barents Sea",
                   #"Faroes",
                   #"Greater North Sea",
                   #"Norwegian Sea"
                   )
percList <- c(#80, 
              #90, 
              100)
numberOfStocks <- 5
guildList <- c("Demersal", "Pelagic", "Benthic", "Crustacean", "Elasmobranch")
yearsIncluded <- c(2022:2024)

# load RDBES
load("./data/extendedRDBES.RData")


#### sum over megrims and anglers
cl.all2 <- cl.all2 %>%
  mutate(CLspeciesFaoCode = case_when(
    CLspeciesFaoCode %in% c("ANK", "MON", "MNZ") ~ "ANF", 
    CLspeciesFaoCode %in% c("LDB", "MEG") ~ "LEZ",
    TRUE ~ CLspeciesFaoCode
    
  ))


sid <- readRDS("./bootstrap/data/sidAll_2025.rds")
sid <- sid %>%
  mutate(CLspeciesFaoCode = toupper(substr(StockKeyLabel, 1, 3))) %>%
  select(CLspeciesFaoCode, FisheriesGuild) %>%
  filter(!is.na(FisheriesGuild)) %>%
  distinct()
## prb with reb that is defined in 2 different guilds...
sid <- sid %>%
  filter(!(CLspeciesFaoCode == "REB" & FisheriesGuild == "Demersal"))


Ecoregion = "Bay Of Biscay and the Iberian Coast"
guild = "Benthic"
percLand = 100


threshold <- "InNumberOfSpecies" # or  "InPercentage" # defines if the OTH group is based on the percentage of landings or the number of species to plot


for (Ecoregion in ecoregionList){
  dir.create(paste("output/", 
                   Ecoregion,
                   "/",
                   sep = "")
  )
  areaAll <- get_csquare(ecoregion = Ecoregion, convert2sf = TRUE)
  area <- unique(areaAll$stat_rec)
  for(guild in guildList) {
    for (percLand in percList) {
      print(Ecoregion);print(guild); print(percLand)
      # get ICES squares linked to the Ecoregion
      # load ecoregion map
      ecoregion <- Ecoregion
      shape_ecoregion <- icesFO::load_ecoregion(ecoregion)
      
      #filter RDBED based on ICES Square list
      landingsStatRect <- cl.all2 %>%
        filter(CLstatisticalRectangle %in% area)
      
      #sum over ICES Square and Species 
      # TO DO - get number of vessels
      landingsStatRect <- landingsStatRect %>%
        group_by(CLstatisticalRectangle, CLspeciesFaoCode, CLyear) %>%
        summarise(weight = sum(CLscientificWeight_perVessel, na.rm = T), 
                  value = sum(CLlandingsValue_perVessel, na.rm = T))  %>%
        group_by(CLstatisticalRectangle, CLspeciesFaoCode) %>%
        summarise(weight = mean(weight, na.rm = T), 
                  value = mean(value, na.rm = T)) 
      #add guild
      landingsStatRect <- landingsStatRect %>%
        left_join(sid, by = "CLspeciesFaoCode") %>%
        ungroup()
      
      #add long lat
      landingsStatRect <- landingsStatRect %>%
        bind_cols(ices.rect(landingsStatRect$CLstatisticalRectangle)) %>%
        mutate(N = 1) %>%
        filter(!is.na(lon))
      
      #extract species list based on guild
      listspp <- landingsStatRect %>%
        filter(FisheriesGuild == guild) %>%
        distinct(CLspeciesFaoCode) %>%
        pull()
      ##manually add species to cru
      if(guild == "Crustacean") {
        listspp <- c(listspp, "CRE", "SCE", "WHE")
      }
      
      
      
      # data prep
      landingsStatRect <- landingsStatRect %>%
        filter(CLspeciesFaoCode %in% listspp) %>%
        group_by(CLstatisticalRectangle, lon, lat, CLspeciesFaoCode) %>%
        summarise(weight = sum(weight, na.rm = TRUE), .groups = "drop") 
      ## test to get the main species included in the percentage defined 
      
      if(threshold == "InPercentage"){
          test <- landingsStatRect %>%
            group_by(CLspeciesFaoCode) %>%
            summarise(weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
            arrange(desc(weight)) %>%
            mutate(Cumsum = cumsum(weight), 
                   perc = Cumsum / max(Cumsum) * 100)
          
          testSpp <- test %>%
            filter(perc <= percLand) %>%
            distinct(CLspeciesFaoCode) %>%
            pull()
      } else if (threshold == "InNumberOfSpecies"){
        test <- landingsStatRect %>%
          group_by(CLspeciesFaoCode) %>%
          summarise(weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(weight)) 
        
        testSpp <- test%>%
          head(n = numberOfStocks) %>%
          distinct(CLspeciesFaoCode) %>%
          pull()
        removedSpp  <- test%>%
          tail(-numberOfStocks) %>%
          distinct(CLspeciesFaoCode) %>%
          pull()
      }
      ## group species not included in perc to OTH
      landingsStatRect <- landingsStatRect %>%
        mutate(CLspeciesFaoCode = case_when(
          CLspeciesFaoCode %in% testSpp ~ CLspeciesFaoCode, 
          TRUE ~ "OTH"
        )) %>%
        group_by(CLstatisticalRectangle, lon, lat, CLspeciesFaoCode) %>%
        summarise(weight = sum(weight, na.rm = TRUE), .groups = "drop") 
      textOTH <- landingsStatRect %>%
        group_by(CLspeciesFaoCode) %>%
        summarise(weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
        mutate(perc = weight / sum(weight) *100)
      
      
      pie_data <- landingsStatRect %>%
        pivot_wider(names_from = CLspeciesFaoCode, values_from = weight, values_fill = 0)
      # landings per square (size of the pie)
      pie_data$sum_weight <- rowSums(pie_data[ , !(names(pie_data) %in% c("CLstatisticalRectangle", "lon", "lat"))])
      
      coords <- pie_data %>% select(lon, lat)
      pies <- pie_data %>% select(-CLstatisticalRectangle, -lon, -lat, -sum_weight)
      
      # Palette de couleurs 
      species_names <- colnames(pies)
      n_spp <- length(species_names)
      palette <- brewer.pal(max(3, min(n_spp, 8)), "Set2")
      if (n_spp > 8) palette <- colorRampPalette(brewer.pal(8, "Set2"))(n_spp)
      
      # pies size
      max_radius <- 15
      radius <- sqrt(pie_data$sum_weight) / max(sqrt(pie_data$sum_weight), na.rm=TRUE) * max_radius
      
      xlim <- c(min(landingsStatRect$lon, na.rm = T),max(landingsStatRect$lon, na.rm = T))
      ylim <- c(min(landingsStatRect$lat, na.rm = T) - 2, max(landingsStatRect$lat, na.rm = T) + 2)
      
      

      # map
      a <- leaflet(pie_data) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addPolygons(
          data = shape_ecoregion,
          fillColor = "transparent",
          color = "black",
          weight = 1
        )%>%
        addMinicharts(
          lng = coords$lon,
          lat = coords$lat,
          chartdata = as.matrix(pies),
          type = "pie",
          width = radius,
          height = radius,
          colorPalette = palette,
          legend = TRUE,
          legendPosition = "topright",
          showLabels = TRUE#,
          #autoResize = TRUE
        )
   
      
      if(threshold == "InPercentage") {
        path <- paste("output/", 
                      Ecoregion,
                      "/",
                      percLand,
                      "/",
                      sep = "")
      } else if (threshold == "InNumberOfSpecies") {
        path <- paste("output/", 
                      Ecoregion,
                      "/",
                      numberOfStocks, "_Stocks",
                      "/",
                      sep = "")
      }
      
      dir.create(path)
      
      saveWidget(a, file = paste(path,
                                 guild, 
                                 "_piecharts_map.html",
                                 sep = ""), 
                 selfcontained = TRUE)
      
      #### save informations about the others
      texte <- paste0(
        "List of species included in OTH : ", 
        paste(removedSpp, collapse = ", "), "\n",
        "The percentage of landings made by OTH is : ", textOTH %>% filter(CLspeciesFaoCode == "OTH") %>% select(perc)%>% pull(), "%"
      )
      
      writeLines(texte, paste0(paste(path,
                                     guild,
                                     "resultat.txt")))
      
      
      html_path <- normalizePath( paste(path,
                                        guild,
                                        "_piecharts_map.html",
                                        sep = ""))
      pdf_path  <- normalizePath( paste(path,
                                        guild,  
                                        "_piecharts_map.pdf",
                                        sep = ""), 
                                  mustWork = FALSE)
      
      
      pagedown::chrome_print(
        input = html_path,
        output = pdf_path,
        extra_args = c(
          "--disable-dev-shm-usage",
          "--no-sandbox",
          "--disable-gpu",
          "--headless"
        )
      )
    }      
  }
}
