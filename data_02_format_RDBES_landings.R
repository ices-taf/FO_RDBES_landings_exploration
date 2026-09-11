### load RDBES landings data
load("./data/extendedRDBES.RData")

### libraries
library(tidyverse)
library(countrycode)
library(dplyr)
library(icesVMS)

source("utilities.R")

### data exploration
names(cl.all2)
head(cl.all2)
unique(cl.all2$CLconfidentialityFlag)
table(cl.all2$CLconfidentialityFlag, cl.all2$CLvesselFlagCountry)
table(cl.all2$CLconfidentialityFlag, cl.all2$CLyear)
table(cl.all2$CLarea, cl.all2$CLvesselFlagCountry)


### prefilter for BI and BI areas
Ecoregion = "Bay of Biscay and the Iberian Coast"
BI_areas_landings <- c("27.8.a", 
                        "27.8.b", 
                        "27.8.c", 
                        "27.8.d.2", 
                        "27.8.e.2", 
                        "27.9.a", 
                        "27.9.b.2")

dim(cl.all2)
cl.allArea <- cl.all2 %>%
      mutate(IntraBI = case_when(
        CLarea %in% BI_areas_landings ~ "inside",
        .default = "outside"
      ))

names(cl.allArea)
unique(cl.allArea$CLarea)
table(cl.allArea$CLarea, cl.allArea$CLvesselFlagCountry)

areaBI <- filter(cl.allArea, IntraBI =="inside")
unique(areaBI$CLarea)
#### trying with stat squares to see if more areas are included
areaAll <- get_csquare(ecoregion = Ecoregion, convert2sf = TRUE)
area <- unique(areaAll$stat_rec)
plot(areaAll)
head(areaAll)
unique(areaAll$ices_area)

### checking which csquares are inclided in the boundaries of the ecoregion
plot(areaAll[areaAll$ices_area == "9b1","ices_area"])
plot(areaAll[areaAll$ices_area == "8e1","ices_area"])
plot(areaAll[areaAll$ices_area %in% c("9b1", "8e1", "8d1","8e1"), "ices_area"]) # add ecoregion and areas to map

# define inside/outside of the area of interest
cl.allArea2 <- cl.all2 %>%
    mutate(IntraBI = case_when(
    CLstatisticalRectangle %in% area ~ "inside",
    .default = "outside"
    ))

areaBI_StatRec <- filter(cl.allArea2, IntraBI =="inside")
dim(areaBI_StatRec)
dim(areaBI)

### selecting the columns of interest for the RDBES format
prelim_RDBES <- areaBI_StatRec %>% 
    select(
      CLyear,
      CLvesselFlagCountry,
      CLspeciesFaoCode,
      CLscientificWeight,
    #   Scientific_Name,
    #   English_name.x,
      CLarea
    ) %>%
    rename(
      YEAR = CLyear,
      Country = CLvesselFlagCountry,
      ISO3 = CLvesselFlagCountry,
    #   SPECIES_NAME = Scientific_Name,
    #   COMMON_NAME = English_name.x,
      Area = CLarea,
      VALUE = CLscientificWeight
    )
head(prelim_RDBES)

################### Getting data from ICES ###################
sid <- icesSD::getSD(NULL, as.numeric(format(Sys.Date(), "%Y")))

fish_category <- dplyr::mutate(sid, X3A_CODE = substr(sid$StockKeyLabel, start = 1, stop = 3))
fish_category <- dplyr::select(fish_category, X3A_CODE, FisheriesGuild)
fish_category$X3A_CODE <- toupper(fish_category$X3A_CODE)
fish_category <- unique(fish_category)
#CAA, SEH, SEZ  have no guild
#REB is both pelagic and demersal
sid$FisheriesGuild[which(sid$StockKeyLabel == "caa.27.5a")] <- "Demersal"
#Should we include seals? maybe not
sid <- sid %>% dplyr::filter(SpeciesScientificName != "Pagophilus groenlandicus")
sid <- sid %>% dplyr::filter(SpeciesScientificName != "Cystophora cristata")


species_list <- load_asfis_species()

hist <- load_historical_catches()
hist$Country[which(hist$Country == "Germany, New L\xe4nder")]<- "Germany"

official <- load_official_catches()

manual_attributions <- readxl::read_excel(
  "./Boot/landings_manual_attributions.xlsx",
  sheet = "current_manual_attributions"
) %>%
  as.data.frame()

catch_dat_rdbes <- format_catches_dev_rdbes(
          year = as.numeric(format(Sys.Date(), "%Y")),
          ecoregion = "Bay of Biscay and the Iberian Coast",
          historical = hist,
          official = official,
          preliminary = prelim_RDBES,
          species_list = species_list,
          sid = sid,
          manual_attributions = manual_attributions
          )

head(catch_dat_rdbes)
unique(catch_dat_rdbes$ECOREGION)
unique(catch_dat_rdbes$YEAR)



write.csv(catch_dat_rdbes, file.path("./data", paste0("RDBES_landings_BI", ".csv")), row.names = FALSE)
