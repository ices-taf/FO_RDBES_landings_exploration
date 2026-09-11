### load data created by data_00_createExtendedRDBES.R
load("./data/extendedRDBES.RData")
  
# setUp-----------
## percentage of landings in the area--------
percLand <- 80 # for vessels contributing to at least 80% of the area of interest landings 
percActivity <- 80 # for vessels with at least 80% of their activity in the area of interest

# select area from ecoregion----------
  #area <- c("27.8.a", "27.8.b", "27.8.d")
  # Ecoregion = "Greater North Sea"
  # Ecoregion = "Celtic Seas"
  Ecoregion = "Bay of Biscay and the Iberian Coast"
  # for CS the ecoregion is divided in 3 parts
  # NorthernPart <- c("27.2.a.2", "27.4.a", "27.6.a", "27.6.b.2")
  # CSArea <- c("27.7.b", "27.7.c.2", "27.7.e", "27.7.f", "27.7.g", "27.7.h", "27.7.j.2", "27.7.k.2")
  # IS <- c("27.7.a")


  # for BoB the ecoregion is divided in 2 parts
  BoB <- c("27.8.b",  "27.8.d",  "27.8.a",   "27.8.d.1",  "27.8.d.2")
  Iberian <- c("27.9.a",  "27.9.b.2", "27.9.b.1","27.8.c", "27.8.e.2", "27.8.e.1")


#areaMerge <- c(NorthernPart, CSArea, IS)
areaMerge <- c(BoB, Iberian)


areaAll <- icesVMS::get_csquare(ecoregion = Ecoregion, convert2sf = TRUE)
area <- unique(areaAll$stat_rec)

library(dplyr)

# create objects for analysis----------------

    # define inside/outside of the area of interest
    cl.allArea <- cl.all2 %>%
      mutate(CS = case_when(
        CLstatisticalRectangle %in% area ~ "inside",
        .default = "outside"
      ))

#    cl.allArea <- cl.allArea %>%
#      mutate(IntraCS = case_when(
#        CLarea %in% NorthernPart & CS == "inside" ~ "NothernPart",
#        CLarea %in% CSArea & CS == "inside" ~ "CS",
#        CLarea %in% IS & CS == "inside" ~ "IS",
#        .default = "outside"
#      ))

#    table(cl.allArea$CS, cl.allArea$IntraCS)    

   cl.allArea <- cl.allArea %>%
      mutate(IntraCS = case_when(
        CLarea %in% BoB & CS == "inside" ~ "BoB",
        CLarea %in% Iberian & CS == "inside" ~ "Iberian",
        .default = "outside"
      ))

    table(cl.allArea$CS, cl.allArea$IntraCS)    

 cl.allArea %>% filter(CS=="inside" & IntraCS =="outside") %>% distinct(CLstatisticalRectangle)

 cl.allArea <- cl.allArea %>% 
      mutate(IntraCS = case_when(
        CLstatisticalRectangle %in% c("15E8", "20E6") & CS == "inside" ~ "inside",
        .default = "outside"
      ))

    table(cl.allArea$CS, cl.allArea$IntraCS)    

    # 
    vesselActivity <- cl.allArea %>%
      group_by(CLyear, CLencryptedVesselIds, CS, CLvesselFlagCountry, CLvesselLengthCategory, CLfishingTechnique) %>%
      summarise(weight = sum(CLscientificWeight_perVessel, na.rm = T), 
                value = sum(CLlandingsValue_perVessel, na.rm = T),
                cumSumWgt = cumsum(weight), 
                cumSumValue = cumsum(value)) %>%
      group_by(CLyear, CLencryptedVesselIds) %>%
      arrange(CLyear, CLencryptedVesselIds, desc(weight)) %>%
      group_by(CLyear, CLencryptedVesselIds) %>%
      mutate(wtTot = sum(weight, na.rm = T), 
             valTot = sum(value, na.rm = T), 
             percWt = round(weight / wtTot *100), 
             percVal = round(value / valTot *100))
    
    
    
    MainSppKG <- cl.allArea %>%
      filter(CS == "inside" & CLspeciesFaoCode!="NULL") %>%
      group_by(CLyear,CLvesselFlagCountry, CLspeciesFaoCode) %>%
      summarise(weight = sum(CLscientificWeight_perVessel, na.rm = T), 
                value = sum(CLlandingsValue_perVessel, na.rm = T))%>%
      arrange(desc(CLyear), desc(CLvesselFlagCountry), desc(weight)) %>%
      slice(1:10)
    MainSppEURO <- cl.allArea %>%
      filter(CS == "inside"& CLspeciesFaoCode!="NULL") %>%
      group_by(CLyear,CLvesselFlagCountry, CLspeciesFaoCode) %>%
      summarise(weight = sum(CLscientificWeight_perVessel, na.rm = T), 
                value = sum(CLlandingsValue_perVessel, na.rm = T))%>%
      arrange(desc(CLyear), desc(CLvesselFlagCountry), desc(value)) %>%
      slice(1:10)
    

#### Fishing activity should be unique per vessel!!! Check    
    
    testUniqueFT <- cl.all2 %>%
      group_by(CLyear,CLvesselFlagCountry, CLencryptedVesselIds) %>%
      summarise(nFT = n_distinct(CLfishingTechnique))
    testUniqueFT %>% filter(nFT>1) 
    testUniqueFT %>% filter(nFT>1) %>% distinct(CLvesselFlagCountry) 

    testUniqueFT %>% filter(nFT>1 & CLyear== 2024)
    testUniqueFT %>% filter(nFT>1 ) %>% distinct(CLvesselFlagCountry)

##### main species per country and vessel size
    MainSppVS <- cl.allArea %>%
      filter(CS == "inside" & CLspeciesFaoCode!="NULL") %>%
      group_by(CLyear,CLvesselFlagCountry, CLvesselLengthCategory, CLspeciesFaoCode) %>%
      summarise(weight = sum(CLscientificWeight_perVessel, na.rm = T), 
                value = sum(CLlandingsValue_perVessel, na.rm = T))%>%
      arrange(desc(CLyear), desc(CLvesselFlagCountry), desc(CLvesselLengthCategory), desc(weight)) %>%
      slice(1:10)
    



###### explore by sub area
### sum landings by country and sub area

    Landings_sub_area <- cl.allArea %>%
      filter(CS == "inside" & CLspeciesFaoCode!="NULL") %>%
      group_by(CLyear,CLvesselFlagCountry, IntraCS) %>%
      summarise(weight = sum(CLscientificWeight_perVessel, na.rm = T), 
                value = sum(CLlandingsValue_perVessel, na.rm = T))%>%

      group_by(CLvesselFlagCountry, IntraCS)%>%
      mutate(
                year_min = min(CLyear),
                year_max = max(CLyear),
                N_ymin = weight[CLyear == year_min][1],
                N_ymax = weight[CLyear == year_max][1],

   
              perc_changeWeight = 100 * (weight-N_ymin) / N_ymin
          ) %>%      
        arrange(desc(CLvesselFlagCountry), IntraCS,(CLyear))  %>%
       select(- c( N_ymin, N_ymax, year_min, year_max)) 

###### explore by sub area
### sum landings by species and sub area
    Landings_sub_area_spp <- cl.allArea %>%
      filter(CS == "inside" & CLspeciesFaoCode!="NULL") %>%
      group_by(CLyear,IntraCS, CLspeciesFaoCode) %>%
      summarise(weight = sum(CLscientificWeight_perVessel, na.rm = T), 
                value = sum(CLlandingsValue_perVessel, na.rm = T))%>%

      group_by(CLspeciesFaoCode, IntraCS)%>%
      mutate(
                year_min = min(CLyear),
                year_max = max(CLyear),
                N_ymin = weight[CLyear == year_min][1],
                N_ymax = weight[CLyear == year_max][1],

   
              perc_changeWeight = 100 * (weight-N_ymin) / N_ymin
          ) %>%      
        arrange(desc(CLspeciesFaoCode), IntraCS,(CLyear))  %>%
       select(- c( N_ymin, N_ymax, year_min, year_max)) 

test_mac <- Landings_sub_area_spp  %>% filter(CLyear == 2024 & CLspeciesFaoCode == "MAC")
test_whb <- Landings_sub_area_spp  %>% filter(CLyear == 2024 & CLspeciesFaoCode == "WHB")
### sum and conversion from kg to tonnes
sum(test_mac$weight)/1000
sum(test_whb$weight)/1000
###### vessel with several FT
    cl.allArea %>% filter(CLencryptedVesselIds %in% c("KjuH6h", "bqx6GU") ) %>% distinct(CLfishingTechnique)
###### 

cl.all2 %>%
      group_by(CLyear,CLvesselFlagCountry) %>%
      summarise(nFT = n_distinct(CLfishingTechnique)) %>%
        filter(CLyear==2024) 

## !!! some countries juste have one FT! 


  
    nbreVesselTot <- vesselActivity %>% 
      filter(percWt > 0) %>%
      group_by(CLyear, CLvesselFlagCountry) %>%
      summarise(nbreVessels = n_distinct(CLencryptedVesselIds))
    
nbreVesselTot %>% filter(CLyear==2024)

## !!! BE encrypted vessel is not a vessel name but combinaison of variables!! IE 2021 4 11 27.7.g 31E4 UK WHG HuC  IECOB VL2440 DTS 100D110

BE <- cl.allArea %>% filter(CLvesselFlagCountry=="BE" & CLyear==2021) 
BE %>% summarise(n_distinct(CLencryptedVesselIds))
dim(BE)


    
    nbreVesselInArea_all <- vesselActivity %>% 
      filter(CS == "inside" & percWt > 0) %>%
      group_by(CLyear, CLvesselFlagCountry) %>%
      summarise(nbreVessels = n_distinct(CLencryptedVesselIds))
    
    nbreVesselInAreaFT_all <- vesselActivity %>% 
      filter(CS == "inside" & percWt > 0) %>%
      group_by(CLyear, CLvesselFlagCountry, CLfishingTechnique) %>%
      summarise(nbreVessels = n_distinct(CLencryptedVesselIds))
    
    nbreVessel80PercInArea_all <- vesselActivity %>% 
      filter(CS == "inside" & percWt > percActivity) %>%
      group_by(CLyear, CLvesselFlagCountry) %>%
      summarise(nbreVessels = n_distinct(CLencryptedVesselIds))

    nbreVesselInAreaVS_all <- vesselActivity %>% 
      filter(CS == "inside" & percWt > percActivity) %>%
      group_by(CLyear, CLvesselFlagCountry, CLvesselLengthCategory) %>%
      summarise(nbreVessels = n_distinct(CLencryptedVesselIds))
    

    nbreVesselFT_all <- vesselActivity %>% 
      filter(CS == "inside" & percWt > percActivity) %>%
      group_by(CLyear, CLvesselFlagCountry, CLfishingTechnique) %>%
      summarise(nbreVessels = n_distinct(CLencryptedVesselIds))
      
    nbreVesselInArea <- nbreVesselInArea_all %>% filter(CLyear==2024)
    nbreVesselInAreaFT <- nbreVesselInAreaFT_all %>% filter(CLyear==2024)
    nbreVessel80PercInArea <- nbreVessel80PercInArea_all %>% filter(CLyear==2024)
    nbreVesselInAreaVS <- nbreVesselInAreaVS_all %>% filter(CLyear==2024)
    nbreVesselFT <- nbreVesselFT_all %>% filter(CLyear==2024)

    nbreVesselInArea_all <- nbreVesselInArea_all %>% 
          rename(N_Vessel_Total = nbreVessels)

    nbreVessel80PercInArea_all <- nbreVessel80PercInArea_all %>% 
          rename(N_Vessel_80 = nbreVessels)

    nVessel <- full_join(nbreVesselInArea_all, nbreVessel80PercInArea_all, 
      by=c("CLyear", "CLvesselFlagCountry")) %>%
      arrange(desc(CLvesselFlagCountry), (CLyear)) %>%
      group_by(CLvesselFlagCountry) %>%
          mutate(
                year_min = min(CLyear),
                year_max = max(CLyear),
                N_ymin = N_Vessel_Total[CLyear == year_min][1],
                N_ymax = N_Vessel_Total[CLyear == year_max][1],

              vessel_min = min(N_Vessel_Total),
              vessel_max = max(N_Vessel_Total),
              perc_changeTotal = 100 * (N_Vessel_Total-N_ymin) / N_ymin
          ) %>%
          mutate(
                year_min = min(CLyear),
                year_max = max(CLyear),
                N_ymin = N_Vessel_80[CLyear == year_min][1],
                N_ymax = N_Vessel_80[CLyear == year_max][1],

              vessel_min = min(N_Vessel_80),
              vessel_max = max(N_Vessel_80),
              perc_change80 = 100 * (N_Vessel_80-N_ymin) / vessel_max
          ) %>%
        select(- c(vessel_min, vessel_max, N_ymin, N_ymax, year_min, year_max)) 



    nbreVesselInAreaVS_all <- nbreVesselInAreaVS_all %>%
      arrange(desc(CLvesselFlagCountry), desc(CLvesselLengthCategory), (CLyear)) %>%
      group_by(CLvesselFlagCountry, CLvesselLengthCategory) %>%
          mutate(
                year_min = min(CLyear),
                year_max = max(CLyear),
                N_ymin = nbreVessels[CLyear == year_min][1],
                N_ymax = nbreVessels[CLyear == year_max][1],

              vessel_min = min(nbreVessels),
              vessel_max = max(nbreVessels),
              perc_changeTotal = 100 * (nbreVessels-N_ymin) / N_ymin
          )%>%
          select(- c(vessel_min, vessel_max, N_ymin, N_ymax, year_min, year_max)) 

    nbreVesselFT_all <- nbreVesselFT_all %>%
      arrange(desc(CLvesselFlagCountry), desc(CLfishingTechnique), (CLyear)) %>%
      group_by(CLvesselFlagCountry, CLfishingTechnique) %>%
          mutate(year_min = min(CLyear),
                year_max = max(CLyear),
                N_ymin = nbreVessels[CLyear == year_min][1],
                N_ymax = nbreVessels[CLyear == year_max][1],

              vessel_min = min(nbreVessels),
              vessel_max = max(nbreVessels),
              perc_changeTotal = 100 * (nbreVessels-N_ymin) / N_ymin
          )%>%
            select(- c(vessel_min, vessel_max, N_ymin, N_ymax, year_min, year_max)) 


    # write texte---------------  
      
    #  render( input="parent.rmd", output_file=paste0("dataExplorationInTables.html") )
    
      render( input="TextForFOFromRDBES.rmd", output_file=paste0("TextCountryFormRDBES.html") )
    
    
    