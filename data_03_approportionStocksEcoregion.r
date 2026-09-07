
Ecoregion = "Bay of Biscay and the Iberian Coast"

areaAll <- get_csquare(ecoregion = Ecoregion, convert2sf = TRUE)
area <- unique(areaAll$stat_rec)


### load extended RDBES 
load("./data/extendedRDBES.RData")

dat <- cl.all2 %>% 
    summarise(.by = c(CLyear, CLarea, CLstatisticalRectangle,
    CLspeciesFaoCode), 
    CLscientificWeight = sum(CLscientificWeight, na.rm = TRUE))

dat$CLdiv <- sapply(strsplit(dat$CLarea, ".", fixed = TRUE), \(z) paste(z[1:2], collapse = "."))


### import stock referential from IFREMER. Should use ICES ref but only working 
### for stocks that are not defined using stat rectangles. The IFREMER referential is more complete.
refStk <- read_excel("./Boot/export_referentiel_stocks-scientifiques_20250327.xlsx") 
            FISHERY_ORGP_LABEL, 
            REG_AREA_TRANS_ZONE_FAO, 
            REG_AREA_LOCATION_LEVEL_FK) %>%
    filter(!is.na(FISHERY_ORGP_LABEL)) 
refStk1 <- refStk %>% filter(REG_AREA_LOCATION_LEVEL_FK == 110)
refStk2 <- refStk %>% filter(REG_AREA_LOCATION_LEVEL_FK == 111)
refStk3 <- refStk %>% filter(REG_AREA_LOCATION_LEVEL_FK == 113)


test <- dat %>% 
    left_join(refStk1, by = c("CLspeciesFaoCode", 
    "CLdiv" = "REG_AREA_TRANS_ZONE_FAO")) %>%
    select(-REG_AREA_LOCATION_LEVEL_FK)

sum(dat$CLscientificWeight)/sum(test$CLscientificWeight)

test <- test %>% 
    left_join(refStk2, by = c("CLspeciesFaoCode", 
    "CLarea" = "REG_AREA_TRANS_ZONE_FAO"))  %>%
    select(-REG_AREA_LOCATION_LEVEL_FK)
sum(dat$CLscientificWeight)/sum(test$CLscientificWeight)

test <- test %>% 
    left_join(refStk3, by = c("CLspeciesFaoCode",
    "CLstatisticalRectangle" = "REG_AREA_TRANS_ZONE_FAO")) %>%
    select(-REG_AREA_LOCATION_LEVEL_FK)
sum(dat$CLscientificWeight)/sum(test$CLscientificWeight)


test <- test %>%
    rename(fishstock = FISHERY_ORGP_LABEL.x) %>%
    mutate(fishstock = case_when(
        !is.na(fishstock) ~ fishstock,
        is.na(fishstock) & !is.na(FISHERY_ORGP_LABEL.y) ~ FISHERY_ORGP_LABEL.y,
        is.na(fishstock) & !is.na(FISHERY_ORGP_LABEL) ~ FISHERY_ORGP_LABEL,
        .default = NA_character_
    )) 

sum(dat$CLscientificWeight)/sum(test$CLscientificWeight)




    # define inside/outside of the area of interest
    test <- test %>%
      mutate(CS = case_when(
        CLstatisticalRectangle %in% area ~ "inside",
        .default = "outside"
      ))


test2 <- test %>%
    group_by(CLyear, fishstock, CS) %>%
    summarise(Landings = sum(CLscientificWeight, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(CLyear, fishstock) %>%
    mutate(TotalLandings = sum(Landings, na.rm = TRUE),
        ShareLandingsInEcoregion = (Landings/TotalLandings)*100) %>%
    ungroup()


test2 %>% filter(CLyear == 2024 & fishstock == "mac.27.nea") 

write.csv(test2, file = "./output/data_03_shareLandingsInEcoregion.csv", row.names = FALSE)

ggplot(test2 %>% filter(fishstock == "mac.27.nea"), 
aes(x = CLyear, y = ShareLandingsInEcoregion, color = CS)) +
    geom_line() +
    geom_point() +
    labs(title = "Share of Landings in Ecoregion for mac.27.nea",
         x = "Year",
         y = "Share of Landings (%)") +
    theme_minimal()

