


# Import data---------------------------
RDBESwd <- "~/F/D/data/RDBES_2026_MIXFISH_FO/"

sspCorresp <- data.table::fread(paste0("./bootstrap/data/", "RDBES_especes_fao.csv")) %>% 
  select(ESP_COD_FAO, CLspeciesCode = ICES_APHIA) 

### !! some duplicated CLspeciesCode
sspCorresp %>% group_by(CLspeciesCode) %>% summarise(n= n()) %>% filter(n>1)
### fix, take the first one not to duplicate cl latter
sspCorresp <- sspCorresp %>% distinct(CLspeciesCode, .keep_all = TRUE)


cl.all <- data.table::fread(file = paste0(RDBESwd, "/RDBES Landings CL 2021 2025/CommercialLanding.csv"), quote = "")

unique(cl.all$CLyear)

ce.all <- read.csv(file = paste0(RDBESwd, "/RDBES Effort CE 2021 2025/CommercialEffort.csv"), quote = "")
dim(cl.all); dim(ce.all)


### add CLspeciesFaoCode when missing in cl

cl.all <- left_join(cl.all, sspCorresp)

cl.all <- cl.all %>%
  mutate(CLspeciesFaoCode = case_when(
    CLspeciesFaoCode=="" ~ ESP_COD_FAO,
    TRUE ~ CLspeciesFaoCode
  ))



# create a database splitting lines where several vessels have been input in one strata. The hypothesis here that we divide the values (landins and effort) by the number of vessels

## cl--------------
cl.all <- cl.all %>%
  mutate(CLlandingsValue = as.numeric(CLlandingsValue))


specials <- str_extract_all(cl.all$CLencryptedVesselIds, "[^A-Za-z0-9]")
# Pour obtenir tous les caractères spéciaux uniques :
unique(unlist(specials))



#### differnet special characters per countries...
## FI EI LT DE GB-ENG GB-WLS GB-NIR EE SE FR PL NL DK PT are using ";" as a separator for CLencryptedVesselIds
## ES uses "/" as a separator for CLencryptedVesselIds
## LV uses "-" as a separator for CLencryptedVesselIds


cl.all2 <- cl.all  %>%
  mutate(numVessel = str_count(CLencryptedVesselIds, ";|/|-")) %>%
  mutate(numVessel = case_when(
    numVessel > 0 ~ numVessel + 1,
    .default = 1
  )) %>%
  mutate(
    CLscientificWeight_perVessel = CLscientificWeight / numVessel, 
    CLlandingsValue_perVessel = CLlandingsValue / numVessel
  ) %>%
  mutate(CLencryptedVesselIds = strsplit(CLencryptedVesselIds, ";|/|-")) %>%
  unnest(CLencryptedVesselIds)

# checks
dim(cl.all); dim(cl.all2)
cl.all  %>% summarise((sum(CLlandingsValue, na.rm=T))) / cl.all2 %>% summarise((sum(CLlandingsValue_perVessel, na.rm=T)))



save(cl.all2, file = "./data/extendedRDBES.RData")




## to do!! same for CE but have CEnumberOfUniqueVessels
#ce.all2 <- ce.all  %>%
#  mutate(numVessel = str_count(CLencryptedVesselIds, ";|/|-")) %>%
#  mutate(numVessel = case_when(
#    numVessel > 0 ~ numVessel + 1,
#    .default = 1
#  )) %>%
#  mutate(
#    CLscientificWeight_perVessel = CLscientificWeight / numVessel, 
#    CLlandingsValue_perVessel = CLlandingsValue / numVessel
#  ) %>%
#  mutate(CLencryptedVesselIds = strsplit(CLencryptedVesselIds, ";|/|-")) %>%
# unnest(CLencryptedVesselIds)



#save(cl.all2, file = "./data/extendedRDBES.RData")
