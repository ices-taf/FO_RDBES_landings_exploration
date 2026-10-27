library(icesVMS)
library(dplyr)
Ecoregion = "Bay of Biscay and the Iberian Coast"

areaAll <- get_csquare(ecoregion = Ecoregion, convert2sf = TRUE)
area <- unique(areaAll$stat_rec)
# plot(areaAll)


### load extended RDBES 
load("./data/extendedRDBES.RData")
head(cl.all2) %>% print(width = Inf)
dat <- cl.all2 %>% 
    summarise(.by = c(CLyear, CLarea, CLstatisticalRectangle,
    CLspeciesFaoCode), 
    CLscientificWeight = sum(CLscientificWeight, na.rm = TRUE))

dat$CLdiv <- sapply(strsplit(dat$CLarea, ".", fixed = TRUE), \(z) paste(z[1:2], collapse = "."))
unique(dat$CLdiv)

### import stock referential from IFREMER. Should use ICES ref but only working 
### for stocks that are not defined using stat rectangles. The IFREMER referential is more complete.
refStk <- readxl::read_excel("./Boot/export_referentiel_stocks-scientifiques_20250327.xlsx") 
head(refStk) %>% print(width = Inf)
refStk <- refStk %>%
    select(FISHERY_ORGP_LABEL, 
            REG_AREA_TRANS_ZONE_FAO, 
            REG_AREA_LOCATION_LEVEL_FK,
            TAXON_GROUP_LABEL) %>%
    filter(!is.na(FISHERY_ORGP_LABEL)) 
refStk1 <- refStk %>% filter(REG_AREA_LOCATION_LEVEL_FK == 110)
refStk2 <- refStk %>% filter(REG_AREA_LOCATION_LEVEL_FK == 111)
refStk3 <- refStk %>% filter(REG_AREA_LOCATION_LEVEL_FK == 113)


unique(refStk1$REG_AREA_TRANS_ZONE_FAO)
unique(refStk2$REG_AREA_TRANS_ZONE_FAO)
unique(refStk3$REG_AREA_TRANS_ZONE_FAO)
unique(dat$CLdiv)
test <- dat %>% 
    left_join(refStk1, by = c("CLspeciesFaoCode" = "TAXON_GROUP_LABEL",
    "CLdiv" = "REG_AREA_TRANS_ZONE_FAO")) %>%
    select(-REG_AREA_LOCATION_LEVEL_FK)

sum(dat$CLscientificWeight)/sum(test$CLscientificWeight)

test <- test %>% 
    left_join(refStk2, by = c("CLspeciesFaoCode" = "TAXON_GROUP_LABEL",
    "CLarea" = "REG_AREA_TRANS_ZONE_FAO"))  %>%
    select(-REG_AREA_LOCATION_LEVEL_FK)
sum(dat$CLscientificWeight)/sum(test$CLscientificWeight)

test <- test %>% 
    left_join(refStk3, by = c("CLspeciesFaoCode" = "TAXON_GROUP_LABEL",
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


test_mac <- test2 %>% filter(CLyear == 2024 & fishstock == "mac.27.nea" & CS == "inside") 
test_whb <- test2 %>% filter(CLyear == 2024 & fishstock == "whb.27.1-91214" & CS == "inside") 
### sum and conversion from kg to tonnes for mac and whb
sum(test_mac$Landings)/1000
sum(test_whb$Landings)/1000


write.csv(test2, file = "./output/data_03_shareLandingsInEcoregion.csv", row.names = FALSE)
library(ggplot2)
ggplot(test2 %>% filter(fishstock == "mac.27.nea"), 
aes(x = CLyear, y = ShareLandingsInEcoregion, color = CS)) +
    geom_line() +
    geom_point() +
    labs(title = "Share of Landings in Ecoregion for mac.27.nea",
         x = "Year",
         y = "Share of Landings (%)") +
    theme_minimal()









###### checks for nephorsp
library(sf)
library(dplyr)

nephrops_fu <- st_read(
  "./Boot/ICES_nephrops_FU/Nephrops Functional Units.shp",
  quiet = TRUE
)

# Inspect the functional-unit labels
nephrops_fu |>
  st_drop_geometry() |>
  select(fu_no)

# Check the coordinate reference system
st_crs(nephrops_fu)





############################ testing alternative below ########
library(dplyr)
library(icesVocab)
detail <- getCodeDetail("ICES_StockCode", code = "nep.fu.31")
area <- detail$children %>% filter(Key == "ICES_Area")
detail$children
getCodeList("ICES_Area", code = "27.8.c")

library(icesVocab)
library(dplyr)

stock_area_lookup <- getCodeTypeRelation(
  "ICES_StockCode",
  "ICES_Area"
)

stock_area_lookup |>
  filter(
    ICES_StockCode %in% c("nep.fu.25", "nep.fu.31")
  ) |>
  distinct(ICES_StockCode, ICES_Area)


library(icesSD)
sid <- getSD(year = 2026)
sid <- sid %>% select(StockKeyLabel,SpeciesScientificName,SpeciesCommonName)
names(sid)
# stock_codes <- c("nep.fu.31", "cod.27.7e-k", "had.27.46a20")

areas_by_stock <- lapply(sid$StockKeyLabel, function(code) {
  d <- getCodeDetail("ICES_StockCode", code = code)

  tibble(
    StockKeyLabel = code,
    ICES_Area = d$children$codes$Key[d$children$code_types$Key == "ICES_Area"]
  )
}) %>%
  bind_rows()

areas_by_stock



library(icesVocab)
library(icesSD)
library(dplyr)
library(tidyr)

# Stock -> ICES geographical area
stock_areas <- getCodeTypeRelation(
  "ICES_StockCode",
  "ICES_Area"
)

# Stock -> species (WoRMS AphiaID)
stock_species <- getCodeTypeRelation(
  "ICES_StockCode",
  "SpecWoRMS"
)

# Combine both relationships
stock_lookup <- stock_species %>%
  inner_join(
    stock_areas,
    by = "ICES_StockCode",
    relationship = "many-to-many"
  ) %>%
  transmute(
    fishstock = ICES_StockCode,
    CLspeciesCode = as.character(SpecWoRMS),
    ICES_Area = as.character(ICES_Area)
  ) %>%
  distinct()

head(stock_lookup)



dat_alt <- cl.all2 %>%
  summarise(
    CLscientificWeight = sum(CLscientificWeight, na.rm = TRUE),
    .by = c(
      CLyear,
      CLarea,
      CLstatisticalRectangle,
      CLspeciesCode
    )
  ) %>%
  mutate(
    CLspeciesCode = as.character(CLspeciesCode),
    row_id = row_number()
  )


area_hierarchy <- dat_alt %>%
  distinct(CLarea) %>%
  filter(!is.na(CLarea), nzchar(CLarea)) %>%
  mutate(
    ICES_Area = lapply(CLarea, function(x) {
      parts <- strsplit(x, ".", fixed = TRUE)[[1]]

      vapply(
        seq_along(parts),
        function(i) paste(parts[seq_len(i)], collapse = "."),
        character(1)
      )
    })
  ) %>%
  unnest(ICES_Area)

head(area_hierarchy)


# Build a mapping for the geographical areas
# appearing in the RDBES dataset

area_stock_mapping <- area_hierarchy %>%
  inner_join(
    stock_lookup,
    by = "ICES_Area",
    relationship = "many-to-many"
  ) %>%
  distinct(CLarea, CLspeciesCode, fishstock)


# Find candidate stocks for each RDBES row

matches <- dat_alt %>%
  select(row_id, CLarea, CLspeciesCode) %>%
  inner_join(
    area_stock_mapping,
    by = c("CLarea", "CLspeciesCode"),
    relationship = "many-to-many"
  ) %>%
  group_by(row_id) %>%
  summarise(
    n_stocks = n_distinct(fishstock),

    fishstock = if (n_distinct(fishstock) == 1) {
      first(fishstock)
    } else {
      NA_character_
    },

    candidates = paste(
      sort(unique(fishstock)),
      collapse = "; "
    ),

    .groups = "drop"
  )


# Add the results back to the original RDBES data

dat_matched <- dat_alt %>%
  left_join(matches, by = "row_id") %>%
  mutate(
    n_stocks = coalesce(n_stocks, 0L),

    match_status = case_when(
      n_stocks == 0 ~ "unmatched",
      n_stocks == 1 ~ "unique",
      n_stocks > 1  ~ "ambiguous"
    )
  )


match_quality <- dat_matched %>%
  group_by(match_status) %>%
  summarise(
    n_records = n(),
    Landings_kg = sum(CLscientificWeight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Share_landings = 100 * Landings_kg / sum(Landings_kg)
  )

print(match_quality)

dat_matched %>%
  filter(match_status == "ambiguous") %>%
  select(
    CLyear,
    CLarea,
    CLspeciesCode,
    CLscientificWeight,
    candidates
  ) %>%
  arrange(desc(CLscientificWeight)) %>%
  head(20)

unique(dat$CLarea)

area_coverage <- tibble(
  CLarea = sort(unique(dat$CLarea))
) %>%
  mutate(
    has_exact_relation = CLarea %in% stock_lookup$ICES_Area
  )

print(area_coverage, n = Inf)


exact_candidates <- dat_alt %>%
  select(row_id, CLspeciesCode, CLarea) %>%
  inner_join(
    stock_lookup,
    by = c(
      "CLspeciesCode",
      "CLarea" = "ICES_Area"
    ),
    relationship = "many-to-many"
  ) %>%
  distinct(row_id, fishstock)

exact_quality <- dat_alt %>%
  left_join(
    exact_candidates %>%
      count(row_id, name = "n_stocks"),
    by = "row_id"
  ) %>%
  mutate(
    n_stocks = coalesce(n_stocks, 0L),
    match_status = case_when(
      n_stocks == 0 ~ "unmatched",
      n_stocks == 1 ~ "unique",
      n_stocks > 1 ~ "ambiguous"
    )
  ) %>%
  summarise(
    n_records = n(),
    Landings_kg = sum(CLscientificWeight, na.rm = TRUE),
    .by = match_status
  ) %>%
  mutate(
    Share_landings = 100 * Landings_kg / sum(Landings_kg)
  )

print(exact_quality)


library(icesSD)
library(dplyr)

# Retrieve stock codes active in 2024
active_2024 <- getSD(year = 2024) %>%
  distinct(fishstock = StockKeyLabel)

# Retain exact matches associated with active stocks
exact_candidates_2024 <- exact_candidates %>%
  semi_join(active_2024, by = "fishstock")


compare_quality <- function(candidates) {

  dat_alt %>%
    filter(CLyear == 2024) %>%
    left_join(
      candidates %>%
        count(row_id, name = "n_stocks"),
      by = "row_id"
    ) %>%
    mutate(
      n_stocks = coalesce(n_stocks, 0L),

      match_status = case_when(
        n_stocks == 0 ~ "unmatched",
        n_stocks == 1 ~ "unique",
        n_stocks > 1 ~ "ambiguous"
      )
    ) %>%
    summarise(
      n_records = n(),
      Landings_kg = sum(CLscientificWeight, na.rm = TRUE),
      .by = match_status
    ) %>%
    mutate(
      Share_landings =
        100 * Landings_kg / sum(Landings_kg)
    )
}

# Original exact matching
compare_quality(exact_candidates)

# Exact matching with active stocks only
compare_quality(exact_candidates_2024)


ambiguous_details <- exact_candidates_2024 %>%
  add_count(row_id, name = "n_stocks") %>%
  filter(n_stocks > 1) %>%
  group_by(row_id) %>%
  summarise(
    candidate_stocks = paste(
      sort(unique(fishstock)),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  inner_join(
    dat_alt %>%
      filter(CLyear == 2024),
    by = "row_id"
  ) %>%
  group_by(
    CLspeciesCode,
    CLarea,
    candidate_stocks
  ) %>%
  summarise(
    Landings_tonnes =
      sum(CLscientificWeight, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  arrange(desc(Landings_tonnes))

print(ambiguous_details, n = 20, width = Inf)


target_stocks <- c("mac.27.nea", "whb.27.1-91214")

# Inspect the official ICES areas assigned to these stocks
stock_lookup %>%
  filter(fishstock %in% target_stocks) %>%
  distinct(fishstock, ICES_Area) %>%
  arrange(fishstock, ICES_Area) %>%
  print()


target_matches <- exact_candidates_2024 %>%
  add_count(row_id, name = "n_stocks") %>%
  filter(fishstock %in% target_stocks) %>%
  inner_join(
    dat_alt %>%
      filter(CLyear == 2024) %>%
      select(
        row_id,
        CLarea,
        CLstatisticalRectangle,
        CLscientificWeight
      ),
    by = "row_id"
  ) %>%
  mutate(
    match_status = if_else(
      n_stocks == 1,
      "unique",
      "ambiguous"
    )
  )

target_matches %>%
  summarise(
    n_records = n(),
    Landings_tonnes = sum(CLscientificWeight, na.rm = TRUE) / 1000,
    .by = c(fishstock, match_status, CLarea)
  ) %>%
  arrange(fishstock, CLarea) %>%
  print(n = Inf)
