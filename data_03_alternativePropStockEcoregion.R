# ============================================================================
# RDBES species-level landings -> ICES stock codes -> ecoregion landings
#
# Current version: ICES Vocabulary + yearly ICES Stock Database + ICES GIS.
# Future extension: optional, VALIDATED rectangle/stock rules built from
#                   icesVMS C-squares (or other reliable spatial boundaries).
#
# IMPORTANT:
# * A stock can cover several ICES areas; areas can contain several stocks.
# * A stock whose listed area is FINER than the RDBES CLarea cannot be
#   assumed to cover the whole CLarea. Flag it for spatial refinement.
# * Never multiply the weight of an RDBES record by assigning it to
#   several stocks simultaneously.
# * Final shares refer only to landings that were uniquely stock-assigned
#   AND, for the ecoregion percentage, had usable rectangle information.
# ============================================================================

library(dplyr)
library(tidyr)
library(icesVocab)
library(icesSD)
library(httr)
library(jsonlite)

# ---- 0. Configuration ------------------------------------------------------
Ecoregion <- "Bay of Biscay and the Iberian Coast"
input_file <- "./data/extendedRDBES.RData"
output_dir <- "./output/stock_matching"
cache_dir <- "./data/ices_cache"

# Refresh = TRUE forces new calls to the ICES services rather than RDS cache.
refresh_reference <- FALSE

# This OPTIONAL table is NULL until a trusted C-square/stock mapping is built.
# To incorporate it later, use a table with one row per
# (CLarea, CLstatisticalRectangle, fishstock) and rectangle_status equal to:
#   "fully_inside" : the WHOLE rectangle belongs to that stock's spatial area
#   "outside"      : the rectangle does not overlap that stock's spatial area
#   "partial"      : there is overlap, but the record cannot be allocated
# A C-square intersection alone is NOT proof of fully_inside.
rectangle_rules <- NULL
# Example (AFTER independently generating and validating the rules):
# rectangle_rules <- readRDS("./data/validated_rectangle_stock_rules.rds")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

# Small utility: return a single distinct nonmissing string, otherwise NA.
one_or_na <- function(x) {
  x <- unique(stats::na.omit(as.character(x)))
  if (length(x) == 1L) x else NA_character_
}

# Read an ICES reference from a local cache, or download and save it.
cache_or_fetch <- function(path, fetch, refresh = FALSE) {
  if (!refresh && file.exists(path)) return(readRDS(path))
  x <- fetch()
  saveRDS(x, path)
  x
}

# ---- 1. Prepare and aggregate RDBES landings -------------------------------
# Keep the record's detailed ICES area AND rectangle. They are different
# geographical variables and we may need both to locate a stock.
load(input_file)  # This project file must create cl.all2.
stopifnot(exists("cl.all2"))

required <- c("CLyear", "CLarea", "CLstatisticalRectangle",
              "CLspeciesCode", "CLspeciesFaoCode", "CLscientificWeight")
stopifnot(all(required %in% names(cl.all2)))

# Check that we really use LANDINGS, not discards. In RDBES, Lan is the
# landing catch category. If this variable is absent, the input is assumed
# to have been prepared as a landings-only extract: verify that manually.
if ("CLcatchCategory" %in% names(cl.all2)) {
  message("RDBES catch categories in source:")
  print(table(cl.all2$CLcatchCategory, useNA = "ifany"))
  source_landings <- cl.all2 %>% filter(CLcatchCategory == "Lan")
  if (nrow(source_landings) == 0L) {
    stop("No CLcatchCategory == 'Lan' observations were found")
  }
} else {
  warning("CLcatchCategory is missing: assuming cl.all2 contains ",
          "landings only. Verify this before interpreting output.")
  source_landings <- cl.all2
}

missing_weights <- sum(is.na(source_landings$CLscientificWeight))
if (missing_weights > 0L) {
  warning(missing_weights, " landing rows have missing scientific weights; ",
          "they will be excluded rather than treated as zero.")
}

dat <- source_landings %>%
  filter(!is.na(CLscientificWeight)) %>%
  mutate(
    CLyear = as.integer(CLyear),
    CLarea = trimws(as.character(CLarea)),
    CLstatisticalRectangle = toupper(trimws(as.character(CLstatisticalRectangle))),
    CLspeciesCode = trimws(as.character(CLspeciesCode)),  # WoRMS AphiaID
    CLspeciesFaoCode = trimws(as.character(CLspeciesFaoCode))
  ) %>%
  summarise(
    CLscientificWeight = sum(CLscientificWeight),
    .by = c(CLyear, CLarea, CLstatisticalRectangle,
            CLspeciesCode, CLspeciesFaoCode)
  ) %>%
  mutate(row_id = row_number())

stopifnot(!anyNA(dat$CLyear))

# ---- 2. Download stock-species and stock-area reference tables -------------
# ICES Vocabulary supplies the current relationships, not historical snapshots.
stock_species_raw <- cache_or_fetch(
  file.path(cache_dir, "stock_species.rds"),
  function() icesVocab::getCodeTypeRelation("ICES_StockCode", "SpecWoRMS"),
  refresh_reference
)
stock_areas_raw <- cache_or_fetch(
  file.path(cache_dir, "stock_areas.rds"),
  function() icesVocab::getCodeTypeRelation("ICES_StockCode", "ICES_Area"),
  refresh_reference
)

stopifnot(all(c("ICES_StockCode", "SpecWoRMS") %in% names(stock_species_raw)))
stopifnot(all(c("ICES_StockCode", "ICES_Area") %in% names(stock_areas_raw)))

stock_species <- stock_species_raw %>%
  transmute(fishstock = as.character(ICES_StockCode),
            CLspeciesCode = as.character(SpecWoRMS)) %>%
  filter(!is.na(fishstock), !is.na(CLspeciesCode)) %>%
  distinct()

stock_areas <- stock_areas_raw %>%
  transmute(fishstock = as.character(ICES_StockCode),
            ICES_Area = trimws(as.character(ICES_Area))) %>%
  filter(!is.na(fishstock), !is.na(ICES_Area), nzchar(ICES_Area)) %>%
  distinct()

# ---- 3. Download ACTIVE stock codes separately for each RDBES year --------
# Critical: a 2024 stock list should not simply be applied to 2020 landings.
get_active_year <- function(year) {
  path <- file.path(cache_dir, paste0("active_stocks_", year, ".rds"))
  sd <- cache_or_fetch(
    path,
    function() icesSD::getSD(year = year),
    refresh_reference
  )
  if (!"StockKeyLabel" %in% names(sd) || nrow(sd) == 0L) {
    stop("No usable ICES stock list returned for year ", year)
  }
  tibble(CLyear = as.integer(year), fishstock = as.character(sd$StockKeyLabel)) %>%
    filter(!is.na(fishstock), nzchar(fishstock)) %>%
    distinct()
}

active_stocks <- bind_rows(lapply(sort(unique(dat$CLyear)), get_active_year))

# ---- 4. Create the ICES-area hierarchy ------------------------------------
# Example:
# RDBES 27.7.c.2 vs ICES 27.7.c.2 -> exact
# RDBES 27.7.c.2 vs ICES 27.7.c   -> stock_area_broader (covers RDBES area)
# RDBES 27.7.c   vs ICES 27.7.c.2 -> stock_area_finer (MAY cover part only)
# Using a dot in startsWith() prevents 27.7.c.1 matching 27.7.c.10.
rdbes_areas <- dat %>%
  distinct(CLarea) %>%
  filter(!is.na(CLarea), nzchar(CLarea))

area_relations <- tidyr::crossing(
  CLarea = rdbes_areas$CLarea,
  ICES_Area = unique(stock_areas$ICES_Area)
) %>%
  mutate(
    area_relation = case_when(
      CLarea == ICES_Area ~ "exact",
      startsWith(CLarea, paste0(ICES_Area, ".")) ~ "stock_area_broader",
      startsWith(ICES_Area, paste0(CLarea, ".")) ~ "stock_area_finer",
      TRUE ~ "no_match"
    )
  ) %>%
  filter(area_relation != "no_match")

# ---- 5. Find candidate stocks using species, year and ICES area ------------
# Do the potentially many-to-many joins on DISTINCT keys first (usually far
# fewer rows than the landings data). A stock-area relation is a candidate,
# NOT yet evidence that the landing belongs uniquely to that stock.
stock_year_geography <- active_stocks %>%
  inner_join(stock_species, by = "fishstock", relationship = "many-to-many") %>%
  inner_join(stock_areas, by = "fishstock", relationship = "many-to-many") %>%
  inner_join(area_relations, by = "ICES_Area", relationship = "many-to-many") %>%
  select(CLyear, CLspeciesCode, CLarea, fishstock, area_relation)

key_candidates <- dat %>%
  distinct(CLyear, CLspeciesCode, CLarea) %>%
  inner_join(
    stock_year_geography,
    by = c("CLyear", "CLspeciesCode", "CLarea"),
    relationship = "many-to-many"
  ) %>%
  mutate(match_rank = case_when(
    area_relation == "exact" ~ 1L,
    area_relation == "stock_area_broader" ~ 2L,
    TRUE ~ 3L
  )) %>%
  group_by(CLyear, CLspeciesCode, CLarea, fishstock) %>%
  summarise(
    # One best geographic relation per stock, even if many ICES areas match.
    area_relation = c("exact", "stock_area_broader", "stock_area_finer")[
      min(match_rank)
    ],
    .groups = "drop"
  )

# Attach candidate stocks to individual, aggregated RDBES observations.
# This is where CLstatisticalRectangle will later distinguish finer areas.
candidates <- dat %>%
  select(row_id, CLyear, CLspeciesCode, CLarea, CLstatisticalRectangle) %>%
  inner_join(
    key_candidates,
    by = c("CLyear", "CLspeciesCode", "CLarea"),
    relationship = "many-to-many"
  )

# ---- 6. OPTIONAL C-square-derived refinement hook -------------------------
# This section already accepts future rectangle-level rules. It does NOT
# attempt to call the failing icesVMS service now.
#
# To generate rectangle_rules in future, use VMS C-squares to establish the
# complete relationship between rectangles and stock-area boundaries. Pay
# attention to VMS area-code format (e.g. "7k2") vs ICES_Vocab ("27.7.k.2").
# A rectangle crossing two stock areas is PARTIAL, not FULLY_INSIDE.
if (!is.null(rectangle_rules)) {
  rule_cols <- c("CLarea", "CLstatisticalRectangle", "fishstock",
                 "rectangle_status")
  stopifnot(all(rule_cols %in% names(rectangle_rules)))
  stopifnot(!anyDuplicated(rectangle_rules[rule_cols[1:3]]))
  stopifnot(all(rectangle_rules$rectangle_status %in%
                  c("fully_inside", "outside", "partial")))

  candidates <- candidates %>%
    left_join(
      rectangle_rules %>% select(all_of(rule_cols)),
      by = c("CLarea", "CLstatisticalRectangle", "fishstock"),
      relationship = "many-to-one"
    ) %>%
    # Exclude ONLY areas known to be entirely outside that stock.
    filter(is.na(rectangle_status) | rectangle_status != "outside") %>%
    mutate(match_evidence = if_else(
      rectangle_status == "fully_inside",
      "rectangle_full",
      area_relation,
      missing = area_relation
    ))
} else {
  candidates <- candidates %>%
    mutate(rectangle_status = NA_character_, match_evidence = area_relation)
}

# ---- 7. Resolve only UNIQUE, sufficiently supported stock candidates ------
# 'exact', 'stock_area_broader' and 'rectangle_full' are supported candidates.
# 'stock_area_finer' is possible but NOT a sufficient stand-alone assignment.
# Even one supported stock remains uncertain if another, unresolved finer
# stock could also apply to the same RDBES observation.
match_counts <- candidates %>%
  group_by(row_id) %>%
  summarise(
    n_supported = n_distinct(fishstock[match_evidence != "stock_area_finer"]),
    n_finer_possible = n_distinct(fishstock[match_evidence == "stock_area_finer"]),
    supported_stock = one_or_na(fishstock[match_evidence != "stock_area_finer"]),
    supported_method = one_or_na(match_evidence[match_evidence != "stock_area_finer"]),
    candidate_stocks = paste(sort(unique(fishstock)), collapse = "; "),
    .groups = "drop"
  )

dat_matched <- dat %>%
  left_join(match_counts, by = "row_id", relationship = "one-to-one") %>%
  mutate(
    n_supported = coalesce(n_supported, 0L),
    n_finer_possible = coalesce(n_finer_possible, 0L),
    match_status = case_when(
      n_supported > 1L ~ "ambiguous",
      n_supported == 1L & n_finer_possible > 0L ~ "possible_overlap",
      n_supported == 1L & n_finer_possible == 0L ~ "unique",
      n_supported == 0L & n_finer_possible > 0L ~ "needs_finer_geography",
      TRUE ~ "unmatched"
    ),
    # Only expose a stock code for defensible unique assignments.
    fishstock = if_else(match_status == "unique", supported_stock, NA_character_)
  )

# Fail loudly if a join ever duplicates or drops original RDBES weights.
stopifnot(nrow(dat_matched) == nrow(dat))
stopifnot(!anyDuplicated(dat_matched$row_id))
stopifnot(isTRUE(all.equal(
  sum(dat$CLscientificWeight), sum(dat_matched$CLscientificWeight),
  tolerance = 1e-10
)))

# ---- 8. Quality assessment for ALL stocks, by year ------------------------
match_quality <- dat_matched %>%
  summarise(
    n_records = n(),
    Landings_kg = sum(CLscientificWeight),
    .by = c(CLyear, match_status)
  ) %>%
  group_by(CLyear) %>%
  mutate(Share_landings_pct = 100 * Landings_kg / sum(Landings_kg)) %>%
  ungroup() %>%
  arrange(CLyear, match_status)

print(match_quality, n = Inf)

# Summarise unresolved records rather than counting them in multiple stocks.
unresolved <- dat_matched %>%
  filter(match_status != "unique") %>%
  summarise(
    n_records = n(),
    Landings_kg = sum(CLscientificWeight),
    .by = c(CLyear, CLspeciesCode, CLspeciesFaoCode, CLarea,
            match_status, candidate_stocks)
  ) %>%
  arrange(desc(Landings_kg))

# ---- 9. Ecoregion membership: OFFICIAL ICES GIS fallback ------------------
# This step DOES NOT use icesVMS. It downloads ICES rectangles that overlap
# the ecoregion. A rectangle touching the ecoregion is classed as 'inside',
# NOT weighted by area overlap. Revisit boundary rectangles when C-squares
# become available; landings are not necessarily uniform within a rectangle.
get_ecoregion_rectangles_gis <- function(ecoregion) {
  url <- paste0(
    "https://gis.ices.dk/gis/rest/services/",
    "Mapping_layers/ICES_Statrec_mapto_Ecoregions/MapServer/0/query"
  )
  offset <- 0L
  page_size <- 1000L
  pages <- list()

  repeat {
    res <- httr::GET(
      url,
      query = list(
        where = paste0("Ecoregion = '", gsub("'", "''", ecoregion,
                                                 fixed = TRUE), "'"),
        outFields = "OBJECTID_12,ICESNAME,Ecoregion,PERCENTAGE",
        returnGeometry = "false",
        orderByFields = "OBJECTID_12 ASC",
        resultOffset = offset,
        resultRecordCount = page_size,
        f = "json"
      ),
      httr::timeout(60)
    )
    httr::stop_for_status(res)

    response <- jsonlite::fromJSON(
      httr::content(res, "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
    if (!is.null(response$error)) stop(response$error$message)
    if (is.null(response$features)) stop("Unexpected ICES GIS response")

    n <- length(response$features)
    if (n == 0L) break
    pages[[length(pages) + 1L]] <- bind_rows(lapply(
      response$features, function(x) x$attributes
    ))
    offset <- offset + n

    if (n < page_size && !isTRUE(response$exceededTransferLimit)) break
  }

  out <- bind_rows(pages)
  if (nrow(out) == 0L || !"ICESNAME" %in% names(out)) {
    stop("No statistical rectangles returned for ecoregion: ", ecoregion)
  }
  out %>%
    transmute(
      CLstatisticalRectangle = toupper(trimws(as.character(ICESNAME))),
      Ecoregion = as.character(Ecoregion),
      PERCENTAGE = as.numeric(PERCENTAGE)
    ) %>%
    filter(!is.na(CLstatisticalRectangle),
           nzchar(CLstatisticalRectangle)) %>%
    distinct()
}

eco_cache <- file.path(
  cache_dir,
  paste0("ecoregion_rectangles_", gsub("[^[:alnum:]]+", "_", Ecoregion),
         ".rds")
)

eco_rect <- tryCatch(
  cache_or_fetch(
    eco_cache,
    function() get_ecoregion_rectangles_gis(Ecoregion),
    refresh_reference
  ),
  error = function(e) {
    warning("Ecoregion GIS lookup failed: ", conditionMessage(e),
            ". Stock matching remains available; ecoregion shares will ",
            "not be calculated.")
    tibble(CLstatisticalRectangle = character())
  }
)

eco_available <- nrow(eco_rect) > 0L
if (eco_available) {
  eco_ids <- unique(eco_rect$CLstatisticalRectangle)
  dat_matched <- dat_matched %>%
    mutate(CS = case_when(
      is.na(CLstatisticalRectangle) | !nzchar(CLstatisticalRectangle) ~ "unknown",
      CLstatisticalRectangle %in% eco_ids ~ "inside",
      TRUE ~ "outside"
    ))
} else {
  dat_matched <- dat_matched %>% mutate(CS = NA_character_)
}

# ---- 10. Stock-level totals and share inside the ecoregion -----------------
# Denominator = INSIDE + OUTSIDE among uniquely assigned, geolocated landings.
# Unknown-rectangle weights are reported separately, NOT silently discarded.
# This is NOT a claim of complete landings for the biological ICES stock.
if (eco_available) {
  stock_landings <- dat_matched %>%
    filter(match_status == "unique") %>%
    summarise(
      Inside_kg = sum(CLscientificWeight[CS == "inside"]),
      Outside_kg = sum(CLscientificWeight[CS == "outside"]),
      Unknown_rectangle_kg = sum(CLscientificWeight[CS == "unknown"]),
      n_RDBES_rows = n(),
      .by = c(CLyear, fishstock)
    ) %>%
    mutate(
      Geolocated_kg = Inside_kg + Outside_kg,
      Total_uniquely_assigned_kg = Geolocated_kg + Unknown_rectangle_kg,
      ShareLandingsInEcoregion = if_else(
        Geolocated_kg > 0, 100 * Inside_kg / Geolocated_kg, NA_real_
      ),
      Geo_coverage_pct = if_else(
        Total_uniquely_assigned_kg > 0,
        100 * Geolocated_kg / Total_uniquely_assigned_kg,
        NA_real_
      ),
      Inside_tonnes = Inside_kg / 1000,
      Outside_tonnes = Outside_kg / 1000
    ) %>%
    arrange(CLyear, fishstock)
} else {
  stock_landings <- tibble()
}

# ---- 11. Save both results AND audit trails -------------------------------
# RDS preserves column types and candidate lists; CSV is convenient to inspect.
saveRDS(dat_matched, file.path(output_dir, "rdbes_stock_assignments.rds"))
saveRDS(candidates, file.path(output_dir, "stock_candidates.rds"))
write.csv(match_quality, file.path(output_dir, "matching_quality_by_year.csv"),
          row.names = FALSE)
write.csv(unresolved, file.path(output_dir, "unresolved_landings.csv"),
          row.names = FALSE)
if (eco_available) {
  write.csv(stock_landings,
            file.path(output_dir, "stock_landings_ecoregion.csv"),
            row.names = FALSE)
}

# Inspect these first:
print(match_quality, n = Inf)
if (eco_available) print(head(stock_landings, 20))

# NOTE ON FUTURE C-SQUARE INTEGRATION:
# 1. First smoke-test the service, e.g.:
#      library(icesVMS)
#      example_sq <- get_csquare(stat_rec = "40F3", convert2sf = FALSE)
#    Then query the geographical extent(s) needed for your data.
# 2. Standardise VMS area codes to ICES Vocabulary (they are not identical).
# 3. Validate whole-rectangle inclusion/exclusion in each candidate stock area.
# 4. Build rectangle_rules using the schema in section 0; set it non-NULL.
# 5. Rerun this SAME script. Ambiguous/partial cases will then be reassessed.
# Even with perfect spatial mapping, overlapping biological stocks (e.g.
# herring populations) may still require separate biological information.

