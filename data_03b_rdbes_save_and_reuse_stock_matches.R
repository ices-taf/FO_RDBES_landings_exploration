# Companion to v4 (global) and v5 (SID-scoped) RDBES stock workflows.
# Run A once AFTER the main run finishes, in the same R session.
# Run B later for each ecoregion, including in a fresh R session.
# Run the two sections separately; do not Source the entire file in a new session.
# The checkpoint covers the input data and analysis years used in that run.

# A. Save completed stock matching -----------------------------------------
# Keep the existing stock-reference audit flags without saving FU polygons.
stock_flags <- dplyr::distinct(
  stock_summary, fishstock, Special_rule_required,
  Missing_species_reference, Missing_area_reference
)
stopifnot(!anyDuplicated(stock_flags$fishstock),
          length(support_sfc) == nrow(geography))

dir.create("./data", recursive = TRUE, showWarnings = FALSE)
if (!exists("matching_scope", inherits = FALSE)) {
  matching_scope <- list(ecoregions = NULL, years = sort(unique(dat$CLyear)))
}
# Preserve the completed global checkpoint when saving a scoped run.
checkpoint_file <- if (is.null(matching_scope$ecoregions)) {
  "./data/rdbes_stock_matches.RData"
} else file.path("./data", paste0("rdbes_stock_matches_",
  paste(gsub("[^A-Za-z0-9]", "_", matching_scope$ecoregions), collapse = "__"), ".RData"))
save(
  dat, candidates, geography, support_sfc, stock_list, stock_flags,
  reference_year, input_kg, matching_scope,
  cached, read_gis, normalise, empty, union_geometry,
  relation_vec, audit_landings,
  file = checkpoint_file,
  compress = FALSE
)
message("Saved ", checkpoint_file, "; catch years: ",
        paste(matching_scope$years, collapse = ", "),
        "; reference year: ", reference_year)

# B. Reuse completed matching for another ecoregion -------------------------
# Start in your R project directory. Only this section is needed next time.
library(dplyr)
library(tidyr)
library(sf)
checkpoint_file <- "./data/rdbes_stock_matches.RData"  # your completed global run
# Reset the default for old v4 checkpoints, which did not contain scope metadata.
matching_scope <- list(ecoregions = NULL, years = NULL)
load(checkpoint_file)
if (is.null(matching_scope$years)) matching_scope$years <- sort(unique(dat$CLyear))

Ecoregion <- "Bay of Biscay and the Iberian Coast" # Change this for each new calculation.
if (!is.null(matching_scope$ecoregions) && !Ecoregion %in% matching_scope$ecoregions) {
  stop("This checkpoint covers only: ", paste(matching_scope$ecoregions, collapse = ", "),
       ". Use your global checkpoint or match the additional region first.")
}
cache_dir <- "./data/ices_cache_simple"
refresh_reference <- FALSE
output_dir <- file.path(
  "./output/stock_matching_sid",
  gsub("[^A-Za-z0-9]", "_", Ecoregion)
)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Old v4 checkpoints need the SID ecoregion field from the same reference year.
if (!"SID_ecoregions" %in% names(stock_list)) {
  sid <- cached(paste0("stocks_", reference_year), icesSD::getSD(year = reference_year))
  stock_list <- left_join(stock_list,
    transmute(sid, fishstock = as.character(StockKeyLabel),
              SID_ecoregions = na_if(trimws(as.character(EcoRegion)), "")) |> distinct(),
    by = "fishstock", relationship = "one-to-one")
}
# SID omits Biscay for these two stocks despite their official stock definitions.
# https://asd.ices.dk/viewAdvice/7176 ; https://doi.org/10.17895/ices.advice.25019513
regional_stock_list <- function(stock_list, ecoregions) {
  if (is.null(ecoregions)) return(mutate(stock_list, SID_selection = "all stocks"))
  tags <- strsplit(coalesce(stock_list$SID_ecoregions, ""), ",", fixed = TRUE)
  if (!all(ecoregions %in% trimws(unlist(tags)))) stop("Unknown SID ecoregion name")
  additions <- if ("Bay of Biscay and the Iberian Coast" %in% ecoregions)
    c("mon.27.78abd", "rjb.27.89a") else character()
  stock_list |> mutate(SID_selection = case_when(
    vapply(tags, function(x) any(trimws(x) %in% ecoregions), logical(1)) ~ "SID ecoregion",
    fishstock %in% additions ~ "documented addition",
    is.na(SID_ecoregions) | SID_ecoregions == "" ~ "SID ecoregion missing",
    TRUE ~ NA_character_)) |> filter(!is.na(SID_selection))
}
regional_stocks <- regional_stock_list(stock_list, Ecoregion)

# Match the spellings used by the ICES GIS service and the existing cache.
eco_name <- dplyr::recode(
  Ecoregion,
  "Norwegian Sea" = "Norwegean Sea",
  "Oceanic Northeast Atlantic" = "Oceanic north-east Atlantic"
)
ecoregion <- cached(
  paste0("ecoregion_", gsub("[^A-Za-z0-9]", "_", eco_name)),
  read_gis(
    "ICES_Ecoregions/MapServer/0",
    paste0("Ecoregion='", gsub("'", "''", eco_name), "'")
  )
)
stopifnot(nrow(ecoregion) > 0)

# Replace the previous regional classification; stock assignments stay fixed.
eco_relation <- relation_vec(support_sfc, union_geometry(ecoregion))
geography$CS <- dplyr::recode(
  eco_relation, full = "inside", outside = "outside",
  partial = "boundary", unknown = "unknown"
)
dat$CS <- geography$CS[match(dat$geo_id, geography$geo_id)]
stopifnot(!anyNA(dat$CS))

audit_args <- list(dat = dat, candidates = candidates,
                   stock_list = stock_list, input_kg = input_kg)
if ("years" %in% names(formals(audit_landings)) && !is.null(matching_scope$years)) {
  audit_args$years <- matching_scope$years
}
stock_summary <- do.call(audit_landings, audit_args) |>
  mutate(ReferenceYear = reference_year, Ecoregion = Ecoregion) |>
  left_join(stock_flags, by = "fishstock", relationship = "many-to-one")
stock_summary_ecoregion <- inner_join(stock_summary,
  select(regional_stocks, fishstock, SID_selection), by = "fishstock", relationship = "many-to-one")
matching_summary <- dat |> mutate(match_status = case_when(
  match_status == "unique" & fishstock %in% regional_stocks$fishstock ~ "unique_target_stock",
  match_status == "unique" ~ "unique_other_stock", TRUE ~ match_status)) |>
  summarise(Landings_kg = sum(CLscientificWeight),
            .by = c(CLyear, match_status))
unresolved_landings <- dat |> filter(match_status != "unique")

write.csv(stock_summary_ecoregion, file.path(output_dir, "stock_summary.csv"),
          row.names = FALSE)
write.csv(matching_summary, file.path(output_dir, "matching_summary.csv"),
          row.names = FALSE)
write.csv(unresolved_landings, file.path(output_dir, "unresolved_landings.csv"),
          row.names = FALSE)
message("Saved ", Ecoregion, " results in ", output_dir)

# A new ecoregion needs only its polygon and this regional calculation.
# The CSV contains selected stocks plus explicitly labelled missing SID tags;
# stock_summary keeps the full internal audit, so section A remains reusable.
# To change the RDBES extract, catch years, stock definitions or matching rules,
# rerun the main workflow and create a new checkpoint with section A.
# The same audit interpretation applies: shares/bounds cover uniquely assigned
# landings, and unresolved candidate weights must not be added across stocks.


#####check

library(dplyr)

stocks_check <- c(
  "mac.27.nea",
  "whb.27.1-91214",
  "boc.27.6-8"
)

bridge_check <- dat |>
  filter(
    CLyear == 2024,
    match_status == "unique"
  ) |>
  summarise(
    Assigned_kg = sum(CLscientificWeight),

    Inside_original_rule_kg = sum(
      CLscientificWeight[CLstatisticalRectangle %in% area]
    ),

    B_share_pct = ifelse(
      Assigned_kg > 0,
      100 * Inside_original_rule_kg / Assigned_kg,
      NA_real_
    ),

    .by = c(CLyear, fishstock)
  )

print(bridge_check, width = Inf)
write.csv(
  bridge_check,
  "./output/bridge_check_2024.csv",
  row.names = FALSE
)

sardine_check <- dat |>
  filter(
    CLyear == 2024,
    CLspeciesFaoCode == "PIL"
  ) |>
  mutate(
    inside_original_rule = CLstatisticalRectangle %in% area
  ) |>
  summarise(
    Landings_t = sum(CLscientificWeight) / 1000,
    .by = c(
      CLarea,
      fishstock,
      match_status,
      inside_original_rule
    )
  ) |>
  arrange(desc(Landings_t))

print(sardine_check, n = 30, width = Inf)


sardine_rectangles <- dat |>
  filter(
    CLyear == 2024,
    CLspeciesFaoCode == "PIL",
    CLstatisticalRectangle %in% c("25E4", "25E5")
  ) |>
  mutate(
    inside_original_rule = CLstatisticalRectangle %in% area
  ) |>
  summarise(
    Landings_t = sum(CLscientificWeight) / 1000,
    .by = c(
      CLarea,
      CLstatisticalRectangle,
      fishstock,
      match_status,
      inside_original_rule
    )
  )

print(sardine_rectangles, n = Inf, width = Inf)



# Exact quantity held in the R object, before display rounding
transfer_kg <- sum(sardine_rectangles$Landings_t) * 1000

sardine_comparison <- bridge_check |>
  filter(
    CLyear == 2024,
    fishstock %in% c("pil.27.7", "pil.27.8abd", "pil.27.8c9a")
  ) |>
  mutate(
    Assigned_IFREMER_rule_kg = Assigned_kg + case_when(
      fishstock == "pil.27.8abd" ~  transfer_kg,
      fishstock == "pil.27.7"    ~ -transfer_kg,
      TRUE                     ~ 0
    ),

    Share_IFREMER_rule_pct =
      100 * Inside_original_rule_kg / Assigned_IFREMER_rule_kg
  ) |>
  select(
    fishstock,
    Assigned_kg,
    Assigned_IFREMER_rule_kg,
    B_share_pct,
    Share_IFREMER_rule_pct
  )

print(sardine_comparison, width = Inf)