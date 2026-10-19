# fisheriesXplorer spatial landings: integration recipe
# Place fisheries_landings_map.R in your project's R folder, then source it:
# source("./R/fisheries_landings_map.R")
#
# Required packages (install once, not on every map request):
# install.packages(c("dplyr", "tidyr", "tibble", "sf", "jsonlite",
#                    "leaflet", "htmlwidgets", "htmltools"))
#
# This file defines helpers and examples; it does not load or overwrite your
# working data or create maps merely by being sourced.


# 1. Cache a coastline once, outside renderLeaflet() and outside the guild loop.
#    This optional helper uses public Natural Earth 1:50m land polygons.
#    For detailed coastal work, pass your existing regional land polygons instead.
load_cached_fisheries_land <- function(cache_directory = "./boot/maps") {
  dir.create(cache_directory, recursive = TRUE, showWarnings = FALSE)
  rds <- file.path(cache_directory, "natural_earth_50m_land.rds")
  if (file.exists(rds)) return(readRDS(rds))
  archive <- file.path(cache_directory, "ne_50m_land.zip")
  shapefile <- file.path(cache_directory, "ne_50m_land.shp")
  if (!file.exists(shapefile)) {
    if (!file.exists(archive)) {
      utils::download.file(
        "https://naturalearth.s3.amazonaws.com/50m_physical/ne_50m_land.zip",
        archive, mode = "wb", quiet = TRUE
      )
    }
    utils::unzip(archive, exdir = cache_directory)
  }
  land_sf <- sf::st_read(shapefile, quiet = TRUE)
  saveRDS(land_sf, rds)
  land_sf
}


# 2. Reuse the lookup convention in your original code. The REB exception here
#    is your existing rule: remove its Demersal membership and retain the other
#    membership from SID. Any remaining conflicting memberships cause a clear
#    error in prepare_fisheries_landings(), instead of doubling landings.
make_fisheries_guild_lookup <- function(sid) {
  sid <- as.data.frame(sid)
  if (!"FisheriesGuild" %in% names(sid)) {
    stop("sid requires a FisheriesGuild column.", call. = FALSE)
  }
  if (!"CLspeciesFaoCode" %in% names(sid)) {
    if (!"StockKeyLabel" %in% names(sid)) {
      stop("sid requires CLspeciesFaoCode or StockKeyLabel.")
    }
    sid$CLspeciesFaoCode <- toupper(substr(trimws(as.character(sid$StockKeyLabel)), 1, 3))
  }
  sid$CLspeciesFaoCode <- toupper(trimws(as.character(sid$CLspeciesFaoCode)))
  sid$FisheriesGuild <- trimws(as.character(sid$FisheriesGuild))
  out <- unique(sid[c("CLspeciesFaoCode", "FisheriesGuild")])
  out <- out[!is.na(out$CLspeciesFaoCode) & nzchar(out$CLspeciesFaoCode) &
               !is.na(out$FisheriesGuild) & nzchar(out$FisheriesGuild), , drop = FALSE]
  out[!(out$CLspeciesFaoCode == "REB" & out$FisheriesGuild == "Demersal"), , drop = FALSE]
}


# 3. One shared named palette fixes category colours across guilds, regions and
#    years. Extend these lists with the categories used by fisheriesXplorer.
fisheries_species_names <- c(
  HOM = "Horse mackerel", PIL = "Sardine", WHB = "Blue whiting",
  ANE = "Anchovy", MAC = "Mackerel", ANF = "Anglerfishes", LEZ = "Megrims"
)
fisheries_species_colors <- c(
  HOM = "#2C7BB6", PIL = "#E69F00", WHB = "#8F67AD",
  ANE = "#D55E00", MAC = "#009E73", ANF = "#597FB5", LEZ = "#A65C89"
)


# 4. Replacement for the original nested export loop.
#    rectangle_lists and ecoregion_shapes are NAMED lists, keyed by ecoregion.
#    All data fetching happens before this function; cached inputs also work
#    when get_csquare() or another upstream service is unavailable.
build_fisheries_exports <- function(
    cl.all2, sid, rectangle_lists, ecoregion_shapes, land_sf,
    years = 2022:2024,
    guilds = c("Demersal", "Pelagic", "Benthic", "Crustacean", "Elasmobranch"),
    output_directory = "./output/spatial_landings",
    weight_unit, top_n = 5L, coverage_pct = NULL,
    annual_policy = "strict", coverage_by_region = NULL) {
  if (missing(weight_unit)) stop("Confirm and supply weight_unit: kg, t, or raw.")
  has_region_names <- function(x) {
    is.list(x) && length(x) > 0L && !is.null(names(x)) &&
      !anyNA(names(x)) && all(nzchar(trimws(names(x)))) && !anyDuplicated(names(x))
  }
  if (!has_region_names(rectangle_lists) || !has_region_names(ecoregion_shapes)) {
    stop("rectangle_lists and ecoregion_shapes must be nonempty lists with unique ecoregion names.", call. = FALSE)
  }
  lookup <- make_fisheries_guild_lookup(sid)
  regions <- names(rectangle_lists)
  if (!all(regions %in% names(ecoregion_shapes))) {
    stop("An ecoregion boundary is missing for a named rectangle list.")
  }
  if (!is.null(coverage_by_region) && !has_region_names(coverage_by_region)) {
    stop("coverage_by_region must be NULL or a list with unique ecoregion names.", call. = FALSE)
  }
  if (!is.null(coverage_by_region) && !all(regions %in% names(coverage_by_region))) {
    stop("Include every selected region in coverage_by_region; use an explicit NULL entry where no coverage table is supplied.",
         call. = FALSE)
  }

  # These retain your operational CRE/SCE/WHE grouping. An override now gives
  # each species one membership. SCE/WHE are included because your original
  # workflow included them; the name 'Crustacean' is not a taxonomic correction.
  overrides <- c(CRE = "Crustacean", SCE = "Crustacean", WHE = "Crustacean")
  result <- list()
  for (region in regions) {
    ids <- unique(as.character(rectangle_lists[[region]]))
    shape <- ecoregion_shapes[[region]]
    coverage <- if (is.null(coverage_by_region)) NULL else coverage_by_region[[region]]
    for (guild in guilds) {
      message(region, " | ", guild, " | ", paste(years, collapse = ", "))
      m <- plot_fisheries_landings(
        data = cl.all2, years = years, rectangle_ids = ids,
        guild_lookup = lookup, guild = guild,
        weight_unit = weight_unit,
        boundary = shape, land = land_sf,
        region = region, title = paste(guild, "landings"),
        top_n = top_n, coverage_pct = coverage_pct,
        annual_policy = annual_policy, coverage = coverage,
        guild_overrides = overrides,
        species_names = fisheries_species_names,
        species_colors = fisheries_species_colors,
        source_note = "RDBES records supplied to fisheriesXplorer",
        height = 800
      )
      threshold_tag <- if (!is.null(coverage_pct)) {
        paste0("coverage_", coverage_pct)
      } else if (is.null(top_n)) "all_species" else paste0("top_", top_n, "_species")
      folder <- file.path(output_directory, region, threshold_tag)
      file <- file.path(folder, paste0(guild, "_landings_map.html"))
      save_fisheries_map(m, file, selfcontained = TRUE)
      # Store calculation provenance alongside the generated map. No PDF
      # screenshot is made: it would remove all the interactive behaviour.
      export_fisheries_calculations(m, file.path(folder, paste0(guild, "_calculation")))
      result[[paste(region, guild, sep = " / ")]] <- file
    }
  }
  invisible(result)
}


# 5. Example invocation using your current objects. Run these lines in your
#    project, after loading the original records and map inputs.
#
# source("./R/fisheries_landings_map.R")
# source("./R/example_fisheries_landings.R")
# load("./data/extendedRDBES.RData")
# sid <- readRDS("./boot/data/sidAll_2025.rds")
# land_sf <- load_cached_fisheries_land()
#
# # If ICES_Rect is your vector of Celtic Seas rectangle codes:
# rectangle_lists <- list("Celtic Seas" = ICES_Rect)
# ecoregion_shapes <- list("Celtic Seas" = icesFO::load_ecoregion("Celtic Seas"))
#
# # Or retain your existing c-square-to-rectangle selection, and cache it once:
# areaAll <- get_csquare(ecoregion = "Celtic Seas", convert2sf = TRUE)
# rectangle_lists <- list("Celtic Seas" = unique(areaAll$stat_rec))
# saveRDS(rectangle_lists, "./boot/maps/rectangle_lists.rds")
#
# maps <- build_fisheries_exports(
#   cl.all2, sid, rectangle_lists, ecoregion_shapes, land_sf,
#   years = 2022:2024,
#   weight_unit = "kg",   # Use ONLY if the original weight column is kg.
#   top_n = 5,
#   annual_policy = "strict"
# )
#
# # Alternative: keep enough species to reach at least 90% (the crossing
# # species is included); top_n is ignored when coverage_pct is supplied.
# # Set coverage_pct = 90 in the call above.


# 6. Direct R Shiny integration (adapt your existing UI/server).
#    Load data, lookup, coastline and boundary caches once at app startup.
#    This keeps file I/O and downloads out of the interactive request path.
#
# lookup <- make_fisheries_guild_lookup(sid)
# # Keep the same explicit operational memberships as the export workflow:
# guild_overrides <- c(CRE = "Crustacean", SCE = "Crustacean", WHE = "Crustacean")
#
# ui <- shiny::fluidPage(
#   shiny::selectInput("guild", "Guild", c("Pelagic", "Demersal", "Benthic")),
#   leaflet::leafletOutput("landings_map", height = "800px")
# )
#
# server <- function(input, output, session) {
#   output$landings_map <- leaflet::renderLeaflet({
#     plot_fisheries_landings(
#       data = cl.all2, years = 2022:2024,
#       rectangle_ids = rectangle_lists[["Celtic Seas"]],
#       guild_lookup = lookup, guild = input$guild,
#       guild_overrides = guild_overrides,
#       weight_unit = "kg", land = land_sf,
#       boundary = ecoregion_shapes[["Celtic Seas"]],
#       region = "Celtic Seas", top_n = 5,
#       species_names = fisheries_species_names,
#       species_colors = fisheries_species_colors,
#       annual_policy = "strict", height = 800
#     )
#   })
# }
#
# For a stable finite set of year/guild/region combinations, precompute and
# cache the payloads at startup or in the data-refresh pipeline:
#   prepared <- prepare_fisheries_landings(...)
#   payload <- build_fisheries_map_payload(prepared, boundary = ..., land = ...)
#   saveRDS(payload, "./boot/maps/pelagic_payload.rds")
#   # Within renderLeaflet(): render_fisheries_landings(payload)
# Landings/Composition/Species switches, rectangle selection and export then
# run entirely in the browser. A changed year/guild selection prepares a new
# payload. Do not send individual vessel records to the HTML widget.


# 7. Optional marine display adjustments, recorded by rectangle ID.
#    Supply anchor_overrides = data.frame(rectangle = ..., lon = ..., lat = ...)
#    to plot_fisheries_landings(). Each point must remain inside the WATER part
#    of its original rectangle. Invalid shifts fail; no quantities are moved.
