# ============================================================================
# RDBES landings -> ICES reference stocks -> ecoregion percentages
# Complete workflow with Nephrops functional units and stock-level audit
# Version: 2026-10-24
# ============================================================================
# QUICK START
# 1. Edit workflow_config: input RData, object name and Nephrops shapefile path.
# 2. Keep the shapefile's .shp/.shx/.dbf/.prj files together.
# 3. Install dependencies once if necessary:
#    install.packages(c("dplyr", "tidyr", "sf", "httr", "jsonlite",
#                       "icesVocab", "icesSD", "icesVMS"),
#      repos = c(ICES = "https://ices-tools-prod.r-universe.dev",
#                CRAN = "https://cloud.r-project.org"))
# 4. source("rdbes_stock_ecoregion_workflow_v2.R", encoding = "UTF-8")
# The result is named `results`; results$output_directory contains all outputs.
# Each run uses a separate subdirectory. Reference caches are reused by default.
#
# To load functions without executing the workflow:
#    options(rdbes.workflow.autorun = FALSE)
#    source("rdbes_stock_ecoregion_workflow_v2.R", encoding = "UTF-8")
#    results <- run_rdbes_stock_workflow(workflow_config)
#
# ASSIGNMENT
# Each eligible input row/weight is retained once. A stock is assigned only when
# exactly ONE possible stock remains and the WHOLE reported geographic support
# fits that stock definition. A single PARTIAL candidate is not sufficient.
# Support is rectangle intersected with CLarea where both are usable; otherwise
# the usable geographic input is retained, with its availability recorded.
# Conflicting area/rectangle combinations remain unresolved. Malformed area
# codes cannot bypass the stock-specific restrictions.
#
# Ordinary stock geography is the UNION of its ICES-area associations. Thus
# child areas can jointly cover a reported parent. These associations do not
# prove that biological stocks are spatially separable: the explicit review
# registry gates known exceptions. The registry is not exhaustive.
#
# NEPHROPS
# The supplied FU polygons are mapped to assessed stocks and combined before
# matching. FU1/FU18 remain unmapped. Incomplete grouped FUs are flagged.
# The default rebuilds FU25 from the May2024 annex's five rectangles:
# 15E0,15E1,16E0,16E1,17E1 (2021 revision). The source shapefile is untouched;
# the correction, original geometry, source hashes and definition are retained.
# Disabling this correction leaves FU25 unapproved for automatic allocation.
# outFU stocks use their supplied polygons, never an unmatched-row fallback.
#
# REFERENCE YEAR
# reference_year selects ONE reporting stock list for all analysed catch years.
# Current Vocabulary/GIS downloads are not historical snapshots. FU25 has the
# explicit annex date above. Source provenance is saved. This workflow does not
# reconstruct definitions in force in each historical catch year. Review stock
# splits/replacements for the chosen list; no legacy code is silently removed.
# Optional explicit exclusions are recorded in the configuration snapshot.
#
# ECOREGION SHARES
# For uniquely assigned records, let I/O/B/U = inside/outside/boundary/unknown kg.
# ShareLandingsInEcoregion = 100*I/(I+O)
# AssignedShareLower_pct = 100*I/(I+O+B+U)
# AssignedShareUpper_pct = 100*(I+B+U)/(I+O+B+U)
# Geo_coverage_pct = 100*(I+O)/(I+O+B+U)
# Zero denominators yield NA. Bounds concern ONLY the assigned subset; they do
# not quantify unresolved stock identity, missing landings or RDBES coverage.
# Candidate exposure can overlap across stocks and must NEVER be summed as
# allocated stock landings. Every selected stock-year remains in the audit.
#
# Full official ecoregion polygons determine classification. "boundary" includes
# true partial overlap OR reference-coastline mismatch. It is not proof that
# every such record spans two marine ecoregions. Unmapped coastal fragments are
# not assumed to be land. Coordinates share a documented1mm working precision
# in the source CRS, EPSG3857; only numerical slivers use the tolerance below.
# Projected areas are geometry diagnostics, not undistorted sea areas or catch
# weights. No polygon-area or C-square-count weighting is performed.
# icesVMS supplies optional diagnostic counts for a limited set of rectangles;
# returned cell counts are not landings percentages or a guaranteed full grid.
# VMS failure does not stop the polygon-based calculation.
#
# INPUT
# CLscientificWeight must be kilograms. Negative/nonfinite weights halt the run.
# CLcatchCategory='Lan' is used where present. Otherwise the explicit
# assume_landings_only setting applies. Missing weights are saved for review,
# not changed to zero. CLspeciesCode means WoRMS AphiaID. A UNIQUE official
# SpecASFIS->SpecWoRMS link can fill a missing ID; conflicts/ambiguity stay
# unresolved. Taxonomic groups are not automatically expanded/collapsed.
#
# OUTPUTS
# stock_landings_ecoregion.csv / stock_mapping_audit.csv: all stock-years,
# shares, conditional bounds, assigned weights, exposure, quality and methods.
# matching_quality_by_year.csv: mutually exclusive actual landing-weight totals.
# unresolved_landings.csv: unresolved rows and possible stocks.
# geography_audit.csv: geography availability, full/partial/unknown and diagnostics.
# reference_gaps.csv / reference_area_overlaps.csv: missing/overlapping references.
# mass_balance.csv: original aggregated vs output conservation, yearly/global.
# nephrops_*.csv: definitions, unmapped features, validity and FU25 correction.
# RDS files preserve row assignments, candidates, support and reference snapshots.
# run_manifest.rds/sessionInfo.txt preserve configuration, retrievals and versions.
# First download includes detailed GIS layers; later runs reuse caches.
#
# EXAMPLES AFTER RUNNING
# results$stock_landings |>
#   dplyr::filter(CLyear ==2024,fishstock %in%c("mac.27.nea","whb.27.1-91214")) |>
#   dplyr::select(fishstock,Inside_tonnes,ShareLandingsInEcoregion,
#     AssignedShareLower_pct,AssignedShareUpper_pct,Geo_coverage_pct,
#     unresolved_candidate_exposure_kg,share_quality)
# results$audit$stock_audit |> dplyr::filter(grepl("^(nep|san)[.]",fishstock))
# results$audit$unresolved_records |> dplyr::arrange(dplyr::desc(CLscientificWeight))
#
# REFERENCES (individual stock rules also contain their own source URLs)
# https://ices-tools-prod.r-universe.dev/icesVocab/doc/manual.html
# https://ices-tools-prod.r-universe.dev/icesSD/doc/manual.html
# https://ices-tools-prod.r-universe.dev/icesVMS/doc/manual.html
# https://gis.ices.dk/gis/rest/services/ICES_reference_layers
# https://ices-library.figshare.com/ndownloader/files/48791170
# https://doi.org/10.17895/ices.pub.18622877
# ============================================================================

workflow_config <- list(
  ecoregion = "Bay of Biscay and the Iberian Coast",
  input_file = "./data/extendedRDBES.RData",
  input_object = "cl.all2",
  nephrops_shapefile = "./Boot/ICES_nephrops_FU/Nephrops Functional Units.shp",
  output_dir = "./output/stock_matching_v2",
  cache_dir = "./data/ices_cache_v2",
  reference_year = 2025L,          # One reporting stock list for all catch years
  analysis_years = NULL,          # NULL = all input years; e.g.2022:2024
  refresh_reference = FALSE,      # TRUE = redownload reference and GIS caches
  assume_landings_only = TRUE,    # Only used when CLcatchCategory is absent
  apply_fu25_2021_revision = TRUE,
  exclude_stock_codes = character(), # Explicit documented stock-system choices
  run_vms_diagnostic = TRUE,
  vms_max_rectangles = 20L,        # Largest-weight boundary rectangles
  vms_check_rectangles = NULL,    # Or e.g.c("04D6"); diagnostics only
  relative_sliver_tolerance = 1e-10,
  absolute_sliver_tolerance_m2 = 1
)



# Spatial helpers for the RDBES stock/ecoregion workflow.
# These helpers use full official polygons. VMS C-square counts are not weights.
# All overlay work stays in the services' native EPSG:3857 so sparse rectangle
# edges and the ICES reference boundaries share the original planar topology.
# Projected areas below are used only for numerical sliver diagnostics.

ICES_GIS_BASE <- paste0(
  "https://gis.ices.dk/gis/rest/services/ICES_reference_layers/"
)
ICES_SPATIAL_CRS <- 3857L
ICES_COORDINATE_PRECISION_M <- 0.001

# Independent ICES exports can differ by 0.0001 projected metres along the
# same intended grid edge. Normalise all overlays to a shared 1 mm grid,
# consistent with the services' reported 0.001 m XY tolerance. This is a
# coordinate-precision correction, not a coastal buffer or landings weight.
normalize_ices_geometry <- function(x) {
  x <- sf::st_transform(x, ICES_SPATIAL_CRS)
  x <- sf::st_set_precision(x, 1 / ICES_COORDINATE_PRECISION_M)
  sf::st_make_valid(x)
}

arcgis_read_sf <- function(layer_url, where = "1=1", fields = "*",
                          cache_file = NULL, refresh = FALSE,
                          batch_size = 100L, timeout_seconds = 180) {
  stopifnot(length(layer_url) == 1L, length(where) == 1L,
            batch_size >= 1L, batch_size <= 1000L)
  layer_url <- sub("/+$", "", layer_url)
  fields <- paste(fields, collapse = ",")
  if (!is.null(cache_file) && file.exists(cache_file) && !refresh) {
    cached <- readRDS(cache_file)
    if (is.list(cached) && identical(cached$schema, 2L) &&
        identical(cached$layer_url, layer_url) &&
        identical(cached$where, where) && identical(cached$fields, fields) &&
        identical(cached$out_sr, ICES_SPATIAL_CRS) &&
        identical(cached$precision_m, ICES_COORDINATE_PRECISION_M) &&
        inherits(cached$data, "sf")) {
      out <- cached$data
      attr(out, "source_manifest") <- cached[names(cached) != "data"]
      return(out)
    }
  }
  request_json <- function(url, query) {
    response <- httr::RETRY(
      "GET", url, query = query, httr::timeout(timeout_seconds),
      times = 3L, pause_min = 1, pause_cap = 5, quiet = TRUE
    )
    httr::stop_for_status(response)
    txt <- httr::content(response, "text", encoding = "UTF-8")
    parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                       error = function(e) stop("Invalid GIS JSON from ", url,
                                                 ": ", conditionMessage(e)))
    if (!is.null(parsed$error)) {
      stop("ICES GIS error: ", parsed$error$message, " [", url, "]")
    }
    list(parsed = parsed, text = txt)
  }
  metadata <- request_json(layer_url, list(f = "json"))$parsed
  oid_fields <- vapply(metadata$fields, function(x) {
    if (identical(x$type, "esriFieldTypeOID")) x$name else ""
  }, character(1))
  oid_fields <- oid_fields[nzchar(oid_fields)]
  if (length(oid_fields) != 1L) stop("Cannot identify GIS object-ID field")
  oid <- oid_fields[[1L]]
  if (fields != "*") {
    fields_query <- paste(unique(c(oid, strsplit(fields, ",", fixed = TRUE)[[1]])),
                          collapse = ",")
  } else fields_query <- fields

  ids_response <- request_json(paste0(layer_url, "/query"), list(
    where = where, returnIdsOnly = "true", f = "json"
  ))$parsed
  if (is.null(ids_response$objectIdFieldName)) {
    stop("Unexpected GIS ID response from ", layer_url)
  }
  ids <- sort(unique(as.character(unlist(ids_response$objectIds,
                                        use.names = FALSE))))
  if (length(ids) == 0L) {
    out <- sf::st_sf(geometry = sf::st_sfc(crs = ICES_SPATIAL_CRS))
  } else {
    batches <- split(ids, ceiling(seq_along(ids) / batch_size))
    pages <- lapply(batches, function(wanted_ids) {
      response <- request_json(paste0(layer_url, "/query"), list(
        objectIds = paste(wanted_ids, collapse = ","),
        outFields = fields_query, returnGeometry = "true",
        outSR = ICES_SPATIAL_CRS, f = "geojson"
      ))
      if (isTRUE(response$parsed$exceededTransferLimit) ||
          !identical(response$parsed$type, "FeatureCollection") ||
          length(response$parsed$features) != length(wanted_ids)) {
        stop("Incomplete GIS geometry batch from ", layer_url)
      }
      tmp_geojson <- tempfile(fileext = ".geojson")
      on.exit(unlink(tmp_geojson), add = TRUE)
      writeLines(response$text, tmp_geojson, useBytes = TRUE)
      page <- sf::st_read(tmp_geojson, quiet = TRUE)
      if (!oid %in% names(page) ||
          !setequal(as.character(page[[oid]]), wanted_ids) ||
          anyDuplicated(page[[oid]])) {
        stop("GIS geometry IDs do not match the requested IDs")
      }
      # ArcGIS includes a CRS for non-WGS84 GeoJSON. Check it, never relabel
      # an existing different CRS without transforming coordinates.
      if (is.na(sf::st_crs(page))) {
        stop("GIS GeoJSON has no CRS; cannot verify coordinates")
      }
      normalize_ices_geometry(page)
    })
    out <- do.call(rbind, pages)
    if (nrow(out) != length(ids) || anyDuplicated(out[[oid]])) {
      stop("Incomplete or duplicated ICES GIS download")
    }
  }
  manifest <- list(
    schema = 2L, layer_url = layer_url, where = where, fields = fields,
    out_sr = ICES_SPATIAL_CRS, object_id_field = oid,
    precision_m = ICES_COORDINATE_PRECISION_M,
    retrieved_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    service_version = metadata$currentVersion,
    service_item_id = metadata$serviceItemId,
    source_spatial_reference = metadata$sourceSpatialReference,
    feature_count = nrow(out), layer_metadata = metadata
  )
  if (!is.null(cache_file)) {
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(c(manifest, list(data = out)), cache_file)
  }
  attr(out, "source_manifest") <- manifest
  out
}

canonical_ecoregion <- function(x) {
  x <- trimws(as.character(x))
  x[x == "Norwegean Sea"] <- "Norwegian Sea"
  x[x == "Oceanic north-east Atlantic"] <- "Oceanic Northeast Atlantic"
  x
}

load_ices_spatial_reference <- function(cache_dir, refresh = FALSE,
                                        ecoregion = NULL) {
  areas <- arcgis_read_sf(
    paste0(ICES_GIS_BASE, "ICES_Areas/MapServer/0"),
    fields = "Area_Full", cache_file = file.path(cache_dir, "ices_area_polygons.rds"),
    refresh = refresh
  )
  # Retrieve only the requested target where supplied: the detailed full
  # ecoregion layer is large. The raw GIS names have two spelling variants.
  eco_where <- "1=1"
  if (!is.null(ecoregion)) {
    wanted <- canonical_ecoregion(ecoregion)
    raw_names <- wanted
    raw_names[raw_names == "Norwegian Sea"] <- "Norwegean Sea"
    raw_names[raw_names == "Oceanic Northeast Atlantic"] <- "Oceanic north-east Atlantic"
    literals <- paste0("'", gsub("'", "''", unique(c(wanted, raw_names)),
                                 fixed = TRUE), "'")
    eco_where <- paste0("Ecoregion IN (", paste(literals, collapse = ","), ")")
  }
  ecoregions <- arcgis_read_sf(
    paste0(ICES_GIS_BASE, "ICES_Ecoregions/MapServer/0"),
    where = eco_where,
    fields = "Ecoregion", cache_file = file.path(cache_dir, "ecoregion_polygons.rds"),
    refresh = refresh
  )
  stopifnot("Area_Full" %in% names(areas), "Ecoregion" %in% names(ecoregions))
  areas$Area_Full <- tolower(trimws(as.character(areas$Area_Full)))
  ecoregions$Ecoregion <- canonical_ecoregion(ecoregions$Ecoregion)
  list(areas = areas, ecoregions = ecoregions)
}

load_ices_rectangles <- function(rectangle_ids, cache_dir, refresh = FALSE) {
  rectangle_ids <- sort(unique(toupper(trimws(as.character(rectangle_ids)))))
  rectangle_ids <- rectangle_ids[!is.na(rectangle_ids) & nzchar(rectangle_ids)]
  if (!length(rectangle_ids)) return(sf::st_sf(
    ICESNAME = character(), geometry = sf::st_sfc(crs = ICES_SPATIAL_CRS)
  ))
  # Keep SQL/URL lengths modest, including for large RDBES extracts.
  batches <- split(rectangle_ids, ceiling(seq_along(rectangle_ids) / 150L))
  pages <- lapply(seq_along(batches), function(i) {
    literals <- paste0("'", gsub("'", "''", batches[[i]], fixed = TRUE), "'")
    out <- arcgis_read_sf(
      paste0(ICES_GIS_BASE, "ICES_Statrec_complete/MapServer/0"),
      where = paste0("ICESNAME IN (", paste(literals, collapse = ","), ")"),
      fields = c("ICESNAME", "SOUTH", "WEST", "NORTH", "EAST"),
      cache_file = file.path(cache_dir, sprintf("statrec_polygons_%03d.rds", i)),
      refresh = refresh
    )
    if (nrow(out) == 0L) return(NULL)
    out
  })
  manifests <- lapply(pages, attr, which = "source_manifest")
  pages <- Filter(Negate(is.null), pages)
  if (!length(pages)) return(sf::st_sf(
    ICESNAME = character(), geometry = sf::st_sfc(crs = ICES_SPATIAL_CRS)
  ))
  out <- do.call(rbind, pages)
  out$ICESNAME <- toupper(trimws(as.character(out$ICESNAME)))
  if (anyDuplicated(out$ICESNAME)) stop("Duplicate statistical-rectangle polygons")
  attr(out, "source_manifests") <- manifests
  attr(out, "unresolved_rectangle_ids") <- setdiff(rectangle_ids, out$ICESNAME)
  out
}

polygon_union <- function(x) {
  x <- sf::st_geometry(x)
  x <- normalize_ices_geometry(x)
  dims <- sf::st_dimension(x)
  x <- x[!is.na(dims) & dims == 2L & !sf::st_is_empty(x)]
  if (!length(x)) return(sf::st_sfc(sf::st_geometrycollection(),
                                  crs = ICES_SPATIAL_CRS))
  if (any(sf::st_geometry_type(x) == "GEOMETRYCOLLECTION")) {
    x <- sf::st_collection_extract(x, "POLYGON", warn = FALSE)
  }
  sf::st_union(x)
}

build_area_geometries <- function(area_codes, areas) {
  area_codes <- sort(unique(tolower(trimws(as.character(area_codes)))))
  area_codes <- area_codes[!is.na(area_codes) & nzchar(area_codes)]
  stopifnot(inherits(areas, "sf"), "Area_Full" %in% names(areas))
  geoms <- vector("list", length(area_codes))
  status <- character(length(area_codes))
  n_parts <- integer(length(area_codes))
  for (i in seq_along(area_codes)) {
    code <- area_codes[[i]]
    keep <- areas$Area_Full == code | startsWith(areas$Area_Full, paste0(code, "."))
    n_parts[[i]] <- sum(keep)
    # Never substitute a coarser polygon for an unavailable finer unit.
    status[[i]] <- if (any(keep)) "available" else "missing"
    geoms[[i]] <- polygon_union(areas[keep, , drop = FALSE])[[1L]]
  }
  sf::st_sf(area_code = area_codes, area_geometry_status = status,
            n_reference_parts = n_parts,
            geometry = sf::st_sfc(geoms, crs = ICES_SPATIAL_CRS))
}

build_record_support <- function(keys, area_geometries, rectangles, marine_union) {
  stopifnot(all(c("CLarea", "CLstatisticalRectangle") %in% names(keys)))
  keys <- unique(as.data.frame(keys[c("CLarea", "CLstatisticalRectangle")]))
  keys$CLarea <- tolower(trimws(as.character(keys$CLarea)))
  keys$CLstatisticalRectangle <- toupper(trimws(as.character(keys$CLstatisticalRectangle)))
  keys <- unique(keys)
  keys$geo_id <- seq_len(nrow(keys))
  ai <- match(keys$CLarea, area_geometries$area_code)
  ri <- match(keys$CLstatisticalRectangle, rectangles$ICESNAME)
  keys$area_geometry_available <- !is.na(ai)
  keys$area_geometry_available[!is.na(ai)] <-
    area_geometries$area_geometry_status[ai[!is.na(ai)]] == "available"
  keys$rectangle_geometry_available <- !is.na(ri)
  keys$support_status <- "unknown"
  keys$geometry_note <- NA_character_
  geoms <- vector("list", nrow(keys))
  empty <- sf::st_geometrycollection()
  marine_union <- polygon_union(marine_union)
  for (i in seq_len(nrow(keys))) {
    geoms[[i]] <- empty
    a <- keys$area_geometry_available[[i]]
    r <- keys$rectangle_geometry_available[[i]]
    if (!a && !r) next
    tryCatch({
      if (a && r) {
        g <- sf::st_intersection(sf::st_geometry(rectangles[ri[[i]], ]),
                                 sf::st_geometry(area_geometries[ai[[i]], ]))
        method <- "rectangle_and_area"
      } else if (a) {
        g <- sf::st_geometry(area_geometries[ai[[i]], ])
        method <- "area_only"
      } else {
        # Discard inland portions of full rectangles before classifying sea
        # ecoregions. Absence of a valid CLarea remains explicit in the audit.
        g <- sf::st_intersection(sf::st_geometry(rectangles[ri[[i]], ]), marine_union)
        method <- "rectangle_only"
      }
      g <- polygon_union(g)
      if (sf::st_is_empty(g)[[1L]] || as.numeric(sf::st_area(g)) <= 0) {
        keys$support_status[[i]] <- "support_conflict"
      } else {
        geoms[[i]] <- g[[1L]]
        keys$support_status[[i]] <- method
      }
    }, error = function(e) {
      keys$support_status[[i]] <<- "geometry_error"
      keys$geometry_note[[i]] <<- conditionMessage(e)
    })
  }
  sf::st_sf(keys, geometry = sf::st_sfc(geoms, crs = ICES_SPATIAL_CRS))
}

spatial_relation <- function(support, target, rel_tol = 1e-10,
                             abs_tol_m2 = 1) {
  stopifnot(rel_tol >= 0, rel_tol < 0.01, abs_tol_m2 >= 0)
  support <- sf::st_geometry(normalize_ices_geometry(support))
  n <- length(support)
  out <- data.frame(
    spatial_status = rep("unknown", n),
    support_projected_area_m2 = rep(NA_real_, n),
    overlap_projected_area_m2 = rep(NA_real_, n),
    outside_projected_area_m2 = rep(NA_real_, n),
    sliver_tolerance_m2 = rep(NA_real_, n),
    tolerance_applied = rep(FALSE, n),
    geometry_note = rep(NA_character_, n)
  )
  if (is.null(target) || !length(sf::st_geometry(target))) return(out)
  target <- polygon_union(target)
  if (sf::st_is_empty(target)[[1L]]) return(out)
  if (!isTRUE(sf::st_crs(support) == sf::st_crs(target))) {
    stop("Spatial support and target must use the same CRS")
  }
  for (i in seq_len(n)) {
    if (sf::st_is_empty(support[i])) next
    tryCatch({
      a <- as.numeric(sf::st_area(support[i]))
      tol <- max(abs_tol_m2, rel_tol * a)
      out$support_projected_area_m2[[i]] <- a
      out$sliver_tolerance_m2[[i]] <- tol
      if (is.finite(a) && a > tol) {
      if (length(sf::st_covered_by(support[i], target)[[1L]])) {
        inside <- a
        outside <- 0
      } else if (!length(sf::st_intersects(support[i], target)[[1L]])) {
        inside <- 0
        outside <- a
      } else {
        inside <- sum(as.numeric(sf::st_area(
          sf::st_intersection(support[i], target)
        )))
        outside <- sum(as.numeric(sf::st_area(
          sf::st_difference(support[i], target)
        )))
      }
      out$overlap_projected_area_m2[[i]] <- inside
      out$outside_projected_area_m2[[i]] <- outside
      out$spatial_status[[i]] <- if (outside <= tol) "full" else
        if (inside <= tol) "outside" else "partial"
      out$tolerance_applied[[i]] <- (inside > 0 && inside <= tol) ||
        (outside > 0 && outside <= tol)
      }
    }, error = function(e) {
      out$spatial_status[[i]] <<- "geometry_error"
      out$geometry_note[[i]] <<- conditionMessage(e)
    })
  }
  out
}


# Stock-specific review policy for the RDBES stock-allocation workflow.
# Reference inspection: 24 October 2026; intended default stock system: 2024.
#
# A flag is not evidence that a stock has no landings. The functions below do
# not allocate or split weights. They identify where a generic ICES-area union
# is insufficient. A validated stock-specific polygon can resolve a "geometry"
# flag, but cannot resolve a "nonspatial" flag by itself.
#
# Current vocabulary relations are not a historical snapshot, even when the
# active-stock list is requested for 2024. Keep retrieved tables in the cache,
# record the retrieval date, and describe the output as allocation to the chosen
# reference stock system. Do not imply that historical boundaries were recovered.

stock_risk_registry_v2 <- function(reference_year = 2024L) {
  sid <- "https://stockdatabase.ices.dk/Manage/rollingissues.aspx"
  rows <- list()
  add <- function(stocks, flag, action, note, areas = "", taxon = "",
                  source = sid) {
    z <- expand.grid(fishstock = stocks, restricted_area = areas,
                     stringsAsFactors = FALSE)
    z$risk_class <- flag
    z$action <- action
    z$restricted_taxon <- taxon
    z$reason <- note
    z$source_url <- source
    rows[[length(rows) + 1L]] <<- z
  }

  add(paste0("nep.fu.", c("3-4", "5", "6", "7", "8", "9", "10",
                          "11", "12", "13", "14", "15", "16", "17",
                          "19", "2021", "22", "2324", "25", "2627",
                          "2829", "30", "31", "32", "33", "34")),
      "functional_unit", "geometry",
      "Use the assessment FU geometry; a related ICES division is not the FU boundary.")
  add(c("nep.27.4outFU", "nep.27.6aoutFU", "nep.27.7outFU"),
      "outside_functional_units", "geometry",
      paste("Use the validated complement of all applicable FUs within the",
            "stock's parent area; missing FU membership is not proof of outside."))
  add(paste0("san.sa.", c("1r", "2r", "3r", "4", "5r", "6", "7r")),
      "sandeel_management_area", "geometry",
      paste("Use year-appropriate sandeel-area rectangle definitions; ICES",
            "division links do not define management-area membership."),
      source = "https://eur-lex.europa.eu/eli/reg/2024/257/oj/eng")
  add(c("ane.27.9aS", "ane.27.9aW"), "subdivision_of_9a", "geometry",
      "The western and southern anchovy stock boundaries subdivide Division 9.a.",
      areas = "27.9.a", source = "https://asd.ices.dk/viewAdvice/3820")
  add(c("cap.27.1-2", "cap.27.2a514"), "longitude_split", "geometry",
      "Division 2.a is split at 5 degrees W; generic 2.a membership is insufficient.",
      areas = "27.2.a")
  add(c("pra.27.4a", "pra.27.3a4a"), "east_west_stock_boundary", "geometry",
      "Eastern and western 4.a shrimp stock areas require their specific boundary.",
      areas = "27.4.a")
  add(c("her.27.nirs", "her.27.irls"), "latitude_split", "geometry",
      "Irish Sea herring allocation subdivides 7.a at 52 degrees 30 minutes N.",
      areas = "27.7.a")
  add(c("her.27.6aN", "her.27.6aS7bc"), "partial_division_and_stock_structure",
      "geometry", paste("Use a documented 6.a stock-allocation rule based on",
                        "56 degrees N/7 degrees W; label geographic allocation",
                        "as such, because it is not individual biological stock ID."),
      areas = "27.6.a",
      source = "https://ices-library.figshare.com/ndownloader/files/54092687")

  # These are targeted known problem areas, not a blacklist of all herring/cod.
  add(c("her.27.20-24", "her.27.3a47d"), "mixed_spawning_stocks",
      "nonspatial", paste("North Sea autumn and western Baltic spring herring",
                          "mix in fisheries; generic spatial containment alone",
                          "does not establish the stock of origin."),
      areas = c("27.3.a", "27.4"),
      source = "https://ices-library.figshare.com/ndownloader/files/46738096")
  add(c("her.27.1-24a514a", "her.27.3a47d"), "overlapping_spawning_stocks",
      "nonspatial", "A stock-origin allocation rule is needed where spawning stocks overlap.",
      areas = "27.4.a")
  add(c("her.27.1-24a514a", "her.27.5a"), "overlapping_spawning_stocks",
      "nonspatial", "Norwegian spring and Icelandic summer stock descriptions overlap in 5.a.",
      areas = "27.5.a")
  add(c("cod.27.1-2", "cod.27.1-2.coastN", "cod.27.2.coastS"),
      "coastal_and_offshore_stock_identity", "nonspatial",
      "Coastal and Northeast Arctic cod cannot be separated by generic ICES-area membership.",
      areas = c("27.1", "27.2"))
  add(c("cod.27.22-24", "cod.27.24-32"), "mixed_baltic_cod", "nonspatial",
      "Eastern and western Baltic cod overlap in Subdivision 24; stock allocation needs more than a polygon.",
      areas = "27.3.d.24")
  add("cod.27.21", "kattegat_area_link_too_broad", "geometry",
      "The stock is Subdivision 21; the current vocabulary relation to all 3.a is too broad.",
      areas = "27.3.a")
  add(c("cod.27.21", "cod.27.46a7d20"), "kattegat_cod_stock_mixing",
      "nonspatial", "Northern shelf cod juveniles mix with Kattegat cod; stock ID needs an allocation rule.",
      areas = "27.3.a.21", source = "https://asd.ices.dk/viewAdvice/4028?handler=SubComm")
  add("cod.27.46a7d20", "aggregate_stock_only", "audit",
      "A stock-level estimate does not separate Northwestern, Southern, or Viking assessment components.",
      source = "https://asd.ices.dk/viewAdvice/4028?handler=SubComm")
  add(c("reb.2127.dp", "reb.2127.sp", "reb.27.5a14", "reb.27.14b"),
      "depth_and_habitat_stock_identity", "nonspatial",
      "Shallow/deep pelagic and demersal/slope Sebastes mentella stocks cannot be separated by horizontal areas alone.",
      source = "https://asd.ices.dk/viewAdvice/3737")
  add(c("seh.27.1", "seh.27.125a14"), "overlapping_seal_stocks", "nonspatial",
      "The listed stock areas overlap; retain unresolved unless an applicable stock-ID rule is supplied.",
      areas = "27.1")
  add(c("cyo.27.nea", "guq.27.nea"), "overlapping_taxon_aggregation",
      "nonspatial", paste("Centrophorus squamosus is linked to both a combined",
                          "shark reference and a species-specific stock; choose",
                          "a consistent reporting system before allocation."),
      taxon = "105901")

  # Audits alone do not remove legitimate assignments in unaffected areas.
  # Competing old/new candidates must still prevent unique assignment.
  if (reference_year >= 2024L) {
    add(c("ane.27.9a", "ane.27.9aS", "ane.27.9aW"),
        "reference_stock_split", "audit",
        paste("The 2024 SID list can contain both the original anchovy stock",
              "and its replacements. Do not select the first or closest match."),
        source = "https://vocab.ices.dk/?CodeID=169052&CodeTypeRelID=102")
    add(c("bli.27.nea", "bli.27.5b67", "bli.27.123a4", "bli.27.5b6712"),
        "reference_stock_reconfiguration", "audit",
        paste("Blue ling reference definitions changed in 2024. Overlapping",
              "old/new entries require a documented stock-system choice."),
        source = "https://ices-library.figshare.com/ndownloader/files/49267138")
  }
  ans <- do.call(rbind, rows)
  rownames(ans) <- NULL
  ans
}

stock_mapping_policy_v2 <- function(candidates, reference_year = 2024L,
                                    registry = stock_risk_registry_v2(reference_year)) {
  stopifnot(all(c("fishstock", "CLarea") %in% names(candidates)))
  ans <- candidates
  n <- nrow(ans)
  ans$area_only_allowed <- rep(TRUE, n)
  ans$specific_spatial_rule_required <- rep(FALSE, n)
  ans$nonspatial_rule_required <- rep(FALSE, n)
  ans$review_flag <- rep("", n)
  ans$review_reason <- rep("", n)
  ans$review_sources <- rep("", n)
  stock <- as.character(ans$fishstock)
  area <- trimws(as.character(ans$CLarea))
  taxon <- if ("CLspeciesCode" %in% names(ans)) {
    as.character(ans$CLspeciesCode)
  } else rep(NA_character_, n)
  append_note <- function(old, new) ifelse(nzchar(old), paste(old, new, sep = "; "), new)
  for (i in seq_len(nrow(registry))) {
    r <- registry[i, , drop = FALSE]
    hit <- !is.na(stock) & stock == r$fishstock
    if (nzchar(r$restricted_area)) {
      # Coarse observations that contain the restricted area remain subject to
      # the restriction. Dots prevent e.g. division 2 matching subarea 20.
      a <- r$restricted_area
      spatial_hit <- is.na(area) | !nzchar(area) | area == a |
        startsWith(area, paste0(a, ".")) | startsWith(a, paste0(area, "."))
      hit <- hit & spatial_hit
    }
    if (nzchar(r$restricted_taxon)) {
      hit <- hit & !is.na(taxon) & taxon == r$restricted_taxon
    }
    hit[is.na(hit)] <- FALSE
    if (!any(hit)) next
    ans$review_flag[hit] <- append_note(ans$review_flag[hit], r$risk_class)
    ans$review_reason[hit] <- append_note(ans$review_reason[hit], r$reason)
    ans$review_sources[hit] <- append_note(ans$review_sources[hit], r$source_url)
    if (r$action == "geometry") ans$specific_spatial_rule_required[hit] <- TRUE
    if (r$action == "nonspatial") ans$nonspatial_rule_required[hit] <- TRUE
    if (r$action != "audit") ans$area_only_allowed[hit] <- FALSE
  }
  # Apply the FU requirement also to any new FU absent from the enumerated list.
  extra_fu <- grepl("^nep[.]fu[.]", stock) & !ans$specific_spatial_rule_required
  extra_fu[is.na(extra_fu)] <- FALSE
  ans$area_only_allowed[extra_fu] <- FALSE
  ans$specific_spatial_rule_required[extra_fu] <- TRUE
  ans$review_flag[extra_fu] <- append_note(ans$review_flag[extra_fu], "functional_unit")
  ans$review_reason[extra_fu] <- append_note(ans$review_reason[extra_fu],
      "A validated boundary is required for this FU; it is absent from the inspected registry.")
  # A newly introduced sandeel management-area code must not inherit permission
  # to use the deliberately broad 3+4 candidate envelope as a stock boundary.
  extra_san <- grepl("^san[.]sa[.]", stock) & !ans$specific_spatial_rule_required
  extra_san[is.na(extra_san)] <- FALSE
  ans$area_only_allowed[extra_san] <- FALSE
  ans$specific_spatial_rule_required[extra_san] <- TRUE
  ans$review_flag[extra_san] <- append_note(ans$review_flag[extra_san], "sandeel_management_area")
  ans$review_reason[extra_san] <- append_note(ans$review_reason[extra_san],
      "A validated management-area boundary is required; this code is absent from the inspected registry.")
  ans$review_flag[!nzchar(ans$review_flag)] <- "none"
  ans
}

# The official stock-species table is already a membership list. Exact matches
# to either of two explicitly listed species are legitimate. A genus ID must
# not be expanded into guessed members, and a grouped landing must not be
# divided between its constituent species without evidence.
stock_taxonomy_review_v2 <- function(stock_list, stock_species) {
  stopifnot(all(c("StockKeyLabel", "SpeciesScientificName") %in% names(stock_list)),
            all(c("fishstock", "CLspeciesCode") %in% names(stock_species)))
  d <- unique(stock_list[c("StockKeyLabel", "SpeciesScientificName")])
  d <- d[!duplicated(d$StockKeyLabel), , drop = FALSE]
  names(d)[1L] <- "fishstock"
  d$taxon_membership_ids <- vapply(d$fishstock, function(s) {
    x <- as.character(stock_species$CLspeciesCode[stock_species$fishstock == s])
    paste(sort(unique(x[!is.na(x) & nzchar(x)])), collapse = ";")
  }, character(1L))
  scientific_name <- trimws(as.character(d$SpeciesScientificName))
  d$taxonomy_review <- ifelse(
    !is.na(scientific_name) & grepl("^[A-Za-z-]+$", scientific_name),
    "coarse_taxon_exact_match_only_no_automatic_descendant_expansion",
    ifelse(grepl(",", scientific_name),
           "multiple_species_use_only_explicit_reference_memberships", "none")
  )
  d$taxonomy_review[is.na(d$taxonomy_review)] <- "missing_species_description"
  d
}

# Optional input helper: use a UNIQUE official ASFIS -> WoRMS relation only
# when the supplied AphiaID is missing. Network fetching is deliberately outside
# this helper so the caller can cache the raw table and document its retrieval.
# fetch: icesVocab::getCodeTypeRelation("SpecASFIS", "SpecWoRMS")
# endpoint: https://vocab.ices.dk/services/api/CodeType/Relation/SpecASFIS/SpecWoRMS
# On 24 Oct 2026 this returned 2,387 links for 2,386 FAO codes; AUX was ambiguous.
resolve_fao_worms_v2 <- function(rdbes, asfis_worms) {
  if (all(c("SpecASFIS", "SpecWoRMS") %in% names(asfis_worms))) {
    ref <- asfis_worms[c("SpecASFIS", "SpecWoRMS")]
  } else if (all(c("firstKey", "secondKey") %in% names(asfis_worms))) {
    ref <- asfis_worms[c("firstKey", "secondKey")]
  } else stop("Expected the official SpecASFIS -> SpecWoRMS relation table")
  names(ref) <- c("fao", "aphia")
  ref$fao <- toupper(trimws(as.character(ref$fao)))
  ref$aphia <- trimws(as.character(ref$aphia))
  ref <- unique(ref[!is.na(ref$fao) & nzchar(ref$fao) &
                     !is.na(ref$aphia) & nzchar(ref$aphia), , drop = FALSE])
  mapping <- split(ref$aphia, ref$fao)
  ans <- rdbes
  n <- nrow(ans)
  if (!"CLspeciesCode" %in% names(ans)) ans$CLspeciesCode <- rep(NA_character_, n)
  ans$CLspeciesCode <- trimws(as.character(ans$CLspeciesCode))
  supplied <- !is.na(ans$CLspeciesCode) & nzchar(ans$CLspeciesCode)
  fao <- if ("CLspeciesFaoCode" %in% names(ans)) {
    toupper(trimws(as.character(ans$CLspeciesFaoCode)))
  } else rep(NA_character_, n)
  unique_aphia <- vapply(fao, function(f) {
    if (is.na(f) || !nzchar(f)) return(NA_character_)
    x <- unique(mapping[[f]])
    if (length(x) == 1L) x else NA_character_
  }, character(1L))
  link_count <- vapply(fao, function(f) {
    if (is.na(f) || !nzchar(f)) 0L else length(unique(mapping[[f]]))
  }, integer(1L))
  fill <- !supplied & !is.na(unique_aphia)
  ans$CLspeciesCode[fill] <- unique_aphia[fill]
  ans$species_resolution_status <- ifelse(supplied, "supplied_aphia",
      ifelse(fill, "unique_official_asfis_crosswalk",
             ifelse(link_count > 1L, "ambiguous_asfis_crosswalk", "no_usable_taxon")))
  ans$species_code_conflict <- supplied & !is.na(unique_aphia) &
    ans$CLspeciesCode != unique_aphia
  ans$species_resolution_status[ans$species_code_conflict] <- "supplied_aphia_conflicts_with_asfis"
  # Conflicting input codes are retained for review, not silently overwritten.
  ans
}


# Audit only: never allocates unresolved records or changes source assignments.
# Requires dplyr >= 1.1.1. Call with source_rows = dat (the original aggregated
# input) to verify source-to-result row and weight conservation.
#
# ShareLandingsInEcoregion = inside / (inside + outside), among UNIQUE records
# with resolved ecoregion location. AssignedShareLower_pct and Upper_pct use ALL
# uniquely assigned weight: inside / total and (inside + boundary + unknown) /
# total. These are conditional bounds for the assigned subset, not bounds for
# all biological-stock landings or for unresolved stock assignments.
#
# unresolved_candidate_exposure_kg can occur against several stocks. It is NOT
# allocated stock landings and must NEVER be summed across stocks.
# Existing CS classifications are audited, not independently spatially checked.

audit_rdbes_stock_results <- function(dat_assigned, candidates, active_stocks,
                                     stock_species, stock_areas,
                                     stock_flags = NULL, source_rows = NULL,
                                     weight_tolerance = 1e-8) {
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      utils::packageVersion("dplyr") < "1.1.1") stop("dplyr >= 1.1.1 is required.")
  need <- function(x, cols, label) {
    if (!is.data.frame(x)) stop(label, " must be a data frame.")
    missing <- setdiff(cols, names(x))
    if (length(missing)) stop(label, " lacks: ", paste(missing, collapse = ", "))
  }
  clean <- function(x) trimws(as.character(x))
  good <- function(x) !is.na(x) & nzchar(x)
  year <- function(x) {
    y <- suppressWarnings(as.numeric(as.character(x)))
    if (any(!is.finite(y) | y != floor(y) | y < 1000 | y > 9999)) stop("Invalid CLyear.")
    as.integer(y)
  }
  weights_ok <- function(x, label) {
    if (!is.numeric(x) || any(!is.finite(x) | x < 0) || !is.finite(sum(x))) {
      stop(label, " weights must be numeric, finite, nonmissing and nonnegative.")
    }
  }
  close <- function(x, y) abs(x - y) <= weight_tolerance * pmax(1, abs(x), abs(y))
  if (length(weight_tolerance) != 1L || !is.finite(weight_tolerance) || weight_tolerance < 0) {
    stop("weight_tolerance must be one finite, nonnegative number.")
  }
  need(dat_assigned, c("row_id", "CLyear", "CLscientificWeight", "CLspeciesCode",
    "CLspeciesFaoCode", "CLarea", "CLstatisticalRectangle", "fishstock", "match_status", "CS"), "dat_assigned")
  need(candidates, c("row_id", "fishstock", "assignment_evidence", "reason"), "candidates")
  need(active_stocks, c("CLyear", "fishstock"), "active_stocks")
  need(stock_species, c("fishstock", "CLspeciesCode"), "stock_species")
  need(stock_areas, c("fishstock", "ICES_Area"), "stock_areas")
  weights_ok(dat_assigned$CLscientificWeight, "Output")
  d <- dplyr::mutate(dplyr::ungroup(dat_assigned), row_id = clean(row_id),
    CLyear = year(CLyear), fishstock = clean(fishstock), CLspeciesCode = clean(CLspeciesCode),
    match_status = clean(match_status), CS = clean(CS))
  if (any(!good(d$row_id)) || anyDuplicated(d$row_id)) stop("Output row IDs must be nonmissing and unique.")
  if (any(!good(d$match_status))) stop("Every output row needs a match_status.")
  if (any(!(d$CS %in% c("inside", "outside", "boundary", "unknown")))) {
    stop("CS must be inside, outside, boundary, or unknown; convert missing locations to unknown.")
  }
  is_unique <- d$match_status == "unique"
  if (any(is_unique & !good(d$fishstock)) || any(!is_unique & good(d$fishstock))) {
    stop("fishstock must be populated exactly for match_status == 'unique'.")
  }
  a <- dplyr::distinct(dplyr::transmute(dplyr::ungroup(active_stocks),
    CLyear = year(CLyear), fishstock = clean(fishstock)))
  if (!nrow(a) || any(!good(a$fishstock))) stop("active_stocks needs valid stock-year rows.")
  sp <- dplyr::distinct(dplyr::transmute(dplyr::ungroup(stock_species),
    fishstock = clean(fishstock), CLspeciesCode = clean(CLspeciesCode)))
  ar <- dplyr::distinct(dplyr::transmute(dplyr::ungroup(stock_areas),
    fishstock = clean(fishstock), ICES_Area = clean(ICES_Area)))
  sp <- dplyr::filter(sp, good(fishstock), good(CLspeciesCode))
  ar <- dplyr::filter(ar, good(fishstock), good(ICES_Area))
  cand <- dplyr::transmute(dplyr::ungroup(candidates), row_id = clean(row_id),
    fishstock = clean(fishstock), assignment_evidence = clean(assignment_evidence), reason = as.character(reason))
  if (any(!good(cand$row_id)) || any(!good(cand$fishstock)) ||
      anyDuplicated(cand[c("row_id", "fishstock")])) stop("Candidates need unique row_id/stock keys.")
  if (any(!(cand$assignment_evidence %in% c("full", "partial", "unknown", "outside")))) {
    stop("Candidate evidence must be full, partial, unknown, or outside.")
  }
  if (nrow(dplyr::anti_join(cand, d, by = "row_id"))) stop("A candidate row ID is absent from output.")
  cand <- dplyr::left_join(cand, dplyr::select(d, row_id, CLyear, CLscientificWeight, match_status),
    by = "row_id", relationship = "many-to-one")
  if (nrow(dplyr::anti_join(cand, a, by = c("CLyear", "fishstock")))) stop("A candidate stock-year is inactive.")
  possible <- dplyr::filter(cand, assignment_evidence != "outside")
  unique_data <- dplyr::filter(d, match_status == "unique")
  chosen <- dplyr::filter(possible, assignment_evidence == "full")
  if (nrow(dplyr::anti_join(unique_data, chosen, by = c("row_id", "fishstock")))) {
    stop("Every unique assignment must have a full-evidence candidate.")
  }
  possible_n <- dplyr::count(possible, row_id, name = "n_possible")
  unique_check <- dplyr::left_join(unique_data, possible_n, by = "row_id", relationship = "one-to-one")
  if (any(unique_check$n_possible != 1L)) stop("A unique assignment still has competing possible candidates.")

  # Source comparison is necessary: checking only the output cannot detect lost rows.
  source_present <- !is.null(source_rows)
  d$source_weight_kg <- rep(NA_real_, nrow(d))
  d$row_weight_abs_difference <- rep(NA_real_, nrow(d))
  if (source_present) {
    need(source_rows, c("row_id", "CLscientificWeight"), "source_rows")
    weights_ok(source_rows$CLscientificWeight, "Source")
    src <- dplyr::transmute(dplyr::ungroup(source_rows), row_id = clean(row_id),
      source_weight_kg = CLscientificWeight)
    if (any(!good(src$row_id)) || anyDuplicated(src$row_id)) stop("Source row IDs must be nonmissing and unique.")
    if (nrow(dplyr::anti_join(src, d, by = "row_id")) || nrow(dplyr::anti_join(d, src, by = "row_id"))) {
      stop("Source/output row IDs differ: records were lost or introduced.")
    }
    d <- dplyr::left_join(dplyr::select(d, -source_weight_kg), src, by = "row_id", relationship = "one-to-one")
    d$row_weight_abs_difference <- abs(d$CLscientificWeight - d$source_weight_kg)
    if (any(!close(d$CLscientificWeight, d$source_weight_kg))) stop("Source/output row weights differ.")
    if ("CLyear" %in% names(source_rows)) {
      source_years <- dplyr::transmute(dplyr::ungroup(source_rows), row_id = clean(row_id), original_year = year(CLyear))
      year_check <- dplyr::left_join(dplyr::select(d, row_id, CLyear), source_years, by = "row_id", relationship = "one-to-one")
      if (any(year_check$CLyear != year_check$original_year)) stop("Source/output record years differ.")
    }
  }

  exposure <- dplyr::summarise(possible, candidate_rows = dplyr::n(),
    unresolved_candidate_rows = sum(match_status != "unique"),
    unresolved_candidate_exposure_kg = sum(CLscientificWeight[match_status != "unique"]),
    .by = c(CLyear, fishstock))
  allocated <- dplyr::summarise(unique_data, unique_rows = dplyr::n(),
    uniquely_assigned_kg = sum(CLscientificWeight),
    Inside_kg = sum(CLscientificWeight[CS == "inside"]),
    Outside_kg = sum(CLscientificWeight[CS == "outside"]),
    Boundary_kg = sum(CLscientificWeight[CS == "boundary"]),
    Unknown_kg = sum(CLscientificWeight[CS == "unknown"]), .by = c(CLyear, fishstock))
  out <- dplyr::left_join(a, dplyr::count(sp, fishstock, name = "n_species_reference"),
    by = "fishstock", relationship = "many-to-one")
  out <- dplyr::left_join(out, dplyr::count(ar, fishstock, name = "n_area_reference"),
    by = "fishstock", relationship = "many-to-one")
  out <- dplyr::left_join(out, exposure, by = c("CLyear", "fishstock"), relationship = "one-to-one")
  out <- dplyr::left_join(out, allocated, by = c("CLyear", "fishstock"), relationship = "one-to-one")
  count_cols <- c("n_species_reference", "n_area_reference", "candidate_rows", "unresolved_candidate_rows", "unique_rows")
  weight_cols <- c("unresolved_candidate_exposure_kg", "uniquely_assigned_kg", "Inside_kg", "Outside_kg", "Boundary_kg", "Unknown_kg")
  out <- dplyr::mutate(out, dplyr::across(dplyr::all_of(count_cols), ~ dplyr::coalesce(.x, 0L)),
    dplyr::across(dplyr::all_of(weight_cols), ~ dplyr::coalesce(.x, 0)))
  out$review_flag <- NA_character_
  if (!is.null(stock_flags)) {
    need(stock_flags, c("fishstock", "review_flag"), "stock_flags")
    flags <- dplyr::transmute(dplyr::ungroup(stock_flags), fishstock = clean(fishstock), review_flag = clean(review_flag))
    flags <- dplyr::summarise(dplyr::filter(flags, good(fishstock), good(review_flag)),
      review_flag = paste(sort(unique(review_flag)), collapse = "; "), .by = fishstock)
    out <- dplyr::left_join(dplyr::select(out, -review_flag), flags, by = "fishstock", relationship = "many-to-one")
  }
  out <- dplyr::mutate(out, has_species_reference = n_species_reference > 0L,
    has_area_reference = n_area_reference > 0L,
    has_reference_review_flag = good(review_flag),
    Geolocated_kg = Inside_kg + Outside_kg,
    ShareLandingsInEcoregion = dplyr::if_else(Geolocated_kg > 0, 100 * Inside_kg / Geolocated_kg, NA_real_),
    AssignedShareLower_pct = dplyr::if_else(uniquely_assigned_kg > 0, 100 * Inside_kg / uniquely_assigned_kg, NA_real_),
    AssignedShareUpper_pct = dplyr::if_else(uniquely_assigned_kg > 0,
      100 * (Inside_kg + Boundary_kg + Unknown_kg) / uniquely_assigned_kg, NA_real_),
    Geo_coverage_pct = dplyr::if_else(uniquely_assigned_kg > 0, 100 * Geolocated_kg / uniquely_assigned_kg, NA_real_),
    has_unresolved_assignment_exposure = unresolved_candidate_exposure_kg > 0,
    has_spatial_uncertainty = Boundary_kg + Unknown_kg > 0,
    share_quality = dplyr::case_when(
      !has_species_reference | !has_area_reference ~ "missing_vocabulary_reference",
      has_reference_review_flag ~ "reference_review_required",
      candidate_rows == 0L ~ "no_candidate_records",
      unique_rows == 0L ~ "no_unique_assignments",
      uniquely_assigned_kg <= 0 ~ "no_positive_assigned_landings",
      has_unresolved_assignment_exposure ~ "unresolved_assignment_exposure",
      Geolocated_kg <= 0 ~ "no_spatially_resolved_landings",
      has_spatial_uncertainty ~ "spatial_uncertainty_in_assigned_subset",
      TRUE ~ "resolved_assigned_subset"))
  out <- dplyr::arrange(out, CLyear, fishstock)

  candidate_lists <- dplyr::summarise(possible,
    possible_stocks = paste(sort(unique(fishstock)), collapse = "; "), .by = row_id)
  unresolved <- dplyr::left_join(dplyr::select(dplyr::filter(d, match_status != "unique"),
    -dplyr::any_of(c("possible_stocks", "source_weight_kg", "row_weight_abs_difference"))),
    candidate_lists, by = "row_id", relationship = "one-to-one")
  quality <- dplyr::summarise(d, n_records = dplyr::n(), Landings_kg = sum(CLscientificWeight),
    .by = c(CLyear, CLspeciesCode, CLspeciesFaoCode, CLarea, match_status))
  quality <- dplyr::group_by(quality, CLyear, CLspeciesCode, CLspeciesFaoCode, CLarea)
  quality <- dplyr::ungroup(dplyr::mutate(quality, Share_within_species_area_pct =
    if (sum(Landings_kg) > 0) 100 * Landings_kg / sum(Landings_kg) else NA_real_))

  keys <- dplyr::inner_join(a, sp, by = "fishstock", relationship = "many-to-many")
  taxon_gaps <- dplyr::anti_join(d, dplyr::distinct(keys, CLyear, CLspeciesCode),
    by = c("CLyear", "CLspeciesCode"))
  taxon_gaps <- dplyr::summarise(taxon_gaps, n_records = dplyr::n(), Landings_kg = sum(CLscientificWeight),
    .by = c(CLyear, CLspeciesCode, CLspeciesFaoCode))
  taxon_gaps <- dplyr::mutate(taxon_gaps, fishstock = NA_character_,
    gap_type = dplyr::if_else(good(CLspeciesCode), "no_exact_active_stock_taxon_link", "missing_observed_taxon_code"),
    detail = "An absent exact taxon link may reflect an unassessed taxon or a taxonomic grouping; it is not proof of an error.")
  stock_gaps <- dplyr::transmute(dplyr::filter(out, !has_species_reference | !has_area_reference),
    CLyear, fishstock, CLspeciesCode = NA_character_, CLspeciesFaoCode = NA_character_,
    n_records = NA_integer_, Landings_kg = NA_real_, gap_type = "missing_stock_vocabulary_reference",
    detail = paste0("species_reference=", has_species_reference, "; area_reference=", has_area_reference))
  gaps <- dplyr::bind_rows(stock_gaps, taxon_gaps)
  # Hierarchical area overlaps are association-level warnings, not population overlap proof.
  pairs <- dplyr::inner_join(keys, keys, by = c("CLyear", "CLspeciesCode"),
    suffix = c("_a", "_b"), relationship = "many-to-many")
  pairs <- dplyr::filter(pairs, fishstock_a < fishstock_b)
  pairs <- dplyr::inner_join(pairs, dplyr::rename(ar, fishstock_a = fishstock, area_a = ICES_Area),
    by = "fishstock_a", relationship = "many-to-many")
  pairs <- dplyr::inner_join(pairs, dplyr::rename(ar, fishstock_b = fishstock, area_b = ICES_Area),
    by = "fishstock_b", relationship = "many-to-many")
  pairs <- dplyr::mutate(pairs, area_relation = dplyr::case_when(
    area_a == area_b ~ "same_area", startsWith(area_a, paste0(area_b, ".")) ~ "area_a_within_b",
    startsWith(area_b, paste0(area_a, ".")) ~ "area_b_within_a", TRUE ~ "no_hierarchical_overlap"))
  overlaps <- dplyr::distinct(dplyr::filter(pairs, area_relation != "no_hierarchical_overlap"))

  balance <- dplyr::summarise(d, source_comparison_available = source_present,
    output_rows = dplyr::n(), output_kg = sum(CLscientificWeight),
    source_kg = if (source_present) sum(source_weight_kg) else NA_real_,
    unique_kg = sum(CLscientificWeight[match_status == "unique"]),
    unresolved_kg = sum(CLscientificWeight[match_status != "unique"]),
    max_row_weight_abs_difference = if (source_present) max(c(0, row_weight_abs_difference)) else NA_real_, .by = CLyear)
  stock_totals <- dplyr::summarise(out, stock_output_unique_kg = sum(uniquely_assigned_kg), .by = CLyear)
  balance <- dplyr::left_join(balance, stock_totals, by = "CLyear", relationship = "one-to-one")
  balance <- dplyr::mutate(balance, stock_output_unique_kg = dplyr::coalesce(stock_output_unique_kg, 0))
  # Include a global conservation row even when the input has zero records.
  global <- dplyr::tibble(CLyear = NA_integer_, source_comparison_available = source_present,
    output_rows = nrow(d), output_kg = sum(d$CLscientificWeight),
    source_kg = if (source_present) sum(d$source_weight_kg) else NA_real_,
    unique_kg = sum(d$CLscientificWeight[d$match_status == "unique"]),
    unresolved_kg = sum(d$CLscientificWeight[d$match_status != "unique"]),
    max_row_weight_abs_difference = if (source_present) max(c(0, d$row_weight_abs_difference)) else NA_real_,
    stock_output_unique_kg = sum(out$uniquely_assigned_kg))
  balance <- dplyr::mutate(dplyr::bind_rows(balance, global), scope = ifelse(is.na(CLyear), "all_years", "year"),
    source_mass_preserved = if (source_present) close(output_kg, source_kg) else NA,
    partition_mass_preserved = close(output_kg, unique_kg + unresolved_kg),
    stock_mass_preserved = close(unique_kg, stock_output_unique_kg))
  if (any(!balance$partition_mass_preserved | !balance$stock_mass_preserved) ||
      (source_present && any(!balance$source_mass_preserved))) stop("Mass-conservation checks failed.")
  if (any(!close(out$uniquely_assigned_kg, out$Inside_kg + out$Outside_kg + out$Boundary_kg + out$Unknown_kg))) {
    stop("Stock geographical categories do not conserve uniquely assigned mass.")
  }
  list(stock_audit = out, unresolved_records = unresolved, species_area_quality = quality,
    reference_gaps = gaps, reference_overlaps = overlaps, mass_balance = balance)
}


# Core preparation, assignment, Nephrops integration, and workflow orchestration.

check_workflow_packages <- function() {
  pkgs <- c("dplyr", "tidyr", "sf", "httr", "jsonlite", "icesVocab", "icesSD")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install missing packages: ", paste(missing, collapse = ", "))
  if (utils::packageVersion("dplyr") < "1.1.1") stop("dplyr >= 1.1.1 is required.")
  invisible(TRUE)
}

clean_character <- function(x, lower = FALSE, upper = FALSE) {
  x <- trimws(as.character(x))
  x[is.na(x) | x %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  if (lower) x <- tolower(x)
  if (upper) x <- toupper(x)
  x
}

collapse_values <- function(x) {
  x <- sort(unique(as.character(x[!is.na(x) & nzchar(x)])))
  if (length(x)) paste(x, collapse = "; ") else NA_character_
}

cached_reference <- function(path, fetch, source, refresh = FALSE) {
  if (file.exists(path) && !refresh) {
    obj <- readRDS(path)
    if (is.list(obj) && identical(obj$schema, "rdbes-v2-reference") &&
        identical(obj$source, source) && is.data.frame(obj$data)) return(obj)
  }
  value <- fetch()
  if (!is.data.frame(value) || nrow(value) == 0L) stop("Empty reference: ", source)
  obj <- list(schema = "rdbes-v2-reference", source = source,
    retrieved_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), data = value)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(obj, path)
  obj
}

get_workflow_references <- function(config) {
  specs <- list(
    stock_species = c("ICES_StockCode", "SpecWoRMS"),
    stock_areas = c("ICES_StockCode", "ICES_Area"),
    asfis_worms = c("SpecASFIS", "SpecWoRMS")
  )
  raw <- lapply(names(specs), function(nm) {
    types <- specs[[nm]]
    cached_reference(file.path(config$cache_dir, paste0(nm, ".rds")),
      function() icesVocab::getCodeTypeRelation(types[1], types[2]),
      paste0("https://vocab.ices.dk/services/api/CodeType/Relation/",
             paste(types, collapse = "/")), config$refresh_reference)
  })
  names(raw) <- names(specs)
  raw$stock_list <- cached_reference(
    file.path(config$cache_dir, paste0("stock_list_", config$reference_year, ".rds")),
    function() icesSD::getSD(year = config$reference_year),
    paste0("ICES Stock Database active list ", config$reference_year),
    config$refresh_reference)
  stock_list <- as.data.frame(raw$stock_list$data)
  if (!"StockKeyLabel" %in% names(stock_list)) stop("Stock database lacks StockKeyLabel.")
  stock_list$StockKeyLabel <- clean_character(stock_list$StockKeyLabel)
  stock_list <- stock_list[!is.na(stock_list$StockKeyLabel), , drop = FALSE]
  if (!"StockKeyDescription" %in% names(stock_list)) stock_list$StockKeyDescription <- NA_character_
  if (!"SpeciesScientificName" %in% names(stock_list)) stock_list$SpeciesScientificName <- NA_character_
  selected <- sort(unique(stock_list$StockKeyLabel))
  if (length(config$exclude_stock_codes)) {
    selected <- setdiff(selected, config$exclude_stock_codes)
  }
  if (!length(selected)) stop("No stocks remain in the selected reference stock system.")
  stock_species <- raw$stock_species$data |>
    dplyr::transmute(fishstock = clean_character(ICES_StockCode),
      CLspeciesCode = clean_character(SpecWoRMS)) |>
    dplyr::filter(fishstock %in% selected, !is.na(CLspeciesCode)) |>
    dplyr::distinct()
  stock_areas <- raw$stock_areas$data |>
    dplyr::transmute(fishstock = clean_character(ICES_StockCode),
      ICES_Area = clean_character(ICES_Area, lower = TRUE)) |>
    dplyr::filter(fishstock %in% selected, !is.na(ICES_Area)) |>
    dplyr::distinct()
  list(raw = raw, stock_list = stock_list, selected_stocks = selected,
    stock_species = stock_species, stock_areas = stock_areas,
    asfis_worms = raw$asfis_worms$data)
}

prepare_rdbes_landings <- function(source, asfis_worms, config) {
  source <- as.data.frame(source)
  required <- c("CLyear", "CLarea", "CLstatisticalRectangle", "CLscientificWeight")
  absent <- setdiff(required, names(source))
  if (length(absent)) stop("RDBES input lacks: ", paste(absent, collapse = ", "))
  if (!any(c("CLspeciesCode", "CLspeciesFaoCode") %in% names(source))) {
    stop("Provide CLspeciesCode (WoRMS AphiaID), CLspeciesFaoCode, or both.")
  }
  if (!is.numeric(source$CLscientificWeight)) stop("CLscientificWeight must be numeric kilograms.")
  n_original <- nrow(source)
  category_present <- "CLcatchCategory" %in% names(source)
  if (category_present) {
    source <- source[tolower(trimws(as.character(source$CLcatchCategory))) %in% "lan", , drop = FALSE]
  } else if (!isTRUE(config$assume_landings_only)) {
    stop("No CLcatchCategory column. Set assume_landings_only=TRUE only for a landings-only input.")
  } else {
    message("CLcatchCategory absent: using the configured landings-only assumption.")
  }
  n_landings <- nrow(source)
  missing_weight_rows <- source[is.na(source$CLscientificWeight), , drop = FALSE]
  source <- source[!is.na(source$CLscientificWeight), , drop = FALSE]
  if (any(!is.finite(source$CLscientificWeight) | source$CLscientificWeight < 0)) {
    stop("Nonfinite or negative landings weights found. Resolve these before calculating shares.")
  }
  y <- suppressWarnings(as.numeric(as.character(source$CLyear)))
  if (any(!is.finite(y) | y != floor(y) | y < 1900 | y > 2200)) stop("Invalid CLyear in input.")
  source$CLyear <- as.integer(y)
  if (!is.null(config$analysis_years)) source <- source[source$CLyear %in% config$analysis_years, , drop = FALSE]
  if (!nrow(source)) stop("No usable landings rows remain after the input filters.")
  source$CLarea <- clean_character(source$CLarea, lower = TRUE)
  source$CLstatisticalRectangle <- clean_character(source$CLstatisticalRectangle, upper = TRUE)
  if (!"CLspeciesFaoCode" %in% names(source)) source$CLspeciesFaoCode <- NA_character_
  source$CLspeciesFaoCode <- clean_character(source$CLspeciesFaoCode, upper = TRUE)
  if (!"CLspeciesCode" %in% names(source)) source$CLspeciesCode <- NA_character_
  source$CLspeciesCode <- clean_character(source$CLspeciesCode)
  # The official ASFIS helper works on distinct code combinations first.
  code_pairs <- dplyr::distinct(source, CLspeciesCode, CLspeciesFaoCode)
  resolved_codes <- resolve_fao_worms_v2(code_pairs, asfis_worms)
  code_pairs$species_pair_id <- seq_len(nrow(code_pairs))
  resolved_codes$species_pair_id <- code_pairs$species_pair_id
  names(resolved_codes)[names(resolved_codes) == "CLspeciesCode"] <- "resolved_species_code"
  source <- source |>
    dplyr::left_join(code_pairs, by = c("CLspeciesCode", "CLspeciesFaoCode"),
      relationship = "many-to-one") |>
    dplyr::rename(CLspeciesCode_supplied = CLspeciesCode) |>
    dplyr::left_join(dplyr::select(resolved_codes, -CLspeciesFaoCode),
      by = "species_pair_id", relationship = "many-to-one") |>
    dplyr::mutate(
      # Retain conflicts in the audit, but do not use them for allocation.
      CLspeciesCode = dplyr::if_else(species_code_conflict, NA_character_, resolved_species_code),
      species_resolution_status = dplyr::if_else(
        !is.na(CLspeciesCode) & !grepl("^[0-9]+$", CLspeciesCode),
        "invalid_aphia_identifier", species_resolution_status),
      CLspeciesCode = dplyr::if_else(species_resolution_status == "invalid_aphia_identifier",
                                   NA_character_, CLspeciesCode)
    )
  dat <- source |>
    dplyr::summarise(CLscientificWeight = sum(CLscientificWeight),
      n_source_rows = dplyr::n(),
      .by = c(CLyear, CLarea, CLstatisticalRectangle, CLspeciesCode,
        CLspeciesFaoCode, CLspeciesCode_supplied, resolved_species_code,
        species_resolution_status, species_code_conflict)) |>
    dplyr::mutate(row_id = dplyr::row_number())
  stopifnot(isTRUE(all.equal(sum(dat$CLscientificWeight), sum(source$CLscientificWeight), tolerance = 1e-12)))
  input_quality <- tibble::tibble(
    metric = c("source_rows", "landings_category_rows", "missing_weight_rows",
      "analysed_source_rows", "aggregated_rows", "analysed_landings_kg", "catch_category_present"),
    value = c(n_original, n_landings, nrow(missing_weight_rows), nrow(source),
      nrow(dat), sum(dat$CLscientificWeight), as.integer(category_present)))
  list(dat = dat, input_quality = input_quality, missing_weight_rows = missing_weight_rows,
       species_code_lookup = dplyr::bind_cols(code_pairs, dplyr::select(resolved_codes,
         resolved_species_code, species_resolution_status, species_code_conflict)))
}

nephrops_fu_lookup <- function() {
  single <- c(5:17, 19, 22, 25, 30:34)
  dplyr::bind_rows(
    tibble::tibble(fu_no = paste0("FU", single), fishstock = paste0("nep.fu.", single)),
    tibble::tribble(~fu_no, ~fishstock,
      "4outFU", "nep.27.4outFU", "6aoutFU", "nep.27.6aoutFU", "7outFU", "nep.27.7outFU",
      "FU3", "nep.fu.3-4", "FU4", "nep.fu.3-4", "FU2021", "nep.fu.2021",
      "FU23", "nep.fu.2324", "FU24", "nep.fu.2324",
      "FU26", "nep.fu.2627", "FU27", "nep.fu.2627",
      "FU28", "nep.fu.2829", "FU29", "nep.fu.2829"))
}

build_nephrops_stock_polygons <- function(shapefile, rectangles,
                                           apply_fu25_revision = TRUE) {
  if (!file.exists(shapefile)) stop("Nephrops shapefile not found: ", shapefile)
  fu_original <- sf::st_read(shapefile, quiet = TRUE)
  if (!"fu_no" %in% names(fu_original)) stop("The Nephrops shapefile lacks fu_no.")
  if (is.na(sf::st_crs(fu_original))) stop("The Nephrops shapefile has no CRS.")
  fu_original <- sf::st_transform(fu_original, ICES_SPATIAL_CRS)
  geometry_audit <- tibble::tibble(fu_no = as.character(fu_original$fu_no),
    original_validity = sf::st_is_valid(fu_original, reason = TRUE))
  fu <- fu_original |>
    dplyr::select(fu_no) |>
    dplyr::mutate(fu_no = trimws(as.character(fu_no))) |>
    normalize_ices_geometry() |>
    dplyr::left_join(nephrops_fu_lookup(), by = "fu_no", relationship = "many-to-one")
  if (anyDuplicated(fu$fu_no)) stop("Duplicate FU labels in the shapefile; inspect before allocation.")
  if (any(sf::st_is_empty(fu))) stop("An empty FU geometry was found.")
  fu$boundary_definition <- "uploaded_ICES_GIS_layer"
  fu$geometry_usable <- TRUE
  correction <- tibble::tibble(fishstock = "nep.fu.25", correction_applied = apply_fu25_revision,
    definition = "ICES FU25 stock annex, revised May 2024; 2021 geographic revision",
    rectangles = "15E0; 15E1; 16E0; 16E1; 17E1",
    source = "https://ices-library.figshare.com/ndownloader/files/48791170")
  i25 <- which(fu$fu_no == "FU25")
  if (length(i25) != 1L) stop("Expected exactly one FU25 feature in this Nephrops layer.")
  if (isTRUE(apply_fu25_revision)) {
    fu25_ids <- c("15E0", "15E1", "16E0", "16E1", "17E1")
    if (!all(fu25_ids %in% rectangles$ICESNAME)) stop("Missing official rectangle polygons for the FU25 correction.")
    replacement <- polygon_union(rectangles[match(fu25_ids, rectangles$ICESNAME), ])
    before <- sf::st_geometry(fu)[i25]
    correction$added_projected_area_m2 <- sum(as.numeric(sf::st_area(sf::st_difference(replacement, before))))
    correction$removed_projected_area_m2 <- sum(as.numeric(sf::st_area(sf::st_difference(before, replacement))))
    sf::st_geometry(fu)[i25] <- replacement
    fu$boundary_definition[i25] <- "FU25_annex_May_2024_five_rectangles"
  } else {
    # An unreconciled source boundary cannot prove inclusion/exclusion for FU25.
    fu$geometry_usable[i25] <- FALSE
    fu$boundary_definition[i25] <- "FU25_unreconciled_GIS_boundary"
    correction$added_projected_area_m2 <- NA_real_
    correction$removed_projected_area_m2 <- NA_real_
  }
  unmapped <- dplyr::filter(fu, is.na(fishstock))
  stocks <- fu |>
    dplyr::filter(!is.na(fishstock)) |>
    dplyr::group_by(fishstock) |>
    dplyr::summarise(fu_members = collapse_values(fu_no),
      boundary_definition = collapse_values(boundary_definition),
      geometry_usable = all(geometry_usable), do_union = TRUE, .groups = "drop")
  expected_members <- nephrops_fu_lookup()
  stocks$expected_fu_members <- vapply(stocks$fishstock, function(s) {
    collapse_values(expected_members$fu_no[expected_members$fishstock == s])
  }, character(1))
  stocks$missing_fu_members <- vapply(stocks$fishstock, function(s) {
    collapse_values(setdiff(expected_members$fu_no[expected_members$fishstock == s],
      fu$fu_no[!is.na(fu$fishstock) & fu$fishstock == s]))
  }, character(1))
  stocks$geometry_complete <- is.na(stocks$missing_fu_members) & stocks$geometry_usable
  sidecars <- Sys.glob(paste0(tools::file_path_sans_ext(shapefile), ".*"))
  list(polygons = stocks, tagged = fu, original = fu_original, unmapped = unmapped,
    geometry_audit = geometry_audit, correction = correction,
    source_md5 = tools::md5sum(sidecars))
}

build_stock_geometries <- function(stocks, stock_areas, area_geometries, nephrops) {
  g <- vector("list", length(stocks))
  ans <- tibble::tibble(fishstock = stocks, spatial_basis = "ICES_area_union",
    geometry_usable = TRUE, geometry_complete = FALSE,
    missing_area_codes = NA_character_, reference_area_codes = NA_character_)
  for (i in seq_along(stocks)) {
    ids <- unique(stock_areas$ICES_Area[stock_areas$fishstock == stocks[i]])
    ai <- match(ids, area_geometries$area_code)
    good <- !is.na(ai)
    good[good] <- area_geometries$area_geometry_status[ai[good]] == "available"
    ans$reference_area_codes[i] <- collapse_values(ids)
    ans$missing_area_codes[i] <- collapse_values(ids[!good])
    ans$geometry_complete[i] <- length(ids) > 0L && all(good)
    g[[i]] <- polygon_union(area_geometries[ai[good], ])[[1L]]
    ni <- match(stocks[i], nephrops$polygons$fishstock)
    if (!is.na(ni)) {
      g[[i]] <- sf::st_geometry(nephrops$polygons)[[ni]]
      ans$spatial_basis[i] <- "Nephrops_FU_geometry"
      ans$geometry_usable[i] <- nephrops$polygons$geometry_usable[ni]
      ans$geometry_complete[i] <- nephrops$polygons$geometry_complete[ni]
    }
    # Sandeel vocabulary area links are incomplete for some management stocks.
    # Use a broad, deliberately inclusive 3+4 candidate envelope, then require
    # a management-area rule. This cannot by itself create a unique assignment.
    if (grepl("^san[.]sa[.]", stocks[i])) {
      ai_san <- match(c("27.3", "27.4"), area_geometries$area_code)
      if (all(!is.na(ai_san)) && all(area_geometries$area_geometry_status[ai_san] == "available")) {
        g[[i]] <- polygon_union(area_geometries[ai_san, ])[[1L]]
        ans$spatial_basis[i] <- "sandeel_broad_candidate_envelope"
        ans$geometry_complete[i] <- TRUE
      }
    }
  }
  sf::st_sf(ans, geometry = sf::st_sfc(g, crs = ICES_SPATIAL_CRS))
}

build_stock_candidate_rules <- function(dat, support, stock_geometries,
                                         stock_species, config) {
  key <- dat |>
    dplyr::distinct(geo_id, CLspeciesCode, CLarea, CLstatisticalRectangle) |>
    dplyr::filter(!is.na(CLspeciesCode))
  pairs <- key |>
    dplyr::inner_join(stock_species, by = "CLspeciesCode", relationship = "many-to-many") |>
    dplyr::distinct(geo_id, CLspeciesCode, CLarea, CLstatisticalRectangle, fishstock) |>
    dplyr::left_join(dplyr::select(sf::st_drop_geometry(support), geo_id,
      area_geometry_available), by = "geo_id", relationship = "many-to-one")
  # An invalid nonblank area code cannot bypass a region-specific stock rule.
  # Keep its raw value for the audit, but evaluate the policy as unknown area.
  original_area <- pairs$CLarea
  pairs$CLarea[!pairs$area_geometry_available] <- NA_character_
  pairs <- stock_mapping_policy_v2(pairs, reference_year = config$reference_year)
  pairs$policy_area <- pairs$CLarea
  pairs$CLarea <- original_area
  geo_pairs <- dplyr::distinct(pairs, geo_id, fishstock)
  geo_pairs$geometry_evidence <- rep("unknown", nrow(geo_pairs))
  geo_pairs$geometry_note <- rep(NA_character_, nrow(geo_pairs))
  geo_pairs$tolerance_applied <- rep(FALSE, nrow(geo_pairs))
  for (s in unique(geo_pairs$fishstock)) {
    ix <- which(geo_pairs$fishstock == s)
    st <- stock_geometries[match(s, stock_geometries$fishstock), ]
    rel <- spatial_relation(support[match(geo_pairs$geo_id[ix], support$geo_id), ], st,
      rel_tol = config$relative_sliver_tolerance, abs_tol_m2 = config$absolute_sliver_tolerance_m2)
    evidence <- rel$spatial_status
    evidence[evidence == "geometry_error"] <- "unknown"
    if (!st$geometry_usable) evidence[] <- "unknown"
    if (!st$geometry_complete) evidence[evidence == "outside"] <- "unknown"
    geo_pairs$geometry_evidence[ix] <- evidence
    geo_pairs$geometry_note[ix] <- rel$geometry_note
    geo_pairs$tolerance_applied[ix] <- rel$tolerance_applied
  }
  pairs <- pairs |>
    dplyr::left_join(geo_pairs, by = c("geo_id", "fishstock"), relationship = "many-to-one") |>
    dplyr::left_join(sf::st_drop_geometry(stock_geometries), by = "fishstock", relationship = "many-to-one") |>
    dplyr::mutate(
      assignment_evidence = dplyr::case_when(
        geometry_evidence == "outside" ~ "outside",
        nonspatial_rule_required ~ "unknown",
        specific_spatial_rule_required & spatial_basis != "Nephrops_FU_geometry" ~ "unknown",
        !geometry_usable ~ "unknown",
        TRUE ~ geometry_evidence
      ),
      reason = dplyr::case_when(
        assignment_evidence == "outside" ~ "record_support_outside_stock_candidate_geometry",
        nonspatial_rule_required ~ paste0("stock_identity_rule_required: ", review_reason),
        specific_spatial_rule_required & spatial_basis != "Nephrops_FU_geometry" ~
          paste0("specific_stock_boundary_required: ", review_reason),
        !geometry_usable ~ "stock_geometry_not_approved_for_allocation",
        assignment_evidence == "full" ~ paste0("whole_record_support_in_", spatial_basis),
        assignment_evidence == "partial" ~ "record_support_crosses_stock_boundary",
        !geometry_complete ~ "missing_or_incomplete_stock_area_geometry",
        TRUE ~ "record_geography_unresolved"
      ))
  # Explicit domain exclusions for stocks described only in NAFO 1. This is
  # not a stock assignment and does not invent a missing NAFO polygon.
  # Source: ICES Stock Database descriptions for cod.21.1.isc/.osc.
  outside_nafo <- pairs$area_geometry_available &
    pairs$fishstock %in% c("cod.21.1.isc", "cod.21.1.osc") &
    !is.na(pairs$CLarea) & (pairs$CLarea == "27" | startsWith(pairs$CLarea, "27."))
  pairs$assignment_evidence[outside_nafo] <- "outside"
  pairs$reason[outside_nafo] <- "ICES_area_27_is_outside_NAFO_only_stock_definition"
  # The Greenland offshore stock spans NAFO1 and ICES14; other ICES subareas
  # are outside its explicitly described candidate domain.
  outside_greenland <- pairs$area_geometry_available & pairs$fishstock == "cod.21.27.1.14" &
    !is.na(pairs$CLarea) & startsWith(pairs$CLarea, "27.") &
    !(pairs$CLarea == "27.14" | startsWith(pairs$CLarea, "27.14."))
  pairs$assignment_evidence[outside_greenland] <- "outside"
  pairs$reason[outside_greenland] <- "ICES_area_outside_explicit_Greenland_stock_domain"
  candidates <- dat |>
    dplyr::select(row_id, CLyear, geo_id, CLspeciesCode) |>
    dplyr::inner_join(pairs, by = c("geo_id", "CLspeciesCode"), relationship = "many-to-many")
  stopifnot(!anyDuplicated(candidates[c("row_id", "fishstock")]))
  list(candidates = candidates, rules = pairs)
}

resolve_stock_candidates <- function(dat, candidates) {
  possible <- dplyr::filter(candidates, assignment_evidence != "outside")
  counts <- possible |>
    dplyr::summarise(n_possible = dplyr::n_distinct(fishstock),
      n_full = sum(assignment_evidence == "full"),
      n_partial = sum(assignment_evidence == "partial"),
      n_unknown = sum(assignment_evidence == "unknown"),
      supported_stock = if (sum(assignment_evidence == "full") == 1L)
        fishstock[assignment_evidence == "full"] else NA_character_,
      candidate_stocks = collapse_values(fishstock), .by = row_id)
  dat |>
    dplyr::left_join(counts, by = "row_id", relationship = "one-to-one") |>
    dplyr::mutate(dplyr::across(c(n_possible, n_full, n_partial, n_unknown), ~ dplyr::coalesce(.x, 0L)),
      match_status = dplyr::case_when(
        is.na(CLspeciesCode) ~ "unresolved_taxon",
        n_full == 1L & n_possible == 1L ~ "unique",
        n_full > 1L ~ "ambiguous",
        n_full == 1L & n_possible > 1L ~ "possible_overlap",
        n_partial > 0L ~ "needs_finer_geography",
        n_unknown > 0L ~ "needs_reference_or_stock_identity_rule",
        TRUE ~ "unmatched"),
      fishstock = dplyr::if_else(match_status == "unique", supported_stock, NA_character_))
}

classify_ecoregion_support <- function(support, ecoregions, ecoregion, config) {
  target <- ecoregions[ecoregions$Ecoregion == canonical_ecoregion(ecoregion), ]
  if (!nrow(target)) stop("No polygon found for ecoregion: ", ecoregion)
  rel <- spatial_relation(support, target,
    rel_tol = config$relative_sliver_tolerance, abs_tol_m2 = config$absolute_sliver_tolerance_m2)
  dplyr::bind_cols(sf::st_drop_geometry(support), rel |>
    dplyr::rename(ecoregion_geometry_note = geometry_note,
      ecoregion_spatial_status = spatial_status,
      ecoregion_tolerance_applied = tolerance_applied)) |>
    dplyr::mutate(CS = dplyr::case_when(ecoregion_spatial_status == "full" ~ "inside",
      ecoregion_spatial_status == "outside" ~ "outside",
      ecoregion_spatial_status == "partial" ~ "boundary", TRUE ~ "unknown"),
      ecoregion_classification_note = dplyr::case_when(
        CS == "boundary" ~ "partial_target_overlap_or_reference_coastline_mismatch",
        CS == "unknown" ~ "geography_or_geometry_unresolved",
        TRUE ~ "resolved_against_target_ecoregion_polygon"))
}

vms_csquare_diagnostic <- function(rectangles, config) {
  empty <- tibble::tibble(CLstatisticalRectangle = character(), ecoregion = character(),
    n_returned_rows = integer(), n_distinct_csquares = integer(), diagnostic_status = character())
  if (!isTRUE(config$run_vms_diagnostic) || !length(rectangles)) return(empty)
  if (!requireNamespace("icesVMS", quietly = TRUE)) {
    warning("icesVMS unavailable: skipping optional C-square diagnostic.")
    return(empty)
  }
  out <- lapply(rectangles, function(rec) {
    tryCatch({
      obj <- cached_reference(file.path(config$cache_dir, paste0("vms_", rec, ".rds")),
        function() icesVMS::get_csquare(stat_rec = rec, convert2sf = FALSE),
        paste0("https://data.ices.dk/vms/API/csquares?stat_rec=", rec), config$refresh_reference)
      x <- obj$data
      if (!all(c("c_square", "ecoregion") %in% names(x))) stop("Unexpected VMS C-square columns.")
      x |>
        dplyr::mutate(ecoregion = canonical_ecoregion(ecoregion)) |>
        dplyr::summarise(n_returned_rows = dplyr::n(), n_distinct_csquares = dplyr::n_distinct(c_square),
          .by = ecoregion) |>
        dplyr::mutate(CLstatisticalRectangle = rec, diagnostic_status = "returned_cells_only_no_weighting")
    }, error = function(e) tibble::tibble(CLstatisticalRectangle = rec, ecoregion = NA_character_,
      n_returned_rows = NA_integer_, n_distinct_csquares = NA_integer_,
      diagnostic_status = paste0("unavailable: ", conditionMessage(e))))
  })
  dplyr::bind_rows(out)
}

make_stock_review_flags <- function(rules, registry, selected_stocks) {
  # A valid FU polygon crossed by a coarse landing is a data-resolution issue,
  # not a bad-reference flag. Let unresolved exposure describe those cases.
  flags <- rules |>
    dplyr::filter(assignment_evidence == "unknown", review_flag != "none",
      nonspatial_rule_required |
        (specific_spatial_rule_required & spatial_basis != "Nephrops_FU_geometry") |
        !geometry_usable) |>
    dplyr::select(fishstock, review_flag) |>
    dplyr::distinct()
  dplyr::bind_rows(flags, registry |>
    dplyr::filter(risk_class %in% c("reference_stock_split", "reference_stock_reconfiguration"),
      fishstock %in% selected_stocks) |>
    dplyr::transmute(fishstock, review_flag = risk_class)) |>
    dplyr::distinct()
}

run_rdbes_stock_workflow <- function(config) {
  check_workflow_packages()
  dir.create(config$cache_dir, recursive = TRUE, showWarnings = FALSE)
  if (length(config$reference_year) != 1L || is.na(config$reference_year)) stop("Set one stock reference year.")
  if (isTRUE(config$apply_fu25_2021_revision) && config$reference_year < 2021L) {
    stop("The configured FU25 revision is for the post-2021 stock definition; choose the intended reference stock system.")
  }
  message("Reading ICES references for stock list ", config$reference_year, ".")
  refs <- get_workflow_references(config)
  input_env <- new.env(parent = baseenv())
  load(config$input_file, envir = input_env)
  if (!exists(config$input_object, envir = input_env, inherits = FALSE)) {
    stop("The RData file does not contain object ", config$input_object)
  }
  prepared <- prepare_rdbes_landings(get(config$input_object, envir = input_env), refs$asfis_worms, config)
  dat <- prepared$dat
  active_stocks <- tidyr::crossing(CLyear = sort(unique(dat$CLyear)), fishstock = refs$selected_stocks)

  message("Loading complete ICES area, rectangle, and ecoregion polygons.")
  spatial <- load_ices_spatial_reference(config$cache_dir, config$refresh_reference, config$ecoregion)
  rectangles <- load_ices_rectangles(c(dat$CLstatisticalRectangle,
    "15E0", "15E1", "16E0", "16E1", "17E1"), config$cache_dir, config$refresh_reference)
  area_geometries <- build_area_geometries(c(dat$CLarea, refs$stock_areas$ICES_Area, "27.3", "27.4"), spatial$areas)
  # ALL ICES areas supply the marine domain, never only the target ecoregion.
  support <- build_record_support(dplyr::distinct(dat, CLarea, CLstatisticalRectangle),
    area_geometries, rectangles, marine_union = spatial$areas)
  dat <- dat |>
    dplyr::left_join(dplyr::select(sf::st_drop_geometry(support), CLarea, CLstatisticalRectangle, geo_id),
      by = c("CLarea", "CLstatisticalRectangle"), relationship = "many-to-one")
  stopifnot(!anyNA(dat$geo_id), !anyDuplicated(dat$row_id))

  message("Preparing Nephrops FU stock boundaries and FU25 definition correction.")
  nephrops <- build_nephrops_stock_polygons(config$nephrops_shapefile, rectangles,
    apply_fu25_revision = config$apply_fu25_2021_revision)
  stock_geometries <- build_stock_geometries(refs$selected_stocks, refs$stock_areas,
    area_geometries, nephrops)
  message("Classifying stock candidates; partial and competing matches stay unresolved.")
  matched <- build_stock_candidate_rules(dat, support, stock_geometries, refs$stock_species, config)
  dat_assigned <- resolve_stock_candidates(dat, matched$candidates)
  eco_lookup <- classify_ecoregion_support(support, spatial$ecoregions, config$ecoregion, config)
  dat_assigned <- dat_assigned |>
    dplyr::left_join(dplyr::select(eco_lookup, -CLarea, -CLstatisticalRectangle),
      by = "geo_id", relationship = "many-to-one")

  registry <- stock_risk_registry_v2(config$reference_year)
  flags <- make_stock_review_flags(matched$rules, registry, refs$selected_stocks)
  audit <- audit_rdbes_stock_results(dat_assigned, matched$candidates, active_stocks,
    refs$stock_species, refs$stock_areas, stock_flags = flags, source_rows = dat)
  descriptions <- refs$stock_list |>
    dplyr::summarise(stock_description = collapse_values(StockKeyDescription), .by = StockKeyLabel) |>
    dplyr::rename(fishstock = StockKeyLabel)
  stock_landings <- audit$stock_audit |>
    dplyr::left_join(descriptions, by = "fishstock", relationship = "many-to-one") |>
    dplyr::left_join(sf::st_drop_geometry(stock_geometries), by = "fishstock", relationship = "many-to-one") |>
    dplyr::mutate(Ecoregion = config$ecoregion, StockReferenceYear = config$reference_year,
      Share_basis = "unique_stock_assignments_with_resolved_ecoregion_location",
      Inside_tonnes = Inside_kg / 1000, Outside_tonnes = Outside_kg / 1000,
      Boundary_tonnes = Boundary_kg / 1000, Unknown_tonnes = Unknown_kg / 1000)
  audit$stock_audit <- stock_landings
  matching_quality <- dat_assigned |>
    dplyr::summarise(n_records = dplyr::n(), Landings_kg = sum(CLscientificWeight), .by = c(CLyear, match_status)) |>
    dplyr::group_by(CLyear) |>
    dplyr::mutate(Share_landings_pct = if (sum(Landings_kg) > 0) 100 * Landings_kg / sum(Landings_kg) else NA_real_) |>
    dplyr::ungroup()
  vms_ids <- config$vms_check_rectangles
  if (is.null(vms_ids)) {
    vms_ids <- dat_assigned |>
      dplyr::filter(CS == "boundary", !is.na(CLstatisticalRectangle)) |>
      dplyr::summarise(kg = sum(CLscientificWeight), .by = CLstatisticalRectangle) |>
      dplyr::arrange(dplyr::desc(kg)) |>
      utils::head(config$vms_max_rectangles) |>
      dplyr::pull(CLstatisticalRectangle)
  }
  vms_ids <- unique(vms_ids[vms_ids %in% rectangles$ICESNAME])
  vms_diagnostic <- vms_csquare_diagnostic(vms_ids, config)
  taxonomy_review <- stock_taxonomy_review_v2(refs$stock_list, refs$stock_species)

  dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)
  run_dir <- tempfile(paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_"), tmpdir = config$output_dir)
  dir.create(run_dir, recursive = TRUE)
  write_table <- function(x, filename) {
    utils::write.csv(as.data.frame(x), file.path(run_dir, filename), row.names = FALSE, na = "")
  }
  write_table(stock_landings, "stock_landings_ecoregion.csv")
  write_table(audit$stock_audit, "stock_mapping_audit.csv")
  write_table(matching_quality, "matching_quality_by_year.csv")
  write_table(audit$unresolved_records, "unresolved_landings.csv")
  write_table(audit$species_area_quality, "species_area_quality.csv")
  write_table(audit$reference_gaps, "reference_gaps.csv")
  write_table(audit$reference_overlaps, "reference_area_overlaps.csv")
  write_table(audit$mass_balance, "mass_balance.csv")
  write_table(eco_lookup, "geography_audit.csv")
  write_table(prepared$input_quality, "input_quality.csv")
  write_table(prepared$species_code_lookup, "species_code_lookup.csv")
  write_table(taxonomy_review, "stock_taxonomy_review.csv")
  write_table(registry, "stock_reference_rules.csv")
  write_table(sf::st_drop_geometry(nephrops$polygons), "nephrops_stock_definitions.csv")
  write_table(sf::st_drop_geometry(nephrops$unmapped), "nephrops_unmapped_features.csv")
  write_table(nephrops$geometry_audit, "nephrops_geometry_validity.csv")
  write_table(nephrops$correction, "nephrops_fu25_correction.csv")
  write_table(vms_diagnostic, "vms_csquare_diagnostic.csv")
  if (nrow(prepared$missing_weight_rows)) saveRDS(prepared$missing_weight_rows, file.path(run_dir, "input_missing_weight_rows.rds"))
  saveRDS(dat_assigned, file.path(run_dir, "rdbes_stock_assignments.rds"))
  saveRDS(matched$candidates, file.path(run_dir, "stock_candidates.rds"))
  saveRDS(matched$rules, file.path(run_dir, "stock_geography_rules.rds"))
  saveRDS(nephrops$polygons, file.path(run_dir, "nephrops_stock_polygons.rds"))
  saveRDS(support, file.path(run_dir, "record_geographic_support.rds"))
  saveRDS(refs, file.path(run_dir, "reference_snapshot.rds"))
  saveRDS(list(config = config, fu_source_md5 = nephrops$source_md5,
    stock_reference_sources = lapply(refs$raw, function(x) x[setdiff(names(x), "data")]),
    area_source = attr(spatial$areas, "source_manifest"),
    ecoregion_source = attr(spatial$ecoregions, "source_manifest"),
    rectangle_sources = attr(rectangles, "source_manifests"),
    session = utils::sessionInfo()), file.path(run_dir, "run_manifest.rds"))
  utils::capture.output(utils::sessionInfo(), file = file.path(run_dir, "sessionInfo.txt"))
  message("Completed. Outputs: ", normalizePath(run_dir, winslash = "/", mustWork = TRUE))
  print(matching_quality, n = Inf)
  print(dplyr::count(stock_landings, share_quality), n = Inf)
  invisible(list(stock_landings = stock_landings, assignments = dat_assigned,
    candidates = matched$candidates, geography_rules = matched$rules,
    geography_audit = eco_lookup, matching_quality = matching_quality,
    audit = audit, nephrops = nephrops, stock_geometries = stock_geometries,
    vms_diagnostic = vms_diagnostic, output_directory = run_dir))
}

# ---- Run the complete workflow --------------------------------------------
if (isTRUE(getOption("rdbes.workflow.autorun", TRUE))) {
  results <- run_rdbes_stock_workflow(workflow_config)
}
