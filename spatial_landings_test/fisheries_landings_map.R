# fisheriesXplorer — spatial landings map
# Source this file once. R >= 4.0.
#
# Public entry points:
#   prepare_fisheries_landings()   explicit annual aggregation + diagnostics
#   build_fisheries_map_payload()  true reporting cells + marine display anchors
#   render_fisheries_landings()    interactive Leaflet widget
#   plot_fisheries_landings()      all three steps in one call
#   save_fisheries_map()           self-contained HTML export
#   export_fisheries_calculations() local CSV calculation record
#
# All JavaScript and CSS are embedded below. No companion asset directory,
# leaflet.minicharts plugin, live VMS API, or JavaScript build step is required.
# Read README_fisheries_landings.txt and example_fisheries_landings.R for integration and semantics.

# Fisheries landings: explicit aggregation and defensible display geometry.
#
# This file can be sourced without attaching packages or loading sf. Packages are
# checked when the relevant helper is called. No function downloads data, changes
# the working directory, loads an RData file, or modifies an input object.

.fx_data_require <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Install the following R packages first: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
}

.fx_data_code <- function(x) toupper(trimws(as.character(x)))

.fx_data_columns <- function(x, columns, object) {
  missing <- setdiff(columns, names(x))
  if (length(missing)) {
    stop(object, " is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

.fx_data_named_codes <- function(x, argument) {
  if (is.null(x) || !length(x)) return(stats::setNames(character(), character()))
  if (is.null(names(x)) || anyNA(x) || anyNA(names(x))) {
    stop(argument, " must be a named character vector without missing values.", call. = FALSE)
  }
  x <- stats::setNames(.fx_data_code(x), .fx_data_code(names(x)))
  if (any(!nzchar(names(x))) || any(!nzchar(x)) || anyDuplicated(names(x))) {
    stop(argument, " must have unique, nonempty species-code names and values.", call. = FALSE)
  }
  x
}

.fx_data_recode <- function(x, mapping) {
  i <- match(x, names(mapping))
  present <- !is.na(i)
  x[present] <- unname(mapping[i[present]])
  x
}

.fx_data_sum <- function(x) {
  # A missing observation invalidates a total. The caller decides whether an
  # absent row is a recorded zero; NA values themselves are never zero-filled.
  if (anyNA(x)) NA_real_ else sum(x)
}

.fx_data_known_sum <- function(x) {
  # Used only for diagnostics explicitly described as known subtotals.
  if (!length(x) || all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

#' Prepare rectangle-level mean annual reported landings
#'
#' All species in a rectangle use the same years and denominator. This prevents
#' a species recorded in one year from being compared with another species's
#' three-year average. The measure is the sum of the supplied weight column:
#' a column ending in `_perVessel` is not automatically expanded to fleet totals.
#' Verify that the upstream expansion/weighting is appropriate for your purpose.
#'
#' @param data A data.frame of the supplied landings records.
#' @param years Explicit integer years to include; filtering happens before sums.
#' @param rectangle_ids Optional rectangle codes defining the spatial selection.
#'   Whole-rectangle quantities are retained. Membership does not imply that all
#'   landings occurred inside an ecoregion boundary crossing that rectangle.
#' @param guild_lookup Species-to-guild table, required when guild is specified.
#' @param guild Optional single guild name. Unmapped species are excluded from
#'   this guild and listed with their known weights in diagnostics.
#' @param weight_col Weight column. It must already be numeric, nonnegative, and
#'   use NA for unknown/suppressed values. Text confidentiality codes are rejected.
#' @param weight_unit Required: "kg", "t", or "raw". kg is converted to tonnes;
#'   t stays in tonnes; raw is displayed as "source units".
#' @param rectangle_col,species_col,year_col Names of the source columns.
#' @param lookup_species_col,lookup_guild_col Names of lookup columns.
#' @param species_recodes Named source-code-to-final-code vector. Applied to both
#'   records and lookup before aggregation and classification. Chained recodes
#'   are rejected so group membership is unambiguous.
#' @param guild_overrides Named species-code-to-guild vector. Each override
#'   replaces all lookup memberships for that canonical code; no duplicate join.
#' @param annual_policy "strict" (default), "available", or "zero". Strict
#'   publishes a mean only if every selected rectangle-year is usable. Available
#'   averages every species over the same usable years and reports their count.
#'   Zero explicitly declares years without source rows to be recorded zero.
#'   Explicit NA weights or coverage=FALSE are never turned into zero.
#' @param coverage Optional table with unique rectangle/year keys and a logical,
#'   nonmissing complete column. TRUE confirms the supplied rectangle-year is
#'   complete, including a year without rows; FALSE excludes it. A missing key
#'   falls back to source-row presence, and this fallback is reported.
#' @param top_n Number of individual species/groups to display. Ignored when
#'   coverage_pct is set. NULL keeps every positive species separately.
#' @param coverage_pct Optional (0,100] target. The last species needed to reach
#'   or exceed the target is included, fixing the usual crossing-species error.
#' @param species_names Optional named code-to-common-name character vector.
#' @param species_colors Optional named code-to-CSS-color vector. Use one shared
#'   vector across maps for a stable fisheriesXplorer-wide color assignment.
#' @param title,region,source_note Optional strings carried into the map metadata.
#'
#' @details
#' A year with any source rows establishes the presence of a rectangle-year in
#' the supplied dataset. Within a usable rectangle-year, a species absent from
#' those records contributes zero to the recorded total. Any explicit missing
#' weight in the selected guild invalidates that year for every selected species.
#' None of these rules demonstrate completeness of country/fleet submissions.
#' Temporal completeness and fisheries reporting coverage are different claims.
#'
#' No source-record deduplication is attempted: equal-looking records may be
#' legitimate separate vessels, trips, or strata. Resolve duplicate source IDs
#' upstream, where the true record key is known.
#'
#' @return A list containing data (display-group means), raw_data (canonical
#'   species means), species (legend metadata), rectangle_summary, meta,
#'   diagnostics, annual, and annual_coverage. Annual tables remain available for
#'   archiving but are not automatically embedded in a published client bundle.
#' @export
prepare_fisheries_landings <- function(
    data, years, rectangle_ids = NULL, guild_lookup = NULL, guild = NULL,
    weight_col = "CLscientificWeight_perVessel", weight_unit,
    rectangle_col = "CLstatisticalRectangle", species_col = "CLspeciesFaoCode",
    year_col = "CLyear", lookup_species_col = "CLspeciesFaoCode",
    lookup_guild_col = "FisheriesGuild",
    species_recodes = c(ANK = "ANF", MON = "ANF", MNZ = "ANF", LDB = "LEZ", MEG = "LEZ"),
    guild_overrides = NULL, annual_policy = c("strict", "available", "zero"),
    coverage = NULL, top_n = 5L, coverage_pct = NULL, species_names = NULL,
    species_colors = NULL, title = NULL, region = NULL, source_note = NULL) {
  .fx_data_require(c("dplyr", "tidyr", "tibble"))
  if (!is.data.frame(data)) stop("data must be a data.frame.", call. = FALSE)
  .fx_data_columns(data, c(rectangle_col, species_col, year_col, weight_col), "data")
  if (missing(weight_unit)) {
    stop("Specify weight_unit = 'kg', 't', or 'raw'; units cannot be inferred from the column name.",
         call. = FALSE)
  }
  weight_unit <- match.arg(weight_unit, c("kg", "t", "raw"))
  annual_policy <- match.arg(annual_policy)
  if (!is.numeric(years) || !length(years) || anyNA(years) ||
      any(!is.finite(years)) || any(years != floor(years))) {
    stop("years must be a nonempty vector of finite integer years.", call. = FALSE)
  }
  years <- sort(unique(as.integer(years)))
  if (!is.numeric(data[[weight_col]])) {
    stop(weight_col, " must be numeric. Resolve text/confidentiality codes explicitly; do not coerce them to zero.",
         call. = FALSE)
  }
  if (!is.null(guild) && (length(guild) != 1L || is.na(guild) || !nzchar(trimws(guild)))) {
    stop("guild must be one nonempty name.", call. = FALSE)
  }
  if (!is.null(coverage_pct) && (length(coverage_pct) != 1L ||
      !is.numeric(coverage_pct) || is.na(coverage_pct) || !is.finite(coverage_pct) ||
      coverage_pct <= 0 || coverage_pct > 100)) {
    stop("coverage_pct must be a number greater than 0 and at most 100.", call. = FALSE)
  }
  if (is.null(coverage_pct) && !is.null(top_n) &&
      (length(top_n) != 1L || !is.numeric(top_n) || is.na(top_n) || !is.finite(top_n) ||
       top_n < 1 || top_n != floor(top_n))) {
    stop("top_n must be a positive integer or NULL.", call. = FALSE)
  }
  recodes <- .fx_data_named_codes(species_recodes, "species_recodes")
  chained <- names(recodes)[unname(recodes) %in% names(recodes) &
                              unname(recodes) != names(recodes)]
  if (length(chained)) {
    stop("species_recodes contains a chain/cycle. Map every original code directly to its final code.",
         call. = FALSE)
  }

  raw_year <- data[[year_col]]
  if (is.numeric(raw_year)) {
    bad_year <- !is.na(raw_year) & (!is.finite(raw_year) | raw_year != floor(raw_year))
    if (any(bad_year)) stop(year_col, " contains invalid year values.", call. = FALSE)
    parsed_year <- as.integer(raw_year)
  } else {
    raw_year <- trimws(as.character(raw_year))
    bad_year <- !is.na(raw_year) & !grepl("^[0-9]{4}$", raw_year)
    if (any(bad_year)) stop(year_col, " contains invalid year strings.", call. = FALSE)
    parsed_year <- as.integer(raw_year)
  }
  working <- tibble::tibble(
    rectangle = .fx_data_code(data[[rectangle_col]]),
    originalSpecies = .fx_data_code(data[[species_col]]), year = parsed_year,
    value = as.numeric(data[[weight_col]]) / if (weight_unit == "kg") 1000 else 1
  )
  selected_years <- dplyr::filter(working, .data$year %in% years)
  unallocated <- dplyr::filter(selected_years, is.na(.data$rectangle) | !nzchar(.data$rectangle))
  if (!is.null(rectangle_ids)) {
    rectangle_ids <- sort(unique(.fx_data_code(rectangle_ids)))
    if (!length(rectangle_ids) || anyNA(rectangle_ids) || any(!nzchar(rectangle_ids))) {
      stop("rectangle_ids must contain nonempty rectangle codes.", call. = FALSE)
    }
  }
  usable_coordinates <- !is.na(selected_years$rectangle) & nzchar(selected_years$rectangle)
  in_region <- if (is.null(rectangle_ids)) usable_coordinates else
    usable_coordinates & selected_years$rectangle %in% rectangle_ids
  regional <- selected_years[in_region, , drop = FALSE]
  outside <- selected_years[usable_coordinates & !in_region, , drop = FALSE]
  if (any(!is.na(regional$value) & (!is.finite(regional$value) | regional$value < 0))) {
    stop("Selected weights contain negative or non-finite values. Resolve their source meaning before mapping.",
         call. = FALSE)
  }
  if (anyNA(regional$originalSpecies) || any(!nzchar(regional$originalSpecies))) {
    stop("Selected records contain missing species codes. Assign an explicit source category upstream.",
         call. = FALSE)
  }
  regional$species <- .fx_data_recode(regional$originalSpecies, recodes)
  other_code <- "__OTHER_SPECIES__"
  if (other_code %in% regional$species) {
    stop("The reserved display code ", other_code, " occurs in the source data.", call. = FALSE)
  }
  source_presence <- dplyr::summarise(
    dplyr::group_by(regional, .data$rectangle, .data$year),
    nSourceRecords = dplyr::n(), .groups = "drop"
  )

  unmapped <- tibble::tibble(species = character(), nRecords = integer(), knownValue = double())
  resolved_lookup <- NULL
  selected <- regional
  if (!is.null(guild)) {
    if (is.null(guild_lookup) || !is.data.frame(guild_lookup)) {
      stop("Provide guild_lookup when selecting a guild.", call. = FALSE)
    }
    .fx_data_columns(guild_lookup, c(lookup_species_col, lookup_guild_col), "guild_lookup")
    lookup <- tibble::tibble(
      species = .fx_data_recode(.fx_data_code(guild_lookup[[lookup_species_col]]), recodes),
      guild = trimws(as.character(guild_lookup[[lookup_guild_col]]))
    )
    lookup <- dplyr::filter(lookup, !is.na(.data$species), nzchar(.data$species),
                            !is.na(.data$guild), nzchar(.data$guild))
    if (!is.null(guild_overrides) && length(guild_overrides)) {
      if (is.null(names(guild_overrides)) || anyNA(guild_overrides) || anyNA(names(guild_overrides))) {
        stop("guild_overrides must be a named species-to-guild vector.", call. = FALSE)
      }
      overrides <- tibble::tibble(
        species = .fx_data_recode(.fx_data_code(names(guild_overrides)), recodes),
        guild = trimws(as.character(guild_overrides))
      )
      if (any(!nzchar(overrides$species)) || any(!nzchar(overrides$guild))) {
        stop("guild_overrides contains an empty code or guild name.", call. = FALSE)
      }
      overrides <- dplyr::distinct(overrides)
      if (anyDuplicated(overrides$species)) {
        stop("guild_overrides assigns conflicting guilds to a canonical species code.", call. = FALSE)
      }
      lookup <- dplyr::bind_rows(dplyr::filter(lookup, !.data$species %in% overrides$species), overrides)
    }
    lookup <- dplyr::distinct(lookup)
    relevant_lookup <- dplyr::filter(lookup, .data$species %in% regional$species)
    ambiguous <- dplyr::filter(dplyr::count(relevant_lookup, .data$species), .data$n > 1L)
    if (nrow(ambiguous)) {
      stop("Conflicting guild memberships for: ", paste(ambiguous$species, collapse = ", "),
           ". Resolve them with explicit guild_overrides; a join must not duplicate their landings.",
           call. = FALSE)
    }
    unmapped_rows <- dplyr::filter(regional, !.data$species %in% lookup$species)
    unmapped <- dplyr::summarise(dplyr::group_by(unmapped_rows, .data$species),
      nRecords = dplyr::n(), knownValue = .fx_data_known_sum(.data$value), .groups = "drop")
    selected_codes <- lookup$species[lookup$guild == guild]
    # Membership filtering cannot multiply records, unlike a one-to-many join.
    selected <- dplyr::filter(regional, .data$species %in% selected_codes)
    resolved_lookup <- lookup
  }
  if (is.null(rectangle_ids)) rectangle_ids <- sort(unique(regional$rectangle))
  if (!length(rectangle_ids)) {
    stop("No rectangle IDs remain in the selected years/area. Supply rectangle_ids to represent a known empty grid.",
         call. = FALSE)
  }

  # A compact annual table keeps the original canonical species before display
  # grouping. A partly missing sum remains NA, not a misleading known subtotal.
  annual <- dplyr::summarise(dplyr::group_by(selected, .data$rectangle, .data$species, .data$year),
    nRecords = dplyr::n(), nMissing = sum(is.na(.data$value)),
    value = .fx_data_sum(.data$value), .groups = "drop")
  selected_status <- dplyr::summarise(dplyr::group_by(selected, .data$rectangle, .data$year),
    nSelectedRecords = dplyr::n(), nMissing = sum(is.na(.data$value)), .groups = "drop")
  calendar <- tidyr::expand_grid(rectangle = rectangle_ids, year = years)
  calendar <- dplyr::left_join(calendar, source_presence, by = c("rectangle", "year"))
  calendar <- dplyr::left_join(calendar, selected_status, by = c("rectangle", "year"))
  calendar$nSourceRecords[is.na(calendar$nSourceRecords)] <- 0L
  calendar$nSelectedRecords[is.na(calendar$nSelectedRecords)] <- 0L
  calendar$nMissing[is.na(calendar$nMissing)] <- 0L
  calendar$coverageDeclared <- NA
  coverage_unused <- 0L
  if (!is.null(coverage)) {
    if (!is.data.frame(coverage)) stop("coverage must be a data.frame.", call. = FALSE)
    .fx_data_columns(coverage, c("rectangle", "year", "complete"), "coverage")
    if (!is.logical(coverage$complete) || anyNA(coverage$complete)) {
      stop("coverage$complete must contain only logical TRUE/FALSE, without NA.", call. = FALSE)
    }
    if (!is.numeric(coverage$year) || anyNA(coverage$year) ||
        any(!is.finite(coverage$year)) || any(coverage$year != floor(coverage$year))) {
      stop("coverage$year must contain finite integer years.", call. = FALSE)
    }
    cov <- tibble::tibble(rectangle = .fx_data_code(coverage$rectangle),
      year = as.integer(coverage$year), coverageDeclared = coverage$complete)
    if (anyNA(cov$rectangle) || any(!nzchar(cov$rectangle)) ||
        anyDuplicated(as.data.frame(cov[c("rectangle", "year")]))) {
      stop("coverage must have unique, nonmissing rectangle/year keys.", call. = FALSE)
    }
    coverage_unused <- sum(!cov$rectangle %in% rectangle_ids | !cov$year %in% years)
    calendar$coverageDeclared <- NULL
    calendar <- dplyr::left_join(calendar, cov, by = c("rectangle", "year"))
  }
  calendar$hasSourceRecords <- calendar$nSourceRecords > 0L
  fallback <- calendar$hasSourceRecords | annual_policy == "zero"
  calendar$usable <- ifelse(is.na(calendar$coverageDeclared), fallback, calendar$coverageDeclared) &
    calendar$nMissing == 0L
  calendar$status <- ifelse(calendar$nMissing > 0L, "explicit_missing_weight",
    ifelse(!is.na(calendar$coverageDeclared) & !calendar$coverageDeclared, "coverage_incomplete",
    ifelse(calendar$usable & !calendar$hasSourceRecords, "declared_zero",
    ifelse(calendar$usable, "recorded", "no_source_records"))))

  summary <- dplyr::summarise(dplyr::group_by(calendar, .data$rectangle),
    nYearsObserved = sum(.data$usable), nYearsExpected = length(years),
    nYearsWithSourceRecords = sum(.data$hasSourceRecords),
    nYearsWithMissingWeights = sum(.data$nMissing > 0L),
    nYearsDeclaredZero = sum(.data$status == "declared_zero"),
    complete = all(.data$usable), yearsUsed = list(.data$year[.data$usable]),
    .groups = "drop")
  summary$publishable <- if (annual_policy == "available") summary$nYearsObserved > 0L else summary$complete
  summary$denominator <- ifelse(summary$publishable,
    if (annual_policy == "available") summary$nYearsObserved else length(years), NA_real_)
  summary <- summary[match(rectangle_ids, summary$rectangle), , drop = FALSE]

  species_universe <- sort(unique(selected$species))
  raw_matrix <- matrix(0, nrow = length(rectangle_ids), ncol = length(species_universe),
    dimnames = list(rectangle_ids, species_universe))
  if (length(species_universe)) raw_matrix[!summary$publishable, ] <- NA_real_
  usable_annual <- dplyr::inner_join(annual,
    dplyr::select(dplyr::filter(calendar, .data$usable), "rectangle", "year"),
    by = c("rectangle", "year"))
  species_sums <- dplyr::summarise(dplyr::group_by(usable_annual, .data$rectangle, .data$species),
    value = sum(.data$value), .groups = "drop")
  if (nrow(species_sums)) {
    ii <- match(species_sums$rectangle, rectangle_ids)
    jj <- match(species_sums$species, species_universe)
    raw_matrix[cbind(ii, jj)] <- species_sums$value / summary$denominator[ii]
  }
  summary$total <- if (length(species_universe)) rowSums(raw_matrix) else
    ifelse(summary$publishable, 0, NA_real_)
  raw_data <- if (length(species_universe)) {
    out <- tidyr::expand_grid(rectangle = rectangle_ids, species = species_universe)
    out$value <- as.vector(t(raw_matrix))
    out
  } else tibble::tibble(rectangle = character(), species = character(), value = double())

  # Rank only values that could actually be published under the selected policy.
  rank <- tibble::tibble(code = species_universe,
                         total = if (length(species_universe)) colSums(raw_matrix, na.rm = TRUE) else double())
  rank <- dplyr::arrange(dplyr::filter(rank, .data$total > 0), dplyr::desc(.data$total), .data$code)
  if (!nrow(rank)) {
    keep <- character()
  } else if (!is.null(coverage_pct)) {
    prior_share <- c(0, head(cumsum(rank$total) / sum(rank$total), -1L))
    keep <- rank$code[prior_share < coverage_pct / 100]
    # All positive species are retained at 100%, independent of rounding noise.
    if (coverage_pct == 100) keep <- rank$code
  } else {
    keep <- if (is.null(top_n)) rank$code else head(rank$code, as.integer(top_n))
  }
  members_other <- setdiff(species_universe, keep)
  display_codes <- c(keep, if (length(members_other)) other_code)
  display_matrix <- matrix(0, nrow = length(rectangle_ids), ncol = length(display_codes),
    dimnames = list(rectangle_ids, display_codes))
  if (length(display_codes)) {
    display_matrix[!summary$publishable, ] <- NA_real_
    if (length(keep)) display_matrix[, keep] <- raw_matrix[, keep, drop = FALSE]
    if (length(members_other)) {
      display_matrix[, other_code] <- rowSums(raw_matrix[, members_other, drop = FALSE])
    }
  }
  display_data <- if (length(display_codes)) {
    out <- tidyr::expand_grid(rectangle = rectangle_ids, species = display_codes)
    out$value <- as.vector(t(display_matrix))
    out
  } else tibble::tibble(rectangle = character(), species = character(), value = double())

  palette <- c("#176B87", "#D78A24", "#409B7D", "#AF629E", "#5F83BD", "#C45F4A", "#7C8747", "#766CAA")
  default_colors <- stats::setNames(rep(palette, length.out = length(species_universe)), species_universe)
  # Give displayed categories distinct fallback colors even when the full source
  # species universe is much larger than the palette. A supplied named palette
  # overrides these choices and is the stable cross-map production option.
  if (length(keep)) default_colors[keep] <- if (length(keep) <= length(palette)) palette[seq_along(keep)] else
    grDevices::hcl.colors(length(keep), palette = "Dark 3")
  legend_names <- stats::setNames(species_universe, species_universe)
  for (argument in c("species_names", "species_colors")) {
    supplied <- get(argument)
    if (!is.null(supplied) && length(supplied)) {
      if (is.null(names(supplied)) || anyNA(supplied) || anyNA(names(supplied)) ||
          any(!nzchar(as.character(supplied))) || anyDuplicated(.fx_data_code(names(supplied)))) {
        stop(argument, " must be a named character vector with unique code names and nonempty values.", call. = FALSE)
      }
      names(supplied) <- .fx_data_code(names(supplied))
      matches <- intersect(names(supplied), species_universe)
      if (argument == "species_names") legend_names[matches] <- as.character(supplied[matches]) else
        default_colors[matches] <- as.character(supplied[matches])
    }
  }
  if (length(species_universe)) {
    rgb_values <- tryCatch(grDevices::col2rgb(unname(default_colors)), error = function(e)
      stop("species_colors contains a color R cannot validate. Use hex colors such as #176B87.", call. = FALSE))
    # Convert R colour names (for example, royalblue) to the six-digit hex form
    # accepted by the client; retain code names so the lookup stays aligned.
    default_colors <- stats::setNames(grDevices::rgb(
      rgb_values[1L, ], rgb_values[2L, ], rgb_values[3L, ], maxColorValue = 255
    ), names(default_colors))
  }
  legend <- tibble::tibble(code = display_codes,
    name = c(unname(legend_names[keep]), if (length(members_other)) "Other species"),
    color = c(unname(default_colors[keep]), if (length(members_other)) "#8C98A5"),
    total = if (length(display_codes)) colSums(display_matrix, na.rm = TRUE) else double())
  # There is no regional total if every rectangle is unpublishable.
  region_total <- if (any(summary$publishable)) sum(summary$total[summary$publishable]) else NA_real_
  if (is.na(region_total) && nrow(legend)) legend$total[] <- NA_real_
  legend$share <- if (!is.na(region_total) && region_total > 0) legend$total / region_total else rep(NA_real_, nrow(legend))
  legend$members <- lapply(display_codes, function(code) if (code == other_code) members_other else code)
  legend$isOther <- legend$code == other_code
  recode_audit <- dplyr::distinct(dplyr::select(selected, "originalSpecies", "species"))
  diagnostics <- list(
    nInputRows = nrow(data), nRowsInSelectedYears = nrow(selected_years),
    nRowsOutsideSelectedYears = sum(!working$year %in% years),
    nRowsWithMissingYear = sum(is.na(working$year)),
    nRowsUnallocatedRectangle = nrow(unallocated),
    knownValueUnallocatedRectangle = .fx_data_known_sum(unallocated$value),
    nRowsOutsideRectangleSelection = nrow(outside),
    knownValueOutsideRectangleSelection = .fx_data_known_sum(outside$value),
    nRegionalRows = nrow(regional), nSelectedGuildRows = nrow(selected),
    nMissingWeights = sum(is.na(selected$value)), nUnmappedSpecies = nrow(unmapped),
    unmappedSpecies = unmapped, resolvedGuildLookup = resolved_lookup,
    speciesRecodes = recode_audit, otherSpecies = members_other,
    nCoverageRowsUnused = coverage_unused,
    nRectangleYearsWithoutCoverageDeclaration = sum(is.na(calendar$coverageDeclared)),
    nRectangleYearsWithExplicitMissing = sum(calendar$nMissing > 0L),
    nRectangleYearsDeclaredZero = sum(calendar$status == "declared_zero"),
    nUnpublishableRectangles = sum(!summary$publishable)
  )
  notes <- c(
    "Values describe reported landings in the supplied records; temporal completeness does not establish full fleet or country coverage.",
    "All displayed species in a rectangle use the same usable years and denominator.",
    "Landings belong to whole selected statistical rectangles; display clipping does not allocate quantities to the sea or to an ecoregion intersection.")
  if (is.null(coverage) || anyNA(calendar$coverageDeclared)) notes <- c(notes,
    "Where coverage is not declared, a rectangle-year with source records is treated as present in the supplied dataset; an absent species contributes zero of those records.")
  if (annual_policy == "zero") notes <- c(notes,
    "The caller explicitly declared absent rectangle-years zero; explicit missing weights remain unknown.")
  if (annual_policy == "available") notes <- c(notes,
    "Available-year means can use different year subsets between rectangles; compare their years-used indicators.")
  if (nrow(unmapped)) notes <- c(notes,
    "Some regional species lack a guild mapping and were excluded from the selected guild; see diagnostics.")
  meta <- list(
    title = if (is.null(title)) "Fisheries landings" else as.character(title),
    region = if (is.null(region)) "Selected rectangles" else as.character(region),
    guild = if (is.null(guild)) "All species" else as.character(guild),
    years = years, period = paste(years, collapse = ", "),
    aggregation = "Mean annual reported landings", measureLabel = "Mean annual reported landings",
    annualPolicy = annual_policy,
    inputWeightColumn = weight_col, inputWeightUnit = weight_unit,
    unit = if (weight_unit == "raw") "source units" else "t",
    unitLabel = if (weight_unit == "raw") "source units per year" else "tonnes per year",
    total = region_total, nRectangles = nrow(summary),
    nPublishableRectangles = sum(summary$publishable),
    nMissingRectangles = sum(!summary$publishable),
    nCompleteRectangles = sum(summary$complete),
    nPositiveRectangles = sum(summary$total > 0, na.rm = TRUE),
    nZeroRectangles = sum(summary$total == 0, na.rm = TRUE),
    nOriginalSpecies = length(species_universe), nDisplayedSpecies = length(keep),
    topN = if (is.null(coverage_pct)) top_n else NULL, coveragePercent = coverage_pct,
    otherCode = other_code, otherSpecies = members_other,
    thresholdLabel = if (!is.null(coverage_pct)) paste0("Species reaching ", coverage_pct, "% of published landings") else
      if (is.null(top_n)) "All positive species shown separately" else paste0("Top ", top_n, " species / groups"),
    otherShare = if (other_code %in% legend$code) legend$share[match(other_code, legend$code)] else 0,
    sourceNote = source_note,
    sourceDescription = if (is.null(source_note)) "Supplied landings records" else as.character(source_note),
    notes = notes,
    coverageMeaning = "Usable years among selected years; not the proportion of the fishery represented."
  )
  structure(list(data = display_data, raw_data = raw_data, species = legend,
    rectangle_summary = summary, meta = meta, diagnostics = diagnostics,
    annual = annual, annual_coverage = calendar), class = c("fx_landings_prepared", "list"))
}

.fx_data_geojson <- function(x) {
  if (is.null(x)) return(NULL)
  .fx_data_require(c("sf", "jsonlite"))
  if (inherits(x, "sfc")) x <- sf::st_sf(geometry = x)
  path <- tempfile(fileext = ".geojson")
  on.exit(unlink(path), add = TRUE)
  sf::st_write(sf::st_transform(x, 4326), path, driver = "GeoJSON", quiet = TRUE,
               layer_options = "RFC7946=YES")
  result <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  result$crs <- NULL
  result
}

.fx_data_polygon_parts <- function(x, crs) {
  if (!length(x) || all(sf::st_is_empty(x))) return(sf::st_sfc(crs = crs))
  types <- as.character(sf::st_geometry_type(x))
  if (!any(types %in% c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"))) {
    return(sf::st_sfc(crs = crs))
  }
  x <- sf::st_make_valid(x)
  # Calling st_collection_extract() on an already polygonal sfc emits an
  # unnecessary warning even with warn=FALSE. Extract only actual collections.
  if (any(as.character(sf::st_geometry_type(x)) == "GEOMETRYCOLLECTION")) {
    x <- sf::st_collection_extract(x, "POLYGON", warn = FALSE)
  }
  if (!length(x) || all(sf::st_is_empty(x))) return(sf::st_sfc(crs = crs))
  x <- sf::st_cast(x, "POLYGON", warn = FALSE)
  x[!sf::st_is_empty(x)]
}

.fx_data_ices_centres <- function(ids) {
  # The ICES code grid begins at 36 N and 44 W; A has columns 0:3, B onward
  # covers ten degrees per letter, I is skipped, and the last code is M8.
  # This is the reporting-grid definition, not a geocoding inference from data.
  # Reference: https://www.ices.dk/data/maps/Pages/ICES-statistical-rectangles.aspx
  starts <- c(A = -44, B = -40, C = -30, D = -20, E = -10, F = 0,
              G = 10, H = 20, J = 30, K = 40, L = 50, M = 60)
  ok <- grepl("^[0-9]{2}[A-HJ-M][0-9]$", ids)
  row <- suppressWarnings(as.integer(substr(ids, 1, 2)))
  letter <- substr(ids, 3, 3)
  column <- suppressWarnings(as.integer(substr(ids, 4, 4)))
  ok <- ok & !is.na(row) & row >= 1L & row <= 99L &
    !(letter == "A" & column > 3L) & !(letter == "M" & column > 8L)
  ok[is.na(ok)] <- FALSE
  if (any(!ok)) stop("Invalid ICES rectangle codes: ", paste(ids[!ok], collapse = ", "), call. = FALSE)
  data.frame(rectangle = ids, lon = unname(starts[letter]) + column + 0.5,
             lat = 35.75 + 0.5 * row, stringsAsFactors = FALSE)
}

.fx_data_dense_geographic <- function(x, step = 0.01) {
  # Subdivide longitude/latitude edges by linear coordinate interpolation before
  # projection. st_segmentize() on long/lat instead follows great circles, which
  # would bow a statistical rectangle's constant-latitude north/south edges.
  dense_ring <- function(m) {
    pieces <- lapply(seq_len(nrow(m) - 1L), function(i) {
      n <- max(1L, ceiling(max(abs(m[i + 1L, 1:2] - m[i, 1:2])) / step))
      f <- seq(0, 1, length.out = n + 1L)[seq_len(n)]
      cbind(m[i, 1] + f * (m[i + 1L, 1] - m[i, 1]),
            m[i, 2] + f * (m[i + 1L, 2] - m[i, 2]))
    })
    rbind(do.call(rbind, pieces), m[nrow(m), 1:2, drop = FALSE])
  }
  g <- sf::st_geometry(x)
  dense <- lapply(g, function(shape) {
    if (inherits(shape, "POLYGON")) sf::st_polygon(lapply(shape, dense_ring)) else
      if (inherits(shape, "MULTIPOLYGON")) sf::st_multipolygon(lapply(shape, function(p) lapply(p, dense_ring))) else
        stop("Rectangle geometry must contain polygons.", call. = FALSE)
  })
  sf::st_geometry(x) <- sf::st_sfc(dense, crs = sf::st_crs(x))
  x
}

#' Add rectangle and coastal display geometry to prepared landings
#'
#' @param prepared Output from prepare_fisheries_landings().
#' @param rectangle_sf Optional official sf rectangle polygons. Each requested
#'   code must occur exactly once. The source CRS must be declared.
#' @param centres Optional data.frame with rectangle/lon/lat. Used to construct
#'   1-degree longitude by 0.5-degree latitude rectangles. Centres must agree with
#'   the ICES grid; supplying official rectangle_sf is preferable when available.
#'   If neither input is supplied, the ICES code-grid definition supplies the
#'   midpoints. Codes and supplied centres are checked against the same lookup.
#' @param rectangle_id_col Rectangle ID column in rectangle_sf.
#' @param boundary Optional sf ecoregion boundary; an outline, never an allocation.
#' @param land Optional sf land POLYGON/MULTIPOLYGON geometry. Its local union is
#'   subtracted from the display rectangles only; no values are redistributed.
#' @param anchor_overrides Optional rectangle/lon/lat data.frame of desired symbol
#'   anchors. Requires land so both sea position and same-rectangle containment
#'   can be checked. An override moves a display symbol, not the underlying data.
#' @param analysis_crs Projected CRS for planar clipping and anchor calculations.
#'   EPSG:3035 is suitable for this European application; use a suitable local
#'   projected CRS for other regions. Output is always longitude/latitude 4326.
#'
#' @return Client bundle with meta, species, rectangles, rawRectangles, boundary,
#'   and land. Display rectangles have GeoJSON properties id, lon0, lat0, lon,
#'   lat, anchorMoved, anchorHidden, anchorStatus, values, total, nYearsObserved,
#'   nYearsExpected, complete, publishable, and yearsUsed. A wholly land-covered
#'   cell has no display geometry/anchor, but remains in rawRectangles and totals.
#' @export
build_fisheries_map_payload <- function(
    prepared, rectangle_sf = NULL, centres = NULL, rectangle_id_col = "rectangle",
    boundary = NULL, land = NULL, anchor_overrides = NULL, analysis_crs = 3035) {
  .fx_data_require(c("sf", "jsonlite"))
  if (!inherits(prepared, "fx_landings_prepared")) {
    stop("prepared must be the output of prepare_fisheries_landings().", call. = FALSE)
  }
  summary <- prepared$rectangle_summary
  ids <- summary$rectangle
  target_crs <- sf::st_crs(analysis_crs)
  if (is.na(target_crs) || isTRUE(sf::st_is_longlat(target_crs))) {
    stop("analysis_crs must identify an appropriate projected CRS.", call. = FALSE)
  }
  if (!is.null(rectangle_sf) && !is.null(centres)) {
    stop("Supply rectangle_sf or centres, not both.", call. = FALSE)
  }
  if (is.null(rectangle_sf)) {
    expected_centres <- .fx_data_ices_centres(ids)
    if (is.null(centres)) {
      centres <- expected_centres
    }
    .fx_data_columns(centres, c("rectangle", "lon", "lat"), "centres")
    centres$rectangle <- .fx_data_code(centres$rectangle)
    if (anyDuplicated(centres$rectangle)) stop("centres has duplicate rectangle IDs.", call. = FALSE)
    centres <- centres[match(ids, centres$rectangle), , drop = FALSE]
    if (!is.numeric(centres$lon) || !is.numeric(centres$lat) ||
        any(!is.finite(centres$lon)) || any(!is.finite(centres$lat)) ||
        any(abs(centres$lon) > 180) || any(abs(centres$lat) >= 90)) {
      stop("Every selected rectangle requires a finite longitude/latitude centre.", call. = FALSE)
    }
    if (any(abs((centres$lon - 0.5) - round(centres$lon - 0.5)) > 1e-7) ||
        any(abs((centres$lat - 0.25) * 2 - round((centres$lat - 0.25) * 2)) > 1e-7)) {
      stop("centres do not lie on the ICES 1-degree by 0.5-degree midpoint grid.", call. = FALSE)
    }
    mismatch <- abs(centres$lon - expected_centres$lon) > 1e-7 |
      abs(centres$lat - expected_centres$lat) > 1e-7
    if (any(mismatch)) {
      stop("Centre coordinates do not match their ICES rectangle IDs: ", paste(ids[mismatch], collapse = ", "), call. = FALSE)
    }
    polygons <- lapply(seq_along(ids), function(i) {
      x <- centres$lon[i]; y <- centres$lat[i]
      sf::st_polygon(list(rbind(c(x - 0.5, y - 0.25), c(x + 0.5, y - 0.25),
        c(x + 0.5, y + 0.25), c(x - 0.5, y + 0.25), c(x - 0.5, y - 0.25))))
    })
    rectangles <- sf::st_sf(rectangle = ids, geometry = sf::st_sfc(polygons, crs = 4326))
    original_xy <- as.matrix(centres[c("lon", "lat")])
    geometry_source <- "ICES 1-degree by 0.5-degree grid from validated centres"
  } else {
    if (!inherits(rectangle_sf, "sf") || is.na(sf::st_crs(rectangle_sf))) {
      stop("rectangle_sf must be an sf object with a declared CRS.", call. = FALSE)
    }
    .fx_data_columns(rectangle_sf, rectangle_id_col, "rectangle_sf")
    supplied_ids <- .fx_data_code(rectangle_sf[[rectangle_id_col]])
    wanted <- supplied_ids %in% ids
    if (anyDuplicated(supplied_ids[wanted])) {
      stop("rectangle_sf has duplicate selected IDs. Dissolve their geometry to one row per rectangle first.",
           call. = FALSE)
    }
    idx <- match(ids, supplied_ids)
    if (anyNA(idx)) stop("No polygon found for: ", paste(ids[is.na(idx)], collapse = ", "), call. = FALSE)
    rectangles <- sf::st_sf(rectangle = ids,
      geometry = sf::st_geometry(sf::st_transform(sf::st_make_valid(rectangle_sf[idx, ]), 4326)))
    if (any(sf::st_is_empty(rectangles)) ||
        any(!as.character(sf::st_geometry_type(rectangles)) %in% c("POLYGON", "MULTIPOLYGON"))) {
      stop("Every selected rectangle_sf geometry must be a nonempty polygon.", call. = FALSE)
    }
    # Bounding-box midpoints preserve the statistical rectangle's original
    # reference location; sea-safe anchors are stored separately below.
    original_xy <- t(vapply(sf::st_geometry(rectangles), function(g) {
      b <- sf::st_bbox(g); c(mean(b[c("xmin", "xmax")]), mean(b[c("ymin", "ymax")]))
    }, numeric(2)))
    geometry_source <- "Supplied rectangle polygons"
  }
  rectangles <- .fx_data_dense_geographic(rectangles)
  projected <- sf::st_transform(rectangles, target_crs)
  reference_points <- sf::st_transform(sf::st_as_sf(
    data.frame(lon = original_xy[, 1], lat = original_xy[, 2]),
    coords = c("lon", "lat"), crs = 4326), target_crs)

  display_land <- NULL
  land_union <- NULL
  if (!is.null(land)) {
    if (!inherits(land, c("sf", "sfc")) || is.na(sf::st_crs(land))) {
      stop("land must be sf/sfc land polygons with a declared CRS, not coastline lines.", call. = FALSE)
    }
    land_wgs <- sf::st_transform(sf::st_make_valid(land), 4326)
    g <- sf::st_geometry(land_wgs)
    types <- as.character(sf::st_geometry_type(g))
    if (any(!types %in% c("POLYGON", "MULTIPOLYGON"))) {
      stop("land must contain only POLYGON/MULTIPOLYGON land areas.", call. = FALSE)
    }
    # Preserve supplied land context for the client, while doing expensive
    # projection and union only on a regional crop. In particular, never project
    # global land directly into a local CRS such as EPSG:3035.
    display_land <- sf::st_sf(geometry = g)
    box <- sf::st_bbox(rectangles)
    box[c("xmin", "ymin")] <- box[c("xmin", "ymin")] - 5
    box[c("xmax", "ymax")] <- box[c("xmax", "ymax")] + 5
    box["xmin"] <- max(-179.999, box["xmin"]); box["xmax"] <- min(179.999, box["xmax"])
    box["ymin"] <- max(-89.9, box["ymin"]); box["ymax"] <- min(89.9, box["ymax"])
    g <- sf::st_crop(g, box)
    if (length(g) && !all(sf::st_is_empty(g))) {
      g <- sf::st_transform(g, target_crs)
      land_union <- sf::st_union(g)
    }
  }
  overrides_xy <- NULL
  if (!is.null(anchor_overrides) && nrow(anchor_overrides)) {
    if (is.null(land)) stop("Supply land polygons to validate offshore anchor_overrides.", call. = FALSE)
    .fx_data_columns(anchor_overrides, c("rectangle", "lon", "lat"), "anchor_overrides")
    anchor_overrides$rectangle <- .fx_data_code(anchor_overrides$rectangle)
    if (anyNA(anchor_overrides$rectangle) || anyDuplicated(anchor_overrides$rectangle) ||
        any(!anchor_overrides$rectangle %in% ids) || !is.numeric(anchor_overrides$lon) ||
        !is.numeric(anchor_overrides$lat) || any(!is.finite(anchor_overrides$lon)) ||
        any(!is.finite(anchor_overrides$lat)) || any(abs(anchor_overrides$lon) > 180) ||
        any(abs(anchor_overrides$lat) >= 90)) {
      stop("anchor_overrides requires unique selected rectangle IDs and finite geographic coordinates.", call. = FALSE)
    }
    overrides_xy <- sf::st_transform(sf::st_as_sf(anchor_overrides,
      coords = c("lon", "lat"), crs = 4326), target_crs)
  }

  display_geometry <- vector("list", length(ids))
  anchors <- vector("list", length(ids))
  anchor_status <- character(length(ids))
  anchor_hidden <- logical(length(ids))
  anchor_moved <- logical(length(ids))
  for (i in seq_along(ids)) {
    cell <- sf::st_geometry(projected)[i]
    marine <- if (is.null(land_union)) cell else sf::st_difference(cell, land_union)
    parts <- .fx_data_polygon_parts(marine, target_crs)
    override_i <- if (is.null(overrides_xy)) NA_integer_ else match(ids[i], overrides_xy$rectangle)
    if (!length(parts)) {
      if (!is.na(override_i)) stop("Anchor override targets a rectangle with no marine display area: ", ids[i], call. = FALSE)
      display_geometry[[i]] <- sf::st_geometrycollection()
      anchors[[i]] <- sf::st_point()
      anchor_status[i] <- "no_marine_geometry"
      anchor_hidden[i] <- TRUE
      next
    }
    water <- sf::st_union(parts)
    display_geometry[[i]] <- water[[1L]]
    reference <- sf::st_geometry(reference_points)[i]
    reference_inside <- length(sf::st_within(reference, water)[[1L]]) > 0L
    if (!is.na(override_i)) {
      point <- sf::st_geometry(overrides_xy)[override_i]
      if (!length(sf::st_within(point, water)[[1L]])) {
        stop("Anchor override must lie strictly inside the marine part of its own rectangle: ", ids[i], call. = FALSE)
      }
      anchor_status[i] <- "manual_sea_anchor"
    } else if (reference_inside) {
      point <- reference
      anchor_status[i] <- if (is.null(land)) "coast_unchecked" else "sea_reference"
    } else {
      # Point-on-surface is guaranteed to be inside a chosen polygon. Select the
      # largest marine component so a symbol is not placed on a small island-side
      # sliver. This is a display anchor, not an inferred fishing location.
      biggest <- which.max(as.numeric(sf::st_area(parts)))
      point <- sf::st_point_on_surface(parts[biggest])
      anchor_status[i] <- if (is.null(land)) "coast_unchecked" else "moved_to_sea"
    }
    anchors[[i]] <- point[[1L]]
    anchor_moved[i] <- as.numeric(sf::st_distance(reference, point, by_element = TRUE)) > 1
  }
  display_sf <- sf::st_sf(rectangle = ids,
    geometry = sf::st_sfc(display_geometry, crs = target_crs))
  anchor_points <- sf::st_transform(sf::st_sfc(anchors, crs = target_crs), 4326)
  anchor_xy <- matrix(NA_real_, nrow = length(ids), ncol = 2)
  visible <- which(!anchor_hidden)
  if (length(visible)) anchor_xy[visible, ] <- sf::st_coordinates(anchor_points[visible])[, 1:2, drop = FALSE]

  display_json <- .fx_data_geojson(display_sf)
  raw_json <- .fx_data_geojson(rectangles)
  codes <- prepared$species$code
  value_matrix <- matrix(NA_real_, nrow = length(ids), ncol = length(codes),
    dimnames = list(ids, codes))
  if (nrow(prepared$data)) {
    value_matrix[cbind(match(prepared$data$rectangle, ids), match(prepared$data$species, codes))] <- prepared$data$value
  }
  for (i in seq_along(ids)) {
    # Named lists ensure values become a JSON object, not an array whose species
    # meaning depends on column order. NA values serialize as null in the writer.
    values <- stats::setNames(lapply(seq_along(codes), function(j) value_matrix[i, j]), codes)
    properties <- list(id = ids[i], lon0 = original_xy[i, 1], lat0 = original_xy[i, 2],
      lon = anchor_xy[i, 1], lat = anchor_xy[i, 2], anchorMoved = anchor_moved[i],
      anchorHidden = anchor_hidden[i], anchorStatus = anchor_status[i], values = values,
      total = summary$total[i], nYearsObserved = summary$nYearsObserved[i],
      nYearsExpected = summary$nYearsExpected[i], complete = summary$complete[i],
      publishable = summary$publishable[i], yearsUsed = as.list(unname(summary$yearsUsed[[i]])),
      nYearsWithSourceRecords = summary$nYearsWithSourceRecords[i],
      nYearsWithMissingWeights = summary$nYearsWithMissingWeights[i])
    display_json$features[[i]]$properties <- properties
    if (anchor_hidden[i]) display_json$features[[i]]["geometry"] <- list(NULL)
    raw_json$features[[i]]$properties <- list(id = ids[i], total = summary$total[i],
      complete = summary$complete[i], publishable = summary$publishable[i])
  }
  if (!is.null(boundary)) {
    if (!inherits(boundary, c("sf", "sfc")) || is.na(sf::st_crs(boundary))) {
      stop("boundary must be sf/sfc with a declared CRS.", call. = FALSE)
    }
    repaired <- sf::st_geometry(sf::st_make_valid(boundary))
    # A repaired polygon can contain collapsed point/line fragments. Within
    # each collection retain polygon parts if present, otherwise line parts.
    # Keep ordinary line boundaries and ignore stray points, which Leaflet
    # would otherwise draw as unrelated default markers.
    boundary_parts <- function(g) {
      if (isTRUE(sf::st_is_empty(g))) return(list())
      if (inherits(g, "GEOMETRYCOLLECTION")) {
        parts <- do.call(c, lapply(g, boundary_parts))
        if (!length(parts)) return(list())
        polygonal <- vapply(parts, function(p) inherits(p, c("POLYGON", "MULTIPOLYGON")), logical(1))
        return(if (any(polygonal)) parts[polygonal] else parts)
      }
      if (inherits(g, c("POLYGON", "MULTIPOLYGON", "LINESTRING", "MULTILINESTRING"))) return(list(g))
      if (inherits(g, c("POINT", "MULTIPOINT"))) return(list())
      stop("boundary must contain polygon or line geometries.", call. = FALSE)
    }
    parts <- do.call(c, lapply(repaired, boundary_parts))
    cleaned <- sf::st_sfc(if (length(parts)) parts else list(), crs = sf::st_crs(boundary))
    cleaned <- cleaned[!sf::st_is_empty(cleaned)]
    if (!length(cleaned)) {
      stop("boundary must contain a nonempty polygon or line; point-only geometries are not boundary outlines.",
           call. = FALSE)
    }
    boundary <- sf::st_sf(geometry = cleaned)
  }
  legend <- lapply(seq_len(nrow(prepared$species)), function(i) {
    s <- prepared$species[i, ]
    list(code = s$code, id = s$code, name = s$name, color = s$color,
         total = s$total, share = s$share, members = as.list(unname(s$members[[1L]])), isOther = s$isOther)
  })
  meta <- prepared$meta
  meta$geometrySource <- geometry_source
  meta$marineClippingApplied <- !is.null(land)
  meta$nMovedAnchors <- sum(anchor_moved)
  meta$nHiddenAnchors <- sum(anchor_hidden)
  meta$geometryNotes <- c(
    "Symbol positions identify reporting rectangles, not vessels, ports, or exact fishing locations.",
    if (is.null(land)) "No land mask was supplied; symbol anchors have not been checked against the coast." else
      "Display polygons and symbol anchors use the supplied land mask; values remain whole-rectangle quantities.")
  # Preserve array shape even for one year/species/note under auto_unbox=TRUE.
  meta$years <- as.list(meta$years)
  meta$otherSpecies <- as.list(meta$otherSpecies)
  meta$notes <- as.list(c(meta$notes, meta$geometryNotes))
  meta$geometryNotes <- as.list(meta$geometryNotes)
  list(meta = meta, species = legend, rectangles = display_json, rawRectangles = raw_json,
       boundary = .fx_data_geojson(boundary), land = .fx_data_geojson(display_land))
}


# Keep GeoJSON coordinates in numeric vectors/matrices before JSON encoding.
# jsonlite::fromJSON(..., simplifyVector = FALSE) preserves GeoJSON nesting but
# otherwise creates a tiny R list for every position and coordinate. Encoding
# tens of thousands of those scalar lists is expensive. A numeric matrix is
# encoded in the same row-major JSON shape, without changing any coordinates.
.fx_normalise_geojson_coordinates <- function(x) {
  if (is.null(x)) return(NULL)

  position <- function(z) {
    if (is.list(z)) {
      if (!length(z)) return(z)
      if (!all(vapply(z, function(v) is.numeric(v) && length(v) == 1L,
                      logical(1)))) {
        stop("GeoJSON positions must contain numeric coordinates.", call. = FALSE)
      }
      z <- unlist(z, use.names = FALSE)
    }
    if (!is.numeric(z) || !length(z) %in% c(2L, 3L)) {
      stop("GeoJSON positions must contain two or three numeric coordinates.",
           call. = FALSE)
    }
    unname(z)
  }

  positions <- function(z) {
    if (is.matrix(z)) {
      if (!is.numeric(z) || !ncol(z) %in% c(2L, 3L)) {
        stop("GeoJSON coordinate matrices must have two or three numeric columns.",
             call. = FALSE)
      }
      dimnames(z) <- NULL
      return(z)
    }
    if (!is.list(z)) {
      stop("GeoJSON position sequences must be lists or numeric matrices.",
           call. = FALSE)
    }
    if (!length(z)) return(z)
    points <- lapply(z, position)
    dimensions <- lengths(points)
    if (any(!dimensions %in% c(2L, 3L)) || length(unique(dimensions)) != 1L) {
      stop("GeoJSON positions in one sequence must have the same dimensions.",
           call. = FALSE)
    }
    matrix(unlist(points, use.names = FALSE), ncol = dimensions[1L], byrow = TRUE)
  }

  geometry <- function(g) {
    if (is.null(g)) return(NULL)
    if (!is.list(g) || !is.character(g$type) || length(g$type) != 1L) {
      stop("GeoJSON geometry must have a single type.", call. = FALSE)
    }
    if (g$type == "GeometryCollection") {
      g$geometries <- lapply(g$geometries, geometry)
      return(g)
    }
    g$coordinates <- switch(g$type,
      Point = position(g$coordinates),
      MultiPoint = positions(g$coordinates),
      LineString = positions(g$coordinates),
      MultiLineString = lapply(g$coordinates, positions),
      Polygon = lapply(g$coordinates, positions),
      MultiPolygon = lapply(g$coordinates, function(p) lapply(p, positions)),
      stop("Unsupported GeoJSON geometry type: ", g$type, call. = FALSE)
    )
    g
  }

  feature <- function(f) {
    # Single-bracket replacement deliberately keeps a null geometry member.
    f["geometry"] <- list(geometry(f$geometry))
    f
  }
  if (identical(x$type, "FeatureCollection")) {
    x$features <- lapply(x$features, feature)
    return(x)
  }
  if (identical(x$type, "Feature")) return(feature(x))
  geometry(x)
}


.FX_LANDINGS_JS <- r"-----(/* fisheriesXplorer spatial landings client. Leaflet 1.x; no additional plugins. */
(function (global) {
  'use strict';
  var instanceCount = 0;
  var SVG_NS = 'http://www.w3.org/2000/svg';
  var DEFAULT_COLOURS = ['#007f86', '#bd713c', '#6d63a8', '#5592b5', '#af587e', '#808d50'];

  function node(tag, className, text) {
    var out = document.createElement(tag);
    if (className) out.className = className;
    if (text !== undefined && text !== null) out.textContent = String(text);
    return out;
  }
  function empty(element) { while (element.firstChild) element.removeChild(element.firstChild); }
  function finite(value) { return typeof value === 'number' && isFinite(value); }
  function nonnegative(value) { return finite(value) && value >= 0 ? value : null; }
  function knownSum(values) {
    var known = values.filter(function (v) { return finite(v); });
    return known.length ? known.reduce(function (a, b) { return a + b; }, 0) : null;
  }
  function safeColour(value, fallback) {
    return typeof value === 'string' && /^#[0-9a-f]{3}(?:[0-9a-f]{3})?$/i.test(value) ? value : fallback;
  }
  function exactNumber(value) {
    if (!finite(value)) return 'Not available';
    // Readable labels; the unrounded Number is retained in titles and CSV.
    var options = value !== 0 && Math.abs(value) < 0.01 ? {maximumSignificantDigits: 3} : {maximumFractionDigits: 2};
    return value.toLocaleString('en-GB', options).replace(/,/g, '\u202f');
  }
  function shortNumber(value) {
    if (!finite(value)) return '\u2014';
    var abs = Math.abs(value);
    var scale = abs >= 1e9 ? 1e9 : abs >= 1e6 ? 1e6 : abs >= 1e3 ? 1e3 : 1;
    var suffix = scale === 1e9 ? 'bn' : scale === 1e6 ? 'm' : scale === 1e3 ? 'k' : '';
    var amount = value / scale;
    return amount.toLocaleString('en-GB', {maximumFractionDigits: amount < 10 ? 2 : amount < 100 ? 1 : 0}) + suffix;
  }
  function percent(value, total) {
    if (!finite(value) || !finite(total) || total <= 0) return '\u2014';
    var share = 100 * value / total;
    if (share > 0 && share < 0.1) return '<0.1%';
    return share.toLocaleString('en-GB', {maximumFractionDigits: 1}) + '%';
  }
  function escapeHTML(value) {
    return String(value).replace(/[&<>"']/g, function (c) {
      return {'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'}[c];
    });
  }
  function icon(name) {
    var svg = document.createElementNS(SVG_NS, 'svg');
    svg.setAttribute('viewBox', '0 0 24 24');
    svg.setAttribute('fill', 'none');
    svg.setAttribute('stroke', 'currentColor');
    svg.setAttribute('stroke-width', '1.8');
    svg.setAttribute('stroke-linecap', 'round');
    svg.setAttribute('stroke-linejoin', 'round');
    svg.setAttribute('aria-hidden', 'true');
    var paths = {
      download: ['M12 3v12', 'm7 10 5 5 5-5', 'M5 16v4h14v-4'],
      reset: ['M3 10a9 9 0 1 1 2 8', 'M3 4v6h6'],
      close: ['m6 6 12 12', 'm18 6-12 12'],
      wheel: ['M12 2v5', 'M7 5h10a2 2 0 0 1 2 2v9a7 7 0 0 1-14 0V7a2 2 0 0 1 2-2Z', 'M12 10v3'],
      arrow: ['M18 12H6', 'm11 7-5 5 5 5'],
      focus: ['M8 3H3v5', 'M16 3h5v5', 'M21 16v5h-5', 'M3 16v5h5', 'M8 12h8', 'M12 8v8']
    };
    (paths[name] || paths.focus).forEach(function (d) {
      var path = document.createElementNS(SVG_NS, 'path');
      path.setAttribute('d', d); svg.appendChild(path);
    });
    return svg;
  }
  function button(label, className, iconName) {
    var out = node('button', className);
    out.type = 'button';
    if (iconName) out.appendChild(icon(iconName));
    out.appendChild(node('span', '', label));
    return out;
  }
  function interpolateColour(t) {
    var stops = [[240, 249, 246], [158, 214, 198], [60, 161, 157], [11, 108, 124], [15, 49, 76]];
    t = Math.max(0, Math.min(1, t));
    var i = Math.min(stops.length - 2, Math.floor(t * (stops.length - 1)));
    var f = t * (stops.length - 1) - i;
    return '#' + stops[i].map(function (v, j) {
      return Math.round(v + (stops[i + 1][j] - v) * f).toString(16).padStart(2, '0');
    }).join('');
  }
  function csvField(value) {
    if (value === null || value === undefined) return '';
    var text = String(value);
    // Keep numeric negatives numeric; prevent imported text from becoming a formula.
    if (typeof value === 'string' && /^[=+@\-\t\r]/.test(text)) text = "'" + text;
    return /[",\r\n]/.test(text) ? '"' + text.replace(/"/g, '""') + '"' : text;
  }

  global.fisheriesLandingsMap = function (map, el, bundle) {
    if (!global.L || !map || !el) throw new Error('A Leaflet map and its element are required.');
    if (el._fxMap && typeof el._fxMap.destroy === 'function') el._fxMap.destroy();
    bundle = bundle || {};
    var L = global.L;
    var meta = bundle.meta || {};
    var originalParent = el.parentNode;
    var originalStyle = el.getAttribute('style');
    var originalClassName = el.className;
    var originalRole = el.getAttribute('role');
    var originalLabel = el.getAttribute('aria-label');
    var originalWheelZoom = map.scrollWheelZoom ? map.scrollWheelZoom.enabled() : false;
    var initialHeight = Math.max(620, el.getBoundingClientRect().height || 780);
    var unit = meta.unit || 't';
    var measure = meta.measureLabel || 'Landings';
    var species = (bundle.species || []).map(function (s, i) {
      var isOther = !!(s.isOther || s.code === meta.otherCode || (!meta.otherCode && s.code === 'OTH'));
      return {code: String(s.code), name: isOther ? 'Other species' : String(s.name || s.code), isOther: isOther,
        members: Array.isArray(s.members) ? s.members.map(String) : [],
        color: isOther ? '#8b9299' : safeColour(s.color, DEFAULT_COLOURS[i % DEFAULT_COLOURS.length])};
    });
    var speciesLookup = new Map(species.map(function (s) { return [s.code, s]; }));
    var features = (bundle.rectangles && bundle.rectangles.features || []).map(function (feature) {
      var p = feature.properties || {};
      var values = {};
      species.forEach(function (s) { values[s.code] = nonnegative(p.values && p.values[s.code]); });
      return {type: 'Feature', geometry: feature.geometry || null, properties: Object.assign({}, p, {
        id: String(p.id), values: values, total: nonnegative(p.total)
      })};
    });
    var byId = new Map(features.map(function (f) { return [f.properties.id, f]; }));
    var rawById = new Map((bundle.rawRectangles && bundle.rawRectangles.features || []).map(function (f) {
      return [String(f.properties && f.properties.id), f];
    }));
    var regionalValues = {};
    species.forEach(function (s) { regionalValues[s.code] = knownSum(features.map(function (f) { return f.properties.values[s.code]; })); });
    var regionalTotal = knownSum(features.map(function (f) { return f.properties.total; }));
    var incompleteCount = features.filter(function (f) { return f.properties.complete === false; }).length;
    var noGeometryCount = features.filter(function (f) { return !f.geometry; }).length;
    var state = {mode: 'landings', species: species.length ? species[0].code : null, selected: null,
      outlines: true, wheelZoom: false, visiblePies: 0, eligiblePies: 0};
    var disposed = false;
    var ownedLayers = [];
    var ownedControls = [];
    var mapListeners = [];
    var paneNames = [];
    var timers = [];
    var objectUrls = [];
    var resizeFrame = null;
    var instance = ++instanceCount;

    var shell = node('section', 'fx-shell');
    shell.style.height = initialHeight + 'px';
    shell.setAttribute('aria-label', 'fisheriesXplorer spatial landings');
    originalParent.insertBefore(shell, el);
    var header = node('header', 'fx-header');
    var heading = node('div', 'fx-heading');
    var brand = node('div', 'fx-brand');
    brand.appendChild(node('span', 'fx-brand-word', 'fisheriesXplorer'));
    brand.appendChild(node('span', 'fx-brand-divider', '/'));
    brand.appendChild(node('span', '', 'SPATIAL LANDINGS'));
    heading.appendChild(brand);
    heading.appendChild(node('h2', 'fx-title', meta.title || 'Explore the pattern of landings'));
    var context = node('div', 'fx-context');
    [meta.region, meta.guild, meta.period].filter(Boolean).forEach(function (item) {
      context.appendChild(node('span', 'fx-context-item', item));
    });
    heading.appendChild(context); header.appendChild(heading);
    if (meta.preview) {
      var badge = node('span', 'fx-preview-badge', 'DATA PREVIEW');
      badge.title = 'Reconstructed from the supplied map; see Data and methods.';
      header.appendChild(badge);
    } else {
      var badgeLive = node('span', 'fx-data-badge', 'ICES REPORTING RECTANGLES');
      header.appendChild(badgeLive);
    }
    shell.appendChild(header);

    var toolbar = node('div', 'fx-toolbar');
    var tabs = node('div', 'fx-modes');
    tabs.setAttribute('role', 'group'); tabs.setAttribute('aria-label', 'Map view');
    var modeButtons = {};
    [['landings', 'Landings'], ['composition', 'Composition'], ['species', 'Species']].forEach(function (entry) {
      var btn = button(entry[1], 'fx-mode');
      btn.dataset.mode = entry[0];
      if (!species.length && entry[0] !== 'landings') btn.disabled = true;
      btn.addEventListener('click', function () { setMode(entry[0]); });
      tabs.appendChild(btn); modeButtons[entry[0]] = btn;
    });
    toolbar.appendChild(tabs);
    var speciesControl = node('label', 'fx-species-control');
    speciesControl.appendChild(node('span', 'fx-sr-only', 'Species or category shown on the map'));
    var speciesSelect = node('select', 'fx-select');
    species.forEach(function (s) {
      var option = node('option', '', s.isOther ? s.name : s.name === s.code ? s.code : s.name + ' (' + s.code + ')');
      option.value = s.code; speciesSelect.appendChild(option);
    });
    speciesSelect.addEventListener('change', function () { state.species = speciesSelect.value; updateView(); });
    speciesControl.appendChild(speciesSelect); toolbar.appendChild(speciesControl);
    var toolbarActions = node('div', 'fx-toolbar-actions');
    var gridLabel = node('label', 'fx-check');
    var gridCheck = node('input'); gridCheck.type = 'checkbox'; gridCheck.checked = state.outlines;
    gridCheck.addEventListener('change', function () {
      state.outlines = gridCheck.checked; updateCellStyles();
      if (rawGrid) { if (state.outlines) rawGrid.addTo(map); else map.removeLayer(rawGrid); }
    });
    gridLabel.appendChild(gridCheck); gridLabel.appendChild(node('span', '', 'Grid'));
    gridLabel.title = 'Show the full reporting-rectangle grid'; toolbarActions.appendChild(gridLabel);
    var download = button('Download CSV', 'fx-button fx-download', 'download');
    download.title = 'Download all reporting rectangles for this view. Zoom and selection do not filter the export.';
    download.addEventListener('click', downloadCSV); toolbarActions.appendChild(download);
    toolbar.appendChild(toolbarActions); shell.appendChild(toolbar);

    var body = node('div', 'fx-body'); shell.appendChild(body);
    var canvas = node('div', 'fx-map-canvas'); body.appendChild(canvas);
    canvas.appendChild(el); el.classList.add('fx-leaflet-map');
    el.setAttribute('role', 'region'); el.setAttribute('aria-label', 'Interactive map of landings by ICES reporting rectangle');
    var mapStatus = node('div', 'fx-map-status');
    mapStatus.setAttribute('role', 'status'); mapStatus.setAttribute('aria-live', 'polite');
    canvas.appendChild(mapStatus);
    var aside = node('aside', 'fx-panel'); body.appendChild(aside);
    aside.setAttribute('aria-label', 'Landings details');
    var selectArea = node('div', 'fx-rectangle-control');
    var rectLabel = node('label', 'fx-field-label');
    rectLabel.appendChild(node('span', '', 'Inspect a reporting rectangle'));
    var rectangleSelect = node('select', 'fx-select fx-rectangle-select');
    var allOption = node('option', '', 'All rectangles \u00b7 regional overview'); allOption.value = '';
    rectangleSelect.appendChild(allOption);
    features.slice().sort(function (a, b) { return a.properties.id.localeCompare(b.properties.id); }).forEach(function (f) {
      var p = f.properties;
      var op = node('option', '', p.id + ' \u00b7 ' + (p.total === null ? 'data unavailable' : shortNumber(p.total) + ' ' + unit));
      op.value = p.id; rectangleSelect.appendChild(op);
    });
    rectangleSelect.addEventListener('change', function () { selectRectangle(rectangleSelect.value || null, true); });
    rectLabel.appendChild(rectangleSelect); selectArea.appendChild(rectLabel); aside.appendChild(selectArea);
    var panelContent = node('div', 'fx-panel-content'); aside.appendChild(panelContent);
    var methods = node('details', 'fx-methods');
    methods.appendChild(node('summary', '', 'Data and methods'));
    var methodBody = node('div', 'fx-methods-body');
    methodBody.appendChild(node('p', '', measure + (meta.period ? ' \u00b7 ' + meta.period : '') + '. Unit: ' + unit + '.'));
    methodBody.appendChild(node('p', '', 'Polygons show reporting areas, not the precise positions of vessels or catches. Colour represents the value per rectangle, not landings per square kilometre.'));
    methodBody.appendChild(node('p', '', 'Composition symbols have equal screen size and show shares of available category values. Symbols that overlap are hidden until you zoom in; their rectangles remain selectable.'));
    if (meta.thresholdLabel) methodBody.appendChild(node('p', '', meta.thresholdLabel));
    if (meta.otherSpecies && meta.otherSpecies.length) methodBody.appendChild(node('p', '', 'Other species: ' + meta.otherSpecies.join(', ') + '.'));
    (meta.notes || []).forEach(function (note) { methodBody.appendChild(node('p', '', note)); });
    if (meta.sourceDescription) methodBody.appendChild(node('p', 'fx-source', 'Source: ' + meta.sourceDescription));
    methodBody.appendChild(node('p', 'fx-source', 'CSV downloads include all rectangles for the active view, with full numeric precision and metadata. Map zoom and selection do not filter the export.'));
    methods.appendChild(methodBody); aside.appendChild(methods);

    function pane(suffix, z) {
      var name = 'fx-' + instance + '-' + suffix;
      var created = map.createPane(name); created.style.zIndex = String(z);
      paneNames.push(name); return name;
    }
    var landPane = pane('land', 240);
    map.getPane(landPane).style.pointerEvents = 'none';
    var cellPane = pane('cells', 410);
    var gridPane = pane('grid', 415);
    map.getPane(gridPane).style.pointerEvents = 'none';
    var boundaryPane = pane('boundary', 425);
    map.getPane(boundaryPane).style.pointerEvents = 'none';
    var selectionPane = pane('selection', 440);
    var markerPane = pane('markers', 460);
    var labelPane = pane('labels', 350);
    map.getPane(labelPane).style.pointerEvents = 'none';
    function own(layer) { ownedLayers.push(layer); return layer; }
    function addControl(control) { control.addTo(map); ownedControls.push(control); return control; }
    if (bundle.land) own(L.geoJSON(bundle.land, {pane: landPane, interactive: false,
      style: {fillColor: '#edece5', fillOpacity: 1, color: '#c8d2d0', opacity: 1, weight: 1}})).addTo(map);
    var placeLabels = own(L.layerGroup()).addTo(map);
    (bundle.labels || []).forEach(function (label) {
      if (!finite(label.lon) || !finite(label.lat) || !label.text) return;
      var textLabel = node('span', 'fx-place-label ' + (label.type === 'sea' ? 'fx-sea-label' : 'fx-land-label'), label.text);
      // outerHTML is safe here: all label text was inserted with textContent. Leaflet 1.3 requires a string.
      L.marker([label.lat, label.lon], {pane: labelPane, interactive: false, keyboard: false,
        icon: L.divIcon({html: textLabel.outerHTML, className: 'fx-place-icon', iconSize: [160, 30], iconAnchor: [80, 15]})}).addTo(placeLabels);
    });
    var rawGrid = bundle.rawRectangles ? own(L.geoJSON(bundle.rawRectangles, {pane: gridPane,
      interactive: false, style: {fill: false, color: '#779394', opacity: 0.27, weight: 0.7}})) : null;
    if (rawGrid && state.outlines) rawGrid.addTo(map);
    if (bundle.boundary) own(L.geoJSON(bundle.boundary, {pane: boundaryPane, interactive: false,
      style: {fill: false, color: '#46676f', opacity: 0.7, weight: 1.35, dashArray: '5 5'}})).addTo(map);
    var selectionLayer = own(L.layerGroup()).addTo(map);
    var donutLayer = own(L.layerGroup()).addTo(map);
    var cellLayers = new Map();
    var colourMax = 0;
    var metric = function (p) { return state.mode === 'species' ? nonnegative(p.values[state.species]) : p.total; };
    var cells = own(L.geoJSON({type: 'FeatureCollection', features: features}, {
      pane: cellPane,
      style: function (f) { return cellStyle(f.properties); },
      onEachFeature: function (feature, layer) {
        var p = feature.properties; cellLayers.set(p.id, layer);
        layer.bindTooltip(function () { return tooltip(p); }, {className: 'fx-leaflet-tooltip', sticky: true, opacity: 1, direction: 'top'});
        layer.on('mouseover', function () {
          if (p.id !== state.selected) layer.setStyle({weight: 1.8, color: '#163e51', opacity: 1});
        });
        layer.on('mouseout', function () { layer.setStyle(cellStyle(p)); });
        layer.on('click', function (event) {
          if (event.originalEvent) L.DomEvent.stopPropagation(event.originalEvent);
          selectRectangle(p.id, false);
        });
        layer.on('add', function () {
          var path = layer.getElement && layer.getElement();
          if (path) {
            path.setAttribute('tabindex', '-1');
            path.setAttribute('aria-label', 'Reporting rectangle ' + p.id + '. Use the rectangle selector for keyboard access.');
          }
        });
      }
    })).addTo(map);

    var legend = node('div', 'fx-map-legend');
    var legendControl = L.control({position: 'bottomleft'});
    legendControl.onAdd = function () { L.DomEvent.disableClickPropagation(legend); L.DomEvent.disableScrollPropagation(legend); return legend; };
    addControl(legendControl);
    var actions = node('div', 'fx-map-actions');
    var resetButton = button('Reset view', 'fx-map-action', 'reset');
    resetButton.addEventListener('click', reset); actions.appendChild(resetButton);
    var wheelButton = button('Scroll zoom off', 'fx-map-action', 'wheel');
    wheelButton.setAttribute('aria-pressed', 'false');
    wheelButton.title = 'Enable or disable zooming with the scroll wheel. Dragging and the +/\u2212 buttons always work.';
    wheelButton.addEventListener('click', function () {
      state.wheelZoom = !state.wheelZoom;
      if (map.scrollWheelZoom) {
        if (state.wheelZoom) map.scrollWheelZoom.enable(); else map.scrollWheelZoom.disable();
      }
      wheelButton.lastChild.textContent = state.wheelZoom ? 'Scroll zoom on' : 'Scroll zoom off';
      wheelButton.setAttribute('aria-pressed', String(state.wheelZoom));
    });
    actions.appendChild(wheelButton);
    var actionsControl = L.control({position: 'topleft'});
    actionsControl.onAdd = function () { L.DomEvent.disableClickPropagation(actions); L.DomEvent.disableScrollPropagation(actions); return actions; };
    addControl(actionsControl);
    if (L.control.scale) addControl(L.control.scale({position: 'bottomright', imperial: false}));
    if (map.scrollWheelZoom) map.scrollWheelZoom.disable();
    if (map.attributionControl && meta.mapAttribution) map.attributionControl.addAttribution(escapeHTML(meta.mapAttribution));

    // Optional external basemap is opt-in. The vector land layer works without a connection.
    var webTiles = null;
    if (meta.tileUrl && /^https:\/\//.test(meta.tileUrl)) {
      var tileLabel = node('label', 'fx-check');
      var tileCheck = node('input'); tileCheck.type = 'checkbox';
      tileLabel.appendChild(tileCheck); tileLabel.appendChild(node('span', '', 'Web basemap'));
      tileLabel.title = 'Load a detailed background map over the internet';
      toolbarActions.insertBefore(tileLabel, download);
      tileCheck.addEventListener('change', function () {
        if (tileCheck.checked) {
          if (!webTiles) webTiles = own(L.tileLayer(meta.tileUrl, {attribution: escapeHTML(meta.tileAttribution || ''), maxZoom: 18}));
          webTiles.addTo(map);
          map.getPane(landPane).style.display = 'none';
        } else {
          if (webTiles) map.removeLayer(webTiles);
          map.getPane(landPane).style.display = '';
        }
      });
    }

    function cellStyle(p) {
      var value = metric(p);
      var selected = state.selected === p.id;
      var composition = state.mode === 'composition';
      var fill = value === null ? '#d3d9db' : value === 0 ? '#f7fbfa' : interpolateColour(colourMax > 0 ? Math.log1p(value) / Math.log1p(colourMax) : 0);
      return {fillColor: composition ? (value === null ? '#d3d9db' : value === 0 ? '#f7fbfa' : '#b8d4ce') : fill,
        fillOpacity: composition ? 0.34 : 0.93,
        color: selected ? '#072f4b' : value === null ? '#849397' : '#477e80',
        weight: selected ? 2.6 : state.outlines ? 0.55 : 0.15,
        opacity: selected ? 1 : state.outlines ? 0.52 : 0.08,
        dashArray: value === null ? '2 3' : null};
    }
    function updateCellStyles() {
      cells.eachLayer(function (layer) { if (layer.feature) layer.setStyle(cellStyle(layer.feature.properties)); });
      var selected = cellLayers.get(state.selected);
      if (selected && selected.bringToFront) selected.bringToFront();
    }
    function tooltip(p) {
      var box = node('div', 'fx-tooltip-content');
      box.appendChild(node('strong', '', 'Rectangle ' + p.id));
      var value = metric(p);
      var label = state.mode === 'species' && speciesLookup.has(state.species) ? speciesLookup.get(state.species).name : measure;
      var tooltipValue = node('span', '', label + ': ' + exactNumber(value) + (value === null ? '' : ' ' + unit));
      if (finite(value)) tooltipValue.title = 'Full precision: ' + String(value) + ' ' + unit;
      box.appendChild(tooltipValue);
      if (p.complete === false) box.appendChild(node('span', 'fx-tooltip-caveat', 'Some expected data are missing'));
      box.appendChild(node('small', '', 'Click to inspect composition'));
      return box;
    }
    function setMode(mode) {
      if (['landings', 'composition', 'species'].indexOf(String(mode).toLowerCase()) === -1) return;
      if (!species.length && String(mode).toLowerCase() !== 'landings') return;
      state.mode = String(mode).toLowerCase(); updateView();
    }
    function updateView() {
      if (disposed) return;
      Object.keys(modeButtons).forEach(function (m) {
        modeButtons[m].setAttribute('aria-pressed', String(state.mode === m));
        modeButtons[m].classList.toggle('is-active', state.mode === m);
      });
      speciesControl.hidden = state.mode !== 'species'; speciesSelect.value = state.species || '';
      var allValues = features.map(function (f) { return metric(f.properties); }).filter(finite);
      colourMax = allValues.length ? Math.max.apply(null, allValues) : 0;
      updateCellStyles(); renderLegend(); renderPanel(); renderSelection(); updateDonuts();
    }
    function renderLegend() {
      empty(legend);
      if (state.mode === 'composition') {
        legend.appendChild(node('div', 'fx-legend-title', 'Composition of landings'));
        legend.appendChild(node('div', 'fx-legend-subtitle', 'Equal-size pies \u00b7 shares only'));
        var list = node('div', 'fx-species-legend');
        species.forEach(function (s) {
          var item = button(s.isOther ? 'Other' : s.code, 'fx-legend-species');
          var swatch = node('i', 'fx-swatch'); swatch.style.backgroundColor = s.color;
          item.insertBefore(swatch, item.firstChild); item.title = s.name + ': show this category on the map';
          item.addEventListener('click', function () { state.species = s.code; setMode('species'); }); list.appendChild(item);
        });
        legend.appendChild(list);
      } else {
        var selectedSpecies = speciesLookup.get(state.species);
        legend.appendChild(node('div', 'fx-legend-title', state.mode === 'species' && selectedSpecies ? selectedSpecies.name : measure));
        legend.appendChild(node('div', 'fx-legend-subtitle', 'Per rectangle \u00b7 ' + unit + ' \u00b7 log(1 + value) colour'));
        if (colourMax > 0) {
          var ramp = node('div', 'fx-colour-ramp');
          ramp.style.background = 'linear-gradient(to right, ' + [0, .25, .5, .75, 1].map(interpolateColour).join(', ') + ')';
          legend.appendChild(ramp);
          var ticks = node('div', 'fx-colour-ticks');
          [0, 1 / 3, 2 / 3, 1].forEach(function (position) {
            var value = Math.expm1(Math.log1p(colourMax) * position);
            var tick = node('span', '', shortNumber(value)); tick.title = String(value) + ' ' + unit;
            ticks.appendChild(tick);
          });
          legend.appendChild(ticks);
        } else legend.appendChild(node('p', 'fx-no-positive', 'No positive values in this view'));
      }
      var keys = node('div', 'fx-legend-keys');
      [['#f7fbfa', 'Reported zero'], ['#d3d9db', 'No value']].forEach(function (key) {
        var item = node('span'); var swatch = node('i', 'fx-swatch fx-swatch-square'); swatch.style.backgroundColor = key[0];
        item.appendChild(swatch); item.appendChild(document.createTextNode(key[1])); keys.appendChild(item);
      });
      legend.appendChild(keys);
    }
    function renderPanel() {
      empty(panelContent);
      var feature = state.selected ? byId.get(state.selected) : null;
      var p = feature ? feature.properties : null;
      var values = p ? p.values : regionalValues;
      var total = p ? p.total : regionalTotal;
      var head = node('div', 'fx-panel-heading');
      head.appendChild(node('div', 'fx-kicker', p ? 'SELECTED REPORTING AREA' : 'REGIONAL OVERVIEW'));
      var titleLine = node('div', 'fx-panel-title-line');
      titleLine.appendChild(node('h3', '', p ? 'Rectangle ' + p.id : meta.region || 'All reporting rectangles'));
      if (p) {
        var clearButton = button('Clear selection', 'fx-icon-button', 'close');
        clearButton.title = 'Return to regional overview'; clearButton.addEventListener('click', function () { selectRectangle(null, false); });
        titleLine.appendChild(clearButton);
      }
      head.appendChild(titleLine); panelContent.appendChild(head);
      var amount = node('div', 'fx-total');
      amount.appendChild(node('span', 'fx-total-number', shortNumber(total)));
      amount.appendChild(node('span', 'fx-total-unit', unit)); panelContent.appendChild(amount);
      panelContent.appendChild(node('div', 'fx-total-label', measure));
      if (total !== null) {
        var exactTotal = node('div', 'fx-exact-value', exactNumber(total) + ' ' + unit);
        exactTotal.title = 'Full precision: ' + String(total) + ' ' + unit; panelContent.appendChild(exactTotal);
      }
      else panelContent.appendChild(node('div', 'fx-exact-value', 'No total value available'));
      var metadata = node('div', 'fx-panel-metrics');
      if (p) {
        var regionalShare = node('div'); regionalShare.appendChild(node('strong', '', percent(total, regionalTotal)));
        regionalShare.appendChild(node('span', '', 'of regional landings')); metadata.appendChild(regionalShare);
        var years = node('div');
        var observed = p.nYearsObserved; var expected = p.nYearsExpected;
        years.appendChild(node('strong', '', finite(observed) ? observed + (finite(expected) ? ' / ' + expected : '') : '\u2014'));
        years.appendChild(node('span', '', 'years represented')); metadata.appendChild(years);
      } else {
        var rects = node('div'); rects.appendChild(node('strong', '', features.length)); rects.appendChild(node('span', '', 'reporting rectangles')); metadata.appendChild(rects);
        var categories = node('div'); categories.appendChild(node('strong', '', species.length)); categories.appendChild(node('span', '', 'display categories')); metadata.appendChild(categories);
      }
      panelContent.appendChild(metadata);
      if (p && p.complete === false) panelContent.appendChild(node('div', 'fx-callout', 'Some expected observations are missing. These values and shares describe the available data.'));
      if (!p && incompleteCount) panelContent.appendChild(node('div', 'fx-callout', incompleteCount + (incompleteCount === 1 ? ' rectangle has' : ' rectangles have') + ' incomplete expected data. Regional values sum the available rectangle values.'));
      if (p && (!finite(p.nYearsObserved) || !finite(p.nYearsExpected))) panelContent.appendChild(node('p', 'fx-small-note', 'Year coverage is not available in this file.'));
      if (!p && noGeometryCount) panelContent.appendChild(node('p', 'fx-small-note', noGeometryCount + (noGeometryCount === 1 ? ' rectangle lacks a marine display polygon; it remains' : ' rectangles lack a marine display polygon; they remain') + ' in this overview and the export.'));
      var chartTitle = node('div', 'fx-chart-heading');
      chartTitle.appendChild(node('h4', '', 'Species composition'));
      chartTitle.appendChild(node('span', '', unit + ' / share'));
      panelContent.appendChild(chartTitle);
      panelContent.appendChild(node('div', 'fx-share-scale', 'Share of displayed landings \u00b7 full bar = 100%'));
      var chart = node('div', 'fx-bars');
      var compositionTotal = knownSum(species.map(function (s) { return values[s.code]; }));
      species.forEach(function (s) {
        var value = values[s.code];
        var bar = node('button', 'fx-bar-row'); bar.type = 'button';
        bar.setAttribute('aria-label', 'Show ' + s.name + ' on the map, ' + exactNumber(value) + (value === null ? '' : ' ' + unit));
        bar.classList.toggle('is-highlighted', state.mode === 'species' && state.species === s.code);
        bar.addEventListener('click', function () { state.species = s.code; setMode('species'); });
        var labelLine = node('div', 'fx-bar-label-line');
        var labelText = node('span', 'fx-bar-name');
        var swatch = node('i', 'fx-swatch'); swatch.style.backgroundColor = s.color; labelText.appendChild(swatch);
        labelText.appendChild(document.createTextNode(s.name));
        if (s.name !== s.code && !s.isOther) labelText.appendChild(node('small', '', s.code));
        labelLine.appendChild(labelText); labelLine.appendChild(node('span', 'fx-bar-percent', percent(value, compositionTotal)));
        bar.appendChild(labelLine);
        var track = node('span', 'fx-bar-track');
        var fill = node('span', 'fx-bar-fill'); fill.style.backgroundColor = s.color;
        fill.style.width = finite(value) && compositionTotal > 0 ? (100 * value / compositionTotal) + '%' : '0%'; track.appendChild(fill); bar.appendChild(track);
        var barValue = node('span', 'fx-bar-value', exactNumber(value) + (value === null ? '' : ' ' + unit));
        if (finite(value)) barValue.title = 'Full precision: ' + String(value) + ' ' + unit;
        bar.appendChild(barValue);
        chart.appendChild(bar);
      });
      if (!species.length) chart.appendChild(node('p', 'fx-small-note', 'No species categories are available.'));
      panelContent.appendChild(chart);
      panelContent.appendChild(node('p', 'fx-chart-hint', 'Select a species above to map its landings. Shares use available category values. Labels are rounded; hover for full precision or download CSV.'));
      if (p) {
        var anchorStatus = p.anchorStatus || (p.anchorMoved ? 'moved' : '');
        if (p.anchorHidden || anchorStatus === 'no_marine_geometry' || !feature.geometry) {
          panelContent.appendChild(node('div', 'fx-callout', 'No marine display area found; check geometry. The original reporting rectangle is highlighted where available.'));
        } else if (p.anchorMoved) {
          panelContent.appendChild(node('div', 'fx-anchor-note', 'The composition symbol is placed at a marine display anchor within this reporting area. The line connects it to the original rectangle centre. Neither point is a vessel location.'));
        } else if (anchorStatus === 'coast_unchecked') {
          panelContent.appendChild(node('p', 'fx-small-note', 'The symbol uses the supplied rectangle centre; a land mask was not available to check its position.'));
        }
        var zoomButton = button('Zoom to this rectangle', 'fx-button fx-zoom-selected', 'focus');
        zoomButton.addEventListener('click', function () { zoomToRectangle(state.selected); }); panelContent.appendChild(zoomButton);
      }
    }
    function selectRectangle(id, pan) {
      if (id !== null && id !== undefined && id !== '' && !byId.has(String(id))) return false;
      state.selected = id === null || id === undefined || id === '' ? null : String(id);
      rectangleSelect.value = state.selected || '';
      updateCellStyles(); renderPanel(); renderSelection(); updateDonuts();
      if (state.selected && pan) {
        var p = byId.get(state.selected).properties;
        if (finite(p.lat) && finite(p.lon)) {
          if (!map.getBounds().pad(-0.13).contains([p.lat, p.lon])) map.panTo([p.lat, p.lon], {animate: false});
        } else zoomToRectangle(state.selected);
      }
      return true;
    }
    function renderSelection() {
      selectionLayer.clearLayers();
      if (!state.selected) return;
      var feature = byId.get(state.selected); if (!feature) return;
      var p = feature.properties;
      var raw = rawById.get(state.selected);
      if (raw) L.geoJSON(raw, {pane: selectionPane, interactive: false,
        style: {fill: false, color: '#123e59', opacity: 0.9, weight: 2, dashArray: '5 4'}}).addTo(selectionLayer);
      if (p.anchorMoved && finite(p.lon0) && finite(p.lat0) && finite(p.lon) && finite(p.lat)) {
        L.polyline([[p.lat0, p.lon0], [p.lat, p.lon]], {pane: selectionPane, interactive: false, color: '#153f58', weight: 1.5, opacity: 0.9, dashArray: '3 3'}).addTo(selectionLayer);
        L.circleMarker([p.lat0, p.lon0], {pane: selectionPane, radius: 3.5, color: '#fff', weight: 1.4, fillColor: '#143d57', fillOpacity: 1})
          .bindTooltip(node('span', '', 'Original rectangle centre'), {className: 'fx-leaflet-tooltip'}).addTo(selectionLayer);
        if (state.mode !== 'composition') L.circleMarker([p.lat, p.lon], {pane: selectionPane, radius: 4.5, color: '#fff', weight: 1.5, fillColor: '#007d86', fillOpacity: 1})
          .bindTooltip(node('span', '', 'Marine display anchor; not a vessel location'), {className: 'fx-leaflet-tooltip'}).addTo(selectionLayer);
      }
    }
    function zoomToRectangle(id) {
      var feature = rawById.get(id) || byId.get(id);
      if (!feature || !feature.geometry) return;
      var bounds = L.geoJSON(feature).getBounds();
      if (bounds.isValid()) map.fitBounds(bounds, {padding: [60, 60], maxZoom: 9, animate: false});
    }
    function donut(p) {
      var wrap = node('div', 'fx-donut');
      wrap.classList.toggle('is-selected', p.id === state.selected);
      var svg = document.createElementNS(SVG_NS, 'svg'); svg.setAttribute('viewBox', '0 0 36 36');
      svg.setAttribute('aria-hidden', 'true');
      var sum = knownSum(species.map(function (s) { return p.values[s.code]; }));
      var offset = 0; var radius = 11.7; var circumference = 2 * Math.PI * radius;
      species.forEach(function (s) {
        var value = p.values[s.code];
        if (!finite(value) || value <= 0 || !sum) return;
        var circle = document.createElementNS(SVG_NS, 'circle');
        circle.setAttribute('cx', '18'); circle.setAttribute('cy', '18'); circle.setAttribute('r', String(radius));
        circle.setAttribute('fill', 'none'); circle.setAttribute('stroke', s.color); circle.setAttribute('stroke-width', '10');
        var length = value / sum * circumference;
        circle.setAttribute('stroke-dasharray', length + ' ' + (circumference - length));
        circle.setAttribute('stroke-dashoffset', String(-offset)); circle.setAttribute('transform', 'rotate(-90 18 18)');
        svg.appendChild(circle); offset += length;
      });
      var centre = document.createElementNS(SVG_NS, 'circle'); centre.setAttribute('cx', '18'); centre.setAttribute('cy', '18'); centre.setAttribute('r', '5.8'); centre.setAttribute('fill', '#fff'); svg.appendChild(centre);
      wrap.appendChild(svg); return wrap;
    }
    var markerCache = new Map();
    function updateDonuts() {
      if (disposed) return;
      if (state.mode !== 'composition') {
        donutLayer.clearLayers(); state.visiblePies = 0; state.eligiblePies = 0;
        mapStatus.textContent = features.length + ' reporting rectangles \u00b7 select a cell to explore'; return;
      }
      var size = map.getSize();
      var candidates = features.filter(function (f) {
        var p = f.properties;
        if (p.anchorHidden || !finite(p.lat) || !finite(p.lon)) return false;
        var sum = knownSum(species.map(function (s) { return p.values[s.code]; }));
        if (!finite(sum) || sum <= 0) return false;
        var point = map.latLngToContainerPoint([p.lat, p.lon]);
        return point.x >= -18 && point.y >= -18 && point.x <= size.x + 18 && point.y <= size.y + 18;
      }).sort(function (a, b) {
        if (a.properties.id === state.selected) return -1;
        if (b.properties.id === state.selected) return 1;
        return a.properties.id.localeCompare(b.properties.id);
      });
      var accepted = []; var bins = new Map(); var distance = 37;
      candidates.forEach(function (f) {
        var p = f.properties; var point = map.latLngToContainerPoint([p.lat, p.lon]);
        var bx = Math.floor(point.x / distance); var by = Math.floor(point.y / distance); var overlaps = false;
        for (var dx = -1; dx <= 1; dx++) for (var dy = -1; dy <= 1; dy++) {
          (bins.get((bx + dx) + ':' + (by + dy)) || []).forEach(function (other) {
            if (Math.pow(point.x - other.x, 2) + Math.pow(point.y - other.y, 2) < distance * distance) overlaps = true;
          });
        }
        if (!overlaps || p.id === state.selected) {
          accepted.push(f); var key = bx + ':' + by;
          if (!bins.has(key)) bins.set(key, []); bins.get(key).push(point);
        }
      });
      var acceptedIds = new Set(accepted.map(function (f) { return f.properties.id; }));
      donutLayer.eachLayer(function (marker) { if (!acceptedIds.has(marker._fxRectangle)) donutLayer.removeLayer(marker); });
      accepted.forEach(function (f) {
        var p = f.properties; var marker = markerCache.get(p.id);
        if (!marker) {
          marker = L.marker([p.lat, p.lon], {pane: markerPane, keyboard: false, bubblingMouseEvents: false,
            icon: L.divIcon({html: donut(p).outerHTML, className: 'fx-donut-icon', iconSize: [34, 34], iconAnchor: [17, 17]}),
            zIndexOffset: p.id === state.selected ? 1000 : 0});
          marker._fxRectangle = p.id;
          marker.bindTooltip(function () { return tooltip(p); }, {className: 'fx-leaflet-tooltip', opacity: 1, direction: 'top', offset: [0, -16]});
          marker.on('click', function (event) { if (event.originalEvent) L.DomEvent.stopPropagation(event.originalEvent); selectRectangle(p.id, false); });
          markerCache.set(p.id, marker);
        }
        if (!donutLayer.hasLayer(marker)) marker.addTo(donutLayer);
        marker.setZIndexOffset(p.id === state.selected ? 1000 : 0);
        var element = marker.getElement();
        if (element) {
          var symbol = element.querySelector('.fx-donut'); if (symbol) symbol.classList.toggle('is-selected', p.id === state.selected);
          element.setAttribute('aria-hidden', 'true');
        }
      });
      state.visiblePies = accepted.length; state.eligiblePies = candidates.length;
      mapStatus.textContent = candidates.length ? accepted.length + ' of ' + candidates.length + ' pies visible' +
        (accepted.length < candidates.length ? ' \u00b7 zoom in for more' : ' \u00b7 equal size shows shares') : 'No positive-value pies in this view';
    }
    function reset() {
      selectRectangle(null, false);
      var bounds = cells.getBounds();
      if ((!bounds || !bounds.isValid()) && rawGrid) bounds = rawGrid.getBounds();
      if (bounds && bounds.isValid()) map.fitBounds(bounds, {paddingTopLeft: [42, 45], paddingBottomRight: [42, 130], maxZoom: 7, animate: false});
      updateDonuts();
    }
    function downloadCSV() {
      var columns = ['region', 'guild', 'period', 'measure', 'unit', 'map_view', 'rectangle', 'species_code', 'species_name', 'is_other_group', 'group_members', 'landings', 'rectangle_total', 'share_of_known_percent', 'years_observed', 'years_expected', 'complete', 'anchor_moved', 'anchor_status', 'original_longitude', 'original_latitude', 'display_longitude', 'display_latitude', 'preview', 'source', 'notes', 'other_species'];
      var rows = [columns.join(',')];
      var exported = state.mode === 'species' ? species.filter(function (s) { return s.code === state.species; }) : species;
      features.forEach(function (f) {
        var p = f.properties; var denominator = knownSum(species.map(function (s) { return p.values[s.code]; }));
        exported.forEach(function (s) {
          var value = p.values[s.code];
          var exportCode = s.isOther && /^__/.test(s.code) ? 'OTHER_GROUP' : s.code;
          var row = [meta.region, meta.guild, meta.period, measure, unit, state.mode, p.id, exportCode, s.name, s.isOther, s.members.join('; '), value, p.total,
            finite(value) && denominator > 0 ? value / denominator * 100 : null,
            p.nYearsObserved, p.nYearsExpected, p.complete, p.anchorMoved, p.anchorStatus, p.lon0, p.lat0, p.lon, p.lat,
            !!meta.preview, meta.sourceDescription, (meta.notes || []).join(' | '), (meta.otherSpecies || []).join('; ')];
          rows.push(row.map(csvField).join(','));
        });
      });
      var blob = new Blob(['\ufeff' + rows.join('\r\n')], {type: 'text/csv;charset=utf-8;'});
      var url = URL.createObjectURL(blob); objectUrls.push(url); var anchor = node('a');
      anchor.href = url;
      var exportSpecies = speciesLookup.get(state.species);
      anchor.download = [meta.region || 'region', meta.guild || 'landings', meta.period || '', state.mode, state.mode === 'species' ? exportSpecies && exportSpecies.isOther ? 'Other' : state.species : ''].filter(Boolean).join('_').replace(/[^a-zA-Z0-9_.-]+/g, '_') + '.csv';
      shell.appendChild(anchor); anchor.click(); anchor.remove();
      timers.push(setTimeout(function () { URL.revokeObjectURL(url); objectUrls = objectUrls.filter(function (value) { return value !== url; }); }, 1000));
    }
    function listen(name, fn) { map.on(name, fn); mapListeners.push([name, fn]); }
    listen('moveend zoomend', updateDonuts);
    listen('click', function () { if (state.selected) selectRectangle(null, false); });
    var resizeObserver = typeof ResizeObserver !== 'undefined' ? new ResizeObserver(function () {
      if (resizeFrame !== null) cancelAnimationFrame(resizeFrame);
      resizeFrame = requestAnimationFrame(function () {
        if (!disposed) {
          shell.classList.toggle('fx-compact', shell.clientWidth <= 760);
          shell.classList.toggle('fx-narrow', shell.clientWidth <= 480);
          map.invalidateSize({pan: false}); updateDonuts();
        }
      });
    }) : null;
    if (resizeObserver) resizeObserver.observe(canvas);
    var styleObserver = typeof MutationObserver !== 'undefined' ? new MutationObserver(function () {
      // htmlwidgets/Shiny update el's inline height on resize, although CSS controls its canvas height.
      var height = el.style.height;
      if (/^\d+(?:\.\d+)?px$/.test(height) && parseFloat(height) >= 420 && shell.style.height !== height) shell.style.height = height;
    }) : null;
    if (styleObserver) styleObserver.observe(el, {attributes: true, attributeFilter: ['style']});
    function destroy() {
      if (disposed) return; disposed = true;
      if (resizeObserver) resizeObserver.disconnect(); if (styleObserver) styleObserver.disconnect();
      if (resizeFrame !== null) cancelAnimationFrame(resizeFrame);
      timers.forEach(clearTimeout);
      objectUrls.forEach(function (url) { URL.revokeObjectURL(url); });
      mapListeners.forEach(function (entry) { map.off(entry[0], entry[1]); });
      ownedControls.forEach(function (control) { control.remove(); });
      ownedLayers.forEach(function (layer) { if (map.hasLayer(layer)) map.removeLayer(layer); });
      markerCache.clear();
      paneNames.forEach(function (name) {
        var existing = map.getPane(name); if (existing && existing.parentNode) existing.parentNode.removeChild(existing);
        if (map._panes) delete map._panes[name];
        if (map._paneRenderers && map._paneRenderers[name]) {
          var renderer = map._paneRenderers[name]; if (map.hasLayer(renderer)) map.removeLayer(renderer);
          delete map._paneRenderers[name];
        }
      });
      if (map.attributionControl && meta.mapAttribution) map.attributionControl.removeAttribution(escapeHTML(meta.mapAttribution));
      if (map.scrollWheelZoom) {
        if (originalWheelZoom) map.scrollWheelZoom.enable(); else map.scrollWheelZoom.disable();
      }
      if (shell.parentNode) { shell.parentNode.insertBefore(el, shell); shell.remove(); }
      if (originalStyle === null) el.removeAttribute('style'); else el.setAttribute('style', originalStyle);
      el.className = originalClassName;
      if (originalRole === null) el.removeAttribute('role'); else el.setAttribute('role', originalRole);
      if (originalLabel === null) el.removeAttribute('aria-label'); else el.setAttribute('aria-label', originalLabel);
      if (el._fxMap && el._fxMap.destroy === destroy) delete el._fxMap;
    }
    el._fxMap = {setMode: setMode, selectRectangle: selectRectangle, reset: reset,
      getState: function () { return Object.assign({}, state, {rectangleCount: features.length, regionalTotal: regionalTotal}); },
      destroy: destroy, downloadCSV: downloadCSV, bundle: bundle, map: map};
    shell.classList.toggle('fx-compact', shell.clientWidth <= 760);
    shell.classList.toggle('fx-narrow', shell.clientWidth <= 480);
    updateView();
    timers.push(setTimeout(function () { if (!disposed) { map.invalidateSize({pan: false}); reset(); } }, 0));
    return el._fxMap;
  };
})(window);
)-----"

.FX_LANDINGS_CSS <- r"-----(/* Scoped styles: safe to embed next to other Shiny/Bootstrap/htmlwidgets content. */
.fx-shell {
  --fx-ink: #163748; --fx-muted: #617783; --fx-teal: #007f86; --fx-line: #dce5e6;
  --fx-paper: #ffffff; --fx-soft: #f4f8f8;
  box-sizing: border-box; position: relative; display: flex; flex-direction: column;
  width: 100%; max-width: 100%; min-height: 620px; margin: 0; overflow: hidden;
  border: 1px solid #dce5e6; border-radius: 16px; background: var(--fx-paper);
  box-shadow: 0 8px 32px rgba(26, 60, 75, .075);
  color: var(--fx-ink); font-family: Inter, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  font-size: 13px; line-height: 1.45;
}
.fx-shell *, .fx-shell *::before, .fx-shell *::after { box-sizing: border-box; }
.fx-shell [hidden] { display: none !important; }
.fx-shell button, .fx-shell input, .fx-shell select { font: inherit; }
.fx-shell button { cursor: pointer; text-transform: none; letter-spacing: normal; }
.fx-shell button:focus-visible, .fx-shell select:focus-visible, .fx-shell summary:focus-visible, .fx-shell input:focus-visible {
  outline: 3px solid #dfaa43; outline-offset: 3px;
}
.fx-shell button:disabled { cursor: default; opacity: .5; }
.fx-shell h2, .fx-shell h3, .fx-shell h4, .fx-shell p { margin: 0; color: inherit; font-family: inherit; }
.fx-shell .fx-sr-only {
  position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px; overflow: hidden;
  clip: rect(0, 0, 0, 0); white-space: nowrap; border: 0;
}
.fx-header { flex: 0 0 auto; display: flex; align-items: center; justify-content: space-between; gap: 22px; padding: 23px 28px 20px; }
.fx-heading { min-width: 0; }
.fx-brand { display: flex; align-items: center; flex-wrap: wrap; gap: 11px; color: #5d7782; font-size: 9px; font-weight: 650; letter-spacing: 1.6px; }
.fx-brand-word { font-size: 14px; color: #007b83; font-weight: 750; letter-spacing: -.4px; }
.fx-brand-divider { color: #a7b8bc; font-size: 13px; font-weight: 400; }
.fx-shell .fx-title { margin-top: 7px; font-size: clamp(20px, 2.2vw, 27px); line-height: 1.18; font-weight: 680; letter-spacing: -.75px; }
.fx-context { display: flex; flex-wrap: wrap; margin-top: 9px; color: #526c78; font-size: 12px; gap: 0; }
.fx-context-item + .fx-context-item::before { content: "\00b7"; display: inline-block; padding: 0 9px; color: #8a9ca4; }
.fx-preview-badge, .fx-data-badge { flex: 0 0 auto; align-self: flex-start; margin-top: 3px; padding: 6px 9px; font-size: 8px; font-weight: 700; letter-spacing: 1px; border-radius: 5px; }
.fx-preview-badge { background: #fff3d6; border: 1px solid #ecd6a8; color: #806019; }
.fx-data-badge { color: #648089; background: #f2f7f7; border: 1px solid #dce8e8; }
.fx-toolbar { display: flex; flex: 0 0 auto; align-items: center; flex-wrap: wrap; gap: 10px; padding: 11px 22px; border-top: 1px solid #e8eeee; border-bottom: 1px solid var(--fx-line); background: #fbfcfc; }
.fx-modes { display: flex; gap: 2px; padding: 3px; border: 1px solid #dce6e7; background: #edf3f3; border-radius: 9px; }
.fx-shell .fx-mode { position: relative; min-height: 33px; padding: 7px 16px; color: #506b76; border: 0; border-radius: 6px; background: transparent; font-size: 12px; font-weight: 650; line-height: 1.3; }
.fx-shell .fx-mode:hover { color: #006d75; background: #e2eeee; }
.fx-shell .fx-mode.is-active { background: #fff; color: #086772; box-shadow: 0 1px 5px rgba(20, 60, 70, .13); }
.fx-species-control { max-width: 215px; margin: 0; }
.fx-shell .fx-select { display: block; width: 100%; height: 39px; padding: 8px 26px 8px 10px; border: 1px solid #cfdddd; border-radius: 7px; background: #fff; color: #214b5c; line-height: 1.2; }
.fx-toolbar-actions { display: flex; align-items: center; gap: 16px; margin-left: auto; }
.fx-check { display: inline-flex; align-items: center; gap: 6px; margin: 0; color: #536e79; font-size: 11px; font-weight: 500; cursor: pointer; white-space: nowrap; }
.fx-check input { width: 14px; height: 14px; margin: 0; accent-color: #007f86; }
.fx-shell .fx-button { display: inline-flex; align-items: center; justify-content: center; gap: 7px; min-height: 37px; padding: 7px 11px; background: #fff; border: 1px solid #d0dfe2; border-radius: 7px; color: #315766; font-size: 11px; font-weight: 650; line-height: 1.3; text-decoration: none; }
.fx-shell .fx-button:hover { color: #006e76; background: #f1f9f8; border-color: #a8cbcd; }
.fx-button svg, .fx-map-action svg { display: block; width: 16px; height: 16px; flex-shrink: 0; }
.fx-body { display: grid; grid-template-columns: minmax(0, 1fr) 316px; flex: 1 1 auto; min-height: 0; }
.fx-map-canvas { position: relative; min-width: 0; min-height: 0; overflow: hidden; background: #e6f0f2; }
.fx-map-canvas > .fx-leaflet-map { width: 100% !important; height: 100% !important; min-height: 0 !important; margin: 0 !important; z-index: 0; background: #e7f1f3; font-family: inherit; }
.fx-map-status { position: absolute; top: 13px; left: 50%; transform: translateX(-50%); z-index: 600; max-width: calc(100% - 190px); padding: 7px 11px; background: rgba(255, 255, 255, .93); border: 1px solid rgba(196, 215, 219, .9); border-radius: 6px; box-shadow: 0 2px 8px rgba(20, 50, 64, .05); color: #466875; font-size: 10px; font-weight: 550; line-height: 1.35; text-align: center; pointer-events: none; }
.fx-panel { display: flex; flex-direction: column; gap: 0; min-height: 0; padding: 0; background: #fff; border-left: 1px solid var(--fx-line); overflow-y: auto; overscroll-behavior: contain; scrollbar-width: thin; scrollbar-color: #bed0d5 transparent; }
.fx-rectangle-control { padding: 19px 22px 16px; border-bottom: 1px solid #e7eded; }
.fx-field-label { display: block; margin: 0; color: #647e87; font-size: 10px; font-weight: 550; }
.fx-field-label > span { display: block; margin-bottom: 6px; }
.fx-shell .fx-rectangle-select { font-size: 11px; }
.fx-panel-content { padding: 22px 22px 18px; }
.fx-kicker { margin-bottom: 6px; font-size: 8px; color: #67858f; font-weight: 700; letter-spacing: 1.35px; }
.fx-panel-title-line { display: flex; align-items: center; gap: 8px; justify-content: space-between; }
.fx-shell .fx-panel-title-line h3 { font-size: 18px; font-weight: 650; line-height: 1.3; letter-spacing: -.4px; }
.fx-shell .fx-icon-button { display: flex; align-items: center; justify-content: center; flex: 0 0 auto; width: 28px; height: 28px; padding: 5px; background: #f0f5f5; border: 1px solid #dbe6e8; color: #637d87; border-radius: 50%; }
.fx-icon-button svg { width: 15px; height: 15px; }
.fx-icon-button > span { position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px; overflow: hidden; clip: rect(0,0,0,0); }
.fx-total { display: flex; gap: 6px; align-items: baseline; margin-top: 19px; }
.fx-total-number { font-size: 35px; font-weight: 650; color: #0d4e62; letter-spacing: -1.5px; line-height: 1.1; }
.fx-total-unit { font-size: 17px; color: #6d8791; }
.fx-total-label { margin-top: 4px; font-size: 11px; font-weight: 550; color: #526f7b; }
.fx-exact-value { margin-top: 3px; color: #7a9098; font-size: 10px; font-variant-numeric: tabular-nums; overflow-wrap: anywhere; }
.fx-panel-metrics { display: grid; grid-template-columns: 1fr 1fr; gap: 16px; padding: 16px 0; margin-bottom: 9px; border-bottom: 1px solid #e3eceb; }
.fx-panel-metrics > div { display: flex; flex-direction: column; }
.fx-panel-metrics strong { font-size: 18px; font-weight: 620; color: #325b6a; }
.fx-panel-metrics span { color: #7a9199; font-size: 9px; }
.fx-shell .fx-callout { margin: 12px 0; padding: 10px 11px; border-radius: 6px; border-left: 3px solid #ceaa57; background: #fff8e9; color: #77643b; font-size: 10px; line-height: 1.55; }
.fx-shell .fx-small-note { margin: 9px 0; color: #738c95; font-size: 10px; line-height: 1.55; }
.fx-chart-heading { display: flex; justify-content: space-between; align-items: baseline; gap: 8px; margin-top: 16px; margin-bottom: 9px; }
.fx-shell .fx-chart-heading h4 { color: #385f6d; font-size: 11px; font-weight: 700; }
.fx-chart-heading > span { font-size: 9px; color: #8ba0a6; white-space: nowrap; }
.fx-share-scale { margin-top: -4px; margin-bottom: 7px; color: #82979e; font-size: 9px; }
.fx-bars { display: flex; flex-direction: column; gap: 1px; }
.fx-shell .fx-bar-row { display: block; width: calc(100% + 12px); margin-left: -6px; padding: 7px 6px 6px; border: 1px solid transparent; border-radius: 5px; background: transparent; text-align: left; }
.fx-shell .fx-bar-row:hover, .fx-shell .fx-bar-row.is-highlighted { border-color: #dfebeb; background: #f4f9f9; }
.fx-bar-label-line { display: flex; justify-content: space-between; align-items: flex-start; gap: 6px; }
.fx-bar-name { display: inline-flex; gap: 5px; align-items: center; flex-wrap: wrap; color: #385b68; font-size: 10px; font-weight: 600; line-height: 1.4; }
.fx-bar-name small { color: #91a2a8; font-size: 8px; font-weight: 500; }
.fx-swatch { display: inline-block; width: 7px; height: 7px; flex: 0 0 auto; border-radius: 50%; vertical-align: middle; }
.fx-bar-percent { color: #587681; font-size: 10px; font-weight: 600; white-space: nowrap; font-variant-numeric: tabular-nums; }
.fx-bar-track { display: block; width: 100%; height: 5px; margin-top: 5px; border-radius: 2px; background: #eef3f3; overflow: hidden; }
.fx-bar-fill { display: block; height: 100%; border-radius: 2px; }
.fx-bar-value { display: block; margin-top: 3px; color: #7d929a; font-size: 10px; font-variant-numeric: tabular-nums; overflow-wrap: anywhere; }
.fx-shell .fx-chart-hint { margin-top: 10px; color: #86999f; font-size: 9px; line-height: 1.5; }
.fx-shell .fx-anchor-note { margin-top: 15px; padding: 10px 11px; border-radius: 5px; background: #eef6f6; color: #567680; font-size: 10px; line-height: 1.6; }
.fx-shell .fx-zoom-selected { width: 100%; margin-top: 14px; }
.fx-methods { margin-top: auto; border-top: 1px solid #e1eaea; background: #f9fbfb; }
.fx-methods summary { padding: 15px 22px; color: #5e7c86; font-size: 10px; font-weight: 600; cursor: pointer; list-style-position: inside; }
.fx-methods-body { padding: 0 22px 15px; }
.fx-shell .fx-methods-body p { margin-bottom: 10px; color: #718992; font-size: 10px; line-height: 1.6; overflow-wrap: anywhere; }
.fx-shell .fx-methods-body p:last-child { margin-bottom: 0; }
.fx-shell .fx-methods-body .fx-source { font-size: 9px; color: #81979e; }
.fx-shell .leaflet-control-zoom { overflow: hidden; margin-left: 13px !important; margin-top: 13px !important; border: 1px solid #cbdcde !important; border-radius: 7px !important; box-shadow: 0 2px 8px rgba(20, 50, 64, .06) !important; }
.fx-shell .leaflet-control-zoom a { width: 31px !important; height: 30px !important; line-height: 29px !important; color: #456774 !important; background: #fff !important; font-size: 20px !important; font-weight: 400 !important; }
.fx-shell .leaflet-control-zoom a:hover { background: #f1f8f7 !important; }
.fx-shell .leaflet-control-zoom-in { border-bottom-color: #d9e4e6 !important; }
.fx-map-actions { display: flex; flex-direction: column; gap: 5px; margin-left: 13px !important; margin-top: 8px !important; }
.fx-shell .fx-map-action { display: flex; align-items: center; gap: 6px; min-height: 31px; width: max-content; max-width: 130px; padding: 6px 8px; background: rgba(255, 255, 255, .96); border: 1px solid #cbdcde; border-radius: 6px; color: #527580; font-size: 9px; font-weight: 550; box-shadow: 0 2px 6px rgba(20,50,64,.04); }
.fx-shell .fx-map-action[aria-pressed="true"] { background: #e2f2ee; color: #006870; border-color: #82b6b5; }
.fx-shell .fx-map-action:hover { background: #edf7f5; color: #006b74; }
.fx-map-legend { width: 260px; max-width: calc(100vw - 90px); margin: 0 0 17px 13px !important; padding: 12px 14px 10px; color: #365b68; background: rgba(255,255,255,.96); border: 1px solid #cfdee0; border-radius: 8px; box-shadow: 0 3px 10px rgba(20,50,64,.065); }
.fx-legend-title { font-size: 11px; font-weight: 650; }
.fx-legend-subtitle { margin-top: 2px; font-size: 9px; color: #779099; }
.fx-colour-ramp { height: 9px; margin-top: 11px; border-radius: 3px; border: 1px solid rgba(40,85,95,.13); }
.fx-colour-ticks { display: flex; justify-content: space-between; margin-top: 3px; color: #67858e; font-size: 9px; font-variant-numeric: tabular-nums; }
.fx-legend-keys { display: flex; gap: 15px; margin-top: 8px; padding-top: 7px; border-top: 1px solid #e5eeee; color: #799098; font-size: 8px; }
.fx-legend-keys > span { display: flex; align-items: center; gap: 5px; }
.fx-swatch-square { width: 8px; height: 8px; border: 1px solid #bccdce; border-radius: 1px; }
.fx-species-legend { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 4px 8px; margin-top: 8px; }
.fx-shell .fx-legend-species { display: inline-flex; align-items: center; gap: 4px; justify-content: flex-start; min-width: 0; padding: 2px 0; border: 0; background: none; color: #557782; font-size: 9px; }
.fx-shell .fx-legend-species:hover { color: #006e77; text-decoration: underline; }
.fx-shell .fx-no-positive { margin: 8px 0; font-size: 10px; color: #7b9098; }
.fx-shell .fx-donut-icon { border: 0; background: transparent; }
.fx-shell .fx-place-icon { border: 0; background: transparent; pointer-events: none; }
.fx-place-label { display: flex; align-items: center; justify-content: center; width: 160px; height: 30px; white-space: nowrap; text-align: center; font-family: inherit; text-shadow: 0 1px 3px rgba(255,255,255,.85); }
.fx-land-label { color: #84918a; font-size: 11px; font-weight: 600; letter-spacing: 2px; text-transform: uppercase; }
.fx-sea-label { color: #8baab5; font-family: Georgia, serif; font-size: 14px; font-style: italic; letter-spacing: .8px; }
.fx-donut { width: 34px; height: 34px; border-radius: 50%; background: #fff; border: 1px solid rgba(255,255,255,.95); box-shadow: 0 1px 4px rgba(15,55,70,.22); }
.fx-donut svg { display: block; width: 100%; height: 100%; }
.fx-donut.is-selected { outline: 2px solid #073c58; outline-offset: 2px; box-shadow: 0 0 0 5px rgba(255,255,255,.85); }
.fx-shell .fx-leaflet-tooltip { padding: 10px 12px; border: 1px solid #bfcfd3; border-radius: 7px; background: #fff; color: #274e5c; box-shadow: 0 4px 16px rgba(12,45,60,.13); font-family: inherit; }
.fx-tooltip-content { display: flex; flex-direction: column; gap: 3px; max-width: 300px; white-space: normal; overflow-wrap: anywhere; }
.fx-tooltip-content strong { font-size: 12px; font-weight: 650; }
.fx-tooltip-content span { font-size: 11px; }
.fx-tooltip-content small { margin-top: 3px; font-size: 9px; color: #83979e; }
.fx-tooltip-content .fx-tooltip-caveat { font-size: 9px; color: #a17b30; }
.fx-shell .leaflet-control-attribution { max-width: 100%; padding: 2px 5px; background: rgba(255,255,255,.8); color: #84959b; font-size: 8px; }
.fx-shell .leaflet-control-attribution a { color: #67828c; }
.fx-shell .leaflet-control-scale { margin-bottom: 12px !important; margin-right: 12px !important; }
.fx-shell .leaflet-control-scale-line { border-color: #80979f; background: rgba(255,255,255,.65); color: #68858f; font-size: 8px; }
/* These classes follow the widget width, so narrow Shiny columns also stack correctly. */
.fx-shell.fx-compact { height: auto !important; min-height: 0; }
.fx-shell.fx-compact .fx-body { display: flex; flex-direction: column; min-height: 0; }
.fx-shell.fx-compact .fx-map-canvas { flex: 0 0 500px; height: 500px; min-height: 500px; }
.fx-shell.fx-compact .fx-panel { display: block; border-left: 0; border-top: 1px solid var(--fx-line); overflow: visible; }
.fx-shell.fx-compact .fx-data-badge { display: none; }
.fx-shell.fx-narrow .fx-header { padding: 18px 17px 16px; gap: 10px; align-items: flex-start; }
.fx-shell.fx-narrow .fx-brand-divider, .fx-shell.fx-narrow .fx-brand > span:last-child { display: none; }
.fx-shell.fx-narrow .fx-title { font-size: 21px; line-height: 1.25; }
.fx-shell.fx-narrow .fx-toolbar { gap: 9px; padding: 10px 13px; }
.fx-shell.fx-narrow .fx-modes { flex: 1 1 auto; }
.fx-shell.fx-narrow .fx-mode { flex: 1 1 0; padding: 8px 10px; font-size: 11px; }
.fx-shell.fx-narrow .fx-toolbar-actions { flex: 1 0 100%; justify-content: space-between; margin-left: 0; }
.fx-shell.fx-narrow .fx-species-control { max-width: none; width: 100%; }
.fx-shell.fx-narrow .fx-map-canvas { flex-basis: 480px; height: 480px; min-height: 480px; }
.fx-shell.fx-narrow .fx-map-status { top: 12px; right: 12px; left: auto; transform: none; max-width: calc(100% - 70px); padding: 7px 9px; }
.fx-shell.fx-narrow .fx-bars { grid-template-columns: 1fr; }
.fx-shell.fx-narrow .fx-map-legend { width: 235px; padding: 10px 12px 9px; }
@media (max-width: 1050px) {
  .fx-body { grid-template-columns: minmax(0, 1fr) 284px; }
  .fx-header { padding: 21px 22px 17px; }
  .fx-toolbar { padding-right: 16px; padding-left: 16px; }
  .fx-shell .fx-mode { padding-right: 12px; padding-left: 12px; }
  .fx-toolbar-actions { gap: 11px; }
  .fx-species-control { max-width: 180px; }
  .fx-rectangle-control, .fx-panel-content { padding-right: 18px; padding-left: 18px; }
  .fx-map-status { max-width: calc(100% - 160px); font-size: 9px; }
  .fx-data-badge { display: none; }
}
@media (max-width: 760px) {
  .fx-shell { height: auto !important; min-height: 0; border-radius: 11px; }
  .fx-header { padding: 18px 17px 16px; gap: 10px; }
  .fx-brand { gap: 8px; font-size: 8px; letter-spacing: 1.2px; }
  .fx-brand-word { font-size: 13px; }
  .fx-shell .fx-title { font-size: 22px; }
  .fx-context { font-size: 11px; }
  .fx-preview-badge { padding: 5px 6px; font-size: 7px; letter-spacing: .6px; }
  .fx-toolbar { gap: 9px; padding: 10px 13px; }
  .fx-shell .fx-mode { min-height: 36px; padding: 8px 15px; }
  .fx-toolbar-actions { margin-left: auto; }
  .fx-body { display: flex; flex-direction: column; min-height: 0; }
  .fx-map-canvas { flex: 0 0 500px; height: 500px; min-height: 500px; }
  .fx-panel { display: block; border-left: 0; border-top: 1px solid var(--fx-line); overflow: visible; }
  .fx-rectangle-control { padding: 17px 20px 14px; }
  .fx-panel-content { padding: 20px; }
  .fx-panel-metrics { margin-top: 5px; }
  .fx-bars { display: grid; grid-template-columns: 1fr 1fr; gap: 8px 24px; }
  .fx-total { margin-top: 15px; }
  .fx-methods summary { padding: 16px 20px; }
  .fx-methods-body { padding: 0 20px 15px; }
  .fx-map-status { max-width: calc(100% - 165px); font-size: 9px; }
}
@media (max-width: 480px) {
  .fx-header { align-items: flex-start; }
  .fx-brand-divider, .fx-brand > span:last-child { display: none; }
  .fx-shell .fx-title { font-size: 21px; line-height: 1.25; }
  .fx-preview-badge { margin-top: 0; }
  .fx-modes { flex: 1 1 auto; }
  .fx-shell .fx-mode { flex: 1 1 0; padding: 8px 10px; font-size: 11px; }
  .fx-toolbar-actions { flex: 1 0 100%; justify-content: space-between; margin-left: 0; }
  .fx-species-control { max-width: none; width: 100%; }
  .fx-map-canvas { flex-basis: 480px; height: 480px; min-height: 480px; }
  .fx-map-status { top: 12px; right: 12px; left: auto; transform: none; max-width: calc(100% - 70px); padding: 7px 9px; }
  .fx-map-actions { margin-top: 8px !important; }
  .fx-shell .fx-map-action { max-width: 112px; padding: 6px 7px; font-size: 8px; }
  .fx-map-legend { width: 235px; padding: 10px 12px 9px; }
  .fx-legend-subtitle { font-size: 8px; }
  .fx-shell .leaflet-control-scale { margin-bottom: 157px !important; }
  .fx-bars { grid-template-columns: 1fr; gap: 1px; }
  .fx-shell .fx-bar-row { padding-top: 8px; padding-bottom: 8px; }
  .fx-bar-name, .fx-bar-percent { font-size: 11px; }
  .fx-bar-value { font-size: 10px; }
  .fx-bar-track { height: 6px; }
  .fx-panel-metrics span { font-size: 10px; }
  .fx-shell .fx-small-note, .fx-shell .fx-callout, .fx-shell .fx-anchor-note { font-size: 11px; }
}
@media (prefers-reduced-motion: reduce) { .fx-shell *, .fx-shell *::before, .fx-shell *::after { scroll-behavior: auto !important; transition: none !important; } }
)-----"

# Public rendering and export helpers. The distribution builder combines this
# file with the preparation/geometry helpers and the inline JavaScript/CSS.

#' Render an interactive fisheries landings map from a prepared payload
#'
#' @param payload A list returned by build_fisheries_map_payload().
#' @param height Widget height in pixels. The mobile detail panel stacks below it.
#' @param width Widget width, normally "100%".
#' @param initial_view One of "landings", "composition", or "species".
#' @param element_id Optional unique htmlwidget ID; leave NULL in Shiny modules.
#' @return A leaflet htmlwidget, suitable for renderLeaflet() or saveWidget().
#' @details The map contains its own vector coastline when land was supplied.
#' All three views, the linked details panel, and CSV download work without an
#' R server after export. No live data API or basemap tile server is required.
#' The composition view uses equal-sized screen-space donuts: size has no
#' quantitative meaning. Landings amounts are encoded by cell colour and bars.
render_fisheries_landings <- function(payload, height = 800, width = "100%",
                                     initial_view = c("landings", "composition", "species"),
                                     element_id = NULL) {
  .fx_data_require(c("leaflet", "htmlwidgets", "jsonlite"))
  initial_view <- match.arg(initial_view)
  if (!is.list(payload) || is.null(payload$rectangles$features) ||
      is.null(payload$meta) || is.null(payload$species)) {
    stop("payload must be returned by build_fisheries_map_payload().", call. = FALSE)
  }
  if (length(height) != 1L || !is.numeric(height) || !is.finite(height) || height < 420) {
    stop("height must be a single number of pixels, at least 420.", call. = FALSE)
  }
  # Serialize explicitly so GeoJSON feature arrays, coordinates, and named
  # species values retain their shape through htmlwidgets' data conversion.
  # Numeric coordinate matrices serialize much faster than per-coordinate R
  # lists while retaining the exact same GeoJSON structure and values.
  for (key in c("rectangles", "rawRectangles", "boundary", "land")) {
    if (!is.null(payload[[key]])) {
      payload[[key]] <- .fx_normalise_geojson_coordinates(payload[[key]])
    }
  }
  payload_json <- as.character(jsonlite::toJSON(
    payload, auto_unbox = TRUE, null = "null", na = "null",
    digits = 17, dataframe = "rows", matrix = "rowmajor", force = TRUE
  ))
  widget <- leaflet::leaflet(
    width = width, height = height, elementId = element_id,
    options = leaflet::leafletOptions(
      minZoom = 3, maxZoom = 12, zoomSnap = 0.25,
      zoomControl = TRUE, scrollWheelZoom = FALSE,
      preferCanvas = FALSE, attributionControl = TRUE
    )
  )
  hook <- paste0(
    "function(el, x, data) {\n",
    "  var style = document.getElementById('fx-landings-styles-v1');\n",
    "  if (!style) { style = document.createElement('style'); style.id = 'fx-landings-styles-v1'; document.head.appendChild(style); }\n",
    "  style.textContent = data.css;\n",
    .FX_LANDINGS_JS, "\n",
    "  var map = (this && typeof this.getMap === 'function') ? this.getMap() : this;\n",
    "  window.fisheriesLandingsMap(map, el, JSON.parse(data.payload));\n",
    "  if (data.initialView !== 'landings' && el._fxMap) el._fxMap.setMode(data.initialView);\n",
    "}"
  )
  widget <- htmlwidgets::onRender(widget, hook, data = list(
    payload = payload_json, css = .FX_LANDINGS_CSS, initialView = initial_view
  ))
  attr(widget, "fisheries_payload") <- payload
  widget
}

#' Prepare, position, and plot landings from an RDBES-style data frame
#'
#' @param data Input rows; see prepare_fisheries_landings().
#' @param years Explicit years included in the annual mean.
#' @param weight_unit Required: "kg", "t", or "raw". Kilograms are converted to
#'   tonnes exactly once. "raw" keeps values and labels them "source units".
#' @param rectangle_ids Optional vector of ICES reporting rectangles selected
#'   for the ecoregion; no area weighting or ecoregion apportionment is performed.
#' @param guild_lookup Species/guild lookup, with column names configurable in ...
#' @param guild Guild name, or NULL to include all species in the selected input.
#' @param boundary Optional sf ecoregion boundary for context.
#' @param land Optional sf land POLYGON/MULTIPOLYGON layer. Supplying a cached
#'   suitable-resolution coastline enables marine anchors and an offline basemap.
#' @param rectangle_sf Optional official sf rectangle polygons.
#' @param centres Optional data.frame with rectangle/lon/lat grid centres.
#' @param rectangle_id_col ID column in rectangle_sf (default "rectangle").
#' @param anchor_overrides Optional data.frame rectangle/lon/lat. Overrides must
#'   remain inside their own marine cell. They change drawing position only.
#' @param analysis_crs Projected CRS used for coastal geometric operations.
#' @param height Widget height, default 800 pixels.
#' @param width Widget width, default "100%".
#' @param initial_view Initial map view; default "landings".
#' @param element_id Optional unique widget ID.
#' @param ... Additional preparation arguments, including top_n, coverage_pct,
#'   annual_policy, coverage, guild_overrides, species_names, species_colors,
#'   region, title, source_note, and configurable input column names.
#' @return A leaflet htmlwidget. attr(x, "fisheries_prepared") contains local
#'   preparation diagnostics and annual tables; these are not sent to the browser.
#' @examples
#' # m <- plot_fisheries_landings(
#' #   data = cl.all2, years = 2022:2024, weight_unit = "kg",
#' #   rectangle_ids = area, guild_lookup = sid, guild = "Pelagic",
#' #   boundary = shape_ecoregion, land = land_sf, region = "Celtic Seas",
#' #   top_n = 5, annual_policy = "strict"
#' # )
plot_fisheries_landings <- function(data, years, weight_unit,
                                   rectangle_ids = NULL, guild_lookup = NULL,
                                   guild = NULL, boundary = NULL, land = NULL,
                                   rectangle_sf = NULL, centres = NULL,
                                   rectangle_id_col = "rectangle",
                                   anchor_overrides = NULL, analysis_crs = 3035,
                                   height = 800, width = "100%",
                                   initial_view = c("landings", "composition", "species"),
                                   element_id = NULL, ...) {
  prepared <- prepare_fisheries_landings(
    data = data, years = years, weight_unit = weight_unit,
    rectangle_ids = rectangle_ids, guild_lookup = guild_lookup, guild = guild, ...
  )
  payload <- build_fisheries_map_payload(
    prepared = prepared, rectangle_sf = rectangle_sf, centres = centres,
    rectangle_id_col = rectangle_id_col, boundary = boundary, land = land,
    anchor_overrides = anchor_overrides, analysis_crs = analysis_crs
  )
  widget <- render_fisheries_landings(
    payload, height = height, width = width,
    initial_view = match.arg(initial_view), element_id = element_id
  )
  attr(widget, "fisheries_prepared") <- prepared
  widget
}

#' Save a complete interactive map
#' @param map A widget returned by plot_fisheries_landings() or
#'   render_fisheries_landings().
#' @param file Destination ending in .html; parent directories are created.
#' @param selfcontained TRUE embeds the assets in one file and requires Pandoc
#'   (normally supplied by RStudio). FALSE creates an HTML plus dependency folder.
#' @return Invisibly, the absolute output path.
save_fisheries_map <- function(map, file, selfcontained = TRUE) {
  .fx_data_require("htmlwidgets")
  if (length(file) != 1L || is.na(file) || !grepl("\\.html$", file, ignore.case = TRUE)) {
    stop("file must be one .html output path.", call. = FALSE)
  }
  if (!inherits(map, "htmlwidget")) stop("map must be an htmlwidget.", call. = FALSE)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  htmlwidgets::saveWidget(map, file = file, selfcontained = selfcontained,
                          title = "fisheriesXplorer | Spatial landings")
  invisible(normalizePath(file, mustWork = TRUE))
}

#' Export the local calculation record for review
#' @param x A preparation result or fisheries widget carrying that result.
#' @param directory Output directory. Keep these detailed diagnostic files with
#'   the source analysis; the public HTML contains only the display payload.
#' @return Invisibly, the paths written.
export_fisheries_calculations <- function(x, directory) {
  .fx_data_require("jsonlite")
  if (inherits(x, "htmlwidget")) x <- attr(x, "fisheries_prepared")
  if (is.null(x$data) || is.null(x$rectangle_summary)) {
    stop("x must contain a fisheries preparation result.", call. = FALSE)
  }
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  paths <- character()
  # Convert list columns such as yearsUsed or Other-species members to a
  # semicolon-separated field for a human-readable CSV archive.
  csv_safe <- function(d) {
    d <- as.data.frame(d)
    for (nm in names(d)) if (is.list(d[[nm]])) {
      d[[nm]] <- vapply(d[[nm]], function(z) paste(z, collapse = "; "), character(1))
    }
    d
  }
  tabs <- c("data", "raw_data", "rectangle_summary", "annual", "annual_coverage", "species")
  for (nm in tabs) if (!is.null(x[[nm]])) {
    path <- file.path(directory, paste0(nm, ".csv"))
    utils::write.csv(csv_safe(x[[nm]]), path, row.names = FALSE, na = "")
    paths <- c(paths, path)
  }
  path <- file.path(directory, "method_and_diagnostics.json")
  jsonlite::write_json(list(meta = x$meta, diagnostics = x$diagnostics), path,
                        auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null")
  invisible(c(paths, path))
}
