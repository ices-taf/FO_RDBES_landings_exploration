#' Audit candidate reference points used by add_proxyRefPoints_v2
#'
#' This is a READ-ONLY diagnostic function. It does not change the reference
#' points or the Kobe/status calculations currently used by the app.
#'
#' It retains all recognised standard SAG reference-point fields and all
#' numbered CustomRefPointName/Value fields that exist in the supplied data.
#' Custom point types are inferred ONLY from a numeric selection for SAG
#' chart 3 (Fref) or 4 (Bref); unselected custom points remain Unassigned.
#'
#' The column `considered_in_v2` reproduces the current selection *scope*:
#' original FMSY/MSYBtrigger and settings-selected custom indices 1-4.
#' Within this scope, the lowest positive F reference and highest positive B
#' reference are selected. Ties favour the standard reference point.
#'
#' @return A list with candidates, selected, unresolved_settings, settings,
#'   and discovered_columns data frames.
#' @noRd

audit_sag_reference_points <- function(sag_formatted, sag_settings) {

  need_sag <- c("AssessmentKey", "FMSY", "MSYBtrigger")
  need_settings <- c("AssessmentKey", "settingKey", "SAGChartKey", "settingValue")
  if (length(setdiff(need_sag, names(sag_formatted)))) {
    stop("Missing SAG columns: ",
         paste(setdiff(need_sag, names(sag_formatted)), collapse = ", "))
  }
  if (length(setdiff(need_settings, names(sag_settings)))) {
    stop("Missing settings columns: ",
         paste(setdiff(need_settings, names(sag_settings)), collapse = ", "))
  }

  to_num <- function(z) suppressWarnings(as.numeric(as.character(z)))
  optional_col <- function(name) {
    if (name %in% names(sag_formatted)) {
      as.character(sag_formatted[[name]])
    } else {
      rep(NA_character_, nrow(sag_formatted))
    }
  }

  # Preserve EVERY original assessment row. Ref. values may differ by row/year.
  base <- tibble::tibble(
    .audit_row_id = seq_len(nrow(sag_formatted)),
    AssessmentKey = as.character(sag_formatted$AssessmentKey),
    StockKeyLabel = optional_col("StockKeyLabel"),
    FisheriesGuild = optional_col("FisheriesGuild"),
    AssessmentYear = optional_col("AssessmentYear"),
    Year = optional_col("Year")
  )

  settings <- sag_settings |>
    dplyr::filter(
      as.character(settingKey) == "51",
      as.character(SAGChartKey) %in% c("3", "4")
    ) |>
    dplyr::transmute(
      AssessmentKey = as.character(AssessmentKey),
      SAGChartKey = as.integer(as.character(SAGChartKey)),
      setting_token = as.character(settingValue)
    ) |>
    tidyr::separate_rows(setting_token, sep = ",") |>
    dplyr::mutate(setting_token = trimws(setting_token)) |>
    dplyr::filter(
      !is.na(AssessmentKey), nzchar(AssessmentKey),
      !is.na(setting_token), nzchar(setting_token)
    ) |>
    dplyr::distinct()

  # Column names are matched case-insensitively; keep their actual spelling.
  standard_specs <- tibble::tibble(
    canonical = c(
      "FMSY", "Fpa", "FLim", "Fmanagement",
      "MSYBtrigger", "Bpa", "Blim", "Bmanagement"
    ),
    Indicator = c(rep("Fref", 4), rep("Bref", 4))
  )
  standard_specs$field <- names(sag_formatted)[
    match(tolower(standard_specs$canonical), tolower(names(sag_formatted)))
  ]
  standard_specs <- standard_specs |>
    dplyr::filter(!is.na(field)) |>
    dplyr::distinct(field, .keep_all = TRUE)

  standard_rows <- dplyr::bind_rows(lapply(seq_len(nrow(standard_specs)), function(i) {
    s <- standard_specs[i, ]
    base |>
      dplyr::mutate(
        Indicator = s$Indicator[[1]],
        source = "Standard",
        reference_field = s$field[[1]],
        reference_name = s$canonical[[1]],
        reference_notes = NA_character_,
        custom_index = NA_integer_,
        SAGChartKey = NA_integer_,
        setting_selected = FALSE,
        value = to_num(sag_formatted[[s$field[[1]]]])
      )
  }))

  # Discover all numbered custom columns, not just indices 1-4.
  value_fields <- grep("^CustomRefPointValue[0-9]+$", names(sag_formatted), value = TRUE)
  name_fields  <- grep("^CustomRefPointName[0-9]+$", names(sag_formatted), value = TRUE)
  note_fields  <- grep("^CustomRefPointNotes?[0-9]+$", names(sag_formatted), value = TRUE)
  value_ids <- as.integer(sub("^CustomRefPointValue", "", value_fields))
  name_ids <- as.integer(sub("^CustomRefPointName", "", name_fields))
  note_ids <- as.integer(sub("^CustomRefPointNotes?", "", note_fields))
  custom_indices <- sort(unique(c(value_ids, name_ids, note_ids)))

  custom_raw <- dplyr::bind_rows(lapply(custom_indices, function(i) {
    value_field <- paste0("CustomRefPointValue", i)
    name_field <- paste0("CustomRefPointName", i)
    vals <- if (value_field %in% names(sag_formatted)) {
      to_num(sag_formatted[[value_field]])
    } else {
      rep(NA_real_, nrow(base))
    }
    nm <- if (name_field %in% names(sag_formatted)) {
      trimws(as.character(sag_formatted[[name_field]]))
    } else {
      rep(NA_character_, nrow(base))
    }
    nm[!is.na(nm) & !nzchar(nm)] <- NA_character_
    note_field <- intersect(
      paste0(c("CustomRefPointNote", "CustomRefPointNotes"), i),
      names(sag_formatted)
    )
    notes <- if (length(note_field)) {
      as.character(sag_formatted[[note_field[[1]]]])
    } else {
      rep(NA_character_, nrow(base))
    }

    base |>
      dplyr::mutate(
        source = "Custom",
        reference_field = value_field,
        reference_name = nm,
        reference_notes = notes,
        custom_index = as.integer(i),
        value = vals
      )
  }))

  numeric_settings <- settings |>
    dplyr::filter(grepl("^[0-9]+$", setting_token)) |>
    dplyr::mutate(custom_index = as.integer(setting_token)) |>
    dplyr::select(AssessmentKey, SAGChartKey, custom_index) |>
    dplyr::distinct()

  # Join chart 3 and chart 4 separately, so each join is many-to-one
  # (repeated annual assessment rows are preserved without join warnings).
  if (nrow(custom_raw)) {
    assigned <- dplyr::bind_rows(lapply(c(3L, 4L), function(chart) {
      chart_choices <- numeric_settings |>
        dplyr::filter(SAGChartKey == chart) |>
        dplyr::select(AssessmentKey, custom_index) |>
        dplyr::distinct()
      custom_raw |>
        dplyr::inner_join(
          chart_choices,
          by = c("AssessmentKey", "custom_index")
        ) |>
        dplyr::mutate(
          SAGChartKey = as.integer(chart),
          Indicator = if (chart == 3L) "Fref" else "Bref",
          setting_selected = TRUE
        )
    }))
    unassigned <- custom_raw |>
      dplyr::anti_join(
        numeric_settings |>
          dplyr::select(AssessmentKey, custom_index) |>
          dplyr::distinct(),
        by = c("AssessmentKey", "custom_index")
      ) |>
      dplyr::mutate(
        SAGChartKey = NA_integer_,
        Indicator = "Unassigned",
        setting_selected = FALSE
      )
    custom_rows <- dplyr::bind_rows(assigned, unassigned)
  } else {
    custom_rows <- custom_raw |>
      dplyr::mutate(
        SAGChartKey = integer(0),
        Indicator = character(0),
        setting_selected = logical(0)
      )
  }

  candidates <- dplyr::bind_rows(standard_rows, custom_rows) |>
    dplyr::mutate(
      valid_positive = is.finite(value) & value > 0,
      considered_in_v2 = dplyr::case_when(
        source == "Standard" &
          tolower(reference_field) %in% c("fmsy", "msybtrigger") ~ TRUE,
        source == "Custom" & setting_selected &
          custom_index %in% 1:4 ~ TRUE,
        TRUE ~ FALSE
      ),
      .candidate_id = paste(
        .audit_row_id, Indicator, reference_field,
        dplyr::coalesce(as.character(SAGChartKey), "none"), sep = ":"
      )
    )

  # Exactly reproduce the selection logic in add_proxyRefPoints_v2:
  # use only positive values; minimum F, maximum B; original wins ties.
  picked <- candidates |>
    dplyr::filter(
      considered_in_v2, valid_positive,
      Indicator %in% c("Fref", "Bref")
    ) |>
    dplyr::mutate(
      .rank = dplyr::if_else(Indicator == "Fref", value, -value),
      .tie_original = dplyr::if_else(source == "Standard", 0L, 1L)
    ) |>
    dplyr::arrange(
      .audit_row_id, Indicator, .rank, .tie_original, custom_index
    ) |>
    dplyr::group_by(.audit_row_id, Indicator) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup()

  candidates <- candidates |>
    dplyr::mutate(
      selected_in_v2 = .candidate_id %in% picked$.candidate_id,
      audit_note = dplyr::case_when(
        selected_in_v2 ~ "Selected in current v2",
        !valid_positive ~ "Missing, non-finite, or nonpositive value",
        considered_in_v2 ~ "Compared in v2; another point selected",
        source == "Standard" ~ "Available standard point, not compared in v2",
        source == "Custom" & !setting_selected ~
          "No numeric chart 3/4 setting; type not established",
        source == "Custom" & custom_index > 4L ~
          "Selected in settings but index exceeds v2's 1-4 limit",
        TRUE ~ "Not compared in v2"
      )
    )

  picked_wide <- picked |>
    dplyr::transmute(
      .audit_row_id, Indicator,
      value, reference_name, reference_field, source
    ) |>
    tidyr::pivot_wider(
      id_cols = .audit_row_id,
      names_from = Indicator,
      values_from = c(value, reference_name, reference_field, source),
      names_glue = "{Indicator}_{.value}"
    )

  selected <- base |>
    dplyr::left_join(picked_wide, by = ".audit_row_id")

  # Always expose both types, including for assessments without valid points.
  for (nm in c("Fref_value", "Bref_value")) {
    if (!nm %in% names(selected)) selected[[nm]] <- NA_real_
  }
  for (nm in c(
    "Fref_reference_name", "Bref_reference_name",
    "Fref_reference_field", "Bref_reference_field",
    "Fref_source", "Bref_source"
  )) {
    if (!nm %in% names(selected)) selected[[nm]] <- NA_character_
  }

  # Keep a human-readable list of every POSITIVE candidate compared by v2.
  detail <- candidates |>
    dplyr::filter(
      considered_in_v2, valid_positive,
      Indicator %in% c("Fref", "Bref")
    ) |>
    dplyr::mutate(
      candidate_description = paste0(
        dplyr::coalesce(reference_name, reference_field),
        " [", reference_field, "] = ", signif(value, 8)
      )
    ) |>
    dplyr::group_by(.audit_row_id, Indicator) |>
    dplyr::summarise(
      candidate_count = dplyr::n(),
      candidate_list = paste(candidate_description, collapse = " | "),
      .groups = "drop"
    )

  for (indicator in c("Fref", "Bref")) {
    sub <- detail |>
      dplyr::filter(Indicator == indicator) |>
      dplyr::select(.audit_row_id, candidate_count, candidate_list)
    names(sub)[2:3] <- paste0(indicator, "_", names(sub)[2:3])
    selected <- dplyr::left_join(selected, sub, by = ".audit_row_id")
  }

  unresolved_settings <- settings |>
    dplyr::mutate(
      custom_index = suppressWarnings(as.integer(setting_token)),
      issue = dplyr::case_when(
        !grepl("^[0-9]+$", setting_token) ~
          "Named token: not mapped to a numeric reference in v2",
        !(custom_index %in% custom_indices) ~
          "Numeric index not present in downloaded custom fields",
        custom_index > 4L ~
          "Index present but v2 does not consider custom indices above 4",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(issue))

  if (nrow(selected) != nrow(sag_formatted)) {
    stop("Audit row count changed unexpectedly.")
  }

  list(
    candidates = candidates |>
      dplyr::select(
        .audit_row_id, AssessmentKey, StockKeyLabel, FisheriesGuild,
        AssessmentYear, Year, Indicator, source, reference_field, reference_name,
        custom_index, SAGChartKey, setting_selected, value,
        valid_positive, considered_in_v2, selected_in_v2, audit_note
      ),
    selected = selected |>
      dplyr::relocate(
        .audit_row_id, AssessmentKey, StockKeyLabel, FisheriesGuild,
        AssessmentYear, Year, Fref_value, Fref_reference_name, Fref_reference_field, Fref_source,
        Bref_value, Bref_reference_name, Bref_reference_field, Bref_source
      ),
    unresolved_settings = unresolved_settings,
    settings = settings,
    discovered_columns = tibble::tibble(
      reference_field = c(standard_specs$field, value_fields, name_fields, note_fields),
      source = c(
        rep("Standard", nrow(standard_specs)),
        rep("Custom value", length(value_fields)),
        rep("Custom name", length(name_fields)),
        rep("Custom notes", length(note_fields))
      )
    )
  )
}

#' Reduce the audit to ONE ROW PER STOCK, following the intended year rules
#' in stockstatus_CLD_current_proxy(): F from assessment year - 1; B from
#' assessment year or -1, preferring the latest available positive B reference.
#' This is for provenance review, not a replacement for stockstatus.
#' @noRd
audit_sag_refpoints_by_stock <- function(audit) {
  d <- audit$selected
  needed <- c("StockKeyLabel", "FisheriesGuild", "AssessmentYear", "Year",
              "Fref_value", "Bref_value")
  missing <- setdiff(needed, names(d))
  if (length(missing)) stop("Audit selected table lacks: ", paste(missing, collapse = ", "))

  num <- function(z) suppressWarnings(as.numeric(as.character(z)))
  d <- d |>
    dplyr::mutate(
      .year_num = num(Year),
      .assessment_year_num = num(AssessmentYear)
    ) |>
    dplyr::group_by(StockKeyLabel) |>
    dplyr::mutate(
      .AY_latest = if (all(!is.finite(.assessment_year_num))) {
        NA_real_
      } else {
        max(.assessment_year_num[is.finite(.assessment_year_num)])
      }
    ) |>
    dplyr::ungroup()

  f <- d |>
    dplyr::filter(is.finite(.AY_latest), .year_num == .AY_latest - 1) |>
    dplyr::arrange(StockKeyLabel, dplyr::desc(.year_num)) |>
    dplyr::group_by(StockKeyLabel) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      StockKeyLabel, FisheriesGuild,
      F_AssessmentKey = AssessmentKey,
      F_Year = .year_num,
      Fref = Fref_value,
      Fref_name = dplyr::coalesce(Fref_reference_name, Fref_reference_field),
      Fref_field = Fref_reference_field,
      Fref_source = Fref_source,
      Fref_candidates = Fref_candidate_list
    )

  b <- d |>
    dplyr::filter(
      is.finite(.AY_latest),
      .year_num == .AY_latest | .year_num == .AY_latest - 1,
      is.finite(Bref_value), Bref_value > 0
    ) |>
    dplyr::arrange(StockKeyLabel, dplyr::desc(.year_num)) |>
    dplyr::group_by(StockKeyLabel) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      StockKeyLabel, FisheriesGuild,
      B_AssessmentKey = AssessmentKey,
      B_Year = .year_num,
      Bref = Bref_value,
      Bref_name = dplyr::coalesce(Bref_reference_name, Bref_reference_field),
      Bref_field = Bref_reference_field,
      Bref_source = Bref_source,
      Bref_candidates = Bref_candidate_list
    )

  dplyr::full_join(f, b, by = c("StockKeyLabel", "FisheriesGuild")) |>
    dplyr::arrange(StockKeyLabel)
}

# Example (after running the user's existing data-loading workflow):
sag_formatted <- format_sag(sag, sid)
audit <- audit_sag_reference_points(sag_formatted, SAG_Settings)
View(audit$selected)             # chosen Fref/Bref and full v2 candidate lists
View(audit$candidates)           # all standard/custom fields and inclusion reasons
View(audit$unresolved_settings)  # settings such as Fpa or missing index 5
stock_audit <- audit_sag_refpoints_by_stock(audit)
View(stock_audit)                # ONE ROW per stock, F and B provenance
#
dir.create("./output/reference_points_Fref_Bref", recursive = TRUE, showWarnings = FALSE)
saveRDS(audit, "./output/reference_points_Fref_Bref/SAG_reference_audit.rds")
utils::write.csv(stock_audit,
  "./output/reference_points_Fref_Bref/SAG_selected_refpoints_by_stock.csv", row.names = FALSE)
utils::write.csv(audit$selected,
  "./output/reference_points_Fref_Bref/SAG_selected_refpoints.csv", row.names = FALSE)
utils::write.csv(audit$candidates,
  "./output/reference_points_Fref_Bref/SAG_all_candidate_refpoints.csv", row.names = FALSE)
utils::write.csv(audit$unresolved_settings,
  "./output/reference_points_Fref_Bref/SAG_unresolved_settings.csv", row.names = FALSE)
#
# Optional cross-check of the v2 function's numerical selection:
v2 <- add_proxyRefPoints_v2(sag_formatted, SAG_Settings)
stopifnot(nrow(v2) == nrow(audit$selected))
isTRUE(all.equal(v2$FMSY, audit$selected$Fref_value, check.attributes = FALSE))
isTRUE(all.equal(v2$MSYBtrigger, audit$selected$Bref_value,
                 check.attributes = FALSE))

