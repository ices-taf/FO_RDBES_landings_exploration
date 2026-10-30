source("utilities_GES_pies.R")
source("utilities.R")
source("utilities_discards.R")

SAG_Settings <- getSAG_SettingsEcoregion("Bay of Biscay and the Iberian Coast")

sid <- getSID(2026, "Bay of Biscay and the Iberian Coast")
sag <- getSAG_ecoregion_new("Bay of Biscay and the Iberian Coast")
catch_current <- stockstatus_CLD_current_proxy(add_proxyRefPoints(format_sag(sag, sid),  sag_settings = SAG_Settings))
catch_current_v2 <- stockstatus_CLD_current_proxy(add_proxyRefPoints_v2(format_sag(sag, sid),  sag_settings = SAG_Settings))
head(catch_current) %>% print(width = Inf)
head(catch_current_v2) %>% print(width = Inf)
source("data_05_stock_status.R")

head(catch_current_adj) %>% print(width = Inf)
unique(discard_base_data$Year)


clean_status <- format_sag_status_new(getStatusWebService("Bay of Biscay and the Iberian Coast", sid), sag)
test <- getStatusWebService("Bay of Biscay and the Iberian Coast", sid)
head(clean_status)
head(test)
unique(test$stockSizeStatus)
unique(test$fishingPressureStatus)




####################################### new workflow for Fref - Bref
source("utilities_GES_pies.R")
source("utilities.R")
source("utilities_discards.R")

SAG_Settings <- getSAG_SettingsEcoregion("Icelandic Waters")

sid <- getSID(2026, "Icelandic Waters")
sag <- getSAG_ecoregion_new("Icelandic Waters")

# Reviewed precautionary reference-point selection for ICES SAG.
#
# REQUIREMENTS:
#   source("audit_sag_reference_points.R")   # provided separately
#   library(dplyr)
#   library(tidyr)
#
# The accompanying policy CSV was extracted from the user's completed Excel
# workbook on 2026-10-28. Approvals are user decisions, NOT independent
# verification of units or biological interpretation.
#
# IMPORTANT:
# - Original FMSY/MSYBtrigger are kept in *_original columns.
# - Standard F and B references are kept on their respective axes.
# - Settings-selected CUSTOM points use SAGChartKey 3 for F, 4 for B.
# - The two explicitly reviewer-approved, not-SAG-selected F custom points
#   (rjn.27.8c and sol.27.8c9a) are MANUAL CHART OVERRIDES, never inferred
#   automatically from their names.
# - nep.fu.31's chart-4 point named 'F MSY proxy' is provisionally approved;
#   source_issue_note retains the known anomaly without correcting its label.
# - Other named settings, e.g. BMGT_lower/BMGT_upper, are not silently mapped.
# - Nonpositive or non-finite values are ineligible.
# - Ties favour the original reference, then other standard points, then custom.
#
# This script does not replace add_proxyRefPoints_v2() or the plotting functions.

read_sag_review_policy_v3 <- function(path) {
  if (is.data.frame(path)) {
    policy <- path
  } else {
    policy <- utils::read.csv(
      path, stringsAsFactors = FALSE, check.names = FALSE,
      fileEncoding = "UTF-8", na.strings = c("NA")
    )
  }

  required <- c(
    "StockKeyLabel", "AssessmentKey", "Indicator", "reference_field",
    "reference_name", "source", "review_decision", "matches_numerator",
    "manual_chart_override", "unit_scale", "evidence_notes", "source_issue_note"
  )
  missing <- setdiff(required, names(policy))
  if (length(missing)) {
    stop("Policy is missing columns: ", paste(missing, collapse = ", "))
  }

  normalise_text <- function(z) {
    z <- trimws(as.character(z))
    z[is.na(z)] <- ""
    z
  }
  for (nm in c("StockKeyLabel", "AssessmentKey", "Indicator",
               "reference_field", "reference_name", "source",
               "review_decision", "matches_numerator", "unit_scale",
               "evidence_notes", "source_issue_note")) {
    policy[[nm]] <- normalise_text(policy[[nm]])
  }
  policy$review_decision <- toupper(policy$review_decision)
  policy$matches_numerator <- toupper(policy$matches_numerator)
  policy$manual_chart_override <- toupper(normalise_text(
    policy$manual_chart_override
  )) %in% c("TRUE", "T", "1", "YES")

  allowed_indicators <- c("Fref", "Bref", "Unassigned")
  if (any(!policy$Indicator %in% allowed_indicators)) {
    stop("Policy contains unexpected Indicator values.")
  }
  if (any(!policy$review_decision %in% c("INCLUDE", "EXCLUDE", "REVIEW"))) {
    stop("Policy review_decision must be INCLUDE, EXCLUDE, or REVIEW.")
  }
  if (any(!policy$matches_numerator %in% c("YES", "NO", "UNKNOWN"))) {
    stop("Policy matches_numerator must be YES, NO, or UNKNOWN.")
  }
  if (any(policy$manual_chart_override &
          (policy$source != "Custom" | !policy$Indicator %in% c("Fref", "Bref")))) {
    stop("Only approved F/B CUSTOM references can be manual chart overrides.")
  }

  join_keys <- c(
    "AssessmentKey", "StockKeyLabel", "Indicator", "source",
    "reference_field", "reference_name"
  )
  duplicates <- policy |>
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(n > 1L)
  if (nrow(duplicates)) {
    stop("Policy has duplicate keys. Resolve before using it for selection.")
  }
  policy
}

add_proxyRefPoints_v3 <- function(
    sag_formatted,
    sag_settings,
    review_policy = "./data/SAG_reference_point_policy_approved.csv",
    return_audit = FALSE,
    allow_manual_overrides = TRUE,
    strict_current_review = TRUE
) {
  if (!exists("audit_sag_reference_points", mode = "function") ||
      !exists("audit_sag_refpoints_by_stock", mode = "function")) {
    stop("First source audit_sag_reference_points.R")
  }
  if (!is.data.frame(sag_formatted)) stop("sag_formatted must be a data frame.")
  if (any(c("Fref_value", "Bref_value") %in% names(sag_formatted))) {
    stop("Pass the ORIGINAL format_sag(sag, sid), not output from v3.")
  }

  policy <- read_sag_review_policy_v3(review_policy)
  audit <- audit_sag_reference_points(sag_formatted, sag_settings)
  to_num <- function(z) suppressWarnings(as.numeric(as.character(z)))
  normalise_text <- function(z) {
    z <- trimws(as.character(z))
    z[is.na(z)] <- ""
    z
  }
  keys <- c(
    "AssessmentKey", "StockKeyLabel", "Indicator", "source",
    "reference_field", "reference_name"
  )

  base <- audit$candidates |>
    dplyr::mutate(
      AssessmentKey = as.character(AssessmentKey),
      StockKeyLabel = normalise_text(StockKeyLabel),
      reference_name = normalise_text(reference_name),
      source = as.character(source),
      reference_field = as.character(reference_field),
      Indicator = as.character(Indicator),
      value = to_num(value)
    )
  policy_cols <- c(keys, "review_decision", "matches_numerator",
                   "manual_chart_override", "unit_scale", "evidence_notes",
                   "source_issue_note")
  policy <- policy |> dplyr::select(dplyr::all_of(policy_cols))

  # Every standard point has an intrinsic F/B dimension. Every SAG-selected
  # custom point has its F/B dimension from chart 3/4 in the audit helper.
  assigned <- base |>
    dplyr::filter(Indicator %in% c("Fref", "Bref")) |>
    dplyr::left_join(policy, by = keys) |>
    dplyr::mutate(manual_chart_override = FALSE)

  # A custom point without a numeric chart-3/4 setting cannot be assigned
  # to F or B by its NAME alone. Include it only where the user explicitly
  # approved both its axis and an override in the review policy.
  manual_policy <- policy |>
    dplyr::filter(
      manual_chart_override, Indicator %in% c("Fref", "Bref"),
      review_decision == "INCLUDE", matches_numerator == "YES"
    )
  unassigned <- base |>
    dplyr::filter(Indicator == "Unassigned", source == "Custom")

  if (isTRUE(allow_manual_overrides)) {
    manual <- unassigned |>
      dplyr::select(-Indicator) |>
      dplyr::inner_join(manual_policy, by = setdiff(keys, "Indicator"))
    found_manual <- manual |>
      dplyr::select(dplyr::all_of(keys)) |>
      dplyr::distinct()
    missing_manual <- dplyr::anti_join(manual_policy, found_manual, by = keys)
    if (nrow(missing_manual)) {
      stop("A manual override in the approved review has no matching SAG ",
           "candidate. The snapshot may have changed; review before running v3.")
    }
  } else {
    manual <- assigned[FALSE, , drop = FALSE]
  }

  # Retain the rest of the unassigned custom inventory for inspection,
  # including the explicitly excluded historical eel reference points.
  unassigned_unmapped <- unassigned |>
    dplyr::left_join(
      policy |> dplyr::filter(Indicator == "Unassigned"),
      by = keys
    ) |>
    dplyr::mutate(manual_chart_override = FALSE)

  candidates <- dplyr::bind_rows(assigned, manual, unassigned_unmapped) |>
    dplyr::mutate(
      # Only manual rows explicitly approved for an axis can be used without
      # a SAG chart assignment. Otherwise the SAG chart is always honoured.
      chart_compatible = dplyr::case_when(
        Indicator == "Unassigned" ~ FALSE,
        source == "Standard" ~ TRUE,
        manual_chart_override %in% TRUE ~ TRUE,
        Indicator == "Fref" ~ SAGChartKey == 3L,
        Indicator == "Bref" ~ SAGChartKey == 4L,
        TRUE ~ FALSE
      ),
      chart_compatible = dplyr::coalesce(chart_compatible, FALSE),
      eligibility = dplyr::case_when(
        Indicator == "Unassigned" ~ "Unassigned to F/B",
        !is.finite(value) | value <= 0 ~ "Invalid/nonpositive value",
        !chart_compatible ~ "SAG chart incompatible",
        is.na(review_decision) ~ "Unreviewed",
        review_decision == "EXCLUDE" ~ "Excluded by reviewer",
        review_decision != "INCLUDE" ~ "Review pending",
        is.na(matches_numerator) | matches_numerator != "YES" ~
          "Numerator compatibility not approved",
        TRUE ~ "Eligible"
      ),
      eligible_v3 = eligibility == "Eligible",
      choice_id = paste(
        .audit_row_id, Indicator, source, reference_field, reference_name,
        dplyr::coalesce(as.character(SAGChartKey), "manual_or_none"),
        sep = "::"
      )
    )

  # Prevent silent use of an old review on a changed current SAG snapshot.
  # Historical rows remain in the audit; the approved field-level decisions
  # are carried across years for the same assessment and reference name.
  current_check <- candidates |>
    dplyr::filter(Indicator %in% c("Fref", "Bref")) |>
    dplyr::mutate(
      .AY = to_num(AssessmentYear), .Y = to_num(Year)
    ) |>
    dplyr::group_by(StockKeyLabel) |>
    dplyr::mutate(
      .AY_latest = if (all(!is.finite(.AY))) NA_real_ else max(.AY[is.finite(.AY)])
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      is.finite(value), value > 0,
      is.finite(.AY_latest),
      (Indicator == "Fref" & .Y == .AY_latest - 1) |
        (Indicator == "Bref" & (.Y == .AY_latest | .Y == .AY_latest - 1))
    )
  pending_current <- current_check |>
    dplyr::filter(eligibility %in% c("Unreviewed", "Review pending"))
  if (isTRUE(strict_current_review) && nrow(pending_current)) {
    stop(nrow(pending_current),
         " positive current-year F/B candidate record(s) are not approved ",
         "by the policy. See return_audit with strict_current_review=FALSE ",
         "to inspect them.")
  }

  picked <- candidates |>
    dplyr::filter(eligible_v3, Indicator %in% c("Fref", "Bref")) |>
    dplyr::mutate(
      .rank = dplyr::if_else(Indicator == "Fref", value, -value),
      .tie_original = dplyr::case_when(
        Indicator == "Fref" & reference_field == "FMSY" ~ 0L,
        Indicator == "Bref" & reference_field == "MSYBtrigger" ~ 0L,
        source == "Standard" ~ 1L,
        TRUE ~ 2L
      )
    ) |>
    dplyr::arrange(
      .audit_row_id, Indicator, .rank, .tie_original,
      reference_field, reference_name
    ) |>
    dplyr::group_by(.audit_row_id, Indicator) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup()

  candidates <- candidates |>
    dplyr::mutate(winner_v3 = eligible_v3 & choice_id %in% picked$choice_id)

  # Audit-friendly lists retain ALL eligible candidates, not only winners.
  listings <- candidates |>
    dplyr::filter(eligible_v3, Indicator %in% c("Fref", "Bref")) |>
    dplyr::mutate(
      candidate_label = paste0(
        dplyr::if_else(nzchar(reference_name), reference_name, reference_field),
        " [", reference_field, "] = ", signif(value, 8),
        dplyr::if_else(manual_chart_override %in% TRUE,
                       " (manual chart override)", "")
      )
    ) |>
    dplyr::group_by(.audit_row_id, Indicator) |>
    dplyr::summarise(
      candidate_count = dplyr::n(),
      candidate_list = paste(candidate_label, collapse = " | "),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      id_cols = .audit_row_id,
      names_from = Indicator,
      values_from = c(candidate_count, candidate_list),
      names_glue = "{Indicator}_{.value}"
    )

  selections <- picked |>
    dplyr::transmute(
      .audit_row_id, Indicator,
      value = value,
      reference_name = dplyr::if_else(nzchar(reference_name), reference_name,
                                      reference_field),
      reference_field, source,
      manual_chart_override = manual_chart_override,
      SAGChartKey = SAGChartKey,
      review_note = dplyr::coalesce(source_issue_note, "")
    ) |>
    tidyr::pivot_wider(
      id_cols = .audit_row_id,
      names_from = Indicator,
      values_from = c(value, reference_name, reference_field, source,
                      manual_chart_override, SAGChartKey, review_note),
      names_glue = "{Indicator}_{.value}"
    )

  # The same column names as the original audit's selected data frame make
  # stock-level provenance extraction possible without a new summariser.
  base_rows <- audit$selected |>
    dplyr::select(.audit_row_id, AssessmentKey, StockKeyLabel,
                  FisheriesGuild, AssessmentYear, Year)
  selected_v3 <- base_rows |>
    dplyr::left_join(selections, by = ".audit_row_id") |>
    dplyr::left_join(listings, by = ".audit_row_id")
  for (nm in c("Fref_value", "Bref_value")) {
    if (!nm %in% names(selected_v3)) selected_v3[[nm]] <- NA_real_
  }
  for (nm in c("Fref_reference_name", "Bref_reference_name",
               "Fref_reference_field", "Bref_reference_field",
               "Fref_source", "Bref_source",
               "Fref_candidate_list", "Bref_candidate_list",
               "Fref_review_note", "Bref_review_note")) {
    if (!nm %in% names(selected_v3)) selected_v3[[nm]] <- NA_character_
  }
  for (nm in c("Fref_manual_chart_override", "Bref_manual_chart_override")) {
    if (!nm %in% names(selected_v3)) selected_v3[[nm]] <- FALSE
  }

  out <- sag_formatted |>
    dplyr::mutate(
      .audit_row_id = dplyr::row_number(),
      FMSY_original = to_num(FMSY),
      MSYBtrigger_original = to_num(MSYBtrigger)
    ) |>
    dplyr::left_join(
      selected_v3 |>
        dplyr::select(
          .audit_row_id,
          Fref_value, Bref_value,
          Fref_reference_name, Bref_reference_name,
          Fref_reference_field, Bref_reference_field,
          Fref_source, Bref_source,
          Fref_candidate_list, Bref_candidate_list,
          Fref_manual_chart_override, Bref_manual_chart_override,
          Fref_review_note, Bref_review_note
        ),
      by = ".audit_row_id"
    ) |>
    dplyr::mutate(
      FMSY = to_num(Fref_value),
      MSYBtrigger = to_num(Bref_value),
      FMSY_is_proxy = is.finite(FMSY) &
        !is.na(Fref_reference_field) & Fref_reference_field != "FMSY",
      MSYB_is_proxy = is.finite(MSYBtrigger) &
        !is.na(Bref_reference_field) & Bref_reference_field != "MSYBtrigger",
      FMSY_proxy_name = dplyr::if_else(FMSY_is_proxy,
                                      Fref_reference_name, NA_character_),
      MSYB_proxy_name = dplyr::if_else(MSYB_is_proxy,
                                      Bref_reference_name, NA_character_),
      FMSY_refpoint_source = dplyr::case_when(
        !is.finite(FMSY) ~ "Missing",
        Fref_reference_field == "FMSY" ~ "Original",
        Fref_source == "Custom" ~ "Custom",
        TRUE ~ "Standard alternative"
      ),
      MSYB_refpoint_source = dplyr::case_when(
        !is.finite(MSYBtrigger) ~ "Missing",
        Bref_reference_field == "MSYBtrigger" ~ "Original",
        Bref_source == "Custom" ~ "Custom",
        TRUE ~ "Standard alternative"
      )
    ) |>
    dplyr::select(-.audit_row_id)

  if (nrow(out) != nrow(sag_formatted)) {
    stop("v3 changed the row count; review joins before using its output.")
  }
  if (!isTRUE(return_audit)) return(out)

  v2_current <- audit_sag_refpoints_by_stock(audit)
  v3_current <- audit_sag_refpoints_by_stock(list(selected = selected_v3))
  summarise_ref <- function(d, suffix) {
    out <- d |>
      dplyr::select(StockKeyLabel, Fref, Fref_name, Fref_field,
                    F_Year, Bref, Bref_name, Bref_field, B_Year)
    names(out)[-1] <- paste0(names(out)[-1], "_", suffix)
    out
  }
  difference <- function(a, b) {
    one_missing <- xor(is.na(a), is.na(b))
    both_present <- !is.na(a) & !is.na(b)
    different_number <- rep(FALSE, length(a))
    different_number[both_present] <- abs(a[both_present] - b[both_present]) >
      1e-9 * pmax(1, abs(a[both_present]), abs(b[both_present]))
    one_missing | different_number
  }
  comparison <- dplyr::full_join(
    summarise_ref(v2_current, "v2"),
    summarise_ref(v3_current, "v3"),
    by = "StockKeyLabel"
  ) |>
    dplyr::mutate(
      F_changed = difference(Fref_v2, Fref_v3),
      B_changed = difference(Bref_v2, Bref_v3),
      changed = F_changed | B_changed
    ) |>
    dplyr::arrange(StockKeyLabel)

  list(
    sag = out,
    candidates = candidates,
    selected = selected_v3,
    current_v2 = v2_current,
    current_v3 = v3_current,
    comparison = comparison,
    review_policy = policy,
    unresolved_settings = audit$unresolved_settings,
    unreviewed_current = pending_current
  )
}

# Attach both F/B reference-point provenance records to the one-row-per-stock
# output from stockstatus_CLD_current_proxy(). The original function drops
# assessment keys, names, candidates and source. This helper restores them
# without reclassifying a stock or changing catches/landings/discards.
attach_refpoint_provenance_v3 <- function(stock_current, current_v3) {
  if (!"StockKeyLabel" %in% names(stock_current) ||
      !"StockKeyLabel" %in% names(current_v3)) {
    stop("Both tables require StockKeyLabel.")
  }
  if (anyDuplicated(current_v3$StockKeyLabel)) {
    stop("current_v3 has duplicate StockKeyLabel values; resolve before joining.")
  }
  selected <- current_v3 |>
    dplyr::select(
      StockKeyLabel, F_AssessmentKey, F_Year,
      Fref, Fref_name, Fref_field, Fref_source, Fref_candidates,
      B_AssessmentKey, B_Year,
      Bref, Bref_name, Bref_field, Bref_source, Bref_candidates
    )
  out <- dplyr::left_join(stock_current, selected, by = "StockKeyLabel")
  if (nrow(out) != nrow(stock_current)) {
    stop("Joining the provenance table changed stock row counts.")
  }

  # The older stockstatus_CLD_current_proxy() uses a broad %in% filter for
  # B-years, rather than a row-wise year comparison. Signal any disagreement
  # between the reference value in the status table and the audited choice.
  differs <- function(a, b) {
    a <- suppressWarnings(as.numeric(as.character(a)))
    b <- suppressWarnings(as.numeric(as.character(b)))
    valid <- is.finite(a) & is.finite(b)
    out <- rep(FALSE, length(a))
    out[valid] <- abs(a[valid] - b[valid]) >
      1e-9 * pmax(1, abs(a[valid]), abs(b[valid]))
    out
  }
  if (all(c("FMSY", "Fref") %in% names(out)) &&
      any(differs(out$FMSY, out$Fref))) {
    warning("Some F references differ between stockstatus and v3 audit.")
  }
  if (all(c("MSYBtrigger", "Bref") %in% names(out)) &&
      any(differs(out$MSYBtrigger, out$Bref))) {
    warning("Some B references differ between stockstatus and v3 audit. ",
            "Check the per-stock AY_latest/Year filtering in stockstatus.")
  }
  out
}

# EXAMPLE - run after sourcing utilities_GES_pies.R, utilities.R, and
# audit_sag_reference_points.R, and after obtaining sag, sid, SAG_Settings:
#
make_sag_refpoint_policy(
  original,
  SAG_Settings,
  "./data/SAG_reference_point_policy_approved_IS.csv",
  previous_policy = "./data/SAG_reference_point_policy_approved.csv"
)


original <- format_sag(sag, sid)
result <- add_proxyRefPoints_v3(
  original, SAG_Settings,
  review_policy = "./data/SAG_reference_point_policy_approved.csv",
  return_audit = TRUE
)

# v3_check <- add_proxyRefPoints_v3(
#   sag_formatted,
#   SAG_Settings,
#   review_policy = "./data/SAG_reference_point_policy_approved.csv",
#   return_audit = TRUE,
#   strict_current_review = FALSE
# )

# v3_check$unreviewed_current %>%
#   dplyr::select(
#     StockKeyLabel,
#     AssessmentKey,
#     AssessmentYear,
#     Year,
#     Indicator,
#     SAGChartKey,
#     reference_field,
#     reference_name,
#     value,
#     review_decision,
#     matches_numerator,
#     eligibility
#   ) %>%
#   print(n = Inf, width = Inf)
View(result$current_v3)
View(result$comparison |> dplyr::filter(changed))
View(result$candidates |> dplyr::filter(winner_v3))
View(result$unresolved_settings)

test <- catch_current_v3 %>% filter(StockKeyLabel == "ane.27.9aW") %>% print(width = Inf)
catch_current_v3 <- stockstatus_CLD_current_proxy(result$sag)
catch_current_v3 <- attach_refpoint_provenance_v3(
  catch_current_v3, result$current_v3
)
#
dir.create("./output/reference_points", recursive = TRUE, showWarnings = FALSE)
saveRDS(result, "./output/reference_points/SAG_reference_v3_audit_IS.rds")
utils::write.csv(result$current_v3,
  "./output/reference_points/SAG_refpoints_v3_by_stock_IS.csv", row.names = FALSE)
utils::write.csv(result$comparison,
  "./output/reference_points/SAG_v2_vs_v3_IS.csv", row.names = FALSE)
utils::write.csv(result$candidates,
  "./output/reference_points/SAG_v3_all_candidates_IS.csv", row.names = FALSE)


catch_current_v3 <- stockstatus_CLD_current_proxy(result$sag)
catch_current_v3 <- attach_refpoint_provenance_v3(
  catch_current_v3, result$current_v3
)

head(catch_current_v3) %>% print(width = Inf)
unique(catch_current_v3$Year)

# catch_current <- stockstatus_CLD_current_proxy(add_proxyRefPoints_v2(format_sag(sag, sid),  sag_settings = SAG_Settings))


############### adding ecoregion shares

new_proportions <- read.csv(
  "output/stock_matching_rectangle_check/Bay_of_Biscay_and_the_Iberian_Coast/20261028_011823/stock_share_lookup_with_anchovy_proxy.csv",
  stringsAsFactors = FALSE
)

# 2. Prepare one share per stock and RDBES year.
# Keep NA shares: an unavailable estimate is not a zero share.
share_lookup <- new_proportions %>%
  transmute(
    StockKeyLabel = as.character(fishstock),
    RDBES_share_year = as.integer(CLyear),
    share = ShareLandingsInEcoregion / 100
  )
share_lookup
# Check that the lookup has unique stock-year keys and valid proportions.
stopifnot(
  !anyDuplicated(
    share_lookup[c("StockKeyLabel", "RDBES_share_year")]
  ),
  all(
    is.na(share_lookup$share) |
      between(share_lookup$share, 0, 1)
  )
)

catch_current_adj <- catch_current_v3 %>%
  mutate(
    StockKeyLabel = as.character(StockKeyLabel),
    Year = as.integer(as.character(Year)),

    RDBES_share_year = if_else(Year == 2026L, 2025L, Year)
  ) %>%
  left_join(
    share_lookup %>%
      mutate(
        StockKeyLabel = as.character(StockKeyLabel),
        RDBES_share_year = as.integer(RDBES_share_year)
      ),
    by = c("StockKeyLabel", "RDBES_share_year"),
    relationship = "many-to-one",
    na_matches = "never"
  ) %>%
  mutate(
    share_status = case_when(
      is.na(share) ~ "share unavailable",
      Year == 2026L ~ "2025 share applied to 2026",
      TRUE ~ "same-year share"
    ),

    Catches_ecoregion  = Catches * share,
    Landings_ecoregion = Landings * share,
    Discards_ecoregion = Discards * share
  )


catch_current_adj %>%
  filter(Year == 2026) %>%
  select(
    StockKeyLabel, Year, RDBES_share_year,
    Catches, share, Catches_ecoregion,
    share_status
  ) %>%
  print(n = 30)  
head(catch_current_adj) %>% print(width = Inf)
unique(catch_current_adj$Year)
names(catch_current_adj)

### check the stocks with unavailable shares
stock_shaare_unavailable <- catch_current_adj %>%
  filter(share_status == "share unavailable") %>%
  select(StockKeyLabel, Year, RDBES_share_year, share, share_status,Catches,Landings, Discards, Catches_ecoregion,Landings_ecoregion,Discards_ecoregion) %>%
  print(n = 40, width = Inf)

write.csv(
  stock_shaare_unavailable,
  "output/stock_matching_rectangle_check/Bay_of_Biscay_and_the_Iberian_Coast/20261028_011823/stock_share_unavailable.csv",
  row.names = FALSE
)
# source("data_05_stock_status.R")
# clean_status <- format_sag_status_new(getStatusWebService("Bay of Biscay and the Iberian Coast", sid), sag)