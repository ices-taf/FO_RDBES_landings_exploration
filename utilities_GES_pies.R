getSAG_SettingsEcoregion <- function(Ecoregion) {
        
        EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        sag_settings <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://sag.ices.dk/SAG_API/LatestStocks/Settings?ecoregion=%s", EcoregionCode)
                )
        )
        return(sag_settings)
}

# test <- getSAG_SettingsEcoregion("Celtic Seas")
# test %>%
#   dplyr::filter(
#     settingKey == 51,
#     SAGChartKey %in% c(3, 4)
#   ) %>%
#   dplyr::select(
#     AssessmentKey,
#     SAGChartKey,
#     settingValue
#   ) 

extract_custom_refpoint_choices <- function(sag_settings) {
  out <- sag_settings %>%
    dplyr::filter(settingKey == 51, SAGChartKey %in% c(3, 4)) %>%
    dplyr::transmute(
      AssessmentKey = as.integer(AssessmentKey),
      SAGChartKey = as.integer(SAGChartKey),
      settingValue = as.character(settingValue)
    ) %>%
    tidyr::separate_rows(settingValue, sep = ",") %>%
    dplyr::mutate(settingValue = trimws(settingValue)) %>%
    dplyr::filter(settingValue %in% c("1", "2", "3", "4")) %>%
    dplyr::group_by(AssessmentKey, SAGChartKey) %>%
    dplyr::summarise(settingValue = dplyr::first(settingValue), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = SAGChartKey,
      values_from = settingValue,
      names_prefix = "choice_"
    )

  if (!"choice_3" %in% names(out)) out$choice_3 <- NA_character_
  if (!"choice_4" %in% names(out)) out$choice_4 <- NA_character_

  out %>%
    dplyr::mutate(
      choice_3 = as.integer(choice_3),
      choice_4 = as.integer(choice_4)
    ) %>%
    dplyr::select(AssessmentKey, choice_3, choice_4)
}

extract_custom_refpoint_choices_v2 <- function(sag_settings) {

  sag_settings %>%
    dplyr::filter(
      settingKey == 51,
      SAGChartKey %in% c(3, 4)
    ) %>%
    dplyr::transmute(
      AssessmentKey = as.integer(AssessmentKey),
      SAGChartKey = as.integer(SAGChartKey),
      settingValue = as.character(settingValue)
    ) %>%
    tidyr::separate_rows(
      settingValue,
      sep = ","
    ) %>%
    dplyr::mutate(
      settingValue = trimws(settingValue)
    ) %>%
    dplyr::filter(
      !is.na(settingValue),
      nzchar(settingValue)
    ) %>%
    dplyr::distinct()
}

#' Apply proxy reference points to formatted SAG data
#'
#' Integrates proxy reference-point selections into a formatted SAG reference
#' point dataset. When SAG settings specify that a custom reference point
#' should replace the default reference point, the function overwrites the
#' corresponding values in the dataset.
#'
#' Specifically:
#' - `FMSY` is replaced using the selected custom reference point when a proxy
#'   is defined for the fishing mortality chart (`SAGChartKey == 3`).
#' - `MSYBtrigger` is replaced when a proxy is defined for the spawning stock
#'   biomass chart (`SAGChartKey == 4`).
#'
#' The function also records whether the reference point is a proxy and stores
#' the corresponding proxy reference-point name.
#'
#' @param sag_formatted A formatted SAG reference-point dataset containing
#'   standard reference points and custom reference-point fields. The table must
#'   include the columns:
#'   `AssessmentKey`, `FMSY`, `MSYBtrigger`,
#'   `CustomRefPointName1`–`CustomRefPointName4`, and
#'   `CustomRefPointValue1`–`CustomRefPointValue4`.
#'
#' @param sag_settings A data frame containing SAG settings retrieved from the
#'   SAG settings web service. This is passed internally to
#'   `extract_custom_refpoint_choices()` to determine which proxy reference
#'   points should be applied.
#'
#' @return A modified version of `sag_formatted` with:
#'   \describe{
#'     \item{FMSY}{Possibly replaced by a selected custom reference-point value.}
#'     \item{MSYBtrigger}{Possibly replaced by a selected custom reference-point
#'     value.}
#'     \item{FMSY_is_proxy}{Logical flag indicating whether `FMSY` was replaced
#'     by a proxy reference point.}
#'     \item{MSYB_is_proxy}{Logical flag indicating whether `MSYBtrigger` was
#'     replaced by a proxy reference point.}
#'     \item{FMSY_proxy_name}{Name of the custom reference point used as proxy,
#'     if applicable.}
#'     \item{MSYB_proxy_name}{Name of the custom reference point used as proxy,
#'     if applicable.}
#'   }
#'
#' @details
#' Proxy reference points are defined in SAG settings using `settingKey == 51`.
#' The numeric value (1–4) indicates which of the custom reference points stored
#' in the SAG reference-point dataset should be used.
#'
#' The function:
#' \enumerate{
#'   \item Extracts proxy selections from the SAG settings table.
#'   \item Joins these selections to the formatted SAG dataset using
#'   `AssessmentKey`.
#'   \item Replaces `FMSY` and/or `MSYBtrigger` with the corresponding custom
#'   reference-point values where proxies are defined.
#' }
#'
#' If no proxy is specified for an assessment, the original reference points
#' remain unchanged.
#'
#' @examples
#' \dontrun{
#' sag_settings <- icesSAG::getSAGSettingsForAStock(assessment_keys)
#'
#' sag_final <- add_proxyRefPoints(
#'   sag_formatted = sag_refpts,
#'   sag_settings = sag_settings
#' )
#' }
#'
#' @export
add_proxyRefPoints <- function(sag_formatted, sag_settings) {
  cust_choice <- extract_custom_refpoint_choices(sag_settings)
  
  sag_formatted %>%
    dplyr::left_join(cust_choice, by = "AssessmentKey") %>%
    dplyr::mutate(
      dplyr::across(
        c(FMSY, MSYBtrigger, dplyr::starts_with("CustomRefPointValue")),
        ~ suppressWarnings(as.numeric(.x))
      )
    ) %>%
    dplyr::mutate(
      FMSY_proxy_name = dplyr::case_when(
        choice_3 == 1 ~ CustomRefPointName1,
        choice_3 == 2 ~ CustomRefPointName2,
        choice_3 == 3 ~ CustomRefPointName3,
        choice_3 == 4 ~ CustomRefPointName4,
        TRUE ~ NA_character_
      ),
      MSYB_proxy_name = dplyr::case_when(
        choice_4 == 1 ~ CustomRefPointName1,
        choice_4 == 2 ~ CustomRefPointName2,
        choice_4 == 3 ~ CustomRefPointName3,
        choice_4 == 4 ~ CustomRefPointName4,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::mutate(
      FMSY_is_valid_proxy = !is.na(FMSY_proxy_name) &
        !grepl("custom|loss|mgt|mp|pa|lim|lowerbound|F/F",
               FMSY_proxy_name, ignore.case = TRUE),
      MSYB_is_valid_proxy = !is.na(MSYB_proxy_name) &
        !grepl("custom|loss|mgt|mp|pa|lim|lowerbound|F/F",
               MSYB_proxy_name, ignore.case = TRUE)
    ) %>%
    dplyr::mutate(
      FMSY_is_proxy = !is.na(choice_3) & FMSY_is_valid_proxy,
      MSYB_is_proxy = !is.na(choice_4) & MSYB_is_valid_proxy,
      FMSY = dplyr::coalesce(
        dplyr::case_when(
          FMSY_is_proxy & choice_3 == 1 ~ CustomRefPointValue1,
          FMSY_is_proxy & choice_3 == 2 ~ CustomRefPointValue2,
          FMSY_is_proxy & choice_3 == 3 ~ CustomRefPointValue3,
          FMSY_is_proxy & choice_3 == 4 ~ CustomRefPointValue4,
          TRUE ~ NA_real_
        ),
        FMSY
      ),
      MSYBtrigger = dplyr::coalesce(
        dplyr::case_when(
          MSYB_is_proxy & choice_4 == 1 ~ CustomRefPointValue1,
          MSYB_is_proxy & choice_4 == 2 ~ CustomRefPointValue2,
          MSYB_is_proxy & choice_4 == 3 ~ CustomRefPointValue3,
          MSYB_is_proxy & choice_4 == 4 ~ CustomRefPointValue4,
          TRUE ~ NA_real_
        ),
        MSYBtrigger
      ),
      FMSY_proxy_name = dplyr::if_else(FMSY_is_proxy, FMSY_proxy_name, NA_character_),
      MSYB_proxy_name = dplyr::if_else(MSYB_is_proxy, MSYB_proxy_name, NA_character_)
    ) %>%
    dplyr::select(
      -dplyr::starts_with("choice_"),
      -FMSY_is_valid_proxy,
      -MSYB_is_valid_proxy
    )
}

# Select precautionary reference points from SAG settings.
# F: lowest positive, finite reference value.
# B: highest positive, finite reference value.
# Includes the original FMSY / MSYBtrigger as candidates.
# All numeric custom indices selected for SAG chart 3 (F) or 4 (B)
# are compared; names are not filtered.
# Named setting tokens such as "Fpa" cannot be mapped to CustomRefPointValue1-4
# by index and are reported in a warning, not silently assigned.

add_proxyRefPoints_v2 <- function(sag_formatted, sag_settings) {

  custom_indices <- 1:4
  value_cols <- paste0("CustomRefPointValue", custom_indices)
  name_cols  <- paste0("CustomRefPointName", custom_indices)

  required <- c("AssessmentKey", "FMSY", "MSYBtrigger", value_cols, name_cols)
  missing <- setdiff(required, names(sag_formatted))
  if (length(missing) > 0) {
    stop("Missing columns in sag_formatted: ", paste(missing, collapse = ", "))
  }

  settings_required <- c("AssessmentKey", "settingKey", "SAGChartKey", "settingValue")
  settings_missing <- setdiff(settings_required, names(sag_settings))
  if (length(settings_missing) > 0) {
    stop("Missing columns in sag_settings: ",
         paste(settings_missing, collapse = ", "))
  }

  # Give each assessment row its own ID so repeated AssessmentKeys in the
  # original input are not accidentally merged or duplicated.
  df <- sag_formatted |>
    dplyr::mutate(
      .kobe_row_id = dplyr::row_number(),
      .assessment_key = as.character(AssessmentKey),
      dplyr::across(
        dplyr::all_of(c("FMSY", "MSYBtrigger", value_cols)),
        ~ suppressWarnings(as.numeric(as.character(.x)))
      ),
      FMSY_original = FMSY,
      MSYBtrigger_original = MSYBtrigger
    )

  # Keep EVERY setting choice, including comma-separated lists such as 1,2,3.
  all_choices <- sag_settings |>
    dplyr::filter(
      as.character(settingKey) == "51",
      as.character(SAGChartKey) %in% c("3", "4")
    ) |>
    dplyr::transmute(
      .assessment_key = as.character(AssessmentKey),
      SAGChartKey = as.integer(as.character(SAGChartKey)),
      token = as.character(settingValue)
    ) |>
    tidyr::separate_rows(token, sep = ",") |>
    dplyr::mutate(token = trimws(token)) |>
    dplyr::filter(!is.na(token), nzchar(token))

  # An index from 1 to 4 maps to CustomRefPointValue1-4.
  # A named token (e.g. "Fpa") requires a different mapping and is not guessed.
  ignored_tokens <- setdiff(unique(all_choices$token), as.character(custom_indices))
  if (length(ignored_tokens) > 0) {
    warning(
      "Skipping SAG settings that are not custom-point indices 1-4: ",
      paste(ignored_tokens, collapse = ", "),
      ". These would need an explicit value mapping.",
      call. = FALSE
    )
  }

  choices <- all_choices |>
    dplyr::filter(token %in% as.character(custom_indices)) |>
    dplyr::mutate(custom_index = as.integer(token)) |>
    dplyr::distinct(.assessment_key, SAGChartKey, custom_index)

  # Build one row per custom reference point AND chart, per assessment row.
  # Joining on chart key prevents a many-to-many join.
  custom_values <- dplyr::bind_rows(lapply(c(3L, 4L), function(chart) {
    dplyr::bind_rows(lapply(custom_indices, function(i) {
      df |>
        dplyr::transmute(
          .kobe_row_id,
          .assessment_key,
          SAGChartKey = chart,
          custom_index = i,
          candidate_value = .data[[value_cols[i]]],
          candidate_name = as.character(.data[[name_cols[i]]])
        )
    }))
  }))

  # Match only the custom indices selected for that assessment's F/B chart.
  selected <- custom_values |>
    dplyr::inner_join(
      choices,
      by = c(".assessment_key", "SAGChartKey", "custom_index")
    ) |>
    dplyr::filter(is.finite(candidate_value), candidate_value > 0) |>
    dplyr::mutate(candidate_name = dplyr::na_if(trimws(candidate_name), ""))

  # Lowest F and highest B among ALL selected numerical custom candidates.
  best_f <- selected |>
    dplyr::filter(SAGChartKey == 3) |>
    dplyr::arrange(.kobe_row_id, candidate_value, custom_index) |>
    dplyr::group_by(.kobe_row_id) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      .kobe_row_id,
      FMSY_custom_candidate = candidate_value,
      FMSY_candidate_name = candidate_name
    )

  best_b <- selected |>
    dplyr::filter(SAGChartKey == 4) |>
    dplyr::arrange(.kobe_row_id, dplyr::desc(candidate_value), custom_index) |>
    dplyr::group_by(.kobe_row_id) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      .kobe_row_id,
      MSYB_custom_candidate = candidate_value,
      MSYB_candidate_name = candidate_name
    )

  choose_ref <- function(original, custom, direction) {
    original[!is.finite(original) | original <= 0] <- NA_real_
    custom[!is.finite(custom) | custom <= 0] <- NA_real_
    if (direction == "min") {
      selected_value <- pmin(original, custom, na.rm = TRUE)
    } else {
      selected_value <- pmax(original, custom, na.rm = TRUE)
    }
    selected_value[!is.finite(selected_value)] <- NA_real_
    selected_value
  }

  df |>
    dplyr::left_join(best_f, by = ".kobe_row_id") |>
    dplyr::left_join(best_b, by = ".kobe_row_id") |>
    dplyr::mutate(
      FMSY = choose_ref(FMSY_original, FMSY_custom_candidate, "min"),
      MSYBtrigger = choose_ref(
        MSYBtrigger_original, MSYB_custom_candidate, "max"
      ),
      # On ties, the original reference is preferred.
      FMSY_is_proxy = is.finite(FMSY_custom_candidate) &
        (!is.finite(FMSY_original) | FMSY_original <= 0 |
           FMSY_custom_candidate < FMSY_original),
      MSYB_is_proxy = is.finite(MSYB_custom_candidate) &
        (!is.finite(MSYBtrigger_original) | MSYBtrigger_original <= 0 |
           MSYB_custom_candidate > MSYBtrigger_original),
      FMSY_proxy_name = dplyr::if_else(
        FMSY_is_proxy,
        dplyr::coalesce(FMSY_candidate_name, "Unnamed custom reference point"),
        NA_character_
      ),
      MSYB_proxy_name = dplyr::if_else(
        MSYB_is_proxy,
        dplyr::coalesce(MSYB_candidate_name, "Unnamed custom reference point"),
        NA_character_
      ),
      FMSY_refpoint_source = dplyr::case_when(
        FMSY_is_proxy ~ "Custom",
        is.finite(FMSY) ~ "Original",
        TRUE ~ "Missing"
      ),
      MSYB_refpoint_source = dplyr::case_when(
        MSYB_is_proxy ~ "Custom",
        is.finite(MSYBtrigger) ~ "Original",
        TRUE ~ "Missing"
      )
    ) |>
    dplyr::select(-.kobe_row_id, -.assessment_key)
}


stockstatus_CLD_current_proxy <- function(x) {
  
  # --- Ensure proxy columns exist
  for (nm in c("FMSY_is_proxy","FMSY_proxy_name","MSYB_is_proxy","MSYB_proxy_name")) {
    if (!nm %in% names(x)) x[[nm]] <- NA
  }

  # --- Coerce numerics safely
  num_cols <- c("Year","FishingPressure","StockSize","FMSY","MSYBtrigger",
                "AssessmentYear","Catches","Landings","Discards")
  for (nm in intersect(num_cols, names(x))) {
    x[[nm]] <- suppressWarnings(as.numeric(x[[nm]]))
  }

  # --- Latest assessment year per stock
  x <- x %>%
    dplyr::group_by(StockKeyLabel) %>%
    dplyr::mutate(AY_latest = suppressWarnings(max(AssessmentYear, na.rm = TRUE))) %>%
    dplyr::ungroup()

  # ---------- F side: use Year == AY_latest - 1 ----------
  df_F <- x %>%
    dplyr::filter(Year == AY_latest - 1) %>%
    dplyr::mutate(
      F_FMSY = ifelse(!is.na(FMSY), FishingPressure / FMSY, NA_real_)
    ) %>%
    # if more than one row per stock remains, keep the last (most recent) one
    dplyr::arrange(StockKeyLabel, dplyr::desc(Year)) %>%
    dplyr::group_by(StockKeyLabel) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(
  StockKeyLabel,
  F_Year = Year,
  FisheriesGuild,
  F_FMSY,
  Catches, Landings, Discards,
  FMSY, FishingPressure,
  F_proxy = FMSY_is_proxy,
  F_proxy_name = FMSY_proxy_name
    )

  # ---------- B side: choose latest of AY_latest or AY_latest - 1 with MSYBtrigger ----------
  df_B <- x %>%
    dplyr::filter(Year %in% c(AY_latest, AY_latest - 1)) %>%
    dplyr::arrange(StockKeyLabel, dplyr::desc(Year)) %>%
    dplyr::group_by(StockKeyLabel) %>%
    # keep the latest row that actually has MSYBtrigger
    dplyr::filter(!is.na(MSYBtrigger)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger), StockSize / MSYBtrigger, NA_real_)
    ) %>%
    dplyr::select(
  StockKeyLabel,
  B_Year = Year,
  FisheriesGuild,
  SSB_MSYBtrigger,
  StockSize, MSYBtrigger,
  B_proxy = MSYB_is_proxy,
  B_proxy_name = MSYB_proxy_name
)

  # ---------- Join F and B sides ----------
  df4 <- dplyr::full_join(df_F, df_B, by = c("StockKeyLabel","FisheriesGuild"))
  df4 <- dplyr::full_join(
  df_F,
  df_B,
  by = c("StockKeyLabel", "FisheriesGuild")
) %>%
  dplyr::mutate(
    Year = F_Year
  )
  # ---------- Status classification ----------
  df4 <- df4 %>%
    dplyr::mutate(
      Status = dplyr::case_when(
        is.na(F_FMSY) | is.na(SSB_MSYBtrigger) ~ "GREY",
        F_FMSY < 1 & SSB_MSYBtrigger >= 1       ~ "GREEN",
        TRUE                                    ~ "RED"
      )
    )

  df4
}



plot_GES_pies <- function(x, y, return_data = FALSE, width_px = 800) {
  # --- Responsive sizes
  
  base_size        <- max(14, min(20, round(width_px / 50)))
  caption_size     <- max(8, base_size - 2)
  value_label_size <- max(4, min(9, round(base_size / 3.0)))   # a bit larger than before
  total_label_size <- max(3, min(7, round(base_size / 2.9)))

  cap_lab <- ggplot2::labs(
    title = NULL, x = NULL, y = NULL,
    caption = paste0("ICES Stock Assessment Database, ",
                     format(Sys.Date(), "%d-%b-%y"),
                     ". ICES, Copenhagen")
  )

  colList <- c(
    "GREEN" = "#00B26D",
    "GREY" = "#d3d3d3",
    "ORANGE" = "#ff7f00",
    "RED" = "#d93b1c",
    "qual_RED" = "#d93b5c",
    "qual_GREEN" = "#00B28F"
  )

  df_stock <- dplyr::filter(x, lineDescription == "Maximum sustainable yield") |>
    dplyr::select(StockKeyLabel, FishingPressure, StockSize) |>
    tidyr::gather(Variable, Colour, FishingPressure:StockSize, factor_key = TRUE)

  df2 <- df_stock |>
    dplyr::group_by(Variable, Colour) |>
    dplyr::summarise(COUNT = dplyr::n(), .groups = "drop") |>
    tidyr::spread(Colour, COUNT)
  df2[is.na(df2)] <- 0

  df3 <- dplyr::filter(y, StockKeyLabel %in% df_stock$StockKeyLabel) |>
    # dplyr::mutate(CATCH = ifelse(is.na(Catches) & !is.na(Landings), Landings, Catches)) |>
    dplyr::mutate(CATCH = ifelse(is.na(Catches_ecoregion) & !is.na(Landings_in_ecoregion), Landings_in_ecoregion, Catches_in_ecoregion)) |>
    dplyr::select(StockKeyLabel, CATCH)

  df4 <- dplyr::left_join(df_stock, df3); df4[is.na(df4)] <- 0
  df4 <- df4 |>
    dplyr::group_by(Variable, Colour) |>
    dplyr::summarise(CATCH = sum(CATCH), .groups = "drop") |>
    tidyr::spread(Colour, CATCH)

  df4 <- tidyr::gather(df4, Color, Catch, GREEN:RED, factor_key = TRUE)
  df2 <- tidyr::gather(df2, Color, Stocks, GREEN:RED, factor_key = TRUE)

  df5 <- merge(df2, df4)
  df5[is.na(df5)] <- 0

  tot    <- sum(df5$Catch)  / 2
  stocks <- sum(df5$Stocks) / 2
  df5    <- tidyr::gather(df5, Metric, Value, Stocks:Catch)
  df5    <- dplyr::group_by(df5, Metric) |>
            dplyr::mutate(sum = sum(Value) / 2)

  # keep only catch for plotting
  df5 <- dplyr::filter(df5, Metric != "Stocks")

  # fraction used by polar
  df5$fraction <- df5$Value

  # nicer labels
  df5$Variable <- plyr::revalue(df5$Variable,
                                c("FishingPressure" = "Fishing Pressure",
                                  "StockSize"       = "Stock Size"))
  df5$Metric   <- plyr::revalue(df5$Metric,
                                c("Stocks" = "Number of stocks",
                                  "Catch"  = "Proportion of catch \n(thousand tonnes)"))

  # Display values (000 t for catch)
  df5$Value2 <- ifelse(df5$Metric == "Proportion of catch \n(thousand tonnes)",
                       df5$Value / 1000, df5$Value)
  df5$sum2   <- ifelse(df5$Metric == "Proportion of catch \n(thousand tonnes)",
                       df5$sum / 1000, df5$sum)

  # --- Percent per pie (within each facet)
  df5 <- df5 |>
    dplyr::group_by(Metric, Variable) |>
    dplyr::mutate(
      facet_sum = sum(Value, na.rm = TRUE),
      pct = ifelse(facet_sum > 0, 100 * Value / facet_sum, NA_real_)
    ) |>
    dplyr::ungroup()

  # tidy up values for display
  df5$Value2 <- as.integer(df5$Value2)
  df5$sum2   <- as.integer(df5$sum2)
  df5 <- dplyr::filter(df5, Value2 > 0)
  df5$pct_lab <- sprintf("%.1f%%", df5$pct)

  p1 <- ggplot2::ggplot(df5, ggplot2::aes(x = "", y = fraction, fill = Color)) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    # Single label: value on first line, percent on second line
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(Value2, "\n", pct_lab)),
      position = ggplot2::position_stack(vjust = 0.5),
      size = value_label_size,
      lineheight = 0.95
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0("total = ", sum2), x = 0, y = 0),
      size = total_label_size
    ) +
    ggplot2::scale_fill_manual(values = colList) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::facet_grid(Metric ~ Variable) +
    cap_lab +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.position = "none",
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = caption_size, hjust = 0),
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(8, 10, 26, 10, unit = "pt")
    )

  if (isTRUE(return_data)) {
    df5 <- subset(df5, select = -c(facet_sum, pct, pct_lab))
    df5
  } else {
    p1
  }
}


#' Interactive version of plot_GES_pies, broken down by fisheries guild
#'
#' Produces the same "proportion of catch" pie charts as plot_GES_pies
#' (Fishing Pressure / Stock Size, coloured by GES status) but using plotly,
#' with one row of pies per FisheriesGuild.
plot_GES_pies_plotly <- function(x, y, return_data = FALSE) {

  colList <- c(
    "GREEN" = "#00B26D",
    "GREY" = "#d3d3d3",
    "ORANGE" = "#ff7f00",
    "RED" = "#d93b1c",
    "qual_RED" = "#d93b5c",
    "qual_GREEN" = "#00B28F"
  )

  df_stock <- dplyr::filter(x, lineDescription == "Maximum sustainable yield") |>
    dplyr::select(StockKeyLabel, FisheriesGuild, FishingPressure, StockSize) |>
    tidyr::gather(Variable, Colour, FishingPressure:StockSize, factor_key = TRUE)

  df3 <- dplyr::filter(y, StockKeyLabel %in% df_stock$StockKeyLabel) |>
    # dplyr::mutate(CATCH = ifelse(is.na(Catches) & !is.na(Landings), Landings, Catches)) |>
    dplyr::mutate(CATCH = ifelse(is.na(Catches_in_ecoregion) & !is.na(Landings_in_ecoregion), Landings_in_ecoregion, Catches_in_ecoregion)) |>
    dplyr::select(StockKeyLabel, CATCH)

  df4 <- dplyr::left_join(df_stock, df3, by = "StockKeyLabel")
  df4[is.na(df4)] <- 0

  # Add a second copy labelled Total so the same layout also includes
  # pies aggregated across all fisheries guilds.
  df4_total <- df4 |>
    dplyr::mutate(FisheriesGuild = "Total")
  df4 <- dplyr::bind_rows(df4, df4_total)

  # per-stock catch (same Catches/Landings fallback rule as plot_GES_pies), used for hover breakdown
  bar_width <- 20
  breakdown <- df4 |>
    dplyr::filter(CATCH > 0) |>
    dplyr::group_by(FisheriesGuild, Variable, Colour) |>
    dplyr::mutate(
      grp_total = sum(CATCH, na.rm = TRUE),
      stock_pct = ifelse(grp_total > 0, 100 * CATCH / grp_total, 0)
    ) |>
    dplyr::arrange(dplyr::desc(CATCH), .by_group = TRUE) |>
    dplyr::summarise(
      breakdown_html = paste0(
        "<b>Catch by stock</b><br>",
        paste0(
          StockKeyLabel, ": ",
          strrep("\u2588", pmax(1, round(stock_pct / 100 * bar_width))),
          " ", sprintf("%.1f%%", stock_pct),
          " (", scales::comma(round(CATCH)), " t)",
          collapse = "<br>"
        )
      ),
      .groups = "drop"
    )
  breakdown$Variable <- plyr::revalue(breakdown$Variable,
                                      c("FishingPressure" = "Fishing Pressure",
                                        "StockSize"       = "Stock Size"))
  breakdown$FisheriesGuild <- tools::toTitleCase(tolower(breakdown$FisheriesGuild))

  df4 <- df4 |>
    dplyr::group_by(FisheriesGuild, Variable, Colour) |>
    dplyr::summarise(Catch = sum(CATCH), .groups = "drop")

  # nicer labels, mirroring plot_GES_pies
  df4$Variable <- plyr::revalue(df4$Variable,
                                c("FishingPressure" = "Fishing Pressure",
                                  "StockSize"       = "Stock Size"))
  df4$FisheriesGuild <- tools::toTitleCase(tolower(df4$FisheriesGuild))

  df4 <- df4 |>
    dplyr::group_by(FisheriesGuild, Variable) |>
    dplyr::mutate(
      facet_sum = sum(Catch, na.rm = TRUE),
      pct = ifelse(facet_sum > 0, 100 * Catch / facet_sum, NA_real_),
      sum2 = as.integer(facet_sum / 1000)
    ) |>
    dplyr::ungroup()

  df4 <- dplyr::left_join(df4, breakdown, by = c("FisheriesGuild", "Variable", "Colour"))
  df4$breakdown_html[is.na(df4$breakdown_html)] <- "No landings reported"

  df4$Value2 <- as.integer(df4$Catch / 1000)
  df4 <- dplyr::filter(df4, Catch > 0)

  if (isTRUE(return_data)) {
    return(df4)
  }

  guilds    <- sort(unique(df4$FisheriesGuild))
  variables <- c("Fishing Pressure", "Stock Size")
  n_rows <- length(variables)
  n_cols <- length(guilds)

  # --- Grid layout for domain-based pie subplots
  gap <- 0.06
  col_width  <- (1 - gap * (n_cols - 1)) / n_cols
  row_height <- (1 - gap * (n_rows - 1)) / n_rows

  p <- plotly::plot_ly()

  # dedicated invisible traces so each status appears exactly once in the legend
  legend_colors <- intersect(names(colList), unique(as.character(df4$Colour)))
  for (col in legend_colors) {
    p <- plotly::add_trace(
      p,
      type = "scatter", mode = "markers", inherit = FALSE,
      x = NA_real_, y = NA_real_,
      marker = list(color = colList[[col]], size = 10),
      name = col, legendgroup = col, showlegend = TRUE,
      hoverinfo = "skip"
    )
  }

  annotations <- list()

  for (i in seq_along(variables)) {
    y1 <- 1 - (i - 1) * (row_height + gap)
    y0 <- y1 - row_height

    for (j in seq_along(guilds)) {
      x0 <- (j - 1) * (col_width + gap)
      x1 <- x0 + col_width

      sub <- dplyr::filter(df4, FisheriesGuild == guilds[j], Variable == variables[i])

      if (nrow(sub) == 0) {
        annotations[[length(annotations) + 1]] <- list(
          x = mean(c(x0, x1)), y = mean(c(y0, y1)),
          xref = "paper", yref = "paper",
          text = "No data", showarrow = FALSE, font = list(size = 11)
        )
        next
      }

      show_legend <- FALSE

      p <- plotly::add_trace(
        p,
        data = sub,
        type = "pie",
        labels = ~Colour,
        values = ~Value2,
        domain = list(x = c(x0, x1), y = c(y0, y1)),
        marker = list(colors = colList[as.character(sub$Colour)],
                      line = list(color = "#FFFFFF", width = 1)),
        textinfo = "label+percent",
        hoverinfo = "text",
        text = ~paste0("<b>", Colour, "</b><br>Catch: ", Value2, "k t (",
                        sprintf("%.1f%%", pct), ")<br><br>", breakdown_html),
        showlegend = show_legend,
        sort = FALSE
      )

      annotations[[length(annotations) + 1]] <- list(
        x = mean(c(x0, x1)), y = y0 - 0.02,
        xref = "paper", yref = "paper",
        text = paste0("total = ", unique(sub$sum2), "k t"),
        showarrow = FALSE, font = list(size = 10), yanchor = "top"
      )
    }
  }

  # column headers (FisheriesGuild names)
  for (j in seq_along(guilds)) {
    x0 <- (j - 1) * (col_width + gap)
    x1 <- x0 + col_width
    annotations[[length(annotations) + 1]] <- list(
      x = mean(c(x0, x1)), y = 1.03,
      xref = "paper", yref = "paper",
      text = paste0("<b>", guilds[j], "</b>"),
      showarrow = FALSE, font = list(size = 13)
    )
  }

  # row headers (Variable names)
  for (i in seq_along(variables)) {
    y1 <- 1 - (i - 1) * (row_height + gap)
    y0 <- y1 - row_height
    annotations[[length(annotations) + 1]] <- list(
      x = 0, y = mean(c(y0, y1)),
      xref = "paper", yref = "paper",
      xanchor = "right", xshift = -15,
      text = paste0("<b>", variables[i], "</b>"),
      showarrow = FALSE, font = list(size = 13),
      textangle = -90
    )
  }

  annotations[[length(annotations) + 1]] <- list(
    x = 1, y = -0.05, xref = "paper", yref = "paper",
    xanchor = "right", yanchor = "top", showarrow = FALSE,
    text = paste0("ICES Stock Assessment Database, ",
                  format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"),
    font = list(size = 10)
  )

  plotly::layout(
    p,
    legend = list(title = list(text = "<b>Status:</b>")),
    margin = list(l = 110, t = 60, b = 60, r = 20),
    xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
    yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE),
    annotations = annotations
  ) |>
    plotly::config(
      toImageButtonOptions = list(
        filename = paste0("GES_pies_", format(Sys.Date(), "%d-%b-%y")),
        format = "png",
        scale = 3
      )
    )
}





getStatusWebService <- function(Ecoregion, sid) {
        EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        status <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://sag.ices.dk/SAG_API/LatestStocks/Status?ecoregion=%s", EcoregionCode)
                )
        )
        status_long <- status %>%
                tidyr::unnest(YearStatus)
      
        df_status <- merge(sid, status_long, by = "AssessmentKey", all.x = TRUE)
        df_status$FisheriesGuild <- tolower(df_status$FisheriesGuild)
        
        return(df_status)
}


format_sag_status_new <- function(df,sag) {

        df$AssessmentComponent <- sag$AssessmentComponent[ match(df$AssessmentKey, sag$AssessmentKey) ]
        df$StockKeyLabel <- ifelse(is.na(df$AssessmentComponent) |df$AssessmentComponent == "", df$StockKeyLabel, paste0(df$StockKeyLabel, "_", df$AssessmentComponent))
        df$StockKeyLabel <- gsub("\\s*Substock\\b", "", df$StockKeyLabel, ignore.case = TRUE)
        
        df <- dplyr::mutate(df,status = dplyr::case_when(status == 0 ~ "GREY",
                                                  status == 1 ~ "GREEN",
                                                  status == 2 ~ "GREEN", #qualitative green
                                                  status == 3 ~ "ORANGE",
                                                  status == 4 ~ "RED",
                                                  status == 5 ~ "RED", #qualitative red
                                                  status == 6 ~ "GREY",
                                                  status == 7 ~ "qual_UP",
                                                  status == 8 ~ "qual_STEADY",
                                                  status == 9 ~ "qual_DOWN",
                                                  TRUE ~ "OTHER"),
                            fishingPressure = dplyr::case_when(fishingPressure == "-" &
                                                                type == "Fishing pressure" ~ "FQual",
                                                        TRUE ~ fishingPressure),
                            stockSize = dplyr::case_when(stockSize == "-" &
                                                          type == "Stock Size" ~ "SSBQual",
                                                  TRUE ~ stockSize),
                            stockSize = gsub("MSY BT*|MSY Bt*|MSYBT|MSYBt", "MSYBt", stockSize),
                            variable = dplyr::case_when(type == "Fishing pressure" ~ fishingPressure,
                                                 type == "Stock Size" ~ stockSize,
                                                 TRUE ~ type),
                            variable = dplyr::case_when(lineDescription == "Management plan" &
                                                         type == "Fishing pressure" ~ "FMGT",
                                                 lineDescription == "Management plan" &
                                                         type == "Stock Size" ~ "SSBMGT",
                                                 TRUE ~ variable),
                            variable = dplyr::case_when(
                                    grepl("Fpa", variable) ~ "FPA",
                                    grepl("Bpa", variable) ~ "BPA",
                                    grepl("^Qual*", variable) ~ "SSBQual",
                                    grepl("-", variable) ~ "FQual",
                                    grepl("^BMGT", variable) ~ "SSBMGT",
                                    grepl("MSYBtrigger", variable) ~ "BMSY",
                                    grepl("FMSY", variable) ~ "FMSY",
                                    TRUE ~ variable
                            )) 
        
        df <- dplyr::filter(df,variable != "-")
        
        df <- dplyr::filter(df, lineDescription != "Management plan")
        df <- dplyr::filter(df, lineDescription != "Qualitative evaluation")
        df <- dplyr::mutate(df,key = paste(StockKeyLabel, lineDescription, type))
        # df <- dplyr::mutate(df,key = paste( lineDescription, type)) #stockComponent,
        df<- df[order(-df$year),]
        df <- df[!duplicated(df$key), ]
        df<- subset(df, select = -key)
        df<- subset(df, select = c(StockKeyLabel, AssessmentKey,lineDescription, type, status, FisheriesGuild))#, stockComponent,adviceValue
        df$FisheriesGuild[df$FisheriesGuild == "crustacean"] <- "shellfish" 
        df<- tidyr::spread(df,type, status)
        
        df2<- dplyr::filter(df,lineDescription != "Maximum Sustainable Yield")
        df2<- dplyr::filter(df2,lineDescription != "Maximum sustainable yield")
        
        df <- df %>% dplyr::rename(FishingPressure = `Fishing pressure`,
                            StockSize = `Stock Size`)
      
        df$lineDescription <- gsub("Maximum Sustainable Yield", "Maximum sustainable yield", df$lineDescription)
        df$lineDescription <- gsub("Precautionary Approach", "Precautionary approach", df$lineDescription)
        
        return(df)
}




plot_CLD_bar_app <- function(x, guild, return_data = FALSE) {
  # --- Filter by guild
  df <- if (identical(guild, "All")) x else dplyr::filter(x, FisheriesGuild %in% guild)

   # --- Ensure proxy flags exist  
  if (!"F_proxy" %in% names(df)) warning("Missing 'F_proxy' column in input data. This may indicate an upstream data issue.")  
  if (!"B_proxy" %in% names(df)) warning("Missing 'B_proxy' column in input data. This may indicate an upstream data issue.")  


  # --- Build 'total' per stock (max of Catches/Landings across time)
  df <- df %>%
    dplyr::group_by(StockKeyLabel) %>%
    dplyr::mutate(
      total = ifelse(
        all(is.na(Catches_in_ecoregion) & is.na(Landings_in_ecoregion)), NA_real_,
        max(Catches_in_ecoregion, Landings_in_ecoregion, na.rm = TRUE)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(total))

  # Order stocks by total (smallest at bottom after coord_flip)
  df <- dplyr::mutate(df, StockKeyLabel = forcats::fct_reorder(StockKeyLabel, total))

  # Flag if any reference point is proxy
  df <- df %>% dplyr::mutate(ProxyFlag = (F_proxy %in% TRUE) | (B_proxy %in% TRUE))

  # Status palette
  status_pal <- c(GREEN = "#4daf4a", RED = "#e41a1c", GREY = "#d3d3d3")

  # Caption
  cap_lab <- ggplot2::labs(
    caption = paste0("ICES Stock Assessment Database, ",
                     format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen")
  )

  proxy_stroke <- 2.5

  # --- Base plot (segments; color by Status, no legend)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = StockKeyLabel)) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = StockKeyLabel, y = 0, yend = Catches_in_ecoregion/1000, colour = Status),
      linewidth = 2, na.rm = TRUE, show.legend = FALSE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(y = Landings_in_ecoregion/1000, xend = StockKeyLabel, yend = 0, colour = Status),
      linewidth = 2, na.rm = TRUE, show.legend = FALSE
    )

  # --- Points (NORMAL refpoints: filled; no legend)
  p <- p +
    ggplot2::geom_point(
      data = dplyr::filter(df, !ProxyFlag),
      ggplot2::aes(y = Catches_in_ecoregion/1000, fill = Status),
      shape = 24, colour = "grey35", size = 7, alpha = 0.85,
      na.rm = TRUE, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(df, !ProxyFlag),
      ggplot2::aes(y = Landings_in_ecoregion/1000, fill = Status),
      shape = 21, colour = "grey35", size = 7, alpha = 0.85,
      na.rm = TRUE, show.legend = FALSE
    )

  # --- Points (PROXY refpoints: hollow with Status-colored outline; no legend)
  p <- p +
    ggplot2::geom_point(
      data = dplyr::filter(df, ProxyFlag),
      ggplot2::aes(y = Catches_in_ecoregion/1000, colour = Status),
      shape = 24, fill = NA, size = 7, alpha = 1, stroke = proxy_stroke,
      na.rm = TRUE, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(df, ProxyFlag),
      ggplot2::aes(y = Landings_in_ecoregion/1000, colour = Status),
      shape = 21, fill = NA, size = 7, alpha = 1, stroke = proxy_stroke,
      na.rm = TRUE, show.legend = FALSE
    )

  # --- Scales (suppress Status legends)
  p <- p +
    ggplot2::scale_fill_manual(values = status_pal, guide = "none") +
    ggplot2::scale_colour_manual(values = status_pal, guide = "none")

  # --- Axes, theme
  p <- p +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::labs(x = "Stock code", y = "Catch and Landings (thousand tonnes)") +
    ggplot2::theme(
      plot.caption       = ggplot2::element_text(size = 14),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.1, colour = "grey80")
    ) +
    cap_lab

  # --- Legend (bottom-right): build only entries present in data
  has_land_norm   <- any(!is.na(df$Landings_in_ecoregion) & !df$ProxyFlag, na.rm = TRUE)
  has_land_proxy  <- any(!is.na(df$Landings_in_ecoregion) &  df$ProxyFlag,  na.rm = TRUE)
  has_catch_norm  <- any(!is.na(df$Catches_in_ecoregion)  & !df$ProxyFlag,  na.rm = TRUE)
  has_catch_proxy <- any(!is.na(df$Catches_in_ecoregion)  &  df$ProxyFlag,   na.rm = TRUE)

  legend_keys <- c(
    "Landings"        = 21,
    "Landings \n(Proxy ref. point)"= 21,
    "Catches"         = 24,
    "Catches \n(Proxy ref. point)" = 24
  )
  present <- c(has_land_norm, has_land_proxy, has_catch_norm, has_catch_proxy)
  legend_keys <- legend_keys[present]
  legend_labels <- names(legend_keys)

  if (length(legend_keys) > 0) {
    # Dummy layer to host the legend (alpha=0 so it won't plot; legend uses override.aes)
    p <- p +
      ggplot2::geom_point(
        data = data.frame(Legend = factor(legend_labels, levels = legend_labels)),
        ggplot2::aes(x = 0, y = 0, shape = Legend),
        inherit.aes = FALSE, alpha = 0, show.legend = TRUE
      ) +
      ggplot2::scale_shape_manual(
        name   = NULL,
        breaks = legend_labels,
        values = legend_keys,
        labels = legend_labels
      ) +
      ggplot2::guides(
        shape = ggplot2::guide_legend(
          override.aes = list(
            size   = 6,
            # per-key aesthetics matching 'legend_labels' order:
            fill   = c("Landings"         = "grey60",
                       "Landings \n(Proxy ref. point)" = NA,
                       "Catches"          = "grey60",
                       "Catches \n(Proxy ref. point)"  = NA)[legend_labels],
            colour = c("Landings"         = "grey25",
                       "Landings \n(Proxy ref. point)" = "grey25",
                       "Catches"          = "grey25",
                       "Catches \n(Proxy ref. point)"  = "grey25")[legend_labels],
            stroke = c("Landings"         = 1.0,
                       "Landings \n(Proxy ref. point)" = 2,
                       "Catches"          = 1.0,
                       "Catches \n(Proxy ref. point)"  = 2)[legend_labels],
            alpha  = 1
          ),
          keyheight = ggplot2::unit(30, "pt"),
          keywidth  = ggplot2::unit(30, "pt"),
          byrow = TRUE
        )
      ) +
      ggplot2::theme(
        legend.position      = c(0.98, 0.02),  # bottom-right inside
        legend.justification = c(1, 0),
        legend.background    = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.9),
                                                     colour = "grey85"),
        legend.spacing.y  = ggplot2::unit(10, "pt"),
        legend.key.height    = ggplot2::unit(30, "pt"),
        legend.key.width     = ggplot2::unit(30, "pt")
      )
  } else {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  if (isTRUE(return_data)) df else p
}


#' Interactive (plotly) version of plot_CLD_bar_app
#'
#' Same inputs/logic as plot_CLD_bar_app (guild filter, ecoregion-scoped
#' Catches/Landings, proxy reference-point flagging, ordering by total catch),
#' rendered as an interactive horizontal lollipop chart instead of ggplot2.
plot_CLD_bar_app_plotly <- function(x, guild, return_data = FALSE) {
  # --- Filter by guild
  df <- if (identical(guild, "All")) x else dplyr::filter(x, FisheriesGuild %in% guild)

  # --- Ensure proxy flags exist
  if (!"F_proxy" %in% names(df)) warning("Missing 'F_proxy' column in input data. This may indicate an upstream data issue.")
  if (!"B_proxy" %in% names(df)) warning("Missing 'B_proxy' column in input data. This may indicate an upstream data issue.")

  # --- Build 'total' per stock (max of Catches/Landings across time)
  df <- df %>%
    dplyr::group_by(StockKeyLabel) %>%
    dplyr::mutate(
      total = ifelse(
        all(is.na(Catches_in_ecoregion) & is.na(Landings_in_ecoregion)), NA_real_,
        max(Catches_in_ecoregion, Landings_in_ecoregion, na.rm = TRUE)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(total))

  # Order stocks by total (smallest at bottom, matching plot_CLD_bar_app)
  stock_order <- unique(as.character(dplyr::arrange(df, total)$StockKeyLabel))
  df$StockKeyLabel <- factor(as.character(df$StockKeyLabel), levels = stock_order)

  # Flag if any reference point is proxy
  df <- df %>% dplyr::mutate(ProxyFlag = (F_proxy %in% TRUE) | (B_proxy %in% TRUE))

  status_pal <- c(GREEN = "#4daf4a", RED = "#e41a1c", GREY = "#d3d3d3")
  # precompute an explicit hex colour per row (avoids plotly's discrete colour
  # scale silently falling back to a default colourway for unmatched levels)
  df$status_hex <- unname(status_pal[as.character(df$Status)])
  df$status_hex[is.na(df$status_hex)] <- status_pal[["GREY"]]

  if (isTRUE(return_data)) return(df)

  catch_df <- dplyr::filter(df, !is.na(Catches_in_ecoregion))
  land_df  <- dplyr::filter(df, !is.na(Landings_in_ecoregion))

  hover_catch <- ~paste0("<b>", StockKeyLabel, "</b><br>Catches: ",
                          scales::comma(round(Catches_in_ecoregion)), " t<br>Status: ", Status)
  hover_land  <- ~paste0("<b>", StockKeyLabel, "</b><br>Landings: ",
                          scales::comma(round(Landings_in_ecoregion)), " t<br>Status: ", Status)

  p <- plotly::plot_ly()

  # --- Segments (colour by Status, no legend)
  if (nrow(catch_df) > 0) {
    for (status_colour in unique(catch_df$status_hex)) {
      status_df <- dplyr::filter(catch_df, status_hex == status_colour)
      p <- plotly::add_segments(
        p, data = status_df,
        x = ~0, xend = ~Catches_in_ecoregion / 1000,
        y = ~StockKeyLabel, yend = ~StockKeyLabel,
        line = list(color = status_colour, width = 6), opacity = 0.6,
        showlegend = FALSE, hoverinfo = "skip"
      )
    }
  }
  if (nrow(land_df) > 0) {
    for (status_colour in unique(land_df$status_hex)) {
      status_df <- dplyr::filter(land_df, status_hex == status_colour)
      p <- plotly::add_segments(
        p, data = status_df,
        x = ~0, xend = ~Landings_in_ecoregion / 1000,
        y = ~StockKeyLabel, yend = ~StockKeyLabel,
        line = list(color = status_colour, width = 6), opacity = 0.6,
        showlegend = FALSE, hoverinfo = "skip"
      )
    }
  }

  # --- Points (NORMAL refpoints: filled; PROXY refpoints: hollow outline)
  catch_norm  <- dplyr::filter(catch_df, !ProxyFlag)
  catch_proxy <- dplyr::filter(catch_df,  ProxyFlag)
  land_norm   <- dplyr::filter(land_df,  !ProxyFlag)
  land_proxy  <- dplyr::filter(land_df,   ProxyFlag)

  if (nrow(catch_norm) > 0) {
    p <- plotly::add_markers(
      p, data = catch_norm, x = ~Catches_in_ecoregion / 1000, y = ~StockKeyLabel,
      marker = list(symbol = "triangle-up", size = 16, color = catch_norm$status_hex,
                    line = list(color = rep("grey35", nrow(catch_norm)), width = 1)),
      text = hover_catch, hoverinfo = "text", showlegend = FALSE
    )
  }
  if (nrow(land_norm) > 0) {
    p <- plotly::add_markers(
      p, data = land_norm, x = ~Landings_in_ecoregion / 1000, y = ~StockKeyLabel,
      marker = list(symbol = "circle", size = 16, color = land_norm$status_hex,
                    line = list(color = rep("grey35", nrow(land_norm)), width = 1)),
      text = hover_land, hoverinfo = "text", showlegend = FALSE
    )
  }
  if (nrow(catch_proxy) > 0) {
    p <- plotly::add_markers(
      p, data = catch_proxy, x = ~Catches_in_ecoregion / 1000, y = ~StockKeyLabel,
      marker = list(symbol = "triangle-up-open", size = 16, color = catch_proxy$status_hex, line = list(width = 2.5)),
      text = hover_catch, hoverinfo = "text", showlegend = FALSE
    )
  }
  if (nrow(land_proxy) > 0) {
    p <- plotly::add_markers(
      p, data = land_proxy, x = ~Landings_in_ecoregion / 1000, y = ~StockKeyLabel,
      marker = list(symbol = "circle-open", size = 16, color = land_proxy$status_hex, line = list(width = 2.5)),
      text = hover_land, hoverinfo = "text", showlegend = FALSE
    )
  }

  # --- Legend (shape meaning only, built from entries present in data)
  legend_defs <- list(
    "Landings"                      = list(symbol = "circle",       filled = TRUE),
    "Landings \n(Proxy ref. point)"  = list(symbol = "circle-open",  filled = FALSE),
    "Catches"                        = list(symbol = "triangle-up",      filled = TRUE),
    "Catches \n(Proxy ref. point)"   = list(symbol = "triangle-up-open", filled = FALSE)
  )
  present <- c(nrow(land_norm) > 0, nrow(land_proxy) > 0, nrow(catch_norm) > 0, nrow(catch_proxy) > 0)
  legend_defs <- legend_defs[present]

  for (nm in names(legend_defs)) {
    def <- legend_defs[[nm]]
    p <- plotly::add_markers(
      p, x = -1, y = -1, xaxis = "x2", yaxis = "y2",
      name = nm, legendgroup = nm,
      marker = list(
        symbol = def$symbol, size = 12, color = "black",
        line = list(color = "black", width = if (def$filled) 1 else 2)
      ),
      inherit = FALSE, showlegend = TRUE, hoverinfo = "skip"
    )
  }

  plotly::layout(
    p,
    xaxis = list(title = "Catch and Landings (thousand tonnes)", zeroline = TRUE),
    yaxis = list(title = "Stock code",
                 categoryorder = "array", categoryarray = stock_order),
    # hidden, fully independent axis pair just to host the dummy legend markers
    # (points plotted outside [0,1] so they're clipped and never rendered)
    xaxis2 = list(overlaying = "x", visible = FALSE, range = c(0, 1), fixedrange = TRUE, autorange = FALSE),
    yaxis2 = list(overlaying = "y", visible = FALSE, range = c(0, 1), fixedrange = TRUE, autorange = FALSE),
    legend = list(title = list(text = ""), x = 0.98, y = 0.02,
                  xanchor = "right", yanchor = "bottom",
                  bgcolor = "rgba(255,255,255,0.9)", bordercolor = "grey85", borderwidth = 1),
    hovermode = "closest",
    margin = list(l = 120, b = 60),
    annotations = list(list(
      x = 1, y = -0.08, xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "top", showarrow = FALSE,
      text = paste0("ICES Stock Assessment Database, ",
                    format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"),
      font = list(size = 10)
    ))
  ) |>
    plotly::config(
      toImageButtonOptions = list(
        filename = paste0("CLD_bar_", format(Sys.Date(), "%d-%b-%y")),
        format = "png",
        scale = 3
      )
    )
}


#' Dumbbell version of plot_CLD_bar_app_plotly
#'
#' Same inputs/logic (guild filter, ecoregion-scoped Catches/Landings, proxy
#' flagging, ordering by total catch) but draws Catches and Landings as two
#' points joined by a short segment (instead of two bars anchored at zero).
#' This avoids colour-blending where overlapping catches/landings segments
#' cover each other, and works well with a log-scale x-axis (log_scale = TRUE)
#' so stocks spanning several orders of magnitude remain readable together.
plot_CLD_bar_app_dumbbell_plotly <- function(x, guild, return_data = FALSE, log_scale = TRUE) {
  # --- Filter by guild
  df <- if (identical(guild, "All")) x else dplyr::filter(x, FisheriesGuild %in% guild)

  # --- Ensure proxy flags exist
  if (!"F_proxy" %in% names(df)) warning("Missing 'F_proxy' column in input data. This may indicate an upstream data issue.")
  if (!"B_proxy" %in% names(df)) warning("Missing 'B_proxy' column in input data. This may indicate an upstream data issue.")

  # --- Build 'total' per stock (max of Catches/Landings across time)
  df <- df %>%
    dplyr::group_by(StockKeyLabel) %>%
    dplyr::mutate(
      total = ifelse(
        all(is.na(Catches_in_ecoregion) & is.na(Landings_in_ecoregion)), NA_real_,
        max(Catches_in_ecoregion, Landings_in_ecoregion, na.rm = TRUE)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(total))

  # log scale can't show zero/negative values; drop them (and warn) rather
  # than silently let plotly clip them
  if (isTRUE(log_scale)) {
    n_before <- nrow(df)
    df <- dplyr::filter(df, is.na(Catches_in_ecoregion) | Catches_in_ecoregion > 0,
                             is.na(Landings_in_ecoregion) | Landings_in_ecoregion > 0)
    if (nrow(df) < n_before) {
      warning(sprintf("log_scale = TRUE: dropped %d row(s) with zero/negative Catches or Landings.",
                       n_before - nrow(df)))
    }
  }

  # Order stocks by total (smallest at bottom)
  stock_order <- unique(as.character(dplyr::arrange(df, total)$StockKeyLabel))
  df$StockKeyLabel <- factor(as.character(df$StockKeyLabel), levels = stock_order)

  # Flag if any reference point is proxy
  df <- df %>% dplyr::mutate(ProxyFlag = (F_proxy %in% TRUE) | (B_proxy %in% TRUE))

  status_pal <- c(GREEN = "#4daf4a", RED = "#e41a1c", GREY = "#d3d3d3")
  # precompute an explicit hex colour per row (avoids plotly's discrete colour
  # scale silently falling back to a default colourway for unmatched levels)
  df$status_hex <- unname(status_pal[as.character(df$Status)])
  df$status_hex[is.na(df$status_hex)] <- status_pal[["GREY"]]

  if (isTRUE(return_data)) return(df)

  # --- Connecting segments only where both Catches and Landings are available
  seg_df <- dplyr::filter(df, !is.na(Catches_in_ecoregion) & !is.na(Landings_in_ecoregion))

  catch_df <- dplyr::filter(df, !is.na(Catches_in_ecoregion))
  land_df  <- dplyr::filter(df, !is.na(Landings_in_ecoregion))

  hover_catch <- ~paste0("<b>", StockKeyLabel, "</b><br>Catches: ",
                          scales::comma(round(Catches_in_ecoregion)), " t<br>Status: ", Status)
  hover_land  <- ~paste0("<b>", StockKeyLabel, "</b><br>Landings: ",
                          scales::comma(round(Landings_in_ecoregion)), " t<br>Status: ", Status)

  p <- plotly::plot_ly()

  # --- Connecting segment (black - represents the catch/landing gap, i.e.
  # discards; colour is carried by the markers, not the line, to avoid blending)
  if (nrow(seg_df) > 0) {
    p <- plotly::add_segments(
      p, data = seg_df,
      x = ~Catches_in_ecoregion / 1000, xend = ~Landings_in_ecoregion / 1000,
      y = ~StockKeyLabel, yend = ~StockKeyLabel,
      line = list(color = "black", width = 1.5),
      showlegend = FALSE, hoverinfo = "skip"
    )
  }

  # --- Points (NORMAL refpoints: filled; PROXY refpoints: hollow outline)
  catch_norm  <- dplyr::filter(catch_df, !ProxyFlag)
  catch_proxy <- dplyr::filter(catch_df,  ProxyFlag)
  land_norm   <- dplyr::filter(land_df,  !ProxyFlag)
  land_proxy  <- dplyr::filter(land_df,   ProxyFlag)

  if (nrow(catch_norm) > 0) {
    p <- plotly::add_markers(
      p, data = catch_norm, x = ~Catches_in_ecoregion / 1000, y = ~StockKeyLabel,
      marker = list(symbol = "triangle-up", size = 14, color = catch_norm$status_hex,
                    line = list(color = rep("grey35", nrow(catch_norm)), width = 1)),
      text = hover_catch, hoverinfo = "text", showlegend = FALSE
    )
  }
  if (nrow(land_norm) > 0) {
    p <- plotly::add_markers(
      p, data = land_norm, x = ~Landings_in_ecoregion / 1000, y = ~StockKeyLabel,
      marker = list(symbol = "circle", size = 14, color = land_norm$status_hex,
                    line = list(color = rep("grey35", nrow(land_norm)), width = 1)),
      text = hover_land, hoverinfo = "text", showlegend = FALSE
    )
  }
  if (nrow(catch_proxy) > 0) {
    p <- plotly::add_markers(
      p, data = catch_proxy, x = ~Catches_in_ecoregion / 1000, y = ~StockKeyLabel,
      marker = list(symbol = "triangle-up-open", size = 14, color = catch_proxy$status_hex, line = list(width = 2.5)),
      text = hover_catch, hoverinfo = "text", showlegend = FALSE
    )
  }
  if (nrow(land_proxy) > 0) {
    p <- plotly::add_markers(
      p, data = land_proxy, x = ~Landings_in_ecoregion / 1000, y = ~StockKeyLabel,
      marker = list(symbol = "circle-open", size = 14, color = land_proxy$status_hex, line = list(width = 2.5)),
      text = hover_land, hoverinfo = "text", showlegend = FALSE
    )
  }

  # --- Legend (shape meaning only, built from entries present in data)
  legend_defs <- list(
    "Landings"                      = list(symbol = "circle",       filled = TRUE),
    "Landings \n(Proxy ref. point)"  = list(symbol = "circle-open",  filled = FALSE),
    "Catches"                        = list(symbol = "triangle-up",      filled = TRUE),
    "Catches \n(Proxy ref. point)"   = list(symbol = "triangle-up-open", filled = FALSE)
  )
  present <- c(nrow(land_norm) > 0, nrow(land_proxy) > 0, nrow(catch_norm) > 0, nrow(catch_proxy) > 0)
  legend_defs <- legend_defs[present]

  for (nm in names(legend_defs)) {
    def <- legend_defs[[nm]]
    p <- plotly::add_markers(
      p, x = -1, y = -1, xaxis = "x2", yaxis = "y2",
      name = nm, legendgroup = nm,
      marker = list(
        symbol = def$symbol, size = 12, color = "black",
        line = list(color = "black", width = if (def$filled) 1 else 2)
      ),
      inherit = FALSE, showlegend = TRUE, hoverinfo = "skip"
    )
  }

  x_title <- paste0("Catch and Landings (thousand tonnes)", if (isTRUE(log_scale)) " \u2014 log scale" else "")

  plotly::layout(
    p,
    xaxis = list(title = x_title, type = if (isTRUE(log_scale)) "log" else "linear", zeroline = FALSE),
    yaxis = list(title = "Stock code",
                 categoryorder = "array", categoryarray = stock_order),
    # hidden, fully independent axis pair just to host the dummy legend markers
    # (points plotted outside [0,1] so they're clipped and never rendered)
    xaxis2 = list(overlaying = "x", visible = FALSE, range = c(0, 1), fixedrange = TRUE, autorange = FALSE),
    yaxis2 = list(overlaying = "y", visible = FALSE, range = c(0, 1), fixedrange = TRUE, autorange = FALSE),
    legend = list(title = list(text = ""), x = 0.98, y = 0.02,
                  xanchor = "right", yanchor = "bottom",
                  bgcolor = "rgba(255,255,255,0.9)", bordercolor = "grey85", borderwidth = 1),
    hovermode = "closest",
    margin = list(l = 120, b = 60),
    annotations = list(list(
      x = 1, y = -0.08, xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "top", showarrow = FALSE,
      text = paste0("ICES Stock Assessment Database, ",
                    format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"),
      font = list(size = 10)
    ))
  ) |>
    plotly::config(
      toImageButtonOptions = list(
        filename = paste0("CLD_dumbbell_", format(Sys.Date(), "%d-%b-%y")),
        format = "png",
        scale = 3
      )
    )
}






plot_kobe_app <- function(x, guild, return_data = FALSE){

  cap_lab <- ggplot2::labs(
    caption = paste0("ICES Stock Assessment Database, ",
                     format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen")
  )

  # Filter by guild
  df <- if (identical(guild, "All")) x else dplyr::filter(x, FisheriesGuild %in% guild)

  # Be robust if proxy flags aren't present
  if (!"F_proxy" %in% names(df)) df$F_proxy <- FALSE
  if (!"B_proxy" %in% names(df)) df$B_proxy <- FALSE

  # Flag proxy if either reference point is proxy
  df <- df %>%
    dplyr::mutate(
      ProxyFlag = dplyr::if_else((F_proxy %in% TRUE) | (B_proxy %in% TRUE),
                                 "Proxy reference point", "Reference point")
    )

  # Axes limits
  xmax  <- suppressWarnings(max(df$F_FMSY, na.rm = TRUE))
  xmax2 <- if (is.finite(xmax) && xmax < 3) 3 else xmax + 0.5
  ymax  <- suppressWarnings(max(df$SSB_MSYBtrigger, na.rm = TRUE))
  ymax2 <- if (is.finite(ymax) && ymax < 3) 3 else ymax + 0.5

  # Symbol sizes
  pt_size   <- 10
  proxy_stroke <- 1.8  # <-- thicker outline for empty circle

  kobe <-
    ggplot2::ggplot(df, ggplot2::aes(x = F_FMSY, y = SSB_MSYBtrigger, data_id = StockKeyLabel)) +
    ggplot2::coord_cartesian(xlim = c(0, xmax2), ylim = c(0, ymax2)) +

    # ---- Normal refpoint (filled circle) ----
    ggplot2::geom_point(
      data = dplyr::filter(df, ProxyFlag == "Reference point"),
      ggplot2::aes(color = Status, shape = ProxyFlag),
      size = pt_size, alpha = 0.7, na.rm = TRUE
    ) +

    # ---- Proxy refpoint (empty circle with thicker outline) ----
    ggplot2::geom_point(
      data = dplyr::filter(df, ProxyFlag == "Proxy reference point"),
      ggplot2::aes(color = Status, shape = ProxyFlag),
      size = pt_size, alpha = 0.9, na.rm = TRUE,
      fill = NA, stroke = proxy_stroke
    ) +

    ggplot2::geom_hline(yintercept = 1, color = "grey60", linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 1, color = "grey60", linetype = "dashed") +

    ggrepel::geom_text_repel(
      ggplot2::aes(label = StockKeyLabel),
      segment.size = .25, force = 5, size = 5
    ) +

    # Color by status (no color legend)
    ggplot2::scale_color_manual(
      values = c(GREEN = "#4daf4a", RED = "#e41a1c", GREY = "#d3d3d3"),
      guide = "none"
    ) +

    # Shape legend (auto-drops “Proxy refpoint” if not present)
    ggplot2::scale_shape_manual(
      name   = "ICES reference point",
      values = c("Reference point" = 16,  # filled circle
                 "Proxy reference point"  = 21), # circle with border (uses stroke)
      drop = TRUE
    ) +

    ggplot2::labs(
      x = expression(F/F[MSY]),
      y = expression(SSB/MSY~B[trigger]),
      caption = ""
    ) +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major   = ggplot2::element_blank(),
      plot.caption       = ggplot2::element_text(size = 14),
      legend.position    = c(0.98, 0.98),  # top-right inside plot
      legend.justification = c(1, 1),
      legend.background  = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.85),
                                                 color = "grey85"),
      legend.key.height  = ggplot2::unit(30, "pt"),
      legend.key.width   = ggplot2::unit(30, "pt")
    ) +
    # Make legend symbols neutral (single color) and readable
    ggplot2::guides(
      shape = ggplot2::guide_legend(
        override.aes = list(size = 4, alpha = 1, colour = "grey20", fill = NA, stroke = proxy_stroke)
      )
    ) +
    cap_lab

  if (isTRUE(return_data)) df else kobe
}


plot_kobe_app_plotly <- function(x, guild, return_data = FALSE) {

  # Keep the data preparation aligned with plot_kobe_app
  df <- if (identical(guild, "All")) x else dplyr::filter(x, FisheriesGuild %in% guild)

  if (!"F_proxy" %in% names(df)) df$F_proxy <- FALSE
  if (!"B_proxy" %in% names(df)) df$B_proxy <- FALSE

  df <- df %>%
    dplyr::mutate(
      ProxyFlag = dplyr::if_else((F_proxy %in% TRUE) | (B_proxy %in% TRUE),
                                 "Proxy reference point", "Reference point")
    )

  xmax  <- suppressWarnings(max(df$F_FMSY, na.rm = TRUE))
  xmax2 <- if (is.finite(xmax) && xmax < 3) 3 else xmax + 0.5
  ymax  <- suppressWarnings(max(df$SSB_MSYBtrigger, na.rm = TRUE))
  ymax2 <- if (is.finite(ymax) && ymax < 3) 3 else ymax + 0.5

  if (isTRUE(return_data)) return(df)

  status_pal <- c(GREEN = "#4daf4a", RED = "#e41a1c", GREY = "#d3d3d3")
  df$status_hex <- unname(status_pal[as.character(df$Status)])
  df$status_hex[is.na(df$status_hex)] <- status_pal[["GREY"]]

  normal_df <- dplyr::filter(df, ProxyFlag == "Reference point")
  proxy_df  <- dplyr::filter(df, ProxyFlag == "Proxy reference point")

  p <- plotly::plot_ly()
  stock_annotations <- list()

  # Washed-out Kobe quadrants with hoverable descriptions. The polygon traces
  # sit behind the markers and provide hover information that layout shapes do not.
  add_kobe_quadrant <- function(plot, x_values, y_values, fill_colour, title, description) {
    plotly::add_trace(
      plot,
      type = "scatter",
      mode = "lines",
      x = x_values,
      y = y_values,
      fill = "toself",
      fillcolor = fill_colour,
      line = list(color = "rgba(0,0,0,0)", width = 0),
      hoveron = "fills",
      name = title,
      text = description,
      hoverinfo = "text",
      hovertemplate = paste0("<b>", title, "</b><br>%{text}<extra></extra>"),
      inherit = FALSE,
      showlegend = FALSE
    )
  }

  p <- add_kobe_quadrant(
    p, c(0, 1, 1, 0, 0), c(1, 1, ymax2, ymax2, 1),
    "rgba(0, 210, 0, 0.10)", "Green quadrant",
    "Healthy stock: biomass is above BMSY and fishing pressure is below FMSY."
  )
  p <- add_kobe_quadrant(
    p, c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0),
    "rgba(255, 235, 0, 0.10)", "Yellow quadrant",
    "Overfished, but fishing pressure is below FMSY and has been reduced to safer levels."
  )
  p <- add_kobe_quadrant(
    p, c(1, xmax2, xmax2, 1, 1), c(1, 1, ymax2, ymax2, 1),
    "rgba(255, 165, 0, 0.10)", "Orange quadrant",
    "Biomass is adequate, but fishing pressure is above FMSY and overfishing is occurring."
  )
  p <- add_kobe_quadrant(
    p, c(1, xmax2, xmax2, 1, 1), c(0, 0, 1, 1, 0),
    "rgba(255, 0, 0, 0.10)", "Red quadrant",
    "Critical zone: the stock is overfished and fishing pressure is above FMSY."
  )

  add_status_points <- function(plot, data, symbol, size, line_width, opacity) {
    if (nrow(data) == 0) return(plot)

    for (status_colour in unique(data$status_hex)) {
      status_df <- dplyr::filter(
        data,
        status_hex == status_colour,
        !is.na(F_FMSY),
        !is.na(SSB_MSYBtrigger)
      )
      if (nrow(status_df) == 0) next
      plot <- plotly::add_markers(
        plot, data = status_df,
        x = ~F_FMSY, y = ~SSB_MSYBtrigger,
        marker = list(
          symbol = symbol, size = size,
          color = rep(status_colour, nrow(status_df)),
          line = list(color = rep("grey20", nrow(status_df)),
                      width = line_width)
        ),
        customdata = ~StockKeyLabel,
        hovertemplate = paste0(
          "<b>%{customdata}</b><br>",
          "F/FMSY: %{x:.2f}<br>",
          "SSB/MSY Btrigger: %{y:.2f}<br>",
          "Status: ", status_df$Status,
          "<extra></extra>"
        ),
        showlegend = FALSE,
        opacity = opacity
      )
      for (row_index in seq_len(nrow(status_df))) {
        stock_annotations[[length(stock_annotations) + 1]] <<- list(
          x = status_df$F_FMSY[[row_index]],
          y = status_df$SSB_MSYBtrigger[[row_index]],
          xref = "x", yref = "y",
          text = status_df$StockKeyLabel[[row_index]],
          xanchor = "left", yanchor = "middle", xshift = 18,
          showarrow = FALSE,
          font = list(size = 10, color = "grey20")
        )
      }
    }
    plot
  }

  p <- add_status_points(p, normal_df, "circle", 12, 1, 0.7)
  p <- add_status_points(p, proxy_df, "circle-open", 12, 1.8, 0.9)

  # Neutral legend entries describe the two reference-point symbols.
  legend_defs <- list(
    "Reference point" = list(symbol = "circle", width = 1),
    "Proxy reference point" = list(symbol = "circle-open", width = 1.8)
  )
  for (nm in names(legend_defs)) {
    def <- legend_defs[[nm]]
    p <- plotly::add_markers(
      p, x = -1, y = -1, xaxis = "x2", yaxis = "y2", name = nm,
      marker = list(symbol = def$symbol, size = 12, color = "black",
            line = list(color = "black", width = def$width)),
      inherit = FALSE, showlegend = TRUE, hoverinfo = "skip"
    )
  }

  plotly::layout(
    p,
    xaxis = list(title = "F/FMSY", range = c(0, xmax2), zeroline = FALSE),
    yaxis = list(title = "SSB/MSY Btrigger", range = c(0, ymax2), zeroline = FALSE),
    xaxis2 = list(overlaying = "x", visible = FALSE, range = c(0, 1),
            fixedrange = TRUE, autorange = FALSE),
    yaxis2 = list(overlaying = "y", visible = FALSE, range = c(0, 1),
            fixedrange = TRUE, autorange = FALSE),
    shapes = list(
      list(type = "line", x0 = 1, x1 = 1, y0 = 0, y1 = ymax2,
           line = list(color = "grey60", dash = "dash")),
      list(type = "line", x0 = 0, x1 = xmax2, y0 = 1, y1 = 1,
           line = list(color = "grey60", dash = "dash"))
    ),
    legend = list(title = list(text = "ICES reference point"),
                  x = 0.98, y = 0.98, xanchor = "right", yanchor = "top",
                  bgcolor = "rgba(255,255,255,0.85)",
                  bordercolor = "grey85", borderwidth = 1),
    hovermode = "closest",
    margin = list(l = 90, r = 30, t = 30, b = 70),
    annotations = c(stock_annotations, list(list(
      x = 1, y = -0.14, xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "top", showarrow = FALSE,
      text = paste0("ICES Stock Assessment Database, ",
                    format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"),
      font = list(size = 10)
    )))
  ) |>
    plotly::config(
      toImageButtonOptions = list(
        filename = paste0("Kobe_", format(Sys.Date(), "%d-%b-%y")),
        format = "png",
        scale = 3
      )
    )
}




### version that does not have the quadrants colors, just new mapping for colors


plot_kobe_app_plotly_v2 <- function(
    x,
    guild,
    return_data = FALSE,
    size_by_catches = FALSE,
    min_point_size = 10,
    max_point_size = 26
) {

  # 1. Validate marker sizes and filter stocks
  if (length(min_point_size) != 1 || length(max_point_size) != 1 ||
      !is.numeric(min_point_size) || !is.numeric(max_point_size) ||
      !is.finite(min_point_size) || !is.finite(max_point_size) ||
      min_point_size <= 0 || max_point_size <= min_point_size) {
    stop("Require 0 < min_point_size < max_point_size.")
  }

  df <- if (identical(guild, "All")) {
    x
  } else {
    dplyr::filter(x, FisheriesGuild %in% guild)
  }

  # 2. Proxy flags
  if (!"F_proxy" %in% names(df)) df$F_proxy <- FALSE
  if (!"B_proxy" %in% names(df)) df$B_proxy <- FALSE

  df <- dplyr::mutate(
    df,
    ProxyFlag = dplyr::if_else(
      (F_proxy %in% TRUE) | (B_proxy %in% TRUE),
      "Proxy reference point", "Reference point"
    )
  )

  # 3. Colours depend on plotted coordinates, not the Status column
  df <- dplyr::mutate(
    df,
    KobeStatus = dplyr::case_when(
      !is.finite(F_FMSY) | !is.finite(SSB_MSYBtrigger) ~ "Unknown",
      F_FMSY <= 1 & SSB_MSYBtrigger >= 1 ~ "Green",
      F_FMSY > 1 & SSB_MSYBtrigger < 1 ~ "Red",
      TRUE ~ "Orange"
    ),
    KobeDescription = dplyr::case_when(
      KobeStatus == "Green" ~
        "Biomass above threshold; fishing pressure sustainable",
      KobeStatus == "Red" ~
        "Biomass below threshold; overfishing occurring",
      KobeStatus == "Orange" & F_FMSY > 1 ~
        "Biomass above threshold; overfishing occurring",
      KobeStatus == "Orange" & SSB_MSYBtrigger < 1 ~
        "Biomass below threshold; fishing pressure below threshold",
      TRUE ~ "Status unknown"
    )
  )

  status_pal <- c(
    Green = "#4daf4a",
    Orange = "#ff8c00",
    Red = "#e41a1c",
    Unknown = "#d3d3d3"
  )
  df$status_hex <- unname(status_pal[df$KobeStatus])

  # 4. Use catches in the ecoregion, falling back to landings
  df <- dplyr::mutate(
    df,
    CatchSizeValue = dplyr::coalesce(
      dplyr::if_else(
        is.finite(Catches_ecoregion) & Catches_ecoregion >= 0,
        Catches_ecoregion, NA_real_
      ),
      dplyr::if_else(
        is.finite(Landings_ecoregion) & Landings_ecoregion >= 0,
        Landings_ecoregion, NA_real_
      )
    ),
    CatchSizeSource = dplyr::case_when(
      is.finite(Catches_ecoregion) & Catches_ecoregion >= 0 ~
        "Catches in ecoregion",
      is.finite(Landings_ecoregion) & Landings_ecoregion >= 0 ~
        "Landings in ecoregion",
      TRUE ~ "No catch/landing data"
    )
  )

  # Only complete, nonnegative coordinates correspond to plotted stocks
  visible_rows <- with(
    df,
    is.finite(F_FMSY) & is.finite(SSB_MSYBtrigger) &
      F_FMSY >= 0 & SSB_MSYBtrigger >= 0
  )

  visible_catches <- df$CatchSizeValue[
    visible_rows & is.finite(df$CatchSizeValue)
  ]
  c_min <- if (length(visible_catches)) min(visible_catches) else NA_real_
  c_max <- if (length(visible_catches)) max(visible_catches) else NA_real_

  # 5. Square-root size scaling; same formula is used for size legend
  scale_catches <- function(values) {
    result <- rep(min_point_size, length(values))
    valid <- is.finite(values) & values >= 0

    if (!any(valid) || !is.finite(c_min)) return(result)

    if (c_min == c_max) {
      result[valid] <- (min_point_size + max_point_size) / 2
      return(result)
    }

    result[valid] <- min_point_size +
      (sqrt(values[valid]) - sqrt(c_min)) /
      (sqrt(c_max) - sqrt(c_min)) *
      (max_point_size - min_point_size)

    pmax(min_point_size, pmin(max_point_size, result))
  }

  df$point_size <- if (isTRUE(size_by_catches)) {
    scale_catches(df$CatchSizeValue)
  } else {
    rep(12, nrow(df))
  }

  if (isTRUE(return_data)) return(df)

  # 6. Dynamic LINEAR axes with independent 10% padding
  axis_padding <- 0.10
  plot_df <- df[visible_rows, , drop = FALSE]

  if (nrow(plot_df) > 0) {
    xmin <- min(c(plot_df$F_FMSY, 1))
    xmax <- max(c(plot_df$F_FMSY, 1))
    ymin <- min(c(plot_df$SSB_MSYBtrigger, 1))
    ymax <- max(c(plot_df$SSB_MSYBtrigger, 1))

    xpad <- max(0.1, (xmax - xmin) * axis_padding)
    ypad <- max(0.1, (ymax - ymin) * axis_padding)

    x_range <- c(max(0, xmin - xpad), xmax + xpad)
    y_range <- c(max(0, ymin - ypad), ymax + ypad)
  } else {
    x_range <- c(0, 2)
    y_range <- c(0, 2)
  }

  # 7. Initialize separate marker groups and annotations
  normal_df <- dplyr::filter(df, ProxyFlag == "Reference point")
  proxy_df <- dplyr::filter(df, ProxyFlag == "Proxy reference point")
  p <- plotly::plot_ly()
  stock_annotations <- list()

  # 8. Add stocks, with colours and per-stock hover labels
  add_status_points <- function(plot, data, symbol, line_width, opacity) {
    if (nrow(data) == 0) return(plot)

    for (status_name in unique(data$KobeStatus)) {
      status_df <- dplyr::filter(
        data,
        KobeStatus == status_name,
        is.finite(F_FMSY), is.finite(SSB_MSYBtrigger),
        F_FMSY >= 0, SSB_MSYBtrigger >= 0
      )
      if (nrow(status_df) == 0) next

      status_colour <- status_pal[[status_name]]

      catch_label <- vapply(status_df$CatchSizeValue, function(value) {
        if (!is.finite(value)) return("NA")
        format(round(value, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
      }, character(1))

      status_df$hover_text <- paste0(
        "<b>", status_df$StockKeyLabel, "</b><br>",
        "F/Fref: ", round(status_df$F_FMSY, 2), "<br>",
        "B/Bref: ", round(status_df$SSB_MSYBtrigger, 2), "<br>",
        "Kobe status: ", status_df$KobeStatus, "<br>",
        status_df$KobeDescription, "<br>",
        status_df$CatchSizeSource, ": ", catch_label
      )

      marker_line <- if (identical(symbol, "circle-open")) {
        list(color = status_colour, width = line_width)
      } else {
        # Keep the requested filled-circle outline width of 0.1
        list(color = "#333333", width = 0.1)
      }

      plot <- plotly::add_markers(
        plot,
        data = status_df,
        x = ~F_FMSY,
        y = ~SSB_MSYBtrigger,
        marker = list(
          symbol = symbol,
          size = status_df$point_size,
          color = status_colour,
          line = marker_line
        ),
        customdata = ~hover_text,
        hovertemplate = "%{customdata}<extra></extra>",
        showlegend = FALSE,
        opacity = opacity,
        inherit = FALSE
      )

      for (i in seq_len(nrow(status_df))) {
        stock_annotations[[length(stock_annotations) + 1]] <<- list(
          x = status_df$F_FMSY[[i]],
          y = status_df$SSB_MSYBtrigger[[i]],
          xref = "x", yref = "y",
          text = status_df$StockKeyLabel[[i]],
          xanchor = "left", yanchor = "middle",
          xshift = status_df$point_size[[i]] / 2 + 8,
          showarrow = FALSE,
          font = list(size = 10, color = "#333333")
        )
      }
    }
    plot
  }

  p <- add_status_points(p, normal_df, "circle", 0.1, 0.75)
  p <- add_status_points(p, proxy_df, "circle-open", 2, 1)

  # 9. Reference-point type legend
  legend_defs <- list(
    "Reference point" = list(symbol = "circle", width = 0.1),
    "Proxy reference point" = list(symbol = "circle-open", width = 1.8)
  )

  for (nm in names(legend_defs)) {
    def <- legend_defs[[nm]]
    p <- plotly::add_markers(
      p,
      x = -1, y = -1,
      xaxis = "x2", yaxis = "y2",
      name = nm,
      marker = list(
        symbol = def$symbol, size = 12, color = "#000000",
        line = list(color = "#000000", width = def$width)
      ),
      legendgroup = "reference_type",
      showlegend = TRUE,
      hoverinfo = "skip",
      inherit = FALSE
    )
  }

  # 10. Marker-size legend, only with catch-based sizing
  if (isTRUE(size_by_catches) && length(visible_catches) > 0) {
    legend_values <- unique(as.numeric(stats::quantile(
      visible_catches,
      probs = c(0, 0.5, 1), na.rm = TRUE, names = FALSE
    )))
    legend_sizes <- scale_catches(legend_values)

    for (i in seq_along(legend_values)) {
      legend_label <- paste0(
        format(round(legend_values[i], 0),
               big.mark = ",", scientific = FALSE, trim = TRUE),
        " tonnes"
      )

      p <- plotly::add_trace(
        p,
        x = 0, y = 0,
        type = "scatter", mode = "markers",
        name = legend_label,
        marker = list(
          symbol = "circle",
          size = legend_sizes[i],
          color = "rgba(140,140,140,0.45)",
          line = list(color = "#666666", width = 1)
        ),
        legendgroup = "catch_size",
        legendgrouptitle = list(
          text = "Catches / landings in ecoregion"
        ),
        visible = "legendonly",
        showlegend = TRUE,
        hoverinfo = "skip",
        inherit = FALSE
      )
    }
  }

  # 11. Footer annotation
  footer_annotation <- list(
    x = 1, y = -0.14,
    xref = "paper", yref = "paper",
    xanchor = "right", yanchor = "top",
    showarrow = FALSE,
    text = paste0(
      "ICES Stock Assessment Database, ",
      format(Sys.Date(), "%d-%b-%y"),
      ". ICES, Copenhagen"
    ),
    font = list(size = 10)
  )

  # 12. Plot layout
  p <- plotly::layout(
    p,
    paper_bgcolor = "white",
    plot_bgcolor = "white",
    xaxis = list(title = "F/Fref", range = x_range, zeroline = FALSE),
    yaxis = list(title = "B/Bref", range = y_range, zeroline = FALSE),
    xaxis2 = list(
      overlaying = "x", visible = FALSE, range = c(0, 1),
      fixedrange = TRUE, autorange = FALSE
    ),
    yaxis2 = list(
      overlaying = "y", visible = FALSE, range = c(0, 1),
      fixedrange = TRUE, autorange = FALSE
    ),
    shapes = list(
      list(
        type = "line", x0 = 1, x1 = 1,
        y0 = y_range[1], y1 = y_range[2],
        line = list(color = "#4D4D4D", dash = "dash")
      ),
      list(
        type = "line", x0 = x_range[1], x1 = x_range[2],
        y0 = 1, y1 = 1,
        line = list(color = "#4D4D4D", dash = "dash")
      )
    ),
    legend = list(
      title = list(text = "ICES reference point"),
      x = if (isTRUE(size_by_catches)) 1.02 else 0.98,
      y = 0.98,
      xanchor = if (isTRUE(size_by_catches)) "left" else "right",
      yanchor = "top",
      bgcolor = "rgba(255,255,255,0.90)",
      bordercolor = "#D9D9D9",
      borderwidth = 1,
      tracegroupgap = 18
    ),
    hovermode = "closest",
    margin = list(
      l = 90,
      r = if (isTRUE(size_by_catches)) 280 else 30,
      t = 30, b = 70
    ),
    annotations = c(stock_annotations, list(footer_annotation))
  )

  # 13. Export configuration
  plotly::config(
    p,
    toImageButtonOptions = list(
      filename = paste0("Kobe_", format(Sys.Date(), "%d-%b-%y")),
      format = "png",
      scale = 3
    )
  )
}


#' Interactive Kobe plot with optional panel for stocks of unknown status
#'
#' Extends plot_kobe_app_plotly_v2() by showing stocks lacking one or both
#' usable reference-point ratios in a separate narrow panel on the right.
#' The panel appears automatically only if such stocks exist in the selected
#' fisheries guild(s). Stocks are plotted with catch-scaled grey circles and
#' labelled by stock code; hover text explains why status is unknown.
#'
#' @param x Stock-level data with StockKeyLabel, FisheriesGuild, F_FMSY,
#'   SSB_MSYBtrigger, Catches_in_ecoregion, and Landings_in_ecoregion.
#' @param guild Fisheries guild(s), or "All".
#' @param return_data If TRUE, return prepared data with KobeStatus, KobePanel,
#'   UnknownReason, CatchSizeValue, CatchSizeSource, and point_size.
#' @param size_by_catches If TRUE, use the same square-root catch-size scaling
#'   for the Kobe plot and the unknown-status panel.
#' @param min_point_size,max_point_size Range of marker diameters (pixels).
#' @param n_size_legend Number of representative sizes shown in the catch-size legend.
#' @param max_unknown_stocks Maximum number of stocks shown in the Unknown
#'   panel, ranked by ecoregion catches (or fallback landings). Default 12.
#'   Use Inf to show all. All stocks remain in return_data and size scaling.
#' @param unknown_row_px Approximate minimum vertical space per displayed
#'   unknown stock (pixels); used to increase plot height when necessary.
#' @return Plotly widget, or prepared data if return_data is TRUE.
#' @noRd
plot_kobe_app_plotly_v3 <- function(
    x,
    guild,
    return_data = FALSE,
    size_by_catches = FALSE,
    min_point_size = 8,
    max_point_size = 25,
    n_size_legend = 4,
    unknown_row_px = 26,
    max_unknown_stocks = 12
) {

  # 1. Validate input and filter stocks.
  if (!is.numeric(min_point_size) || length(min_point_size) != 1L ||
      !is.finite(min_point_size) || min_point_size <= 0 ||
      !is.numeric(max_point_size) || length(max_point_size) != 1L ||
      !is.finite(max_point_size) || max_point_size <= min_point_size) {
    stop("Require 0 < min_point_size < max_point_size.")
  }
  if (!is.numeric(unknown_row_px) || length(unknown_row_px) != 1L ||
      !is.finite(unknown_row_px) || unknown_row_px <= 0) {
    stop("unknown_row_px must be a positive number.")
  }

  if (!is.numeric(max_unknown_stocks) || length(max_unknown_stocks) != 1L ||
      is.na(max_unknown_stocks) || max_unknown_stocks < 1 ||
      (!is.infinite(max_unknown_stocks) &&
       max_unknown_stocks != floor(max_unknown_stocks))) {
    stop("max_unknown_stocks must be a positive integer or Inf.")
  }

  required_cols <- c(
    "StockKeyLabel", "FisheriesGuild", "F_FMSY", "SSB_MSYBtrigger",
    "Catches_in_ecoregion", "Landings_in_ecoregion"
  )
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0L) {
    stop("Missing input columns: ", paste(missing_cols, collapse = ", "))
  }

  df <- if (identical(guild, "All")) {
    x
  } else {
    dplyr::filter(x, FisheriesGuild %in% guild)
  }
  if (nrow(df) == 0L) stop("No stocks for the selected fisheries guild(s).")

  to_number <- function(z) suppressWarnings(as.numeric(as.character(z)))
  for (nm in c("F_FMSY", "SSB_MSYBtrigger",
               "Catches_in_ecoregion", "Landings_in_ecoregion")) {
    df[[nm]] <- to_number(df[[nm]])
  }

  # 2. Standard/proxy point flags.
  if (!"F_proxy" %in% names(df)) df$F_proxy <- FALSE
  if (!"B_proxy" %in% names(df)) df$B_proxy <- FALSE
  df <- dplyr::mutate(
    df,
    ProxyFlag = dplyr::if_else(
      (F_proxy %in% TRUE) | (B_proxy %in% TRUE),
      "Proxy reference point", "Reference point"
    )
  )

  # 3. Classify using coordinates, not SAG's old Status column.
  # Negative ratios are invalid too: they belong in the unknown panel.
  valid_f <- is.finite(df$F_FMSY) & df$F_FMSY >= 0
  valid_b <- is.finite(df$SSB_MSYBtrigger) & df$SSB_MSYBtrigger >= 0
  known <- valid_f & valid_b

  df$KobeStatus <- dplyr::case_when(
    !known ~ "Unknown",
    df$F_FMSY <= 1 & df$SSB_MSYBtrigger >= 1 ~ "Green",
    df$F_FMSY > 1 & df$SSB_MSYBtrigger < 1 ~ "Red",
    TRUE ~ "Orange"
  )
  df$KobePanel <- ifelse(known, "Kobe", "Unknown")
  df$UnknownReason <- dplyr::case_when(
    !valid_f & !valid_b ~ "F/Fref and B/Bref unavailable or invalid",
    !valid_f ~ "F/Fref unavailable or invalid",
    !valid_b ~ "B/Bref unavailable or invalid",
    TRUE ~ ""
  )
  df$KobeDescription <- dplyr::case_when(
    df$KobeStatus == "Green" ~
      "Biomass above threshold; fishing pressure sustainable",
    df$KobeStatus == "Red" ~
      "Biomass below threshold; overfishing occurring",
    df$KobeStatus == "Orange" & df$F_FMSY > 1 ~
      "Biomass above threshold; overfishing occurring",
    df$KobeStatus == "Orange" & df$SSB_MSYBtrigger < 1 ~
      "Biomass below threshold; fishing pressure below threshold",
    TRUE ~ "Status unknown"
  )

  status_pal <- c(
    Green = "#4daf4a", Orange = "#ff8c00",
    Red = "#e41a1c", Unknown = "#d3d3d3"
  )
  df$status_hex <- unname(status_pal[df$KobeStatus])

  # 4. Catch/landings fallback (0 remains a valid reported quantity).
  catch_value <- df$Catches_in_ecoregion
  landing_value <- df$Landings_in_ecoregion
  catch_value[!is.finite(catch_value) | catch_value < 0] <- NA_real_
  landing_value[!is.finite(landing_value) | landing_value < 0] <- NA_real_
  df$CatchSizeValue <- dplyr::coalesce(catch_value, landing_value)
  df$CatchSizeSource <- dplyr::case_when(
    !is.na(catch_value) ~ "Catches in ecoregion",
    !is.na(landing_value) ~ "Landings in ecoregion",
    TRUE ~ "No catch/landing data"
  )

  # 5. ONE size scale for both panels (including unknown-status stocks).
  size_values <- df$CatchSizeValue[is.finite(df$CatchSizeValue)]
  c_min <- if (length(size_values)) min(size_values) else NA_real_
  c_max <- if (length(size_values)) max(size_values) else NA_real_

  scale_catches <- function(values) {
    out <- rep(min_point_size, length(values))
    good <- is.finite(values) & values >= 0
    if (!any(good) || !is.finite(c_min)) return(out)
    if (c_min == c_max) {
      out[good] <- (min_point_size + max_point_size) / 2
    } else {
      out[good] <- min_point_size +
        (sqrt(values[good]) - sqrt(c_min)) /
        (sqrt(c_max) - sqrt(c_min)) *
        (max_point_size - min_point_size)
    }
    pmax(min_point_size, pmin(max_point_size, out))
  }

  df$point_size <- if (isTRUE(size_by_catches)) {
    scale_catches(df$CatchSizeValue)
  } else {
    rep(12, nrow(df))
  }
  if (isTRUE(return_data)) return(df)

  # 6. Derive linear main-axis limits from stocks with both ratios.
  plot_df <- df[known, , drop = FALSE]
  unknown_df <- df[!known, , drop = FALSE]
  n_unknown_total <- nrow(unknown_df)
  has_unknown <- n_unknown_total > 0L
  # All unknown stocks remain in df; only the right-panel display is capped.
  n_unknown_shown <- as.integer(min(n_unknown_total, max_unknown_stocks))

  if (nrow(plot_df) > 0L) {
    xmin <- min(c(plot_df$F_FMSY, 1))
    xmax <- max(c(plot_df$F_FMSY, 1))
    ymin <- min(c(plot_df$SSB_MSYBtrigger, 1))
    ymax <- max(c(plot_df$SSB_MSYBtrigger, 1))
    xpad <- max(0.1, (xmax - xmin) * 0.10)
    ypad <- max(0.1, (ymax - ymin) * 0.10)
    x_range <- c(max(0, xmin - xpad), xmax + xpad)
    y_range <- c(max(0, ymin - ypad), ymax + ypad)
  } else {
    x_range <- c(0, 2)
    y_range <- c(0, 2)
  }

  # 7. Hover text, with original values retained.
  html_safe <- function(z) {
    z <- ifelse(is.na(z), "NA", as.character(z))
    z <- gsub("&", "&amp;", z, fixed = TRUE)
    z <- gsub("<", "&lt;", z, fixed = TRUE)
    gsub(">", "&gt;", z, fixed = TRUE)
  }
  format_quantity <- function(v) {
    ifelse(
      is.finite(v),
      format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE),
      "NA"
    )
  }
  format_ratio <- function(v) {
    ifelse(is.finite(v) & v >= 0, sprintf("%.2f", v), "NA")
  }

  df$hover_text <- paste0(
    "<b>", html_safe(df$StockKeyLabel), "</b><br>",
    "F/Fref: ", format_ratio(df$F_FMSY), "<br>",
    "B/Bref: ", format_ratio(df$SSB_MSYBtrigger), "<br>",
    "Kobe status: ", df$KobeStatus, "<br>",
    ifelse(df$KobeStatus == "Unknown", df$UnknownReason, df$KobeDescription),
    "<br>", df$CatchSizeSource, ": ", format_quantity(df$CatchSizeValue),
    " t"
  )
  plot_df <- df[known, , drop = FALSE]
  unknown_df <- df[!known, , drop = FALSE]

  p <- plotly::plot_ly()
  stock_annotations <- list()

  # 8. Main Kobe markers (same symbols, colours, and labels as v2).
  add_kobe_points <- function(plot, data, symbol, line_width, opacity) {
    if (nrow(data) == 0L) return(plot)

    for (status_name in unique(data$KobeStatus)) {
      sub <- data[data$KobeStatus == status_name, , drop = FALSE]
      col <- status_pal[[status_name]]
      marker_line <- if (identical(symbol, "circle-open")) {
        list(color = col, width = line_width)
      } else {
        list(color = "#333333", width = 0.1)
      }

      plot <- plotly::add_markers(
        plot,
        data = sub,
        x = ~F_FMSY, y = ~SSB_MSYBtrigger,
        marker = list(
          symbol = symbol, size = sub$point_size,
          color = col, line = marker_line
        ),
        customdata = ~hover_text,
        hovertemplate = "%{customdata}<extra></extra>",
        showlegend = FALSE, opacity = opacity, inherit = FALSE
      )

      for (k in seq_len(nrow(sub))) {
        # In the split view, place labels for right-edge stocks to their
        # left, reducing spillover into the unknown-status panel.
        at_right_edge <- has_unknown &&
          sub$F_FMSY[[k]] > (x_range[[2]] - 0.20 * diff(x_range))
        label_shift <- sub$point_size[[k]] / 2 + 8

        stock_annotations[[length(stock_annotations) + 1L]] <<- list(
          x = sub$F_FMSY[[k]], y = sub$SSB_MSYBtrigger[[k]],
          xref = "x", yref = "y",
          text = html_safe(sub$StockKeyLabel[[k]]),
          xanchor = if (at_right_edge) "right" else "left",
          yanchor = "middle",
          xshift = if (at_right_edge) -label_shift else label_shift,
          showarrow = FALSE,
          font = list(size = 10, color = "#333333")
        )
      }
    }
    plot
  }

  p <- add_kobe_points(
    p, plot_df[plot_df$ProxyFlag == "Reference point", , drop = FALSE],
    "circle", 0.1, 0.75
  )
  p <- add_kobe_points(
    p, plot_df[plot_df$ProxyFlag == "Proxy reference point", , drop = FALSE],
    "circle-open", 2, 1
  )

  # 9. Unknown panel: show up to max_unknown_stocks, ordered by catches.
  # Keep all unknown stocks for return_data and shared size scaling.
  # Separate x3/y3 axes are independent of the F/B axes.
  if (has_unknown) {
    unknown_df <- dplyr::arrange(
      unknown_df, dplyr::desc(CatchSizeValue), StockKeyLabel
    )
    unknown_df <- unknown_df[seq_len(n_unknown_shown), , drop = FALSE]
    unknown_df$PanelX <- rep(0.13, nrow(unknown_df))
    unknown_df$PanelY <- rev(seq_len(nrow(unknown_df)))
    unknown_df$PanelLabel <- substr(as.character(unknown_df$StockKeyLabel), 1, 24)

    for (proxy_type in c("Reference point", "Proxy reference point")) {
      sub <- unknown_df[
        unknown_df$ProxyFlag == proxy_type, , drop = FALSE
      ]
      if (nrow(sub) == 0L) next
      is_proxy <- identical(proxy_type, "Proxy reference point")

      p <- plotly::add_trace(
        p,
        data = sub,
        type = "scatter", mode = "markers+text",
        x = ~PanelX, y = ~PanelY,
        xaxis = "x3", yaxis = "y3",
        text = ~PanelLabel, textposition = "middle right",
        textfont = list(size = 10, color = "#333333"),
        marker = list(
          symbol = if (is_proxy) "circle-open" else "circle",
          size = sub$point_size,
          color = if (is_proxy) "#858585" else status_pal[["Unknown"]],
          line = list(
            color = if (is_proxy) "#858585" else "#333333",
            width = if (is_proxy) 2 else 0.1
          )
        ),
        customdata = ~hover_text,
        hovertemplate = "%{customdata}<extra></extra>",
        showlegend = FALSE, inherit = FALSE,
        cliponaxis = FALSE
      )
    }
  }

  # 10. Legend for standard / proxy markers (unchanged).
  legend_defs <- list(
    "Reference point" = list(symbol = "circle", width = 0.1),
    "Proxy reference point" = list(symbol = "circle-open", width = 1.8)
  )
  for (nm in names(legend_defs)) {
    def <- legend_defs[[nm]]
    p <- plotly::add_markers(
      p, x = -1, y = -1,
      xaxis = "x2", yaxis = "y2", name = nm,
      marker = list(
        symbol = def$symbol, size = 12, color = "#000000",
        line = list(color = "#000000", width = def$width)
      ),
      legendgroup = "reference_type", showlegend = TRUE,
      hoverinfo = "skip", inherit = FALSE
    )
  }

  # 11. ONE size legend covering stocks in BOTH panels.
  if (isTRUE(size_by_catches) && length(size_values) > 0L) {
    # Number of representative sizes displayed in the legend
    if (!is.numeric(n_size_legend) ||
        length(n_size_legend) != 1L ||
        !is.finite(n_size_legend) ||
        n_size_legend < 1 ||
        n_size_legend != as.integer(n_size_legend)) {
      stop("n_size_legend must be a positive integer.")
    }

    legend_probs <- if (n_size_legend == 1) {
      0.5
    } else {
      seq(0, 1, length.out = n_size_legend)
    }

    legend_values <- unique(as.numeric(stats::quantile(
      size_values,
      probs = legend_probs,
      na.rm = TRUE,
      names = FALSE
    )))
    legend_sizes <- scale_catches(legend_values)

    for (i in seq_along(legend_values)) {
      legend_label <- paste0(format_quantity(legend_values[[i]]), " tonnes")
      p <- plotly::add_trace(
        p,
        type = "scatter", mode = "markers", x = 0, y = 0,
        name = legend_label,
        marker = list(
          symbol = "circle", size = legend_sizes[[i]],
          color = "rgba(140,140,140,0.45)",
          line = list(color = "#666666", width = 1)
        ),
        legendgroup = "catch_size",
        legendgrouptitle = list(text = "Catches / landings in ecoregion"),
        visible = "legendonly", showlegend = TRUE,
        hoverinfo = "skip", inherit = FALSE
      )
    }
  }

  # 12. Layout: reserve a side domain only if unknown stocks exist.
  # No unknown stocks -> the regular Kobe plot occupies the full width.
  main_domain <- if (has_unknown) c(0, 0.72) else c(0, 1)
  legend_outside <- isTRUE(size_by_catches) || has_unknown
  layout_shapes <- list(
    list(
      type = "line", x0 = 1, x1 = 1,
      y0 = y_range[[1]], y1 = y_range[[2]],
      line = list(color = "#4D4D4D", dash = "dash")
    ),
    list(
      type = "line", x0 = x_range[[1]], x1 = x_range[[2]],
      y0 = 1, y1 = 1,
      line = list(color = "#4D4D4D", dash = "dash")
    )
  )
  layout_annotations <- stock_annotations

  if (has_unknown) {
    layout_shapes <- c(
      list(
        list(
          type = "rect", xref = "paper", yref = "paper",
          x0 = 0.78, x1 = 1, y0 = 0, y1 = 1,
          fillcolor = "rgba(245,245,245,0.55)",
          line = list(width = 0), layer = "below"
        ),
        list(
          type = "line", xref = "paper", yref = "paper",
          x0 = 0.765, x1 = 0.765, y0 = 0, y1 = 1,
          line = list(color = "#CCCCCC", width = 1)
        )
      ),
      layout_shapes
    )
    layout_annotations[[length(layout_annotations) + 1L]] <- list(
      x = 0.89, y = 1.035, xref = "paper", yref = "paper",
      text = paste0("<b>Unknown status (", n_unknown_total, ")</b>"),
      xanchor = "center", yanchor = "bottom", showarrow = FALSE,
      font = list(size = 12, color = "#333333")
    )
    if (n_unknown_shown < n_unknown_total) {
      layout_annotations[[length(layout_annotations) + 1L]] <- list(
        x = 0.89, y = 1.005, xref = "paper", yref = "paper",
        text = paste0("Showing ", n_unknown_shown, " of ", n_unknown_total),
        xanchor = "center", yanchor = "bottom", showarrow = FALSE,
        font = list(size = 10, color = "#666666")
      )
    }
  }

  layout_annotations[[length(layout_annotations) + 1L]] <- list(
    x = 1, y = -0.14, xref = "paper", yref = "paper",
    xanchor = "right", yanchor = "top", showarrow = FALSE,
    text = paste0(
      "ICES Stock Assessment Database, ",
      format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"
    ),
    font = list(size = 10)
  )

  p <- plotly::layout(
    p,
    height = NULL,
    autosize = TRUE,
    paper_bgcolor = "white", plot_bgcolor = "white",
    xaxis = list(
      title = "F/Fref", domain = main_domain,
      range = x_range, zeroline = FALSE
    ),
    yaxis = list(
      title = "B/Bref", domain = c(0, 1),
      range = y_range, zeroline = FALSE
    ),
    xaxis2 = list(
      overlaying = "x", visible = FALSE, range = c(0, 1),
      fixedrange = TRUE, autorange = FALSE
    ),
    yaxis2 = list(
      overlaying = "y", visible = FALSE, range = c(0, 1),
      fixedrange = TRUE, autorange = FALSE
    ),
    xaxis3 = if (has_unknown) list(
      domain = c(0.79, 1), anchor = "y3", range = c(0, 1),
      visible = FALSE, fixedrange = TRUE
    ) else NULL,
    yaxis3 = if (has_unknown) list(
      domain = c(0, 1), anchor = "x3",
      range = c(0.5, n_unknown_shown + 0.5),
      visible = FALSE, fixedrange = TRUE
    ) else NULL,
    shapes = layout_shapes,
    legend = list(
      title = list(text = "ICES reference point"),
      x = if (legend_outside) 1.02 else 0.98,
      y = 0.98,
      xanchor = if (legend_outside) "left" else "right",
      yanchor = "top",
      bgcolor = "rgba(255,255,255,0.90)",
      bordercolor = "#D9D9D9", borderwidth = 1,
      tracegroupgap = 18
    ),
    hovermode = "closest",
    margin = list(
      l = 90, r = if (legend_outside) 280 else 30,
      t = if (has_unknown) 55 else 30, b = 70
    ),
    annotations = layout_annotations
  )

  plotly::config(
    p,
    responsive = TRUE,
    toImageButtonOptions = list(
      filename = paste0("Kobe_v3_", format(Sys.Date(), "%d-%b-%y")),
      format = "png", scale = 3
    )
  )
}
# #' Interactive Kobe plot with optional panel for stocks of unknown status
# #'
# #' Extends plot_kobe_app_plotly_v2() by showing stocks lacking one or both
# #' usable reference-point ratios in a separate narrow panel on the right.
# #' The panel appears automatically only if such stocks exist in the selected
# #' fisheries guild(s). Stocks are plotted with catch-scaled grey circles and
# #' labelled by stock code; hover text explains why status is unknown.
# #'
# #' @param x Stock-level data with StockKeyLabel, FisheriesGuild, F_FMSY,
# #'   SSB_MSYBtrigger, Catches_in_ecoregion, and Landings_in_ecoregion.
# #' @param guild Fisheries guild(s), or "All".
# #' @param return_data If TRUE, return prepared data with KobeStatus, KobePanel,
# #'   UnknownReason, CatchSizeValue, CatchSizeSource, and point_size.
# #' @param size_by_catches If TRUE, use the same square-root catch-size scaling
# #'   for the Kobe plot and the unknown-status panel.
# #' @param min_point_size,max_point_size Range of marker diameters (pixels).
# #' @param n_size_legend Desired number of representative catch-size circles in the
# #'   separate, actual-pixel-size sidebar key. Equal quantiles, including extremes.
# #' @param unknown_row_px Approximate minimum vertical space per unknown stock
# #'   (pixels); used to increase plot height when necessary.
# #' @return Plotly widget, or prepared data if return_data is TRUE.
# #' @noRd
# plot_kobe_app_plotly_v3 <- function(
#     x,
#     guild,
#     return_data = FALSE,
#     size_by_catches = FALSE,
#     min_point_size = 10,
#     max_point_size = 26,
#     unknown_row_px = 26,
#     n_size_legend = 5
# ) {

#   # 1. Validate input and filter stocks.
#   if (!is.numeric(min_point_size) || length(min_point_size) != 1L ||
#       !is.finite(min_point_size) || min_point_size <= 0 ||
#       !is.numeric(max_point_size) || length(max_point_size) != 1L ||
#       !is.finite(max_point_size) || max_point_size <= min_point_size) {
#     stop("Require 0 < min_point_size < max_point_size.")
#   }
#   if (!is.numeric(unknown_row_px) || length(unknown_row_px) != 1L ||
#       !is.finite(unknown_row_px) || unknown_row_px <= 0) {
#     stop("unknown_row_px must be a positive number.")
#   }

#   if (!is.numeric(n_size_legend) || length(n_size_legend) != 1L ||
#       !is.finite(n_size_legend) || n_size_legend < 1 ||
#       n_size_legend != as.integer(n_size_legend)) {
#     stop("n_size_legend must be a positive integer.")
#   }
#   n_size_legend <- as.integer(n_size_legend)

#   required_cols <- c(
#     "StockKeyLabel", "FisheriesGuild", "F_FMSY", "SSB_MSYBtrigger",
#     "Catches_in_ecoregion", "Landings_in_ecoregion"
#   )
#   missing_cols <- setdiff(required_cols, names(x))
#   if (length(missing_cols) > 0L) {
#     stop("Missing input columns: ", paste(missing_cols, collapse = ", "))
#   }

#   df <- if (identical(guild, "All")) {
#     x
#   } else {
#     dplyr::filter(x, FisheriesGuild %in% guild)
#   }
#   if (nrow(df) == 0L) stop("No stocks for the selected fisheries guild(s).")

#   to_number <- function(z) suppressWarnings(as.numeric(as.character(z)))
#   for (nm in c("F_FMSY", "SSB_MSYBtrigger",
#                "Catches_in_ecoregion", "Landings_in_ecoregion")) {
#     df[[nm]] <- to_number(df[[nm]])
#   }

#   # 2. Standard/proxy point flags.
#   if (!"F_proxy" %in% names(df)) df$F_proxy <- FALSE
#   if (!"B_proxy" %in% names(df)) df$B_proxy <- FALSE
#   df <- dplyr::mutate(
#     df,
#     ProxyFlag = dplyr::if_else(
#       (F_proxy %in% TRUE) | (B_proxy %in% TRUE),
#       "Proxy reference point", "Reference point"
#     )
#   )

#   # 3. Classify using coordinates, not SAG's old Status column.
#   # Negative ratios are invalid too: they belong in the unknown panel.
#   valid_f <- is.finite(df$F_FMSY) & df$F_FMSY >= 0
#   valid_b <- is.finite(df$SSB_MSYBtrigger) & df$SSB_MSYBtrigger >= 0
#   known <- valid_f & valid_b

#   df$KobeStatus <- dplyr::case_when(
#     !known ~ "Unknown",
#     df$F_FMSY <= 1 & df$SSB_MSYBtrigger >= 1 ~ "Green",
#     df$F_FMSY > 1 & df$SSB_MSYBtrigger < 1 ~ "Red",
#     TRUE ~ "Orange"
#   )
#   df$KobePanel <- ifelse(known, "Kobe", "Unknown")
#   df$UnknownReason <- dplyr::case_when(
#     !valid_f & !valid_b ~ "F/Fref and B/Bref unavailable or invalid",
#     !valid_f ~ "F/Fref unavailable or invalid",
#     !valid_b ~ "B/Bref unavailable or invalid",
#     TRUE ~ ""
#   )
#   df$KobeDescription <- dplyr::case_when(
#     df$KobeStatus == "Green" ~
#       "Biomass above threshold; fishing pressure sustainable",
#     df$KobeStatus == "Red" ~
#       "Biomass below threshold; overfishing occurring",
#     df$KobeStatus == "Orange" & df$F_FMSY > 1 ~
#       "Biomass above threshold; overfishing occurring",
#     df$KobeStatus == "Orange" & df$SSB_MSYBtrigger < 1 ~
#       "Biomass below threshold; fishing pressure below threshold",
#     TRUE ~ "Status unknown"
#   )

#   status_pal <- c(
#     Green = "#4daf4a", Orange = "#ff8c00",
#     Red = "#e41a1c", Unknown = "#d3d3d3"
#   )
#   df$status_hex <- unname(status_pal[df$KobeStatus])

#   # 4. Catch/landings fallback (0 remains a valid reported quantity).
#   catch_value <- df$Catches_in_ecoregion
#   landing_value <- df$Landings_in_ecoregion
#   catch_value[!is.finite(catch_value) | catch_value < 0] <- NA_real_
#   landing_value[!is.finite(landing_value) | landing_value < 0] <- NA_real_
#   df$CatchSizeValue <- dplyr::coalesce(catch_value, landing_value)
#   df$CatchSizeSource <- dplyr::case_when(
#     !is.na(catch_value) ~ "Catches in ecoregion",
#     !is.na(landing_value) ~ "Landings in ecoregion",
#     TRUE ~ "No catch/landing data"
#   )

#   # 5. ONE size scale for both panels (including unknown-status stocks).
#   size_values <- df$CatchSizeValue[is.finite(df$CatchSizeValue)]
#   c_min <- if (length(size_values)) min(size_values) else NA_real_
#   c_max <- if (length(size_values)) max(size_values) else NA_real_

#   scale_catches <- function(values) {
#     out <- rep(min_point_size, length(values))
#     good <- is.finite(values) & values >= 0
#     if (!any(good) || !is.finite(c_min)) return(out)
#     if (c_min == c_max) {
#       out[good] <- (min_point_size + max_point_size) / 2
#     } else {
#       out[good] <- min_point_size +
#         (sqrt(values[good]) - sqrt(c_min)) /
#         (sqrt(c_max) - sqrt(c_min)) *
#         (max_point_size - min_point_size)
#     }
#     pmax(min_point_size, pmin(max_point_size, out))
#   }

#   df$point_size <- if (isTRUE(size_by_catches)) {
#     scale_catches(df$CatchSizeValue)
#   } else {
#     rep(12, nrow(df))
#   }
#   if (isTRUE(return_data)) return(df)

#   # 6. Derive linear main-axis limits from stocks with both ratios.
#   plot_df <- df[known, , drop = FALSE]
#   unknown_df <- df[!known, , drop = FALSE]
#   has_unknown <- nrow(unknown_df) > 0L

#   if (nrow(plot_df) > 0L) {
#     xmin <- min(c(plot_df$F_FMSY, 1))
#     xmax <- max(c(plot_df$F_FMSY, 1))
#     ymin <- min(c(plot_df$SSB_MSYBtrigger, 1))
#     ymax <- max(c(plot_df$SSB_MSYBtrigger, 1))
#     xpad <- max(0.1, (xmax - xmin) * 0.10)
#     ypad <- max(0.1, (ymax - ymin) * 0.10)
#     x_range <- c(max(0, xmin - xpad), xmax + xpad)
#     y_range <- c(max(0, ymin - ypad), ymax + ypad)
#   } else {
#     x_range <- c(0, 2)
#     y_range <- c(0, 2)
#   }

#   # 7. Hover text, with original values retained.
#   html_safe <- function(z) {
#     z <- ifelse(is.na(z), "NA", as.character(z))
#     z <- gsub("&", "&amp;", z, fixed = TRUE)
#     z <- gsub("<", "&lt;", z, fixed = TRUE)
#     gsub(">", "&gt;", z, fixed = TRUE)
#   }
#   format_quantity <- function(v) {
#     ifelse(
#       is.finite(v),
#       format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE),
#       "NA"
#     )
#   }
#   format_ratio <- function(v) {
#     ifelse(is.finite(v) & v >= 0, sprintf("%.2f", v), "NA")
#   }

#   df$hover_text <- paste0(
#     "<b>", html_safe(df$StockKeyLabel), "</b><br>",
#     "F/Fref: ", format_ratio(df$F_FMSY), "<br>",
#     "B/Bref: ", format_ratio(df$SSB_MSYBtrigger), "<br>",
#     "Kobe status: ", df$KobeStatus, "<br>",
#     ifelse(df$KobeStatus == "Unknown", df$UnknownReason, df$KobeDescription),
#     "<br>", df$CatchSizeSource, ": ", format_quantity(df$CatchSizeValue),
#     " t"
#   )
#   plot_df <- df[known, , drop = FALSE]
#   unknown_df <- df[!known, , drop = FALSE]

#   p <- plotly::plot_ly()
#   stock_annotations <- list()

#   # 8. Main Kobe markers (same symbols, colours, and labels as v2).
#   add_kobe_points <- function(plot, data, symbol, line_width, opacity) {
#     if (nrow(data) == 0L) return(plot)

#     for (status_name in unique(data$KobeStatus)) {
#       sub <- data[data$KobeStatus == status_name, , drop = FALSE]
#       col <- status_pal[[status_name]]
#       marker_line <- if (identical(symbol, "circle-open")) {
#         list(color = col, width = line_width)
#       } else {
#         list(color = "#333333", width = 0.1)
#       }

#       plot <- plotly::add_markers(
#         plot,
#         data = sub,
#         x = ~F_FMSY, y = ~SSB_MSYBtrigger,
#         marker = list(
#           symbol = symbol, size = sub$point_size,
#           color = col, line = marker_line
#         ),
#         customdata = ~hover_text,
#         hovertemplate = "%{customdata}<extra></extra>",
#         showlegend = FALSE, opacity = opacity, inherit = FALSE
#       )

#       for (k in seq_len(nrow(sub))) {
#         # In the split view, place labels for right-edge stocks to their
#         # left, reducing spillover into the unknown-status panel.
#         at_right_edge <- has_unknown &&
#           sub$F_FMSY[[k]] > (x_range[[2]] - 0.20 * diff(x_range))
#         label_shift <- sub$point_size[[k]] / 2 + 8

#         stock_annotations[[length(stock_annotations) + 1L]] <<- list(
#           x = sub$F_FMSY[[k]], y = sub$SSB_MSYBtrigger[[k]],
#           xref = "x", yref = "y",
#           text = html_safe(sub$StockKeyLabel[[k]]),
#           xanchor = if (at_right_edge) "right" else "left",
#           yanchor = "middle",
#           xshift = if (at_right_edge) -label_shift else label_shift,
#           showarrow = FALSE,
#           font = list(size = 10, color = "#333333")
#         )
#       }
#     }
#     plot
#   }

#   p <- add_kobe_points(
#     p, plot_df[plot_df$ProxyFlag == "Reference point", , drop = FALSE],
#     "circle", 0.1, 0.75
#   )
#   p <- add_kobe_points(
#     p, plot_df[plot_df$ProxyFlag == "Proxy reference point", , drop = FALSE],
#     "circle-open", 2, 1
#   )

#   # 9. Unknown panel: compact stock list with size-consistent grey circles.
#   # Separate x3/y3 axes are independent of the F/B axes.
#   if (has_unknown) {
#     unknown_df <- dplyr::arrange(
#       unknown_df, dplyr::desc(CatchSizeValue), StockKeyLabel
#     )
#     unknown_df$PanelX <- rep(0.13, nrow(unknown_df))
#     unknown_df$PanelY <- rev(seq_len(nrow(unknown_df)))
#     unknown_df$PanelLabel <- substr(as.character(unknown_df$StockKeyLabel), 1, 24)

#     for (proxy_type in c("Reference point", "Proxy reference point")) {
#       sub <- unknown_df[
#         unknown_df$ProxyFlag == proxy_type, , drop = FALSE
#       ]
#       if (nrow(sub) == 0L) next
#       is_proxy <- identical(proxy_type, "Proxy reference point")

#       p <- plotly::add_trace(
#         p,
#         data = sub,
#         type = "scatter", mode = "markers+text",
#         x = ~PanelX, y = ~PanelY,
#         xaxis = "x3", yaxis = "y3",
#         text = ~PanelLabel, textposition = "middle right",
#         textfont = list(size = 10, color = "#333333"),
#         marker = list(
#           symbol = if (is_proxy) "circle-open" else "circle",
#           size = sub$point_size,
#           color = if (is_proxy) "#858585" else status_pal[["Unknown"]],
#           line = list(
#             color = if (is_proxy) "#858585" else "#333333",
#             width = if (is_proxy) 2 else 0.1
#           )
#         ),
#         customdata = ~hover_text,
#         hovertemplate = "%{customdata}<extra></extra>",
#         showlegend = FALSE, inherit = FALSE,
#         cliponaxis = FALSE
#       )
#     }
#   }

#   # 10. Legend for standard / proxy markers (unchanged).
#   legend_defs <- list(
#     "Reference point" = list(symbol = "circle", width = 0.1),
#     "Proxy reference point" = list(symbol = "circle-open", width = 1.8)
#   )
#   for (nm in names(legend_defs)) {
#     def <- legend_defs[[nm]]
#     p <- plotly::add_markers(
#       p, x = -1, y = -1,
#       xaxis = "x2", yaxis = "y2", name = nm,
#       marker = list(
#         symbol = def$symbol, size = 12, color = "#000000",
#         line = list(color = "#000000", width = def$width)
#       ),
#       legendgroup = "reference_type", showlegend = TRUE,
#       hoverinfo = "skip", inherit = FALSE
#     )
#   }

#   # 11. Catch-size key in the RIGHT SIDEBAR, not below the plot.
#   # A separate Cartesian scatter axis retains the true pixel marker
#   # diameters and avoids Plotly's built-in legend-marker size cap.
#   has_size_key <- isTRUE(size_by_catches) && length(size_values) > 0L
#   key_values <- numeric(0)
#   key_sizes <- numeric(0)

#   if (has_size_key) {
#     probs <- if (n_size_legend == 1L) {
#       0.5
#     } else {
#       seq(0, 1, length.out = n_size_legend)
#     }
#     key_values <- unique(as.numeric(stats::quantile(
#       size_values, probs = probs, na.rm = TRUE, names = FALSE
#     )))
#     key_sizes <- scale_catches(key_values)

#     # Use numeric positions on y4; list ascending values from top down.
#     key_y <- rev(seq_along(key_values))
#     key_labels <- paste0(format_quantity(key_values), " t")
#     p <- plotly::add_trace(
#       p,
#       type = "scatter", mode = "markers+text",
#       x = rep(0.18, length(key_values)), y = key_y,
#       xaxis = "x4", yaxis = "y4",
#       marker = list(
#         symbol = "circle",
#         size = as.numeric(key_sizes),
#         sizemode = "diameter", sizeref = 1,
#         color = "rgba(140,140,140,0.45)",
#         line = list(color = "#666666", width = 1)
#       ),
#       text = key_labels, textposition = "middle right",
#       textfont = list(size = 11, color = "#333333"),
#       hoverinfo = "skip", showlegend = FALSE,
#       cliponaxis = FALSE, inherit = FALSE
#     )
#   }

#   # 12. Layout. The side column contains unknown stocks and, if enabled,
#   # a distinct catch-size key below them. Nothing is added below the Kobe
#   # plot, so the size key cannot disappear below the viewport.
#   has_side_panel <- has_unknown || has_size_key
#   main_domain <- if (has_side_panel) c(0, 0.72) else c(0, 1)
#   side_domain <- c(0.79, 0.99)
#   legend_outside <- has_side_panel

#   # Allocate sufficient physical height to retain true marker diameters.
#   unknown_needed_px <- if (has_unknown) {
#     nrow(unknown_df) * max(unknown_row_px, max_point_size + 8)
#   } else 0
#   key_needed_px <- if (has_size_key) {
#     length(key_values) * (max_point_size + 16)
#   } else 0
#   plot_height <- if (has_side_panel) {
#     max(700, ceiling(unknown_needed_px + key_needed_px + 230))
#   } else NULL
#   drawable_height <- if (has_side_panel) plot_height - 55 - 70 else 650

#   # With two panels, allocate the size key its own portion of the sidebar.
#   # When no unknown stocks exist, the key occupies the upper sidebar.
#   if (has_size_key && has_unknown) {
#     key_fraction <- min(0.72, max(0.22, key_needed_px / drawable_height))
#     key_domain <- c(0, key_fraction)
#     unknown_domain <- c(key_fraction + 0.06, 1)
#     key_header_y <- key_fraction + 0.028
#   } else if (has_size_key) {
#     key_domain <- c(0.13, 0.89)
#     unknown_domain <- c(0, 1)
#     key_header_y <- 0.955
#   } else {
#     key_domain <- NULL
#     unknown_domain <- c(0, 1)
#     key_header_y <- NA_real_
#   }

#   layout_shapes <- list(
#     list(
#       type = "line", x0 = 1, x1 = 1,
#       y0 = y_range[[1]], y1 = y_range[[2]],
#       line = list(color = "#4D4D4D", dash = "dash")
#     ),
#     list(
#       type = "line", x0 = x_range[[1]], x1 = x_range[[2]],
#       y0 = 1, y1 = 1,
#       line = list(color = "#4D4D4D", dash = "dash")
#     )
#   )
#   layout_annotations <- stock_annotations

#   if (has_unknown) {
#     layout_shapes <- c(
#       list(
#         list(
#           type = "rect", xref = "paper", yref = "paper",
#           x0 = 0.78, x1 = 1, y0 = unknown_domain[[1]], y1 = 1,
#           fillcolor = "rgba(245,245,245,0.55)",
#           line = list(width = 0), layer = "below"
#         ),
#         list(
#           type = "line", xref = "paper", yref = "paper",
#           x0 = 0.765, x1 = 0.765, y0 = 0, y1 = 1,
#           line = list(color = "#CCCCCC", width = 1)
#         )
#       ),
#       layout_shapes
#     )
#     layout_annotations[[length(layout_annotations) + 1L]] <- list(
#       x = 0.89, y = 1.035, xref = "paper", yref = "paper",
#       text = paste0("<b>Unknown status (", nrow(unknown_df), ")</b>"),
#       xanchor = "center", yanchor = "bottom", showarrow = FALSE,
#       font = list(size = 12, color = "#333333")
#     )
#   }

#   if (has_size_key) {
#     layout_annotations[[length(layout_annotations) + 1L]] <- list(
#       x = 0.89, y = key_header_y,
#       xref = "paper", yref = "paper",
#       text = "<b>Circle size (tonnes)</b>",
#       xanchor = "center", yanchor = "middle", showarrow = FALSE,
#       font = list(size = 11, color = "#333333")
#     )
#     if (has_unknown) {
#       layout_shapes[[length(layout_shapes) + 1L]] <- list(
#         type = "line", xref = "paper", yref = "paper",
#         x0 = 0.79, x1 = 0.99,
#         y0 = key_fraction + 0.049, y1 = key_fraction + 0.049,
#         line = list(color = "#DDDDDD", width = 1)
#       )
#     }
#   }

#   layout_annotations[[length(layout_annotations) + 1L]] <- list(
#     x = 1, y = -0.12, xref = "paper", yref = "paper",
#     xanchor = "right", yanchor = "top", showarrow = FALSE,
#     text = paste0(
#       "ICES Stock Assessment Database, ",
#       format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"
#     ),
#     font = list(size = 10)
#   )

#   p <- plotly::layout(
#     p,
#     height = plot_height,
#     paper_bgcolor = "white", plot_bgcolor = "white",
#     xaxis = list(
#       title = "F/Fref", domain = main_domain,
#       range = x_range, zeroline = FALSE
#     ),
#     yaxis = list(
#       title = "B/Bref", domain = c(0, 1),
#       range = y_range, zeroline = FALSE
#     ),
#     xaxis2 = list(
#       overlaying = "x", visible = FALSE, range = c(0, 1),
#       fixedrange = TRUE, autorange = FALSE
#     ),
#     yaxis2 = list(
#       overlaying = "y", visible = FALSE, range = c(0, 1),
#       fixedrange = TRUE, autorange = FALSE
#     ),
#     xaxis3 = if (has_unknown) list(
#       domain = side_domain, anchor = "y3", range = c(0, 1),
#       visible = FALSE, fixedrange = TRUE
#     ) else NULL,
#     yaxis3 = if (has_unknown) list(
#       domain = unknown_domain, anchor = "x3",
#       range = c(0.5, nrow(unknown_df) + 0.5),
#       visible = FALSE, fixedrange = TRUE
#     ) else NULL,
#     xaxis4 = if (has_size_key) list(
#       domain = side_domain, anchor = "y4", range = c(0, 1),
#       visible = FALSE, fixedrange = TRUE
#     ) else NULL,
#     yaxis4 = if (has_size_key) list(
#       domain = key_domain, anchor = "x4",
#       range = c(0.5, length(key_values) + 0.5),
#       visible = FALSE, fixedrange = TRUE
#     ) else NULL,
#     shapes = layout_shapes,
#     legend = list(
#       title = list(text = "ICES reference point"),
#       x = if (legend_outside) 1.02 else 0.98,
#       y = 0.98,
#       xanchor = if (legend_outside) "left" else "right",
#       yanchor = "top",
#       bgcolor = "rgba(255,255,255,0.90)",
#       bordercolor = "#D9D9D9", borderwidth = 1,
#       tracegroupgap = 18
#     ),
#     hovermode = "closest",
#     margin = list(
#       l = 90, r = if (legend_outside) 280 else 30,
#       t = if (has_unknown) 55 else 40,
#       b = 70
#     ),
#     annotations = layout_annotations
#   )

#   plotly::config(
#     p,
#     toImageButtonOptions = list(
#       filename = paste0("Kobe_v3_", format(Sys.Date(), "%d-%b-%y")),
#       format = "png", scale = 3
#     )
#   )
# }


# Interactive summary of precautionary stock status by fisheries guild.
# Requires ratios calculated using the SELECTED (precautionary) references.
# This code does NOT use SAG's FishingPressure, StockSize or Status columns.
# It counts stocks; it does not weight slices by catch volume.

prepare_precautionary_status_pies <- function(df) {

  required <- c(
    "StockKeyLabel", "FisheriesGuild",
    "FMSY", "MSYBtrigger", "F_FMSY", "SSB_MSYBtrigger"
  )
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  stock_df <- df |>
    dplyr::transmute(
      StockKeyLabel = as.character(StockKeyLabel),
      FisheriesGuild = as.character(FisheriesGuild),
      FMSY = suppressWarnings(as.numeric(as.character(FMSY))),
      MSYBtrigger = suppressWarnings(as.numeric(as.character(MSYBtrigger))),
      F_FMSY = suppressWarnings(as.numeric(as.character(F_FMSY))),
      SSB_MSYBtrigger = suppressWarnings(
        as.numeric(as.character(SSB_MSYBtrigger))
      )
    ) |>
    dplyr::filter(!is.na(StockKeyLabel), nzchar(StockKeyLabel)) |>
    dplyr::mutate(
      FisheriesGuild = dplyr::if_else(
        is.na(FisheriesGuild) | !nzchar(FisheriesGuild),
        "Unknown", FisheriesGuild
      )
    ) |>
    dplyr::distinct()

  if (nrow(stock_df) == 0L) {
    stop("No stocks in the supplied data.")
  }

  # A stock should contribute once per indicator. Do not silently pick
  # between conflicting rows or count it twice.
  duplicated_stocks <- stock_df |>
    dplyr::count(StockKeyLabel, name = "n") |>
    dplyr::filter(n > 1L)

  if (nrow(duplicated_stocks) > 0L) {
    stop(
      "Multiple non-identical rows exist for StockKeyLabel: ",
      paste(utils::head(duplicated_stocks$StockKeyLabel, 8L),
            collapse = ", "),
      ". Supply one row per stock (e.g., the latest assessment)."
    )
  }

  # F/Fref > 1 is red. B/Bref < 1 is red.
  # Exact equality (1) is grouped with GREEN.
  # Missing/invalid references or ratios -> GREY.
  stock_df <- stock_df |>
    dplyr::mutate(
      FStatus = dplyr::case_when(
        !is.finite(FMSY) | FMSY <= 0 ~ "GREY",
        !is.finite(F_FMSY) | F_FMSY < 0 ~ "GREY",
        F_FMSY > 1 ~ "RED",
        TRUE ~ "GREEN"
      ),
      BStatus = dplyr::case_when(
        !is.finite(MSYBtrigger) | MSYBtrigger <= 0 ~ "GREY",
        !is.finite(SSB_MSYBtrigger) | SSB_MSYBtrigger < 0 ~ "GREY",
        SSB_MSYBtrigger < 1 ~ "RED",
        TRUE ~ "GREEN"
      ),
      FReason = dplyr::case_when(
        !is.finite(FMSY) | FMSY <= 0 ~ "F reference unavailable",
        !is.finite(F_FMSY) | F_FMSY < 0 ~ "F/Fref unavailable",
        F_FMSY > 1 ~ "Above F reference",
        TRUE ~ "At or below F reference"
      ),
      BReason = dplyr::case_when(
        !is.finite(MSYBtrigger) | MSYBtrigger <= 0 ~
          "Biomass reference unavailable",
        !is.finite(SSB_MSYBtrigger) | SSB_MSYBtrigger < 0 ~
          "B/Bref unavailable",
        SSB_MSYBtrigger < 1 ~ "Below biomass reference",
        TRUE ~ "At or above biomass reference"
      )
    )

  long_df <- dplyr::bind_rows(
    stock_df |>
      dplyr::transmute(
        StockKeyLabel, FisheriesGuild,
        Indicator = "Fref", Colour = FStatus,
        Ratio = F_FMSY, Reference = FMSY, Reason = FReason
      ),
    stock_df |>
      dplyr::transmute(
        StockKeyLabel, FisheriesGuild,
        Indicator = "Bref", Colour = BStatus,
        Ratio = SSB_MSYBtrigger, Reference = MSYBtrigger,
        Reason = BReason
      )
  )

  # Duplicate into Total AFTER deduplication: each stock counted once
  # in its guild and once in Total.
  long_df <- dplyr::bind_rows(
    long_df,
    dplyr::mutate(long_df, FisheriesGuild = "Total")
  )

  long_df |>
    dplyr::group_by(FisheriesGuild, Indicator, Colour) |>
    dplyr::summarise(
      COUNT = dplyr::n(),
      stock_ids = list(sort(StockKeyLabel)),
      .groups = "drop"
    ) |>
    dplyr::group_by(FisheriesGuild, Indicator) |>
    dplyr::mutate(
      total_stocks = sum(COUNT),
      pct = 100 * COUNT / total_stocks
    ) |>
    dplyr::ungroup()
}


#' Interactive stock-status pies by fisheries guild
#'
#' Creates an interactive grid of pie charts showing the proportion
#' of stocks classified as GREEN, RED, or GREY according to
#' precautionary fishing mortality and biomass reference points.
#'
#' Stock status is calculated independently for fishing pressure
#' (F/Fref) and stock size (B/Bref), rather than using the original
#' status categories provided by the ICES Stock Assessment Database.
#'
#' The plot displays fisheries guilds in columns and the two
#' stock-status indicators in rows.
#'
#' @param df A data frame containing stock-level assessment information,
#'   typically produced by `stockstatus_CLD_current_proxy()` after
#'   precautionary reference points have been selected using
#'   `add_proxyRefPoints_v2()`.
#'
#'   The data are processed by `prepare_precautionary_status_pies()`,
#'   which calculates stock counts and proportions by guild,
#'   indicator, and status category.
#'
#' @param return_data Logical; if TRUE, returns the processed
#'   stock-status summary data instead of the Plotly figure.
#'   Default is FALSE.
#'
#' @param width_px Numeric; approximate width of the plot in pixels.
#'   Used to determine the overall plot height. In Shiny, this can
#'   be obtained from `session$clientData`.
#'   Default is 800.
#'
#' @param max_hover_stocks Positive numeric value specifying the
#'   maximum number of stock identifiers displayed in each pie
#'   slice's hover information. Additional stocks are summarised
#'   as "... and n more". Default is 25.
#'
#' @return
#' If `return_data = FALSE`, a Plotly object containing interactive
#' stock-status pie charts.
#'
#' If `return_data = TRUE`, a data frame returned by
#' `prepare_precautionary_status_pies()`, containing variables such as:
#' \describe{
#'   \item{FisheriesGuild}{Fisheries guild or Total category.}
#'   \item{Indicator}{Reference-point indicator: Fref or Bref.}
#'   \item{Colour}{Stock-status category: GREEN, RED, or GREY.}
#'   \item{COUNT}{Number of stocks in the status category.}
#'   \item{total_stocks}{Total number of stocks in the guild and indicator.}
#'   \item{pct}{Percentage of stocks in the status category.}
#'   \item{stock_ids}{Identifiers of stocks contributing to the slice.}
#' }
#'
#' @details
#' The function uses precautionary reference-point classifications:
#'
#' \strong{Fishing pressure (F/Fref):}
#' \itemize{
#'   \item GREEN: F/Fref <= 1.
#'   \item RED: F/Fref > 1.
#'   \item GREY: The relevant reference point or ratio is unavailable
#'     or invalid.
#' }
#'
#' \strong{Stock size (B/Bref):}
#' \itemize{
#'   \item GREEN: B/Bref >= 1.
#'   \item RED: B/Bref < 1.
#'   \item GREY: The relevant reference point or ratio is unavailable
#'     or invalid.
#' }
#'
#' The underlying precautionary reference-point selection uses the
#' lowest eligible fishing mortality reference point and the highest
#' eligible biomass reference point.
#'
#' \strong{Plot organisation:}
#' \itemize{
#'   \item Columns represent fisheries guilds in the following order:
#'     Benthic, Demersal, Elasmobranch, Pelagic, Shellfish, Total.
#'     Additional guilds, if present, are appended alphabetically.
#'   \item Rows represent Fishing pressure (F/Fref) and
#'     Stock size (B/Bref).
#'   \item The Unknown fisheries guild is excluded as a separate
#'     plot column, but its stocks remain included in Total.
#'   \item Each pie represents the distribution of stock counts,
#'     not catches or landings.
#'   \item The number inside each slice indicates the number of
#'     stocks in that status category.
#'   \item A label below each pie displays the total number of stocks
#'     represented.
#' }
#'
#' \strong{Interactivity:}
#' Hovering over a pie slice displays:
#' \itemize{
#'   \item Fisheries guild and reference-point indicator.
#'   \item Status category (GREEN, RED, or GREY).
#'   \item Number and percentage of stocks in the category.
#'   \item Stock identifiers contributing to that category, up to
#'     `max_hover_stocks`.
#' }
#'
#' \strong{Layout and export:}
#' \itemize{
#'   \item A fixed row spacing of 0.16 separates the two indicators.
#'   \item Plot height is adjusted according to `width_px`, with
#'     a minimum of 700 px and a maximum of 1100 px.
#'   \item Colours are fixed across all pies:
#'     GREEN (#00B26D), RED (#d93b1c), and GREY (#d3d3d3).
#'   \item A shared status legend is displayed.
#'   \item A caption identifies the ICES Stock Assessment Database
#'     and the current date.
#'   \item The Plotly toolbar supports PNG export at scale 3.
#' }
#'
#' The function requires `prepare_precautionary_status_pies()`
#' to be available in the R environment.
#'
#' @examples
#' \dontrun{
#' # Interactive stock-count pies for all fisheries guilds
#' plot_status_prop_pies_plotly_v2(catch_current_v2)
#'
#' # Increase the plot width and maximum hover stock list
#' plot_status_prop_pies_plotly_v2(
#'   catch_current_v2,
#'   width_px = 1400,
#'   max_hover_stocks = 40
#' )
#'
#' # Inspect the underlying stock-status summary
#' status_summary <- plot_status_prop_pies_plotly_v2(
#'   catch_current_v2,
#'   return_data = TRUE
#' )
#' }
#'
#' @seealso
#' `prepare_precautionary_status_pies()`,
#' `stockstatus_CLD_current_proxy()`,
#' `add_proxyRefPoints_v2()`
#'
#' @importFrom plotly plot_ly add_trace layout config
#' @importFrom dplyr mutate filter
#' @noRd
plot_status_prop_pies_plotly_v2 <- function(
    df,
    return_data = FALSE,
    width_px = 800,
    max_hover_stocks = 25
) {

  counts <- prepare_precautionary_status_pies(df)
  if (isTRUE(return_data)) return(counts)

  if (!is.numeric(width_px) || length(width_px) != 1L ||
      !is.finite(width_px) || width_px <= 0) {
    width_px <- 800
  }

  if (!is.numeric(max_hover_stocks) || length(max_hover_stocks) != 1L ||
      !is.finite(max_hover_stocks) || max_hover_stocks < 1) {
    stop("max_hover_stocks must be a positive number.")
  }
  max_hover_stocks <- as.integer(max_hover_stocks)

  status_colors <- c(
    GREEN = "#00B26D",
    RED   = "#d93b1c",
    GREY  = "#d3d3d3"
  )

  indicators <- c("Fref", "Bref")
  headers <- c(
    Fref = "Fishing pressure (F/Fref)",
    Bref = "Stock size (B/Bref)"
  )

  # ---------------------------------------------------------
  # Helper labels for guilds
  # ---------------------------------------------------------

  guild_display_name <- function(g) {
    g <- tolower(trimws(g))
    if (g == "total") return("Total")
    tools::toTitleCase(g)
  }

  # Standardise guild names for plotting
  counts <- counts |>
    dplyr::mutate(
      GuildPlot = tolower(trimws(as.character(FisheriesGuild)))
    )

  # Remove Unknown from displayed guilds only
  counts_plot <- counts |>
    dplyr::filter(GuildPlot != "unknown")

  # Fixed desired order
  desired_guilds <- c(
    "benthic",
    "demersal",
    "elasmobranch",
    "pelagic",
    "shellfish",
    "total"
  )

  present_guilds <- unique(as.character(counts_plot$GuildPlot))
  guilds <- c(
    intersect(desired_guilds, present_guilds),
    sort(setdiff(present_guilds, desired_guilds))
  )

  format_stocks <- function(ids) {
    shown <- utils::head(ids, max_hover_stocks)
    out <- paste(shown, collapse = "<br>")
    n_more <- length(ids) - length(shown)
    if (n_more > 0L) {
      out <- paste0(out, "<br>... and ", n_more, " more")
    }
    out
  }

  counts_plot$hover_text <- vapply(seq_len(nrow(counts_plot)), function(i) {
    paste0(
      "<b>", guild_display_name(counts_plot$GuildPlot[[i]]), " — ",
      headers[[counts_plot$Indicator[[i]]]], "</b><br>",
      "Status: ", counts_plot$Colour[[i]], "<br>",
      "Stocks: ", counts_plot$COUNT[[i]], " / ", counts_plot$total_stocks[[i]],
      " (", sprintf("%.1f", counts_plot$pct[[i]]), "%)<br><br>",
      "<b>Stocks</b><br>", format_stocks(counts_plot$stock_ids[[i]])
    )
  }, character(1))

  # ---------------------------------------------------------
  # Layout:
  #   columns = guilds
  #   rows    = indicators
  # ---------------------------------------------------------

  n_rows <- length(indicators)
  n_cols <- length(guilds)

  top <- 0.88
  bottom <- 0.10
  left <- 0.12
  right <- 0.95

  # Height/spacing adjustments requested
  row_gap <- 0.16
  col_gap <- min(0.025, 0.12 / max(1, n_cols))

  row_height <- (top - bottom - (n_rows - 1L) * row_gap) / n_rows
  col_width  <- (right - left - (n_cols - 1L) * col_gap) / n_cols

  p <- plotly::plot_ly()
  ann <- list()

  # ---------------------------------------------------------
  # Stable legend
  # ---------------------------------------------------------

  for (status in names(status_colors)) {
    p <- plotly::add_trace(
      p,
      type = "scatter",
      mode = "markers",
      x = -1, y = -1,
      name = status,
      marker = list(color = status_colors[[status]], size = 11),
      legendgroup = status,
      showlegend = TRUE,
      hoverinfo = "skip",
      inherit = FALSE
    )
  }

  # ---------------------------------------------------------
  # Add pies
  # ---------------------------------------------------------

  for (i in seq_along(indicators)) {

    y1 <- top - (i - 1L) * (row_height + row_gap)
    y0 <- y1 - row_height

    for (j in seq_along(guilds)) {

      x0 <- left + (j - 1L) * (col_width + col_gap)
      x1 <- x0 + col_width

      sub <- counts_plot[
        counts_plot$GuildPlot == guilds[[j]] &
          counts_plot$Indicator == indicators[[i]],
        , drop = FALSE
      ]

      if (nrow(sub) == 0L) {
        ann[[length(ann) + 1L]] <- list(
          x = (x0 + x1) / 2,
          y = (y0 + y1) / 2,
          xref = "paper", yref = "paper",
          text = "No data",
          showarrow = FALSE,
          font = list(size = 10, color = "#666666")
        )
        next
      }

      sub <- sub[match(c("GREEN", "RED", "GREY"), sub$Colour), , drop = FALSE]
      sub <- sub[!is.na(sub$Colour), , drop = FALSE]

      p <- plotly::add_trace(
        p,
        type = "pie",
        labels = as.character(sub$Colour),
        values = as.numeric(sub$COUNT),
        text = as.character(sub$COUNT),
        customdata = as.character(sub$hover_text),
        hovertemplate = "%{customdata}<extra></extra>",
        textinfo = "text",
        textposition = "inside",
        insidetextorientation = "horizontal",
        sort = FALSE,
        direction = "clockwise",
        marker = list(
          colors = unname(status_colors[as.character(sub$Colour)]),
          line = list(color = "#FFFFFF", width = 1)
        ),
        domain = list(x = c(x0, x1), y = c(y0, y1)),
        showlegend = FALSE,
        inherit = FALSE
      )

      # total stocks under each pie
      pie_total <- unique(sub$total_stocks)
      pie_total <- pie_total[!is.na(pie_total)]

      if (length(pie_total) == 0L) {
        pie_total <- NA_integer_
      } else {
        pie_total <- pie_total[[1]]
      }

      ann[[length(ann) + 1L]] <- list(
        x = (x0 + x1) / 2,
        y = y0 - 0.025,
        xref = "paper", yref = "paper",
        text = paste0("total = ", pie_total, " stocks"),
        showarrow = FALSE,
        font = list(size = 10),
        yanchor = "top"
      )
    }
  }

  # ---------------------------------------------------------
  # Column headers = guilds
  # ---------------------------------------------------------

  for (j in seq_along(guilds)) {
    x0 <- left + (j - 1L) * (col_width + col_gap)
    x1 <- x0 + col_width

    ann[[length(ann) + 1L]] <- list(
      x = (x0 + x1) / 2,
      y = 0.955,
      xref = "paper", yref = "paper",
      xanchor = "center", yanchor = "middle",
      text = paste0("<b>", guild_display_name(guilds[[j]]), "</b>"),
      showarrow = FALSE,
      font = list(size = 12)
    )
  }

  # ---------------------------------------------------------
  # Row headers = indicators
  # ---------------------------------------------------------

  for (i in seq_along(indicators)) {
    y1 <- top - (i - 1L) * (row_height + row_gap)
    y0 <- y1 - row_height

    ann[[length(ann) + 1L]] <- list(
      x = 0.05,
      y = (y0 + y1) / 2,
      xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "middle",
      text = paste0("<b>", headers[[indicators[[i]]]], "</b>"),
      showarrow = FALSE,
      font = list(size = 12),
      textangle = -90
    )
  }

  # footer
  ann[[length(ann) + 1L]] <- list(
    x = 1, y = -0.05,
    xref = "paper", yref = "paper",
    xanchor = "right", yanchor = "top",
    text = paste0(
      "ICES Stock Assessment Database, ",
      format(Sys.Date(), "%d-%b-%y"),
      ". ICES, Copenhagen"
    ),
    showarrow = FALSE,
    font = list(size = 10)
  )

  # Height adjustment requested
  plot_height <- max(700, min(1100, round(width_px * 0.75)))

  p <- plotly::layout(
    p,
    height = plot_height,
    paper_bgcolor = "white",
    plot_bgcolor = "white",
    legend = list(
      title = list(text = "Status"),
      x = 1.01, y = 1,
      xanchor = "left", yanchor = "top",
      bgcolor = "rgba(255,255,255,0.85)"
    ),
    margin = list(l = 95, r = 115, t = 65, b = 85),
    xaxis = list(visible = FALSE, range = c(0, 1), fixedrange = TRUE),
    yaxis = list(visible = FALSE, range = c(0, 1), fixedrange = TRUE),
    annotations = ann
  )

  plotly::config(
    p,
    toImageButtonOptions = list(
      filename = paste0("Status_pies_", format(Sys.Date(), "%d-%b-%y")),
      format = "png",
      scale = 3
    )
  )
}



#' Interactive catch-weighted status pies using precautionary reference points
#'
#' Each stock is classified independently for fishing mortality and biomass,
#' using ratios calculated against the selected precautionary references:
#'   F/Fref > 1  -> RED; otherwise GREEN
#'   B/Bref < 1  -> RED; otherwise GREEN
#'   Missing/invalid reference or ratio -> GREY
#'
#' Pie slices represent the proportion of catch (or fallback landings) in the
#' ecoregion, NOT the proportion of stock counts. Columns are fisheries guilds,
#' including Total; the two rows are F/Fref and B/Bref.
#'
#' @param x One row per stock from stockstatus_CLD_current_proxy(), after
#'   add_proxyRefPoints_v2() has selected the precautionary reference points.
#'   Requires StockKeyLabel, FisheriesGuild, FMSY, MSYBtrigger, F_FMSY,
#'   and SSB_MSYBtrigger.
#' @param y Optional catch source with StockKeyLabel, Catches_in_ecoregion and/or
#'   Landings_in_ecoregion. If NULL, x is used for catches and landings.
#' @param return_data If TRUE, return catch-weighted status summaries.
#' @param width_px Approximate display width (used for responsive height).
#' @param max_hover_stocks Maximum number of stocks listed per hover slice.
#' @return A Plotly object or a summary data frame.
plot_GES_pies_plotly_v2 <- function(
    x,
    y = NULL,
    return_data = FALSE,
    width_px = 800,
    max_hover_stocks = 25
) {

  status_colors <- c(
    GREEN = "#00B26D",
    RED   = "#d93b1c",
    GREY  = "#d3d3d3"
  )

  required_x <- c(
    "StockKeyLabel", "FisheriesGuild", "FMSY", "MSYBtrigger",
    "F_FMSY", "SSB_MSYBtrigger"
  )
  missing_x <- setdiff(required_x, names(x))
  if (length(missing_x) > 0L) {
    stop("Missing columns in x: ", paste(missing_x, collapse = ", "))
  }

  if (is.null(y)) y <- x
  if (!"StockKeyLabel" %in% names(y)) {
    stop("y must contain StockKeyLabel.")
  }
  if (!any(c("Catches_ecoregion", "Landings_ecoregion") %in% names(y))) {
    stop("y (or x if y is NULL) must contain Catches_ecoregion ",
         "and/or Landings_ecoregion.")
  }
  if (!"Catches_ecoregion" %in% names(y)) {
    y$Catches_ecoregion <- NA_real_
  }
  if (!"Landings_ecoregion" %in% names(y)) {
    y$Landings_ecoregion <- NA_real_
  }

  to_number <- function(z) suppressWarnings(as.numeric(as.character(z)))
  clean_nonnegative <- function(z) {
    z <- to_number(z)
    z[!is.finite(z) | z < 0] <- NA_real_
    z
  }
  format_tonnes <- function(z) {
    format(round(z, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  escape_html <- function(z) {
    z <- gsub("&", "&amp;", z, fixed = TRUE)
    z <- gsub("<", "&lt;", z, fixed = TRUE)
    gsub(">", "&gt;", z, fixed = TRUE)
  }

  # ---------------------------------------------------------
  # 1. Calculate INDEPENDENT precautionary F and B status.
  #    Do not use the original SAG FishingPressure/StockSize status.
  # ---------------------------------------------------------
  stock_df <- x |>
    dplyr::transmute(
      StockKeyLabel = as.character(StockKeyLabel),
      FisheriesGuild = as.character(FisheriesGuild),
      FMSY = to_number(FMSY),
      MSYBtrigger = to_number(MSYBtrigger),
      F_FMSY = to_number(F_FMSY),
      SSB_MSYBtrigger = to_number(SSB_MSYBtrigger)
    ) |>
    dplyr::filter(!is.na(StockKeyLabel), nzchar(StockKeyLabel)) |>
    dplyr::mutate(
      FisheriesGuild = dplyr::if_else(
        is.na(FisheriesGuild) | !nzchar(FisheriesGuild),
        "Unknown", FisheriesGuild
      )
    ) |>
    dplyr::distinct()

  if (nrow(stock_df) == 0L) stop("No stocks in x.")

  # Exact duplicate rows are harmless, but differing records for the
  # same stock should not be joined/counted twice.
  repeated <- stock_df |>
    dplyr::count(StockKeyLabel, name = "n") |>
    dplyr::filter(n > 1L)
  if (nrow(repeated) > 0L) {
    stop("Conflicting rows for StockKeyLabel in x: ",
         paste(utils::head(repeated$StockKeyLabel, 8L), collapse = ", "),
         ". Supply one assessed row per stock.")
  }

  stock_df <- stock_df |>
    dplyr::mutate(
      F_Status = dplyr::case_when(
        !is.finite(FMSY) | FMSY <= 0 ~ "GREY",
        !is.finite(F_FMSY) | F_FMSY < 0 ~ "GREY",
        F_FMSY > 1 ~ "RED",
        TRUE ~ "GREEN"
      ),
      B_Status = dplyr::case_when(
        !is.finite(MSYBtrigger) | MSYBtrigger <= 0 ~ "GREY",
        !is.finite(SSB_MSYBtrigger) | SSB_MSYBtrigger < 0 ~ "GREY",
        SSB_MSYBtrigger < 1 ~ "RED",
        TRUE ~ "GREEN"
      )
    )

  # ---------------------------------------------------------
  # 2. Attach ecoregion catches, falling back to landings.
  #    Missing catch weights are not converted into status GREY.
  # ---------------------------------------------------------
  catch_df <- y |>
    dplyr::transmute(
      StockKeyLabel = as.character(StockKeyLabel),
      catch_value = clean_nonnegative(Catches_ecoregion),
      landing_value = clean_nonnegative(Landings_ecoregion)
    ) |>
    dplyr::filter(!is.na(StockKeyLabel), nzchar(StockKeyLabel)) |>
    dplyr::mutate(
      CATCH = dplyr::coalesce(catch_value, landing_value),
      CatchSource = dplyr::case_when(
        !is.na(catch_value) ~ "Catches",
        !is.na(landing_value) ~ "Landings",
        TRUE ~ "Missing"
      )
    ) |>
    dplyr::select(StockKeyLabel, CATCH, CatchSource) |>
    dplyr::distinct()

  repeated_catch <- catch_df |>
    dplyr::count(StockKeyLabel, name = "n") |>
    dplyr::filter(n > 1L)
  if (nrow(repeated_catch) > 0L) {
    stop("Conflicting catch/landing records in y for: ",
         paste(utils::head(repeated_catch$StockKeyLabel, 8L),
               collapse = ", "),
         ". Resolve these before plotting to avoid double counting.")
  }

  stock_df <- stock_df |>
    dplyr::left_join(catch_df, by = "StockKeyLabel")

  # ---------------------------------------------------------
  # 3. Long data: each stock contributes to BOTH indicators.
  # ---------------------------------------------------------
  long_df <- dplyr::bind_rows(
    stock_df |>
      dplyr::transmute(
        StockKeyLabel, FisheriesGuild, CATCH, CatchSource,
        Indicator = "Fref", Colour = F_Status
      ),
    stock_df |>
      dplyr::transmute(
        StockKeyLabel, FisheriesGuild, CATCH, CatchSource,
        Indicator = "Bref", Colour = B_Status
      )
  )

  # Total row is assembled from the same stocks, once per indicator.
  long_df <- dplyr::bind_rows(
    long_df,
    dplyr::mutate(long_df, FisheriesGuild = "Total")
  )

  # Note exclusions for transparency. Stocks with zero or unknown catch
  # cannot contribute to a catch-weighted pie, irrespective of status.
  coverage <- long_df |>
    dplyr::group_by(FisheriesGuild, Indicator) |>
    dplyr::summarise(
      n_stocks = dplyr::n(),
      excluded_stocks = sum(!is.finite(CATCH) | CATCH <= 0),
      .groups = "drop"
    )

  weighted <- long_df |>
    dplyr::filter(is.finite(CATCH), CATCH > 0)

  if (nrow(weighted) == 0L) {
    stop("No stocks have positive Catches_ecoregion or ",
         "Landings_ecoregion values.")
  }

  # Stock-level hover text within each status slice.
  if (!is.numeric(max_hover_stocks) || length(max_hover_stocks) != 1L ||
      !is.finite(max_hover_stocks) || max_hover_stocks < 1) {
    stop("max_hover_stocks must be a positive number.")
  }
  max_hover_stocks <- as.integer(max_hover_stocks)
  bar_width <- 20L

  build_breakdown <- function(stock_ids, catches, sources) {
    ix <- order(catches, decreasing = TRUE)
    stock_ids <- stock_ids[ix]
    catches <- catches[ix]
    sources <- sources[ix]
    total <- sum(catches)
    show <- seq_len(min(length(stock_ids), max_hover_stocks))
    lines <- vapply(show, function(i) {
      pct <- 100 * catches[[i]] / total
      paste0(
        escape_html(stock_ids[[i]]), ": ",
        strrep("\u2588", max(1L, round(bar_width * pct / 100))),
        " ", sprintf("%.1f%%", pct),
        " (", format_tonnes(catches[[i]]), " t; ",
        sources[[i]], ")"
      )
    }, character(1))
    additional <- length(stock_ids) - length(show)
    if (additional > 0L) {
      lines <- c(lines, paste0("... and ", additional, " more stocks"))
    }
    paste(lines, collapse = "<br>")
  }

  slices <- weighted |>
    dplyr::group_by(FisheriesGuild, Indicator, Colour) |>
    dplyr::summarise(
      Catch = sum(CATCH),
      COUNT = dplyr::n(),
      breakdown_html = build_breakdown(StockKeyLabel, CATCH, CatchSource),
      stock_ids = list(StockKeyLabel),
      .groups = "drop"
    ) |>
    dplyr::group_by(FisheriesGuild, Indicator) |>
    dplyr::mutate(
      total_catch = sum(Catch),
      pct = 100 * Catch / total_catch
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(coverage, by = c("FisheriesGuild", "Indicator"))

  headers <- c(
    Fref = "Fishing pressure (F/Fref)",
    Bref = "Stock size (B/Bref)"
  )

  slices$hover_text <- vapply(seq_len(nrow(slices)), function(i) {
    paste0(
      "<b>", escape_html(slices$FisheriesGuild[[i]]), " — ",
      headers[[slices$Indicator[[i]]]], "</b><br>",
      "Status: ", slices$Colour[[i]], "<br>",
      "Catch / landings: ", format_tonnes(slices$Catch[[i]]), " t",
      " (", sprintf("%.1f%%", slices$pct[[i]]), ")<br>",
      "Stocks in slice: ", slices$COUNT[[i]], "<br>",
      "Stocks excluded from this guild (no positive catch): ",
      slices$excluded_stocks[[i]], "<br><br>",
      "<b>Catch by stock</b><br>", slices$breakdown_html[[i]]
    )
  }, character(1))

  if (isTRUE(return_data)) return(slices)

  # ---------------------------------------------------------
  # 4. Plotly layout: guilds as COLUMNS, F/B as TWO ROWS.
  #    Matches the orientation of the original GES pies.
  # ---------------------------------------------------------
  if (!is.numeric(width_px) || length(width_px) != 1L ||
      !is.finite(width_px) || width_px <= 0) {
    width_px <- 800
  }

  # Display guilds left to right, with Total at the end.
  requested_guilds <- c(
    "benthic", "demersal", "elasmobranch",
    "pelagic", "shellfish", "Total"
  )
  actual_guilds <- unique(as.character(slices$FisheriesGuild))
  guilds <- c(
    intersect(requested_guilds, actual_guilds),
    sort(setdiff(actual_guilds, requested_guilds))
  )
  # Hide the Unknown guild column. Keep its stocks in the Total pies.
  guilds <- guilds[tolower(trimws(guilds)) != "unknown"]

  indicators <- c("Fref", "Bref")

  n_cols <- length(guilds)
  n_rows <- length(indicators)

  # Domains are expressed as fractions of the plotting area (0-1).
  # A little extra space between rows accommodates catch-total labels.
  left <- 0.065
  right <- 0.975
  col_gap <- if (n_cols <= 3L) 0.045 else 0.018
  col_width <- (right - left - (n_cols - 1L) * col_gap) / n_cols

  top <- 0.90
  bottom <- 0.12
  row_gap <- 0.16
  row_height <- (top - bottom - (n_rows - 1L) * row_gap) / n_rows

  p <- plotly::plot_ly()
  annotations <- list()

  # Explicit status legend (one entry for each possible colour).
  for (status in names(status_colors)) {
    p <- plotly::add_trace(
      p,
      x = -1, y = -1,
      type = "scatter", mode = "markers",
      name = status,
      marker = list(color = status_colors[[status]], size = 11),
      visible = "legendonly", showlegend = TRUE,
      legendgroup = status, hoverinfo = "skip", inherit = FALSE
    )
  }

  # Two ROWS: fishing pressure (top), biomass (bottom).
  for (i in seq_along(indicators)) {
    y1 <- top - (i - 1L) * (row_height + row_gap)
    y0 <- y1 - row_height

    # Indicator label on the left side of the row.
    annotations[[length(annotations) + 1L]] <- list(
      x = left - 0.030,
      y = mean(c(y0, y1)),
      xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "middle",
      text = paste0("<b>", headers[[indicators[[i]]]], "</b>"),
      textangle = -90,
      showarrow = FALSE, font = list(size = 12)
    )

    # Guilds run across the COLUMNS.
    for (j in seq_along(guilds)) {
      x0 <- left + (j - 1L) * (col_width + col_gap)
      x1 <- x0 + col_width

      sub <- slices[
        slices$FisheriesGuild == guilds[[j]] &
          slices$Indicator == indicators[[i]],
        , drop = FALSE
      ]

      if (nrow(sub) == 0L) {
        annotations[[length(annotations) + 1L]] <- list(
          x = mean(c(x0, x1)), y = mean(c(y0, y1)),
          xref = "paper", yref = "paper",
          text = "No catch data", showarrow = FALSE,
          font = list(size = 10, color = "#666666")
        )
        next
      }

      # Keep a consistent clockwise status order in all pies.
      sub <- sub[match(c("GREEN", "RED", "GREY"), sub$Colour), , drop = FALSE]
      sub <- sub[!is.na(sub$Colour), , drop = FALSE]

      p <- plotly::add_trace(
        p,
        type = "pie",
        labels = as.character(sub$Colour),
        values = as.numeric(sub$Catch),
        customdata = as.character(sub$hover_text),
        hovertemplate = "%{customdata}<extra></extra>",
        textinfo = "label+percent",
        textposition = "auto",
        insidetextorientation = "horizontal",
        sort = FALSE,
        direction = "clockwise",
        marker = list(
          colors = unname(status_colors[as.character(sub$Colour)]),
          line = list(color = "#FFFFFF", width = 1)
        ),
        domain = list(x = c(x0, x1), y = c(y0, y1)),
        showlegend = FALSE,
        inherit = FALSE
      )

      # A total underneath each pie, as in the original guild-by-column plot.
      guild_catch <- unique(sub$total_catch)
      annotations[[length(annotations) + 1L]] <- list(
        x = mean(c(x0, x1)), y = y0 - 0.012,
        xref = "paper", yref = "paper",
        xanchor = "center", yanchor = "top",
        text = paste0(
          "total = ", format(round(guild_catch[[1]] / 1000, 0),
                              big.mark = ",", scientific = FALSE), "k t"
        ),
        showarrow = FALSE, font = list(size = 10)
      )
    }
  }

  # Guild names as COLUMN headers.
  for (j in seq_along(guilds)) {
    x0 <- left + (j - 1L) * (col_width + col_gap)
    annotations[[length(annotations) + 1L]] <- list(
      x = x0 + col_width / 2, y = 0.965,
      xref = "paper", yref = "paper",
      text = paste0(
        "<b>", escape_html(tools::toTitleCase(tolower(guilds[[j]]))), "</b>"
      ),
      showarrow = FALSE, font = list(size = 12)
    )
  }

  annotations[[length(annotations) + 1L]] <- list(
    x = 1, y = -0.04,
    xref = "paper", yref = "paper",
    xanchor = "right", yanchor = "top", showarrow = FALSE,
    text = paste0(
      "ICES Stock Assessment Database, ",
      format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"
    ),
    font = list(size = 10)
  )

  # Only two rows now, so the chart can be much shorter than before.
  plot_height <- max(700, min(820, round(width_px * 0.50)))

  p <- plotly::layout(
    p,
    height = plot_height,
    paper_bgcolor = "white", plot_bgcolor = "white",
    legend = list(
      title = list(text = "Status"),
      x = 1.01, y = 1, xanchor = "left", yanchor = "top",
      bgcolor = "rgba(255,255,255,0.90)"
    ),
    margin = list(l = 100, r = 120, t = 65, b = 75),
    xaxis = list(visible = FALSE, range = c(0, 1), fixedrange = TRUE),
    yaxis = list(visible = FALSE, range = c(0, 1), fixedrange = TRUE),
    annotations = annotations
  )

  plotly::config(
    p,
    toImageButtonOptions = list(
      filename = paste0("GES_pies_precautionary_", format(Sys.Date(), "%d-%b-%y")),
      format = "png", scale = 3
    )
  )
}


#' Interactive Kobe plot with optional panel for stocks of unknown status
#'
#' Extends plot_kobe_app_plotly_v2() by showing stocks lacking one or both
#' usable reference-point ratios in a separate narrow panel on the right.
#' The panel appears automatically only if such stocks exist in the selected
#' fisheries guild(s). Stocks are plotted with catch-scaled grey circles and
#' labelled by stock code; hover text explains why status is unknown.
#'
#' @param x Stock-level data with StockKeyLabel, FisheriesGuild, F_FMSY,
#'   SSB_MSYBtrigger, Catches_in_ecoregion, and Landings_in_ecoregion.
#' @param guild Fisheries guild(s), or "All".
#' @param return_data If TRUE, return prepared data with KobeStatus, KobePanel,
#'   UnknownReason, CatchSizeValue, CatchSizeSource, and point_size.
#' @param size_by_catches If TRUE, use the same square-root catch-size scaling
#'   for the Kobe plot and the unknown-status panel.
#' @param min_point_size,max_point_size Range of marker diameters (pixels).
#' @param n_size_legend Number of representative sizes shown in the catch-size legend.
#' @param max_unknown_stocks Maximum number of stocks shown in the Unknown
#'   panel, ranked by ecoregion catches (or fallback landings). Default 12.
#'   Use Inf to show all. All stocks remain in return_data and size scaling.
#' @param unknown_row_px Approximate minimum vertical space per displayed
#'   unknown stock (pixels); used to increase plot height when necessary.
#' @return Plotly widget, or prepared data if return_data is TRUE.
#' @noRd
plot_kobe_app_plotly_v3 <- function(
    x,
    guild,
    return_data = FALSE,
    size_by_catches = FALSE,
    min_point_size = 8,
    max_point_size = 25,
    n_size_legend = 4,
    unknown_row_px = 26,
    max_unknown_stocks = 12
) {

  # 1. Validate input and filter stocks.
  if (!is.numeric(min_point_size) || length(min_point_size) != 1L ||
      !is.finite(min_point_size) || min_point_size <= 0 ||
      !is.numeric(max_point_size) || length(max_point_size) != 1L ||
      !is.finite(max_point_size) || max_point_size <= min_point_size) {
    stop("Require 0 < min_point_size < max_point_size.")
  }
  if (!is.numeric(unknown_row_px) || length(unknown_row_px) != 1L ||
      !is.finite(unknown_row_px) || unknown_row_px <= 0) {
    stop("unknown_row_px must be a positive number.")
  }

  if (!is.numeric(max_unknown_stocks) || length(max_unknown_stocks) != 1L ||
      is.na(max_unknown_stocks) || max_unknown_stocks < 1 ||
      (!is.infinite(max_unknown_stocks) &&
       max_unknown_stocks != floor(max_unknown_stocks))) {
    stop("max_unknown_stocks must be a positive integer or Inf.")
  }

  required_cols <- c(
    "StockKeyLabel", "FisheriesGuild", "F_FMSY", "SSB_MSYBtrigger",
    "Catches_ecoregion", "Landings_ecoregion"
  )
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0L) {
    stop("Missing input columns: ", paste(missing_cols, collapse = ", "))
  }

  df <- if (identical(guild, "All")) {
    x
  } else {
    dplyr::filter(x, FisheriesGuild %in% guild)
  }
  if (nrow(df) == 0L) stop("No stocks for the selected fisheries guild(s).")

  to_number <- function(z) suppressWarnings(as.numeric(as.character(z)))
  for (nm in c("F_FMSY", "SSB_MSYBtrigger",
               "Catches_ecoregion", "Landings_ecoregion")) {
    df[[nm]] <- to_number(df[[nm]])
  }

  # 2. Standard/proxy point flags.
  if (!"F_proxy" %in% names(df)) df$F_proxy <- FALSE
  if (!"B_proxy" %in% names(df)) df$B_proxy <- FALSE
  df <- dplyr::mutate(
    df,
    ProxyFlag = dplyr::if_else(
      (F_proxy %in% TRUE) | (B_proxy %in% TRUE),
      "Proxy reference point", "Reference point"
    )
  )

  # 3. Classify using coordinates, not SAG's old Status column.
  # Negative ratios are invalid too: they belong in the unknown panel.
  valid_f <- is.finite(df$F_FMSY) & df$F_FMSY >= 0
  valid_b <- is.finite(df$SSB_MSYBtrigger) & df$SSB_MSYBtrigger >= 0
  known <- valid_f & valid_b

  df$KobeStatus <- dplyr::case_when(
    !known ~ "Unknown",
    df$F_FMSY <= 1 & df$SSB_MSYBtrigger >= 1 ~ "Green",
    df$F_FMSY > 1 & df$SSB_MSYBtrigger < 1 ~ "Red",
    TRUE ~ "Orange"
  )
  df$KobePanel <- ifelse(known, "Kobe", "Unknown")
  df$UnknownReason <- dplyr::case_when(
    !valid_f & !valid_b ~ "F/Fref and B/Bref unavailable or invalid",
    !valid_f ~ "F/Fref unavailable or invalid",
    !valid_b ~ "B/Bref unavailable or invalid",
    TRUE ~ ""
  )
  df$KobeDescription <- dplyr::case_when(
    df$KobeStatus == "Green" ~
      "Biomass above threshold; fishing pressure sustainable",
    df$KobeStatus == "Red" ~
      "Biomass below threshold; overfishing occurring",
    df$KobeStatus == "Orange" & df$F_FMSY > 1 ~
      "Biomass above threshold; overfishing occurring",
    df$KobeStatus == "Orange" & df$SSB_MSYBtrigger < 1 ~
      "Biomass below threshold; fishing pressure below threshold",
    TRUE ~ "Status unknown"
  )

  status_pal <- c(
    Green = "#4daf4a", Orange = "#ff8c00",
    Red = "#e41a1c", Unknown = "#d3d3d3"
  )
  df$status_hex <- unname(status_pal[df$KobeStatus])

  # 4. Catch/landings fallback (0 remains a valid reported quantity).
  catch_value <- df$Catches_ecoregion
  landing_value <- df$Landings_ecoregion
  catch_value[!is.finite(catch_value) | catch_value < 0] <- NA_real_
  landing_value[!is.finite(landing_value) | landing_value < 0] <- NA_real_
  df$CatchSizeValue <- dplyr::coalesce(catch_value, landing_value)
  df$CatchSizeSource <- dplyr::case_when(
    !is.na(catch_value) ~ "Catches in ecoregion",
    !is.na(landing_value) ~ "Landings in ecoregion",
    TRUE ~ "No catch/landing data"
  )

  # 5. ONE size scale for both panels (including unknown-status stocks).
  size_values <- df$CatchSizeValue[is.finite(df$CatchSizeValue)]
  c_min <- if (length(size_values)) min(size_values) else NA_real_
  c_max <- if (length(size_values)) max(size_values) else NA_real_

  scale_catches <- function(values) {
    out <- rep(min_point_size, length(values))
    good <- is.finite(values) & values >= 0
    if (!any(good) || !is.finite(c_min)) return(out)
    if (c_min == c_max) {
      out[good] <- (min_point_size + max_point_size) / 2
    } else {
      out[good] <- min_point_size +
        (sqrt(values[good]) - sqrt(c_min)) /
        (sqrt(c_max) - sqrt(c_min)) *
        (max_point_size - min_point_size)
    }
    pmax(min_point_size, pmin(max_point_size, out))
  }

  df$point_size <- if (isTRUE(size_by_catches)) {
    scale_catches(df$CatchSizeValue)
  } else {
    rep(12, nrow(df))
  }
  if (isTRUE(return_data)) return(df)

  # 6. Derive linear main-axis limits from stocks with both ratios.
  plot_df <- df[known, , drop = FALSE]
  unknown_df <- df[!known, , drop = FALSE]
  n_unknown_total <- nrow(unknown_df)
  has_unknown <- n_unknown_total > 0L
  # All unknown stocks remain in df; only the right-panel display is capped.
  n_unknown_shown <- as.integer(min(n_unknown_total, max_unknown_stocks))

  if (nrow(plot_df) > 0L) {
    xmin <- min(c(plot_df$F_FMSY, 1))
    xmax <- max(c(plot_df$F_FMSY, 1))
    ymin <- min(c(plot_df$SSB_MSYBtrigger, 1))
    ymax <- max(c(plot_df$SSB_MSYBtrigger, 1))
    xpad <- max(0.1, (xmax - xmin) * 0.10)
    ypad <- max(0.1, (ymax - ymin) * 0.10)
    x_range <- c(max(0, xmin - xpad), xmax + xpad)
    y_range <- c(max(0, ymin - ypad), ymax + ypad)
  } else {
    x_range <- c(0, 2)
    y_range <- c(0, 2)
  }

  # 7. Hover text, with original values retained.
  html_safe <- function(z) {
    z <- ifelse(is.na(z), "NA", as.character(z))
    z <- gsub("&", "&amp;", z, fixed = TRUE)
    z <- gsub("<", "&lt;", z, fixed = TRUE)
    gsub(">", "&gt;", z, fixed = TRUE)
  }
  format_quantity <- function(v) {
    ifelse(
      is.finite(v),
      format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE),
      "NA"
    )
  }
  format_ratio <- function(v) {
    ifelse(is.finite(v) & v >= 0, sprintf("%.2f", v), "NA")
  }

  df$hover_text <- paste0(
    "<b>", html_safe(df$StockKeyLabel), "</b><br>",
    "F/Fref: ", format_ratio(df$F_FMSY), "<br>",
    "B/Bref: ", format_ratio(df$SSB_MSYBtrigger), "<br>",
    "Kobe status: ", df$KobeStatus, "<br>",
    ifelse(df$KobeStatus == "Unknown", df$UnknownReason, df$KobeDescription),
    "<br>", df$CatchSizeSource, ": ", format_quantity(df$CatchSizeValue),
    " t"
  )
  plot_df <- df[known, , drop = FALSE]
  unknown_df <- df[!known, , drop = FALSE]

  p <- plotly::plot_ly()
  stock_annotations <- list()

  # 8. Main Kobe markers (same symbols, colours, and labels as v2).
  add_kobe_points <- function(plot, data, symbol, line_width, opacity) {
    if (nrow(data) == 0L) return(plot)

    for (status_name in unique(data$KobeStatus)) {
      sub <- data[data$KobeStatus == status_name, , drop = FALSE]
      col <- status_pal[[status_name]]
      marker_line <- if (identical(symbol, "circle-open")) {
        list(color = col, width = line_width)
      } else {
        list(color = "#333333", width = 0.1)
      }

      plot <- plotly::add_markers(
        plot,
        data = sub,
        x = ~F_FMSY, y = ~SSB_MSYBtrigger,
        marker = list(
          symbol = symbol, size = sub$point_size,
          color = col, line = marker_line
        ),
        customdata = ~hover_text,
        hovertemplate = "%{customdata}<extra></extra>",
        showlegend = FALSE, opacity = opacity, inherit = FALSE
      )

      for (k in seq_len(nrow(sub))) {
        # In the split view, place labels for right-edge stocks to their
        # left, reducing spillover into the unknown-status panel.
        at_right_edge <- has_unknown &&
          sub$F_FMSY[[k]] > (x_range[[2]] - 0.20 * diff(x_range))
        label_shift <- sub$point_size[[k]] / 2 + 8

        stock_annotations[[length(stock_annotations) + 1L]] <<- list(
          x = sub$F_FMSY[[k]], y = sub$SSB_MSYBtrigger[[k]],
          xref = "x", yref = "y",
          text = html_safe(sub$StockKeyLabel[[k]]),
          xanchor = if (at_right_edge) "right" else "left",
          yanchor = "middle",
          xshift = if (at_right_edge) -label_shift else label_shift,
          showarrow = FALSE,
          font = list(size = 10, color = "#333333")
        )
      }
    }
    plot
  }

  p <- add_kobe_points(
    p, plot_df[plot_df$ProxyFlag == "Reference point", , drop = FALSE],
    "circle", 0.1, 0.75
  )
  p <- add_kobe_points(
    p, plot_df[plot_df$ProxyFlag == "Proxy reference point", , drop = FALSE],
    "circle-open", 2, 1
  )

  # 9. Unknown panel: show up to max_unknown_stocks, ordered by catches.
  # Keep all unknown stocks for return_data and shared size scaling.
  # Separate x3/y3 axes are independent of the F/B axes.
  if (has_unknown) {
    unknown_df <- dplyr::arrange(
      unknown_df, dplyr::desc(CatchSizeValue), StockKeyLabel
    )
    unknown_df <- unknown_df[seq_len(n_unknown_shown), , drop = FALSE]
    unknown_df$PanelX <- rep(0.13, nrow(unknown_df))
    unknown_df$PanelY <- rev(seq_len(nrow(unknown_df)))
    unknown_df$PanelLabel <- substr(as.character(unknown_df$StockKeyLabel), 1, 24)

    for (proxy_type in c("Reference point", "Proxy reference point")) {
      sub <- unknown_df[
        unknown_df$ProxyFlag == proxy_type, , drop = FALSE
      ]
      if (nrow(sub) == 0L) next
      is_proxy <- identical(proxy_type, "Proxy reference point")

      p <- plotly::add_trace(
        p,
        data = sub,
        type = "scatter", mode = "markers+text",
        x = ~PanelX, y = ~PanelY,
        xaxis = "x3", yaxis = "y3",
        text = ~PanelLabel, textposition = "middle right",
        textfont = list(size = 10, color = "#333333"),
        marker = list(
          symbol = if (is_proxy) "circle-open" else "circle",
          size = sub$point_size,
          color = if (is_proxy) "#858585" else status_pal[["Unknown"]],
          line = list(
            color = if (is_proxy) "#858585" else "#333333",
            width = if (is_proxy) 2 else 0.1
          )
        ),
        customdata = ~hover_text,
        hovertemplate = "%{customdata}<extra></extra>",
        showlegend = FALSE, inherit = FALSE,
        cliponaxis = FALSE
      )
    }
  }

  # 10. Legend for standard / proxy markers (unchanged).
  legend_defs <- list(
    "Reference point" = list(symbol = "circle", width = 0.1),
    "Proxy reference point" = list(symbol = "circle-open", width = 1.8)
  )
  for (nm in names(legend_defs)) {
    def <- legend_defs[[nm]]
    p <- plotly::add_markers(
      p, x = -1, y = -1,
      xaxis = "x2", yaxis = "y2", name = nm,
      marker = list(
        symbol = def$symbol, size = 12, color = "#000000",
        line = list(color = "#000000", width = def$width)
      ),
      legendgroup = "reference_type", showlegend = TRUE,
      hoverinfo = "skip", inherit = FALSE
    )
  }

  # 11. ONE size legend covering stocks in BOTH panels.
  if (isTRUE(size_by_catches) && length(size_values) > 0L) {
    # Number of representative sizes displayed in the legend
    if (!is.numeric(n_size_legend) ||
        length(n_size_legend) != 1L ||
        !is.finite(n_size_legend) ||
        n_size_legend < 1 ||
        n_size_legend != as.integer(n_size_legend)) {
      stop("n_size_legend must be a positive integer.")
    }

    legend_probs <- if (n_size_legend == 1) {
      0.5
    } else {
      seq(0, 1, length.out = n_size_legend)
    }

    legend_values <- unique(as.numeric(stats::quantile(
      size_values,
      probs = legend_probs,
      na.rm = TRUE,
      names = FALSE
    )))
    legend_sizes <- scale_catches(legend_values)

    for (i in seq_along(legend_values)) {
      legend_label <- paste0(format_quantity(legend_values[[i]]), " tonnes")
      p <- plotly::add_trace(
        p,
        type = "scatter", mode = "markers", x = 0, y = 0,
        name = legend_label,
        marker = list(
          symbol = "circle", size = legend_sizes[[i]],
          color = "rgba(140,140,140,0.45)",
          line = list(color = "#666666", width = 1)
        ),
        legendgroup = "catch_size",
        legendgrouptitle = list(text = "Catches / landings in ecoregion"),
        visible = "legendonly", showlegend = TRUE,
        hoverinfo = "skip", inherit = FALSE
      )
    }
  }

  # 12. Layout: reserve a side domain only if unknown stocks exist.
  # No unknown stocks -> the regular Kobe plot occupies the full width.
  main_domain <- if (has_unknown) c(0, 0.72) else c(0, 1)
  legend_outside <- isTRUE(size_by_catches) || has_unknown
  layout_shapes <- list(
    list(
      type = "line", x0 = 1, x1 = 1,
      y0 = y_range[[1]], y1 = y_range[[2]],
      line = list(color = "#4D4D4D", dash = "dash")
    ),
    list(
      type = "line", x0 = x_range[[1]], x1 = x_range[[2]],
      y0 = 1, y1 = 1,
      line = list(color = "#4D4D4D", dash = "dash")
    )
  )
  layout_annotations <- stock_annotations

  if (has_unknown) {
    layout_shapes <- c(
      list(
        list(
          type = "rect", xref = "paper", yref = "paper",
          x0 = 0.78, x1 = 1, y0 = 0, y1 = 1,
          fillcolor = "rgba(245,245,245,0.55)",
          line = list(width = 0), layer = "below"
        ),
        list(
          type = "line", xref = "paper", yref = "paper",
          x0 = 0.765, x1 = 0.765, y0 = 0, y1 = 1,
          line = list(color = "#CCCCCC", width = 1)
        )
      ),
      layout_shapes
    )
    layout_annotations[[length(layout_annotations) + 1L]] <- list(
      x = 0.89, y = 1.035, xref = "paper", yref = "paper",
      text = paste0("<b>Unknown status (", n_unknown_total, ")</b>"),
      xanchor = "center", yanchor = "bottom", showarrow = FALSE,
      font = list(size = 12, color = "#333333")
    )
    if (n_unknown_shown < n_unknown_total) {
      layout_annotations[[length(layout_annotations) + 1L]] <- list(
        x = 0.89, y = 1.005, xref = "paper", yref = "paper",
        text = paste0("Showing ", n_unknown_shown, " of ", n_unknown_total),
        xanchor = "center", yanchor = "bottom", showarrow = FALSE,
        font = list(size = 10, color = "#666666")
      )
    }
  }

  layout_annotations[[length(layout_annotations) + 1L]] <- list(
    x = 1, y = -0.14, xref = "paper", yref = "paper",
    xanchor = "right", yanchor = "top", showarrow = FALSE,
    text = paste0(
      "ICES Stock Assessment Database, ",
      format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"
    ),
    font = list(size = 10)
  )

  p <- plotly::layout(
    p,
    height = NULL,
    autosize = TRUE,
    paper_bgcolor = "white", plot_bgcolor = "white",
    xaxis = list(
      title = "F/Fref", domain = main_domain,
      range = x_range, zeroline = FALSE
    ),
    yaxis = list(
      title = "B/Bref", domain = c(0, 1),
      range = y_range, zeroline = FALSE
    ),
    xaxis2 = list(
      overlaying = "x", visible = FALSE, range = c(0, 1),
      fixedrange = TRUE, autorange = FALSE
    ),
    yaxis2 = list(
      overlaying = "y", visible = FALSE, range = c(0, 1),
      fixedrange = TRUE, autorange = FALSE
    ),
    xaxis3 = if (has_unknown) list(
      domain = c(0.79, 1), anchor = "y3", range = c(0, 1),
      visible = FALSE, fixedrange = TRUE
    ) else NULL,
    yaxis3 = if (has_unknown) list(
      domain = c(0, 1), anchor = "x3",
      range = c(0.5, n_unknown_shown + 0.5),
      visible = FALSE, fixedrange = TRUE
    ) else NULL,
    shapes = layout_shapes,
    legend = list(
      title = list(text = "ICES reference point"),
      x = if (legend_outside) 1.02 else 0.98,
      y = 0.98,
      xanchor = if (legend_outside) "left" else "right",
      yanchor = "top",
      bgcolor = "rgba(255,255,255,0.90)",
      bordercolor = "#D9D9D9", borderwidth = 1,
      tracegroupgap = 18
    ),
    hovermode = "closest",
    margin = list(
      l = 90, r = if (legend_outside) 280 else 30,
      t = if (has_unknown) 55 else 30, b = 70
    ),
    annotations = layout_annotations
  )

  plotly::config(
    p,
    responsive = TRUE,
    toImageButtonOptions = list(
      filename = paste0("Kobe_v3_", format(Sys.Date(), "%d-%b-%y")),
      format = "png", scale = 3
    )
  )
}