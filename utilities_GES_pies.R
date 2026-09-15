getSAG_SettingsEcoregion <- function(Ecoregion) {
        
        EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        sag_settings <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://sag.ices.dk/SAG_API/LatestStocks/Settings?ecoregion=%s", EcoregionCode)
                )
        )
        return(sag_settings)
}



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
      StockKeyLabel, FisheriesGuild, F_FMSY,
      Catches, Landings, Discards, FMSY, FishingPressure,
      F_proxy = FMSY_is_proxy, F_proxy_name = FMSY_proxy_name
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
      StockKeyLabel, Year, FisheriesGuild,
      SSB_MSYBtrigger, StockSize, MSYBtrigger,
      B_proxy = MSYB_is_proxy, B_proxy_name = MSYB_proxy_name
    )

  # ---------- Join F and B sides ----------
  df4 <- dplyr::full_join(df_F, df_B, by = c("StockKeyLabel","FisheriesGuild"))

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

icesFO::plot_GES_pies

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
    dplyr::mutate(CATCH = ifelse(is.na(Catches_in_ecoregion) & !is.na(Landings_in_ecoregion), Landings_in_ecoregion, Catches_in_ecoregion)) |>
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
