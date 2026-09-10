getSAG_SettingsEcoregion <- function(Ecoregion) {
        
        EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        sag_settings <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://sag.ices.dk/SAG_API/LatestStocks/Settings?ecoregion=%s", EcoregionCode)
                )
        )
        return(sag_settings)
}

SAG_Settings <- getSAG_SettingsEcoregion("Bay of Biscay and the Iberian Coast")

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



catch_current <- stockstatus_CLD_current_proxy(add_proxyRefPoints(format_sag(sag, sid),  sag_settings = SAG_Settings))



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
    dplyr::mutate(CATCH = ifelse(is.na(Catches) & !is.na(Landings), Landings, Catches)) |>
    dplyr::select(StockKeyLabel, CATCH)

  df4 <- dplyr::left_join(df_stock, df3, by = "StockKeyLabel")
  df4[is.na(df4)] <- 0

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


#' Total-stock Plotly pies with hover showing the top 5 stocks by catch
#'
#' This mirrors the existing plot_GES_pies_plotly layout but aggregates all
#' fisheries guilds together and reports only the largest stock contributions
#' in the hover text.
plot_GES_pies_plotly2 <- function(x, y, return_data = FALSE) {

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

  if ("Landings_in_ecoregion" %in% names(y)) {
    df3 <- dplyr::filter(y, StockKeyLabel %in% df_stock$StockKeyLabel) |>
      dplyr::mutate(
        CATCH = dplyr::coalesce(
          Landings_in_ecoregion,
          ifelse(is.na(Catches) & !is.na(Landings), Landings, Catches)
        )
      ) |>
      dplyr::select(StockKeyLabel, CATCH)
  } else {
    df3 <- dplyr::filter(y, StockKeyLabel %in% df_stock$StockKeyLabel) |>
      dplyr::mutate(CATCH = ifelse(is.na(Catches) & !is.na(Landings), Landings, Catches)) |>
      dplyr::select(StockKeyLabel, CATCH)
  }

  df_long <- dplyr::left_join(df_stock, df3, by = "StockKeyLabel")
  df_long[is.na(df_long)] <- 0

  detail <- df_long |>
    dplyr::group_by(Variable, Colour, StockKeyLabel) |>
    dplyr::summarise(CATCH = sum(CATCH, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(Variable, Colour) |>
    dplyr::arrange(dplyr::desc(CATCH), .by_group = TRUE) |>
    dplyr::slice_head(n = 5) |>
    dplyr::summarise(
      breakdown_html = paste0(
        "<b>Top 5 stocks</b><br>",
        paste0(
          StockKeyLabel, ": ", scales::comma(round(CATCH)), " t",
          collapse = "<br>"
        )
      ),
      .groups = "drop"
    )

  df4 <- df_long |>
    dplyr::group_by(Variable, Colour) |>
    dplyr::summarise(Catch = sum(CATCH, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(detail, by = c("Variable", "Colour")) |>
    dplyr::mutate(
      Variable = plyr::revalue(Variable,
                               c("FishingPressure" = "Fishing Pressure",
                                 "StockSize"       = "Stock Size")),
      Value2 = as.integer(Catch / 1000)
    ) |>
    dplyr::filter(Catch > 0)

  if (isTRUE(return_data)) {
    return(df4)
  }

  variables <- unique(df4$Variable)
  p <- plotly::plot_ly()
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
  pie_domains <- list(
    list(x = c(0.08, 0.92), y = c(0.55, 0.98)),
    list(x = c(0.08, 0.92), y = c(0.05, 0.48))
  )

  for (i in seq_along(variables)) {
    sub <- dplyr::filter(df4, Variable == variables[i])
    domain <- pie_domains[[i]]

    p <- plotly::add_trace(
      p,
      data = sub,
      type = "pie",
      labels = ~Colour,
      values = ~Value2,
      domain = domain,
      marker = list(colors = colList[as.character(sub$Colour)],
                    line = list(color = "#FFFFFF", width = 1)),
      textinfo = "label+percent",
      hoverinfo = "text",
      text = ~paste0(
        "<b>", Colour, "</b><br>",
        "Catch: ", Value2, "k t<br><br>",
        breakdown_html
      ),
      sort = FALSE,
      hole = 0.25,
      showlegend = FALSE
    )

    annotations[[length(annotations) + 1]] <- list(
      x = 0.5, y = domain$y[[2]] + 0.02,
      xref = "paper", yref = "paper",
      text = paste0("<b>", variables[i], "</b>"),
      showarrow = FALSE, font = list(size = 13), xanchor = "center"
    )
  }

  annotations[[length(annotations) + 1]] <- list(
    x = 1, y = 0, xref = "paper", yref = "paper",
    xanchor = "right", yanchor = "top", showarrow = FALSE,
    text = paste0("ICES Stock Assessment Database, ",
                  format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"),
    font = list(size = 10)
  )

  plotly::layout(
    p,
    legend = list(title = list(text = "<b>Status:</b>")),
    margin = list(l = 40, r = 40, t = 50, b = 70),
    annotations = annotations,
    showlegend = TRUE
  ) |>
    plotly::config(displayModeBar = FALSE)
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

clean_status <- format_sag_status_new(getStatusWebService("Bay of Biscay and the Iberian Coast", sid), sag)


plot_GES_pies(clean_status, catch_current_adj)
p4 <- plot_GES_pies(clean_status, catch_current_adj)

test <- catch_current_adj %>% filter(StockKeyLabel == "mac.27.nea")
p <- plot_GES_pies_plotly(clean_status, catch_current_adj)
file_name <- "GES_pies_catches_adjustedRDBES_BI"
htmlwidgets::saveWidget(
        widget = p,
        file = file.path("./output", paste0(file_name, ".html")),
        selfcontained = TRUE
      )
head(clean_status)
head(catch_current)

p <- plot_GES_pies(clean_status, catch_current_adj)
p2 <- plot_GES_pies_plotly2(clean_status, catch_current_adj)


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
    #   total = ifelse(all(is.na(Catches) & is.na(Landings)), NA,    
                    #  max(Catches, Landings, na.rm = TRUE))
                    total = max(Landings_in_ecoregion, na.rm = TRUE)
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
      ggplot2::aes(xend = StockKeyLabel, y = 0, yend = Catches/1000, colour = Status),
      size = 2, na.rm = TRUE, show.legend = FALSE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(y = Landings/1000, xend = StockKeyLabel, yend = 0, colour = Status),
      size = 2, na.rm = TRUE, show.legend = FALSE
    )

  # --- Points (NORMAL refpoints: filled; no legend)
  p <- p +
    ggplot2::geom_point(
      data = dplyr::filter(df, !ProxyFlag),
      ggplot2::aes(y = Catches/1000, fill = Status),
      shape = 24, colour = "grey35", size = 7, alpha = 0.85,
      na.rm = TRUE, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(df, !ProxyFlag),
      ggplot2::aes(y = Landings/1000, fill = Status),
      shape = 21, colour = "grey35", size = 7, alpha = 0.85,
      na.rm = TRUE, show.legend = FALSE
    )

  # --- Points (PROXY refpoints: hollow with Status-colored outline; no legend)
  p <- p +
    ggplot2::geom_point(
      data = dplyr::filter(df, ProxyFlag),
      ggplot2::aes(y = Catches/1000, colour = Status),
      shape = 24, fill = NA, size = 7, alpha = 1, stroke = proxy_stroke,
      na.rm = TRUE, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(df, ProxyFlag),
      ggplot2::aes(y = Landings/1000, colour = Status),
      shape = 21, fill = NA, size = 7, alpha = 1, stroke = proxy_stroke,
      na.rm = TRUE, show.legend = FALSE
    )

  # --- Scales (suppress Status legends)
  p <- p +
    ggplot2::scale_fill_manual(values = status_pal, guide = "none") +
    ggplot2::scale_colour_manual(values = status_pal, guide = "none")

  # --- Axes, theme
  p <- p +
    ggplot2::coord_equal() +
    ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::labs(x = "Stock code", y = "Catch and Landings (thousand tonnes)") +
    ggplot2::theme(
      plot.caption       = ggplot2::element_text(size = 14),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(size = 0.1, colour = "grey80")
    ) +
    cap_lab

  # --- Legend (bottom-right): build only entries present in data
  has_land_norm   <- any(!is.na(df$Landings) & !df$ProxyFlag, na.rm = TRUE)
  has_land_proxy  <- any(!is.na(df$Landings) &  df$ProxyFlag,  na.rm = TRUE)
  has_catch_norm  <- any(!is.na(df$Catches)  & !df$ProxyFlag,  na.rm = TRUE)
  has_catch_proxy <- any(!is.na(df$Catches)  &  df$ProxyFlag,   na.rm = TRUE)

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
p3 <- plot_CLD_bar_app(catch_current_adj, "Pelagic")
head(catch_current_adj)
