get_ecoregion_acronym <- function(ecoregion) {
  switch(ecoregion,
         "Baltic Sea" = "BtS",
         "Bay of Biscay and the Iberian Coast" = "BI",
         "Celtic Seas" = "CS",
         "Greater North Sea" = "NrS",
         "Norwegian Sea" = "NwS",
         "Icelandic Waters" = "IS",
         "Barents Sea" = "BrS",
         "Greenland Sea" = "GS",
         "Faroes" = "FO",
         "Oceanic Northeast Atlantic" = "ONA",
         "Azores" = "AZ",
         stop("Unknown ecoregion")
  )
}


#' Fetch latest SAG data for an ecoregion
#'
#' Queries the ICES SAG (Stock Assessment Graphs) API for the latest
#' stock data corresponding to a given ecoregion.
#'
#' @param Ecoregion Character scalar giving the full ecoregion name
#'   (e.g. \code{"Greater North Sea"}, \code{"Baltic Sea"}). This is
#'   converted to the corresponding ICES ecoregion code via
#'   [get_ecoregion_acronym()].
#'
#' @return
#' A list or data frame (as returned by \code{jsonlite::fromJSON()})
#' containing the latest SAG data for the requested ecoregion.
#' The exact structure is determined by the SAG API response.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Converts \code{Ecoregion} to its ICES acronym using
#'     [get_ecoregion_acronym()].
#'   \item Calls the SAG API endpoint
#'     \code{https://sag.ices.dk/SAG_API/LatestStocks/Download}
#'     with the \code{ecoregion} query parameter set to that acronym.
#'   \item Parses the JSON response via \code{jsonlite::fromJSON()} and
#'     returns the parsed object directly.
#' }
#'
#' This helper is typically used upstream by the application to obtain
#' the latest assessment data for all stocks in a given ecoregion.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @noRd
getSAG_ecoregion_new <- function(Ecoregion) {
       
        EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        sag <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://sag.ices.dk/SAG_API/LatestStocks/Download?ecoregion=%s", EcoregionCode)
                )
        )
        return(sag)
}

getSID <- function(year, EcoR) {
        
        stock_list_long <- jsonlite::fromJSON(
                URLencode(
                        sprintf("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq %s&$select=StockKeyLabel,
                        EcoRegion,
                        YearOfLastAssessment,
                        AssessmentKey,
                        StockKeyDescription,
                        SpeciesScientificName,
                        SpeciesCommonName,
                        AdviceCategory,
                        DataCategory,
                        YearOfLastAssessment,
                        FisheriesGuild", year)
                )
        )$value

        stock_list_long <- stock_list_long %>%
                mutate(EcoRegion = as.character(EcoRegion)) %>%
                tidyr::separate_rows(EcoRegion, sep = ", ")

        stock_list_long <- stock_list_long %>%
                filter(EcoRegion == EcoR)

        ############ Hard coded for some stocks with assessmentComponents
        stock_list_long <- add_keys(stock_list_long, "cod.27.46a7d20", c(19661,19662))
        stock_list_long <- add_keys(stock_list_long, "cod.21.1.isc", c(19605))
        
        stock_list_long <- stock_list_long[!is.na(stock_list_long$AssessmentKey), ]
        
        stock_list_long$FisheriesGuild[stock_list_long$FisheriesGuild == "crustacean"] <- "shellfish"
        return(stock_list_long)
} 


add_keys <- function(df, stock_label, keys, key_col = "AssessmentKey") {
          template <- df %>%
            dplyr::filter(StockKeyLabel == stock_label) %>%
            dplyr::slice(1)
          additions <- template[rep(1, length(keys)), ]
          additions[[key_col]] <- keys
          dplyr::bind_rows(df, additions)
        }



CLD_trends <- function(x){
        df<- dplyr::select(x,Year,
                       StockKeyLabel,
                       FisheriesGuild,
                       FishingPressure,
                       FMSY,
                       StockSize,
                       MSYBtrigger,
                       Catches,
                       Landings,
                       Discards)     
        df["Discards"][df["Discards"] == 0] <- NA
        df["Catches"][df["Catches"] == 0] <- NA
        df["Landings"][df["Landings"] == 0] <- NA
      
        return(df)
}

format_sag <- function(sag, sid){
        # sid <- load_sid(year)
        sid <- dplyr::filter(sid,!is.na(YearOfLastAssessment))
        # sid <- dplyr::select(sid,StockKeyLabel,FisheriesGuild)
        sid <- dplyr::select(sid,AssessmentKey, FisheriesGuild)
        
        df1 <- merge(sag, sid, all.x = T, all.y = F)
        
        df1 <-as.data.frame(df1)
        
        # df1 <- df1[, colSums(is.na(df1)) < nrow(df1)]
        
        df1$FisheriesGuild <- tolower(df1$FisheriesGuild)
        
        # replace the fisheries guild == crustacean with shellfish
        df1$FisheriesGuild[df1$FisheriesGuild == "crustacean"] <- "shellfish"
        
        check <-unique(df1[c("StockKeyLabel", "Purpose")])
        check <- check[duplicated(check$StockKeyLabel),]
        
        out <- dplyr::anti_join(df1, check)

        out$StockKeyLabel <- ifelse(is.na(out$AssessmentComponent) | out$AssessmentComponent == "", out$StockKeyLabel, paste0(out$StockKeyLabel, "_", out$AssessmentComponent))
        out$StockKeyLabel <- gsub("\\s*Substock\\b", "", out$StockKeyLabel, ignore.case = TRUE)
        
        return(out)
}



### guildRate = guildDiscards / (guildLandings + guildDiscards)
plot_discard_trends_app_plotly <- function(x, year, return_data = FALSE, ecoregion = NULL) {
  
  # Check for non-numeric Year values and warn if any NAs are introduced
  if (all(is.na(x$Discards))) {
    return(
      plotly::plot_ly() %>%
        plotly::layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(list(
            text = "No discards available",
            xref = "paper", yref = "paper", x = 0.5, y = 0.5,
            showarrow = FALSE, font = list(size = 20)
          ))
        )
    )
  }


  # --- Responsive font sizes (fallback to 800px)
  w <- tryCatch({
    if (!is.null(session)) session$clientData[["output_landings_1-discard_trends_width"]] else NA_real_
  }, error = function(e) NA_real_)
  if (is.na(w) || is.null(w)) w <- 800

  base_size         <- max(9,  min(18, round(w / 55)))
  axis_title_size   <- max(10, min(20, round(w / 50)))
  tick_size         <- max(9,  min(16, round(w / 55)))
  legend_title_size <- max(10, min(18, round(w / 55)))
  legend_text_size  <- max(9,  min(16, round(w / 65)))
  title_annot_size  <- max(12, min(22, round(w / 40)))
  caption_size      <- max(8,  min(14, round(w / 70)))

  
  year_numeric <- suppressWarnings(as.numeric(x$Year))
  if (any(is.na(year_numeric) & !is.na(x$Year))) {
    warning("Non-numeric values detected in 'Year' column. These rows will be removed.")
  }
  df <- x %>%
    dplyr::mutate(Year = year_numeric) %>%
    dplyr::filter(!is.na(Year)) %>%
    dplyr::filter(Year %in% seq(2011, year - 1)) %>% 
    dplyr::filter(!is.na(FisheriesGuild))

  df2 <- tidyr::expand(df, Year, tidyr::nesting(StockKeyLabel, FisheriesGuild))
  df <- dplyr::left_join(df, df2, by = c("Year", "StockKeyLabel", "FisheriesGuild"))

  df3 <- df %>%
    dplyr::select(StockKeyLabel, Year, Discards) %>%
    dplyr::distinct() %>%
    tibble::rowid_to_column() %>%
    tidyr::spread(Year, Discards) %>%
    tidyr::gather(Year, Discards, 4:ncol(.)) %>%
    dplyr::mutate(
      Year = as.numeric(Year),
      Discards = as.numeric(Discards)
    )

  df4 <- df %>%
    dplyr::select(StockKeyLabel, Year, Landings) %>%
    dplyr::distinct() %>%
    tibble::rowid_to_column() %>%
    dplyr::group_by(StockKeyLabel) %>%
    tidyr::spread(Year, Landings) %>%
    tidyr::gather(Year, Landings, 4:ncol(.)) %>%
    dplyr::mutate(
      Year = as.numeric(Year),
      Landings = as.numeric(Landings)
    )
  
  df5 <- df %>%
    dplyr::select(-Discards, -Landings) %>%
    dplyr::left_join(df3, by = c("Year", "StockKeyLabel")) %>%
    dplyr::left_join(df4, by = c("Year", "StockKeyLabel")) %>%
    dplyr::group_by(Year, FisheriesGuild) %>%
    dplyr::summarize(
      guildLandings = sum(Landings, na.rm = TRUE) / 1000,
      guildDiscards = sum(Discards, na.rm = TRUE) / 1000,
      .groups = "drop"
    ) %>%
    dplyr::mutate(guildRate = guildDiscards / (guildLandings + guildDiscards)) %>%
    dplyr::filter(!is.na(guildRate))

  if (return_data) {
    return(df5)
  }

  p <- plotly::plot_ly(
    data = df5,
    x = ~Year,
    y = ~guildRate,
    color = ~FisheriesGuild,
    colors = "Set2",
    type = "scatter",
    mode = "lines",
    line = list(width = 3),
    hoverinfo = "text",
    text = ~ paste(
      "Guild:", FisheriesGuild,
      "<br>Year:", Year,
      "<br>Discard rate:", scales::percent(guildRate, accuracy = 0.01),
      "<br>Landings (1000 t):", scales::number(guildLandings, accuracy = 0.01, big.mark = ","),
      "<br>Discards (1000 t):", scales::number(guildDiscards, accuracy = 0.01, big.mark = ",")
    )
  )

  p <- plotly::layout(
    p,
    yaxis = list(
      title = "Discard rate",
       tickformat = ".2%",
      font = list(size = axis_title_size),
      tickfont = list(size = tick_size)
    ),
    xaxis = list(
      title = "Year",
      dtick = 1,
      font = list(size = axis_title_size),
      tickfont = list(size = tick_size)
    ),
    legend = list(title = list(text = "<b>Fisheries guild:</b>")),
    margin = list(b = 120),
    annotations = list(
      list(
        xref = "paper",
        yref = "paper",
        xanchor = "right",
        yanchor = "bottom",
        x = 1, y = -0.4,
        showarrow = FALSE,
        text = paste0("ICES Stock Assessment Database,", format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"),
        font = list(size = caption_size)
      ),
      list(
        text = paste0("Discard trends ", " (", ecoregion, ")"),
        x = 0.01, y = 0.98,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        xanchor = "left", yanchor = "top",
        font = list(size = title_annot_size, color = "black")
      )
    )
  ) %>%
    plotly::config(
      responsive = TRUE,
      toImageButtonOptions = list(
        filename = paste0(ecoregion, "_DiscardTrends_", format(Sys.Date(), "%d-%b-%y")),
        format   = "png",
        scale    = 3
        # width  = 1600,
        # height = 900
      )
    )

  return(p)
}




plot_discard_trends_app_plotly_v2 <- function(
    x, year, return_data = FALSE, ecoregion = NULL,
    complete_pairs = TRUE,
    width_px = 800, max_hover_stocks = 20, bar_width = 24
) {
  # Expects ecoregion-adjusted quantities in x, e.g. values multiplied by
  # a stock-specific ShareLandingsInEcoregion. Regional landings/discards
  # are required. Original Landings/Catches and regional catches are
  # optional; if present they quantify the extent of excluded data.
  #
  # complete_pairs = TRUE: only stocks with BOTH landings and discards
  # contribute to the guild-year numerator/denominator.
  # complete_pairs = FALSE: independently sum available landings/discards,
  # matching the original function's na.rm = TRUE treatment. A stock is
  # excluded only when BOTH landings and discards are missing.
  #
  # The returned data (return_data = TRUE) has one row per plotted guild-year,
  # including all coverage/exclusion metrics and the prepared hover text.

  required <- c("Year", "StockKeyLabel", "FisheriesGuild",
                "Landings_ecoregion", "Discards_ecoregion")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Missing columns in x: ", paste(missing, collapse = ", "))
  }
  if (!is.logical(complete_pairs) || length(complete_pairs) != 1L ||
      is.na(complete_pairs)) {
    stop("complete_pairs must be TRUE or FALSE.")
  }
  if (!is.logical(return_data) || length(return_data) != 1L ||
      is.na(return_data)) {
    stop("return_data must be TRUE or FALSE.")
  }
  if (!is.numeric(year) || length(year) != 1L || !is.finite(year) ||
      year <= 2011 || year != floor(year)) {
    stop("year must be one integer-valued year greater than 2011.")
  }
  if (!is.numeric(max_hover_stocks) || length(max_hover_stocks) != 1L ||
      !is.finite(max_hover_stocks) || max_hover_stocks < 1) {
    stop("max_hover_stocks must be a positive number.")
  }
  if (!is.numeric(bar_width) || length(bar_width) != 1L ||
      !is.finite(bar_width) || bar_width < 2) {
    stop("bar_width must be at least 2.")
  }
  if (!is.numeric(width_px) || length(width_px) != 1L ||
      !is.finite(width_px) || width_px <= 0) width_px <- 800

  max_hover_stocks <- as.integer(max_hover_stocks)
  bar_width <- as.integer(bar_width)

  to_number <- function(z) suppressWarnings(as.numeric(as.character(z)))
  clean_nonnegative <- function(z) {
    z <- to_number(z)
    z[!is.finite(z) | z < 0] <- NA_real_
    z
  }
  escape_html <- function(z) {
    z <- gsub("&", "&amp;", z, fixed = TRUE)
    z <- gsub("<", "&lt;", z, fixed = TRUE)
    gsub(">", "&gt;", z, fixed = TRUE)
  }
  format_tonnes <- function(z) {
    out <- format(round(z, 1), big.mark = ",",
                  scientific = FALSE, trim = TRUE)
    out[is.na(z)] <- "not available"
    out
  }
  # Do not print '0' for an excluded group in which every value is missing.
  # With no excluded stocks the known excluded total is genuinely zero,
  # provided the corresponding column exists.
  known_total <- function(z) {
    if (length(z) == 0L) return(0)
    if (all(is.na(z))) return(NA_real_)
    sum(z, na.rm = TRUE)
  }
  display_thousand <- function(z) {
    if (is.na(z)) "not available" else
      paste0(format_tonnes(z / 1000), " thousand t")
  }
  display_percent <- function(z) {
    ifelse(is.na(z), "not available", sprintf("%.1f%%", z))
  }

  # Each stock's stacked Unicode bar represents its share of the *included*
  # guild-year quantity. Light blocks = landings; dark blocks = discards.
  # In non-paired mode, a missing component is displayed as 'NA', NOT as
  # an observed zero, even though the available component enters the sums.
  build_breakdown <- function(stock_ids, landings, discards) {
    known_landings <- dplyr::coalesce(landings, 0)
    known_discards <- dplyr::coalesce(discards, 0)
    amounts <- known_landings + known_discards
    positive <- which(amounts > 0)
    if (!length(positive)) return("No positive stock quantities")

    order_idx <- positive[order(amounts[positive], decreasing = TRUE)]
    total <- sum(amounts[positive])
    shown <- head(order_idx, max_hover_stocks)

    lines <- vapply(shown, function(i) {
      fraction <- amounts[i] / total
      units <- as.integer(round(bar_width * fraction))
      if (units == 0L) {
        bar <- "\u258f"
      } else {
        discard_units <- as.integer(round(
          units * known_discards[i] / amounts[i]
        ))
        if (units >= 2L && known_landings[i] > 0 &&
            known_discards[i] > 0) {
          discard_units <- max(1L, min(units - 1L, discard_units))
        }
        landing_units <- units - discard_units
        bar <- paste0(
          strrep("\u2591", landing_units),
          strrep("\u2588", discard_units)
        )
      }
      stock_rate <- if (is.na(landings[i]) || is.na(discards[i])) {
        "NA (incomplete)"
      } else {
        sprintf("%.1f%%", 100 * discards[i] / amounts[i])
      }
      paste0(
        escape_html(stock_ids[i]), ": ", bar,
        " ", sprintf("%.1f%%", 100 * fraction), " of guild",
        "<br>   L: ", format_tonnes(landings[i]), " t",
        " | D: ", format_tonnes(discards[i]), " t",
        " | rate: ", stock_rate
      )
    }, character(1))

    additional <- length(order_idx) - length(shown)
    if (additional > 0L) {
      lines <- c(lines, paste0("... and ", additional, " more stocks"))
    }
    paste(lines, collapse = "<br>")
  }

  # Catch and original quantity columns are optional because earlier
  # versions of this function did not require them. Report each separately
  # to avoid conflating full-stock and regional quantities.
  catch_col <- if ("Catches_ecoregion" %in% names(x)) {
    "Catches_ecoregion"
  } else if ("Catches_ecoregion_est" %in% names(x)) {
    "Catches_ecoregion_est"
  } else {
    NULL
  }
  catch_values <- if (is.null(catch_col)) {
    rep(NA_real_, nrow(x))
  } else {
    clean_nonnegative(x[[catch_col]])
  }
  original_landings <- if ("Landings" %in% names(x)) {
    clean_nonnegative(x[["Landings"]])
  } else {
    rep(NA_real_, nrow(x))
  }
  original_catches <- if ("Catches" %in% names(x)) {
    clean_nonnegative(x[["Catches"]])
  } else {
    rep(NA_real_, nrow(x))
  }

  # One observation per stock-year; no rowid_to_column/spread/gather joins.
  df_all <- x |>
    dplyr::transmute(
      Year = to_number(Year),
      StockKeyLabel = as.character(StockKeyLabel),
      FisheriesGuild = as.character(FisheriesGuild),
      Landings_ecoregion = clean_nonnegative(Landings_ecoregion),
      Discards_ecoregion = clean_nonnegative(Discards_ecoregion),
      Catches_ecoregion = catch_values,
      Landings_original = original_landings,
      Catches_original = original_catches
    ) |>
    dplyr::filter(
      is.finite(Year), Year == floor(Year),
      Year >= 2011, Year < year,
      !is.na(StockKeyLabel), nzchar(StockKeyLabel),
      !is.na(FisheriesGuild), nzchar(FisheriesGuild)
    ) |>
    dplyr::distinct()

  repeated <- df_all |>
    dplyr::count(Year, StockKeyLabel, name = "n") |>
    dplyr::filter(n > 1L)
  if (nrow(repeated)) {
    stop(
      "Multiple distinct records for the same stock and year: ",
      paste(utils::head(repeated$StockKeyLabel, 8L), collapse = ", "),
      ". Resolve these before plotting to avoid double counting."
    )
  }

  df_all <- df_all |>
    dplyr::mutate(
      usable = if (complete_pairs) {
        !is.na(Landings_ecoregion) & !is.na(Discards_ecoregion)
      } else {
        !is.na(Landings_ecoregion) | !is.na(Discards_ecoregion)
      }
    )

  # Denominator/data coverage and excluded known regional quantities.
  # Missing excluded values stay missing: excluded totals are *known* totals,
  # not estimates of the unknown quantities.
  coverage <- df_all |>
    dplyr::group_by(Year, FisheriesGuild) |>
    dplyr::summarise(
      n_available = dplyr::n(),
      n_included = sum(usable),
      n_excluded = sum(!usable),
      n_included_missing_landings = sum(usable & is.na(Landings_ecoregion)),
      n_included_missing_discards = sum(usable & is.na(Discards_ecoregion)),
      n_excluded_missing_landings = sum(!usable & is.na(Landings_ecoregion)),
      n_excluded_missing_catches = sum(!usable & is.na(Catches_ecoregion)),
      n_excluded_missing_original_landings =
        sum(!usable & is.na(Landings_original)),
      n_excluded_missing_original_catches =
        sum(!usable & is.na(Catches_original)),
      excluded_landings_original_t = known_total(Landings_original[!usable]),
      excluded_catches_original_t = known_total(Catches_original[!usable]),
      excluded_landings_ecoregion_t =
        known_total(Landings_ecoregion[!usable]),
      excluded_catches_ecoregion_t =
        known_total(Catches_ecoregion[!usable]),
      # Denominator includes all KNOWN regional landings in this guild/year,
      # whether included in the discard-rate calculation or excluded.
      # Unknown regional landings are not implicitly counted as zero.
      known_landings_ecoregion_t =
        sum(Landings_ecoregion, na.rm = TRUE),
      n_missing_landings_ecoregion = sum(is.na(Landings_ecoregion)),
      excluded_share_known_landings_pct = {
        known <- Landings_ecoregion[!is.na(Landings_ecoregion)]
        if (!length(known) || sum(known) <= 0) {
          NA_real_
        } else {
          100 * sum(Landings_ecoregion[!usable], na.rm = TRUE) / sum(known)
        }
      },
      .groups = "drop"
    )

  # In non-paired mode, sums are calculated independently, as in v1.
  included <- df_all |>
    dplyr::filter(usable)

  df5 <- included |>
    dplyr::group_by(Year, FisheriesGuild) |>
    dplyr::summarise(
      guildLandings = sum(Landings_ecoregion, na.rm = TRUE) / 1000,
      guildDiscards = sum(Discards_ecoregion, na.rm = TRUE) / 1000,
      n_stocks = sum(dplyr::coalesce(Landings_ecoregion, 0) +
                       dplyr::coalesce(Discards_ecoregion, 0) > 0),
      breakdown_html = build_breakdown(
        StockKeyLabel, Landings_ecoregion, Discards_ecoregion
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      guildRate = dplyr::if_else(
        guildLandings + guildDiscards > 0,
        guildDiscards / (guildLandings + guildDiscards),
        NA_real_
      )
    ) |>
    dplyr::filter(!is.na(guildRate)) |>
    dplyr::left_join(
      coverage, by = c("Year", "FisheriesGuild"),
      relationship = "one-to-one"
    ) |>
    dplyr::arrange(FisheriesGuild, Year)

  if (nrow(df5)) {
    method_label <- if (complete_pairs) {
      "Paired stocks only"
    } else {
      "Independent available values (v1 method)"
    }
    df5 <- df5 |>
      dplyr::mutate(
        hover_text = paste0(
          "<b>", escape_html(FisheriesGuild), " — ", Year, "</b>",
          "<br>Estimated ecoregion discard rate: ",
          scales::percent(guildRate, accuracy = 0.01),
          "<br>Landings: ", format_tonnes(guildLandings), " thousand t",
          " | Discards: ", format_tonnes(guildDiscards), " thousand t",
          "<br>Method: ", method_label,
          "<br><br><b>Coverage</b>",
          "<br>Included stocks: ", n_included, " of ", n_available,
          " (", n_excluded, " excluded)",
          "<br>Excluded share of <b>known</b> ecoregion landings: ",
          display_percent(excluded_share_known_landings_pct),
          dplyr::if_else(
            n_missing_landings_ecoregion > 0L,
            paste0(
              "<br>Regional landings unknown for ",
              n_missing_landings_ecoregion, " stock(s)"
            ),
            ""
          ),
          "<br><br><b>Included stock composition</b>",
          "<br>\u2591 landings  \u2588 discards",
          "<br>Bar length = share of included guild catch",
          "<br><br>", breakdown_html
        )
      )
  } else {
    df5$hover_text <- character(0)
  }

  # Absolutely no Plotly calls if the caller only wants the plotted data.
  if (isTRUE(return_data)) return(df5)

  if (!nrow(df5)) {
    return(plotly::plot_ly() |>
      plotly::layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(list(
          text = "No usable regional landings and discards available",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        ))
      ))
  }

  # Stable guild-specific colours.
  guild_colours <- c(
    benthic = "#0072B2",
    demersal = "#E69F00",
    elasmobranch = "#009E73",
    pelagic = "#CC79A7",
    shellfish = "#D55E00"
  )
  guilds <- sort(unique(df5$FisheriesGuild))
  other_guilds <- setdiff(guilds, names(guild_colours))
  if (length(other_guilds)) {
    guild_colours <- c(
      guild_colours,
      stats::setNames(
        grDevices::hcl.colors(length(other_guilds), palette = "Dark 3"),
        other_guilds
      )
    )
  }

  tick_size <- max(9, min(16, round(width_px / 55)))
  axis_title_size <- max(10, min(20, round(width_px / 50)))
  title_size <- max(12, min(22, round(width_px / 40)))
  caption_size <- max(8, min(14, round(width_px / 70)))

  p <- plotly::plot_ly()
  for (guild in guilds) {
    sub <- df5 |>
      dplyr::filter(FisheriesGuild == guild) |>
      tidyr::complete(Year = seq.int(2011, year - 1)) |>
      dplyr::arrange(Year)

    p <- plotly::add_trace(
      p, data = sub,
      x = ~Year, y = ~guildRate,
      type = "scatter", mode = "lines+markers",
      name = guild, showlegend = TRUE,
      line = list(color = unname(guild_colours[guild]), width = 3),
      marker = list(color = unname(guild_colours[guild]), size = 5),
      text = ~hover_text,
      hovertemplate = "%{text}<extra></extra>",
      connectgaps = FALSE,
      inherit = FALSE
    )
  }

  region_title <- if (is.null(ecoregion) || !nzchar(as.character(ecoregion))) {
    "Discard trends"
  } else {
    paste0("Discard trends (", ecoregion, ")")
  }

  p <- plotly::layout(
    p,
    hovermode = "closest",
    hoverlabel = list(
      align = "left", bgcolor = "white",
      font = list(family = "Consolas, monospace", size = 11, color = "#222222")
    ),
    xaxis = list(
      title = list(text = "Year", font = list(size = axis_title_size)),
      dtick = 1, tickfont = list(size = tick_size)
    ),
    yaxis = list(
      title = list(text = "Discard rate", font = list(size = axis_title_size)),
      tickformat = ".1%", rangemode = "tozero",
      tickfont = list(size = tick_size)
    ),
    legend = list(title = list(text = "<b>Fisheries guild:</b>")),
    margin = list(l = 75, r = 30, t = 65, b = 115),
    annotations = list(
      list(
        text = region_title, x = 0.01, y = 1.08,
        xref = "paper", yref = "paper", showarrow = FALSE,
        xanchor = "left", yanchor = "top",
        font = list(size = title_size, color = "black")
      ),
      list(
        text = paste0("ICES Stock Assessment Database, ",
                      format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen",
                      " | Regional quantities estimated using recent spatial shares"),
        x = 1, y = -0.24, xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "top", showarrow = FALSE,
        font = list(size = caption_size)
      )
    )
  )

  plotly::config(
    p, responsive = TRUE,
    toImageButtonOptions = list(
      filename = paste0(
        if (is.null(ecoregion)) "Ecoregion" else ecoregion,
        "_DiscardTrends_", format(Sys.Date(), "%d-%b-%y")
      ),
      format = "png", scale = 3
    )
  )
}




#' Plot Historical Fisheries Guild Discard Rates by Ecoregion
#'
#' @description
#' Produces an interactive Plotly line chart showing annual discard rates
#' for fisheries guilds using estimated ecoregion-specific landings and
#' discards.
#'
#' The function aggregates stock-level quantities by fisheries guild and
#' year, calculates discard rates, and provides an interactive HTML hover
#' panel showing stock contributions and data coverage.
#'
#' Two approaches for handling incomplete landings and discard records
#' are supported through the `complete_pairs` argument.
#'
#' @param x
#' A data frame containing historical stock-level observations.
#' Each stock should have at most one distinct record per year.
#'
#' Required columns:
#' \describe{
#'   \item{Year}{Assessment year.}
#'   \item{StockKeyLabel}{Unique ICES stock identifier.}
#'   \item{FisheriesGuild}{Fisheries guild classification.}
#'   \item{Landings_ecoregion}{
#'     Estimated stock landings within the ecoregion, in tonnes.
#'   }
#'   \item{Discards_ecoregion}{
#'     Estimated stock discards within the ecoregion, in tonnes.
#'   }
#' }
#'
#' Optional columns:
#' \describe{
#'   \item{Landings}{Original full-stock landings, in tonnes.}
#'   \item{Catches}{Original full-stock catches, in tonnes.}
#'   \item{Catches_ecoregion}{
#'     Estimated ecoregion catches, in tonnes.
#'     `Catches_ecoregion_est` is also accepted as a fallback.
#'   }
#' }
#'
#' Optional quantities are used only for supplementary coverage
#' diagnostics and do not enter the discard-rate calculation.
#'
#' @param year
#' Integer-valued year defining the upper limit of the time series.
#' Observations from 2011 through `year - 1` are included.
#'
#' @param return_data
#' Logical. Default is `FALSE`.
#'
#' If `FALSE`, returns the interactive Plotly chart.
#'
#' If `TRUE`, returns the aggregated guild-year data used to
#' construct the plot, without creating a Plotly object.
#'
#' @param ecoregion
#' Character string identifying the ecoregion.
#' Used in the plot title and exported image filename.
#' Default is `NULL`.
#'
#' This argument does not filter the input data. The supplied
#' data must already correspond to the intended ecoregion.
#'
#' @param complete_pairs
#' Logical. Default is `TRUE`.
#'
#' Determines how missing landings and discards are handled:
#'
#' \describe{
#'   \item{TRUE}{
#'     Includes a stock-year observation only when both
#'     Landings_ecoregion and Discards_ecoregion are available.
#'
#'     If either value is missing, the entire stock-year
#'     observation is excluded from the calculation.
#'   }
#'   \item{FALSE}{
#'     Includes a stock-year observation when at least one of
#'     Landings_ecoregion or Discards_ecoregion is available.
#'
#'     Landings and discards are summed independently using
#'     `sum(..., na.rm = TRUE)`, following the missing-value
#'     treatment of the original discard trends function.
#'
#'     Observations are excluded only when both quantities
#'     are missing.
#'   }
#' }
#'
#' In both methods, zero is considered a valid observed value,
#' whereas NA represents missing information.
#'
#' @param width_px
#' Numeric. Reference width in pixels used to adjust plot font sizes.
#' Defaults to 800. Invalid values fall back to 800.
#'
#' @param max_hover_stocks
#' Positive integer specifying the maximum number of individual
#' stocks displayed in the HTML hover composition breakdown.
#' Defaults to 20.
#'
#' Stocks are ordered by their estimated regional landings
#' plus discards, from largest to smallest.
#'
#' @details
#' \strong{1. Estimation of regional quantities}
#'
#' The function expects ecoregion-adjusted stock quantities.
#' These should be calculated before calling the function.
#'
#' For example, regional estimates may be obtained using
#' stock-specific spatial allocation factors:
#'
#' \deqn{L_{i,t}^{eco} = L_{i,t} \times s_i}
#'
#' \deqn{D_{i,t}^{eco} = D_{i,t} \times s_i}
#'
#' where:
#'
#' \itemize{
#'   \item L and D are full-stock landings and discards.
#'   \item s_i is the stock-specific ShareLandingsInEcoregion.
#'   \item i identifies the stock and t identifies the year.
#' }
#'
#' If a single spatial share is applied across historical years,
#' the calculation assumes that the stock's geographical
#' distribution of landings remains constant through time.
#'
#' Applying the same share to discards additionally assumes
#' that landings and discards have the same spatial distribution.
#'
#' These assumptions are not tested by this function.
#'
#' \strong{2. Calculation of discard rates}
#'
#' For each fisheries guild and year, the discard rate is:
#'
#' \deqn{R = D / (L + D)}
#'
#' where:
#'
#' \itemize{
#'   \item D is the sum of included regional discards.
#'   \item L is the sum of included regional landings.
#' }
#'
#' Summation is performed separately for each fisheries guild
#' and year.
#'
#' The resulting guildLandings and guildDiscards are expressed
#' in thousands of tonnes.
#'
#' The discard rate is a proportion between 0 and 1.
#' It is displayed as a percentage in the plot.
#'
#' A guild-year observation is omitted when the combined
#' included landings and discards equal zero, because the
#' discard rate is undefined.
#'
#' \strong{3. Stock inclusion and exclusion}
#'
#' With `complete_pairs = TRUE`:
#'
#' \itemize{
#'   \item Stocks with known landings and discards are included.
#'   \item Stocks with missing landings are excluded.
#'   \item Stocks with missing discards are excluded.
#'   \item Stocks with both quantities missing are excluded.
#'   \item Stocks with landings = 0 and discards = 0 are included.
#' }
#'
#' With `complete_pairs = FALSE`:
#'
#' \itemize{
#'   \item Stocks with both quantities available are included.
#'   \item Stocks with only landings available are included.
#'   \item Stocks with only discards available are included.
#'   \item Stocks with both quantities missing are excluded.
#' }
#'
#' A stock with zero landings and zero discards is considered
#' included but does not contribute a positive quantity.
#' Such stocks are not displayed in the stock-composition bars.
#'
#' Stock inclusion is evaluated independently for each year.
#'
#' Stocks without a valid regional allocation factor will
#' generally have missing regional quantities and will be
#' excluded according to the selected method.
#'
#' \strong{4. Data coverage}
#'
#' The function calculates coverage statistics for each
#' fisheries guild and year.
#'
#' Stock coverage includes:
#'
#' \itemize{
#'   \item Number of available stock-year records.
#'   \item Number of included records.
#'   \item Number of excluded records.
#'   \item Number of records with missing regional landings.
#' }
#'
#' The function also calculates the percentage of known
#' regional landings associated with excluded stocks:
#'
#' \deqn{
#' ExcludedShare =
#' 100 \times L_{excluded,known} / L_{all,known}
#' }
#'
#' This measure is stored as:
#'
#' `excluded_share_known_landings_pct`
#'
#' A low value indicates that excluded stocks account for
#' a relatively small fraction of known regional landings.
#'
#' A high value indicates that excluded stocks account for
#' a substantial fraction of known regional landings.
#'
#' IMPORTANT: This percentage uses only stocks with known
#' regional landings. It is not a measure of complete regional
#' data coverage when some regional quantities are unknown.
#'
#' If no positive known regional landings exist, the
#' percentage is returned as NA.
#'
#' \strong{5. Interactive HTML hover}
#'
#' The Plotly hover panel is rendered using
#' `htmlwidgets::onRender()` and JavaScript.
#'
#' For each guild-year observation, the hover displays:
#'
#' \itemize{
#'   \item Fisheries guild and year.
#'   \item Estimated regional discard rate.
#'   \item Total included regional landings and discards.
#'   \item Missing-data calculation method.
#'   \item Number of included and excluded stocks.
#'   \item Excluded share of known regional landings.
#'   \item Number of stocks with unknown regional landings.
#'   \item Individual stock contributions.
#' }
#'
#' Data coverage is represented by a stacked horizontal bar:
#'
#' \itemize{
#'   \item Green: included known regional landings.
#'   \item Orange: excluded known regional landings.
#' }
#'
#' Stock composition is represented using HTML/CSS
#' stacked bars:
#'
#' \itemize{
#'   \item Blue: estimated regional landings.
#'   \item Orange: estimated regional discards.
#' }
#'
#' Each stock bar is scaled relative to total included
#' guild landings plus discards for that year.
#'
#' Only stocks with positive combined quantities appear
#' in the composition breakdown.
#'
#' \strong{6. Data validation}
#'
#' The function removes records with invalid years or missing
#' stock identifiers and fisheries guild classifications.
#'
#' Negative, non-finite, or non-numeric catch quantities are
#' treated as missing.
#'
#' Exact duplicate records are removed.
#'
#' Multiple distinct observations for the same stock and
#' year trigger an error to prevent double counting.
#'
#' Unlike the original function, v3 does not use
#' `rowid_to_column()`, `spread()`, or `gather()` to
#' reconstruct stock-year quantities.
#'
#' @return
#' If `return_data = FALSE`, a Plotly HTML widget displaying
#' annual discard-rate trends by fisheries guild, with
#' custom HTML hover panels.
#'
#' If `return_data = TRUE`, a tibble containing one row
#' per guild-year observation with a defined discard rate.
#'
#' Principal returned columns include:
#'
#' \describe{
#'   \item{Year}{Year of the observation.}
#'   \item{FisheriesGuild}{Fisheries guild.}
#'   \item{guildLandings}{
#'     Included regional landings, in thousands of tonnes.
#'   }
#'   \item{guildDiscards}{
#'     Included regional discards, in thousands of tonnes.
#'   }
#'   \item{guildRate}{
#'     Calculated discard rate, expressed as a proportion.
#'   }
#'   \item{n_stocks}{
#'     Number of included stocks contributing positive
#'     landings plus discards.
#'   }
#'   \item{n_available}{
#'     Number of available stock-year records.
#'   }
#'   \item{n_included}{
#'     Number of records included in the calculation.
#'   }
#'   \item{n_excluded}{
#'     Number of excluded records.
#'   }
#'   \item{excluded_share_known_landings_pct}{
#'     Percentage of known regional landings associated
#'     with excluded records.
#'   }
#'   \item{known_landings_ecoregion_t}{
#'     Total known regional landings, in tonnes.
#'   }
#'   \item{n_missing_landings_ecoregion}{
#'     Number of records with unknown regional landings.
#'   }
#'   \item{stock_breakdown}{
#'     List-column containing individual stock quantities,
#'     discard rates, and contributions to the guild total.
#'   }
#'   \item{hover_text}{
#'     Text summary of the guild-year observation.
#'   }
#'   \item{hover_payload}{
#'     JSON representation used by the custom HTML tooltip.
#'   }
#' }
#'
#' Additional columns provide excluded quantities from
#' original and regional data, and missing-value counts.
#'
#' @note
#' The regional discard rates are estimates based on
#' externally calculated spatial allocation factors.
#'
#' Multiplying a stock's landings and discards by the
#' same positive factor does not change that stock's
#' individual discard rate. It changes the stock's
#' relative contribution to the aggregated guild rate.
#'
#' Therefore, the function spatially reweights stock-level
#' discard rates rather than directly measuring differences
#' in discarding behaviour between geographical areas.
#'
#' High data coverage improves confidence that the plotted
#' rate reflects the available stock information, but does
#' not validate the underlying spatial allocation factors
#' or resolve potential bias in missing discard observations.
#'
#' @examples
#' \dontrun{
#'
#' # Plot regional discard trends using complete pairs
#' plot_discard_trends_app_plotly_v3(
#'   x = discard_estimated,
#'   year = 2026,
#'   ecoregion = "Bay of Biscay",
#'   complete_pairs = TRUE
#' )
#'
#' # Apply the original independent summation approach
#' plot_discard_trends_app_plotly_v3(
#'   x = discard_estimated,
#'   year = 2026,
#'   ecoregion = "Bay of Biscay",
#'   complete_pairs = FALSE
#' )
#'
#' # Return the data without producing a plot
#' df_rates <- plot_discard_trends_app_plotly_v3(
#'   x = discard_estimated,
#'   year = 2026,
#'   complete_pairs = TRUE,
#'   return_data = TRUE
#' )
#'
#' # Examine data coverage
#' df_rates |>
#'   dplyr::select(
#'     Year,
#'     FisheriesGuild,
#'     guildRate,
#'     n_available,
#'     n_included,
#'     n_excluded,
#'     excluded_share_known_landings_pct
#'   )
#' }
#'
#' @export
plot_discard_trends_app_plotly_v3 <- function(
    x, year, return_data = FALSE, ecoregion = NULL,
    complete_pairs = TRUE,
    width_px = 800, max_hover_stocks = 20
) {
  # Expects ecoregion-adjusted quantities in x, e.g. values multiplied by
  # a stock-specific ShareLandingsInEcoregion. Regional landings/discards
  # are required. Original Landings/Catches and regional catches are
  # optional; if present they quantify the extent of excluded data.
  #
  # complete_pairs = TRUE: only stocks with BOTH landings and discards
  # contribute to the guild-year numerator/denominator.
  # complete_pairs = FALSE: independently sum available landings/discards,
  # matching the original function's na.rm = TRUE treatment. A stock is
  # excluded only when BOTH landings and discards are missing.
  #
  # The returned data (return_data = TRUE) has one row per plotted guild-year,
  # including all coverage/exclusion metrics and the structured stock breakdown.
  # The HTML tooltip is drawn in the browser by htmlwidgets::onRender().

  required <- c("Year", "StockKeyLabel", "FisheriesGuild",
                "Landings_ecoregion", "Discards_ecoregion")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Missing columns in x: ", paste(missing, collapse = ", "))
  }
  if (!is.logical(complete_pairs) || length(complete_pairs) != 1L ||
      is.na(complete_pairs)) {
    stop("complete_pairs must be TRUE or FALSE.")
  }
  if (!is.logical(return_data) || length(return_data) != 1L ||
      is.na(return_data)) {
    stop("return_data must be TRUE or FALSE.")
  }
  if (!is.numeric(year) || length(year) != 1L || !is.finite(year) ||
      year <= 2011 || year != floor(year)) {
    stop("year must be one integer-valued year greater than 2011.")
  }
  if (!is.numeric(max_hover_stocks) || length(max_hover_stocks) != 1L ||
      !is.finite(max_hover_stocks) || max_hover_stocks < 1) {
    stop("max_hover_stocks must be a positive number.")
  }
  if (!is.numeric(width_px) || length(width_px) != 1L ||
      !is.finite(width_px) || width_px <= 0) width_px <- 800

  max_hover_stocks <- as.integer(max_hover_stocks)

  to_number <- function(z) suppressWarnings(as.numeric(as.character(z)))
  clean_nonnegative <- function(z) {
    z <- to_number(z)
    z[!is.finite(z) | z < 0] <- NA_real_
    z
  }
  escape_html <- function(z) {
    z <- gsub("&", "&amp;", z, fixed = TRUE)
    z <- gsub("<", "&lt;", z, fixed = TRUE)
    gsub(">", "&gt;", z, fixed = TRUE)
  }
  format_tonnes <- function(z) {
    out <- format(round(z, 1), big.mark = ",",
                  scientific = FALSE, trim = TRUE)
    out[is.na(z)] <- "not available"
    out
  }
  # Do not print '0' for an excluded group in which every value is missing.
  # With no excluded stocks the known excluded total is genuinely zero,
  # provided the corresponding column exists.
  known_total <- function(z) {
    if (length(z) == 0L) return(0)
    if (all(is.na(z))) return(NA_real_)
    sum(z, na.rm = TRUE)
  }
  display_thousand <- function(z) {
    if (is.na(z)) "not available" else
      paste0(format_tonnes(z / 1000), " thousand t")
  }
  display_percent <- function(z) {
    ifelse(is.na(z), "not available", sprintf("%.1f%%", z))
  }

  # Stock-level quantities for the genuine HTML/CSS stacked bars.
  # Each segment's width is a percentage of included guild (L + D),
  # not a percentage of the individual stock. Missing values are kept
  # missing in labels rather than silently represented as observed zeros.
  build_stock_breakdown <- function(stock_ids, landings, discards) {
    known_L <- dplyr::coalesce(landings, 0)
    known_D <- dplyr::coalesce(discards, 0)
    amount <- known_L + known_D
    positive <- which(amount > 0)
    if (!length(positive)) {
      return(list(stocks = list(), n_more = 0L))
    }
    ord <- positive[order(amount[positive], decreasing = TRUE)]
    shown <- utils::head(ord, max_hover_stocks)
    total <- sum(amount[positive])

    stocks <- lapply(shown, function(i) {
      list(
        stock = as.character(stock_ids[i]),
        landings_t = as.numeric(landings[i]),
        discards_t = as.numeric(discards[i]),
        # All widths have the same guild-year denominator.
        landings_share_pct = 100 * known_L[i] / total,
        discards_share_pct = 100 * known_D[i] / total,
        share_pct = 100 * amount[i] / total,
        rate_pct = if (is.na(landings[i]) || is.na(discards[i])) {
          NA_real_
        } else {
          100 * discards[i] / amount[i]
        }
      )
    })
    list(stocks = stocks, n_more = length(ord) - length(shown))
  }

  # Catch and original quantity columns are optional because earlier
  # versions of this function did not require them. Report each separately
  # to avoid conflating full-stock and regional quantities.
  catch_col <- if ("Catches_ecoregion" %in% names(x)) {
    "Catches_ecoregion"
  } else if ("Catches_ecoregion_est" %in% names(x)) {
    "Catches_ecoregion_est"
  } else {
    NULL
  }
  catch_values <- if (is.null(catch_col)) {
    rep(NA_real_, nrow(x))
  } else {
    clean_nonnegative(x[[catch_col]])
  }
  original_landings <- if ("Landings" %in% names(x)) {
    clean_nonnegative(x[["Landings"]])
  } else {
    rep(NA_real_, nrow(x))
  }
  original_catches <- if ("Catches" %in% names(x)) {
    clean_nonnegative(x[["Catches"]])
  } else {
    rep(NA_real_, nrow(x))
  }

  # One observation per stock-year; no rowid_to_column/spread/gather joins.
  df_all <- x |>
    dplyr::transmute(
      Year = to_number(Year),
      StockKeyLabel = as.character(StockKeyLabel),
      FisheriesGuild = as.character(FisheriesGuild),
      Landings_ecoregion = clean_nonnegative(Landings_ecoregion),
      Discards_ecoregion = clean_nonnegative(Discards_ecoregion),
      Catches_ecoregion = catch_values,
      Landings_original = original_landings,
      Catches_original = original_catches
    ) |>
    dplyr::filter(
      is.finite(Year), Year == floor(Year),
      Year >= 2011, Year < year,
      !is.na(StockKeyLabel), nzchar(StockKeyLabel),
      !is.na(FisheriesGuild), nzchar(FisheriesGuild)
    ) |>
    dplyr::distinct()

  repeated <- df_all |>
    dplyr::count(Year, StockKeyLabel, name = "n") |>
    dplyr::filter(n > 1L)
  if (nrow(repeated)) {
    stop(
      "Multiple distinct records for the same stock and year: ",
      paste(utils::head(repeated$StockKeyLabel, 8L), collapse = ", "),
      ". Resolve these before plotting to avoid double counting."
    )
  }

  df_all <- df_all |>
    dplyr::mutate(
      usable = if (complete_pairs) {
        !is.na(Landings_ecoregion) & !is.na(Discards_ecoregion)
      } else {
        !is.na(Landings_ecoregion) | !is.na(Discards_ecoregion)
      }
    )

  # Denominator/data coverage and excluded known regional quantities.
  # Missing excluded values stay missing: excluded totals are *known* totals,
  # not estimates of the unknown quantities.
  coverage <- df_all |>
    dplyr::group_by(Year, FisheriesGuild) |>
    dplyr::summarise(
      n_available = dplyr::n(),
      n_included = sum(usable),
      n_excluded = sum(!usable),
      n_included_missing_landings = sum(usable & is.na(Landings_ecoregion)),
      n_included_missing_discards = sum(usable & is.na(Discards_ecoregion)),
      n_excluded_missing_landings = sum(!usable & is.na(Landings_ecoregion)),
      n_excluded_missing_catches = sum(!usable & is.na(Catches_ecoregion)),
      n_excluded_missing_original_landings =
        sum(!usable & is.na(Landings_original)),
      n_excluded_missing_original_catches =
        sum(!usable & is.na(Catches_original)),
      excluded_landings_original_t = known_total(Landings_original[!usable]),
      excluded_catches_original_t = known_total(Catches_original[!usable]),
      excluded_landings_ecoregion_t =
        known_total(Landings_ecoregion[!usable]),
      excluded_catches_ecoregion_t =
        known_total(Catches_ecoregion[!usable]),
      # Denominator includes all KNOWN regional landings in this guild/year,
      # whether included in the discard-rate calculation or excluded.
      # Unknown regional landings are not implicitly counted as zero.
      known_landings_ecoregion_t =
        sum(Landings_ecoregion, na.rm = TRUE),
      n_missing_landings_ecoregion = sum(is.na(Landings_ecoregion)),
      excluded_share_known_landings_pct = {
        known <- Landings_ecoregion[!is.na(Landings_ecoregion)]
        if (!length(known) || sum(known) <= 0) {
          NA_real_
        } else {
          100 * sum(Landings_ecoregion[!usable], na.rm = TRUE) / sum(known)
        }
      },
      .groups = "drop"
    )

  # In non-paired mode, sums are calculated independently, as in v1.
  included <- df_all |>
    dplyr::filter(usable)

  df5 <- included |>
    dplyr::group_by(Year, FisheriesGuild) |>
    dplyr::summarise(
      guildLandings = sum(Landings_ecoregion, na.rm = TRUE) / 1000,
      guildDiscards = sum(Discards_ecoregion, na.rm = TRUE) / 1000,
      n_stocks = sum(dplyr::coalesce(Landings_ecoregion, 0) +
                       dplyr::coalesce(Discards_ecoregion, 0) > 0),
      stock_breakdown = list(build_stock_breakdown(
        StockKeyLabel, Landings_ecoregion, Discards_ecoregion
      )),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      guildRate = dplyr::if_else(
        guildLandings + guildDiscards > 0,
        guildDiscards / (guildLandings + guildDiscards),
        NA_real_
      )
    ) |>
    dplyr::filter(!is.na(guildRate)) |>
    dplyr::left_join(
      coverage, by = c("Year", "FisheriesGuild"),
      relationship = "one-to-one"
    ) |>
    dplyr::arrange(FisheriesGuild, Year)

  if (nrow(df5)) {
    method_label <- if (complete_pairs) {
      "Paired stocks only"
    } else {
      "Independent available values (v1 method)"
    }
    df5 <- df5 |>
      dplyr::mutate(
        hover_text = paste0(
          "<b>", escape_html(FisheriesGuild), " — ", Year, "</b>",
          "<br>Estimated ecoregion discard rate: ",
          scales::percent(guildRate, accuracy = 0.01),
          "<br>Landings: ", format_tonnes(guildLandings), " thousand t",
          " | Discards: ", format_tonnes(guildDiscards), " thousand t",
          "<br>Method: ", method_label,
          "<br>Included stocks: ", n_included, " of ", n_available,
          " (", n_excluded, " excluded)",
          "<br>Excluded share of known ecoregion landings: ",
          display_percent(excluded_share_known_landings_pct)
        )
      )

    # Each plotted point carries its data as JSON, not pre-rendered HTML.
    # This is also part of the return_data output for reproducibility.
    df5$hover_payload <- vapply(seq_len(nrow(df5)), function(i) {
      d <- df5[i, , drop = FALSE]
      jsonlite::toJSON(
        list(
          guild = d$FisheriesGuild[[1]],
          year = as.integer(d$Year[[1]]),
          discard_rate_pct = 100 * d$guildRate[[1]],
          landings_kt = d$guildLandings[[1]],
          discards_kt = d$guildDiscards[[1]],
          method = method_label,
          n_available = as.integer(d$n_available[[1]]),
          n_included = as.integer(d$n_included[[1]]),
          n_excluded = as.integer(d$n_excluded[[1]]),
          excluded_share_known_landings_pct =
            d$excluded_share_known_landings_pct[[1]],
          n_missing_landings_ecoregion =
            as.integer(d$n_missing_landings_ecoregion[[1]]),
          stocks = d$stock_breakdown[[1]]$stocks,
          additional_stocks = as.integer(d$stock_breakdown[[1]]$n_more)
        ),
        auto_unbox = TRUE,
        na = "null",
        null = "null",
        digits = 12
      ) |> as.character()
    }, character(1))
  } else {
    df5$hover_text <- character(0)
    df5$hover_payload <- character(0)
  }

  # Absolutely no Plotly calls if the caller only wants the plotted data.
  if (isTRUE(return_data)) return(df5)

  if (!nrow(df5)) {
    return(plotly::plot_ly() |>
      plotly::layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(list(
          text = "No usable regional landings and discards available",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        ))
      ))
  }

  # Stable guild-specific colours.
  guild_colours <- c(
    benthic = "#0072B2",
    demersal = "#E69F00",
    elasmobranch = "#009E73",
    pelagic = "#CC79A7",
    shellfish = "#D55E00"
  )
  guilds <- sort(unique(df5$FisheriesGuild))
  other_guilds <- setdiff(guilds, names(guild_colours))
  if (length(other_guilds)) {
    guild_colours <- c(
      guild_colours,
      stats::setNames(
        grDevices::hcl.colors(length(other_guilds), palette = "Dark 3"),
        other_guilds
      )
    )
  }

  tick_size <- max(9, min(16, round(width_px / 55)))
  axis_title_size <- max(10, min(20, round(width_px / 50)))
  title_size <- max(12, min(22, round(width_px / 40)))
  caption_size <- max(8, min(14, round(width_px / 70)))

  p <- plotly::plot_ly()
  for (guild in guilds) {
    sub <- df5 |>
      dplyr::filter(FisheriesGuild == guild) |>
      tidyr::complete(Year = seq.int(2011, year - 1)) |>
      dplyr::arrange(Year)

    p <- plotly::add_trace(
      p, data = sub,
      x = ~Year, y = ~guildRate,
      type = "scatter", mode = "lines+markers",
      name = guild, showlegend = TRUE,
      line = list(color = unname(guild_colours[guild]), width = 3),
      marker = list(color = unname(guild_colours[guild]), size = 5),
      customdata = ~hover_payload,
      # 'none' hides native Plotly hover labels but preserves hover events.
      hoverinfo = "none",
      connectgaps = FALSE,
      inherit = FALSE
    )
  }

  region_title <- if (is.null(ecoregion) || !nzchar(as.character(ecoregion))) {
    "Discard trends"
  } else {
    paste0("Discard trends (", ecoregion, ")")
  }

  p <- plotly::layout(
    p,
    hovermode = "closest",
    xaxis = list(
      title = list(text = "Year", font = list(size = axis_title_size)),
      dtick = 1, tickfont = list(size = tick_size)
    ),
    yaxis = list(
      title = list(text = "Discard rate", font = list(size = axis_title_size)),
      tickformat = ".1%", rangemode = "tozero",
      tickfont = list(size = tick_size)
    ),
    legend = list(title = list(text = "<b>Fisheries guild:</b>")),
    margin = list(l = 75, r = 30, t = 65, b = 115),
    annotations = list(
      list(
        text = region_title, x = 0.01, y = 1.08,
        xref = "paper", yref = "paper", showarrow = FALSE,
        xanchor = "left", yanchor = "top",
        font = list(size = title_size, color = "black")
      ),
      list(
        text = paste0("ICES Stock Assessment Database, ",
                      format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen<br>",
                      "Ecoregion estimates calculated from recent spatial shares derived from RDBES"),
        x = 1, y = -0.24, xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "top", showarrow = FALSE,
        font = list(size = caption_size)
      )
    )
  )

  p <- plotly::config(
    p, responsive = TRUE,
    toImageButtonOptions = list(
      filename = paste0(
        if (is.null(ecoregion)) "Ecoregion" else ecoregion,
        "_DiscardTrends_", format(Sys.Date(), "%d-%b-%y")
      ),
      format = "png", scale = 3
    )
   )

  # Real HTML tooltip: safely render escaped labels and coloured CSS bars.
  # htmlwidgets::onRender runs in the browser after Plotly is drawn.
  js <- r"---(
function(el, x) {
  // Re-rendering a Shiny output must not leave event handlers or tooltips.
  if (typeof el.__discardV3Cleanup === 'function') {
    el.__discardV3Cleanup();
  }

  const tooltip = document.createElement('div');
  tooltip.className = 'discard-v3-tooltip';
  tooltip.setAttribute('role', 'tooltip');
  tooltip.style.display = 'none';
  tooltip.innerHTML = '<style>' +
    '.discard-v3-tooltip{position:fixed;z-index:2147483000;width:400px;max-width:calc(100vw - 20px);max-height:calc(100vh - 20px);overflow-y:auto;box-sizing:border-box;padding:15px 16px;background:#fff;color:#243746;border:1px solid #d8e2ea;border-radius:12px;box-shadow:0 10px 32px rgba(15,33,45,.22);font:13px/1.4 -apple-system,BlinkMacSystemFont,Segoe UI,Arial,sans-serif;pointer-events:auto;}' +
    '.discard-v3-tooltip *{box-sizing:border-box;}' +
    '.discard-v3-tooltip .dv3-head{display:flex;justify-content:space-between;align-items:flex-start;gap:12px;}' +
    '.discard-v3-tooltip .dv3-guild{font-size:16px;font-weight:700;line-height:1.2;}' +
    '.discard-v3-tooltip .dv3-muted{color:#697b87;font-size:11px;}' +
    '.discard-v3-tooltip .dv3-rate{text-align:right;white-space:nowrap;font-size:21px;font-weight:750;color:#17354c;line-height:1.15;}' +
    '.discard-v3-tooltip .dv3-grid{display:grid;grid-template-columns:1fr 1fr;gap:10px;margin:12px 0;}' +
    '.discard-v3-tooltip .dv3-number{font-size:16px;font-weight:650;color:#203d51;}' +
    '.discard-v3-tooltip .dv3-rule{height:1px;background:#e4eaf0;margin:12px 0;}' +
    '.discard-v3-tooltip .dv3-coverage{background:#f2f6f9;border-radius:8px;padding:11px;}' +
    '.discard-v3-tooltip .dv3-row{display:flex;justify-content:space-between;align-items:baseline;gap:8px;}' +
    '.discard-v3-tooltip .dv3-section{font-size:12px;font-weight:700;margin-bottom:6px;}' +
    '.discard-v3-tooltip .dv3-track{display:flex;width:100%;height:10px;background:#e9eef2;border-radius:4px;overflow:hidden;}' +
    '.discard-v3-tooltip .dv3-track-coverage{margin-top:8px;height:11px;background:#e9eef2;}' +
    '.discard-v3-tooltip .dv3-coverage-included{height:100%;background:#398b71;flex:none;}' +
    '.discard-v3-tooltip .dv3-coverage-excluded{height:100%;background:#ce805a;flex:none;}' +
    '.discard-v3-tooltip .dv3-coverage-labels{display:flex;justify-content:space-between;gap:8px;margin-top:5px;font-size:11px;color:#697b87;}' +
    '.discard-v3-tooltip .dv3-L{height:100%;background:#6895af;}' +
    '.discard-v3-tooltip .dv3-D{height:100%;background:#d77a52;}' +
    '.discard-v3-tooltip .dv3-stock{padding:7px 0;border-bottom:1px solid #eef2f5;}' +
    '.discard-v3-tooltip .dv3-stock:last-child{border-bottom:0;}' +
    '.discard-v3-tooltip .dv3-code{font-size:11px;font-weight:650;overflow-wrap:anywhere;}' +
    '.discard-v3-tooltip .dv3-details{font-size:10.5px;color:#687b86;margin-top:3px;}' +
    '.discard-v3-tooltip .dv3-legend{display:flex;gap:10px;align-items:center;font-size:10.5px;color:#6b7b85;}' +
    '.discard-v3-tooltip .dv3-key{display:inline-block;width:9px;height:9px;border-radius:2px;margin-right:4px;}' +
    '</style>';
  document.body.appendChild(tooltip);

  const safe = function(value) {
    return String(value == null ? '' : value).replace(/[&<>"']/g, function(c) {
      return {'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c];
    });
  };
  const isNumber = function(z) {
    return typeof z === 'number' && Number.isFinite(z);
  };
  const fmt = function(z, digits) {
    return isNumber(z) ? z.toLocaleString('en-GB', {
      minimumFractionDigits: digits,
      maximumFractionDigits: digits
    }) : 'NA';
  };
  const pct = function(z, digits) {
    return isNumber(z) ? fmt(z, digits) + '%' : 'Not available';
  };
  const clamp = function(z) {
    return isNumber(z) ? Math.max(0, Math.min(100, z)) : 0;
  };

  const build = function(d) {
    const method = d.method === 'Paired stocks only' ?
      'Paired stocks only' : 'Independent available values';

    // Percentages refer only to regional landings with known values.
    // A missing percentage is NOT equivalent to 0% excluded / 100% included.
    const excludedKnownPct = isNumber(d.excluded_share_known_landings_pct) ?
      clamp(d.excluded_share_known_landings_pct) : null;
    const includedKnownPct = excludedKnownPct === null ?
      null : 100 - excludedKnownPct;
    const coverageBar = excludedKnownPct === null ?
      '<div class="dv3-track dv3-track-coverage"></div>' +
      '<div class="dv3-coverage-labels"><span>Share unavailable: no positive known regional landings</span></div>' :
      '<div class="dv3-track dv3-track-coverage" role="img" aria-label="' +
      pct(includedKnownPct, 1) + ' of known regional landings included; ' +
      pct(excludedKnownPct, 1) + ' excluded">' +
      '<div class="dv3-coverage-included" style="width:' + includedKnownPct + '%"></div>' +
      '<div class="dv3-coverage-excluded" style="width:' + excludedKnownPct + '%"></div></div>' +
      '<div class="dv3-coverage-labels"><span>' + pct(includedKnownPct, 1) +
      ' included</span><span>' + pct(excludedKnownPct, 1) + ' excluded</span></div>';

    let html = '<div class="dv3-head">' +
      '<div><div class="dv3-guild">' + safe(d.guild) + '</div>' +
      '<div class="dv3-muted">' + safe(d.year) + ' · Estimated regional quantities</div></div>' +
      '<div><div class="dv3-rate">' + pct(d.discard_rate_pct, 2) + '</div>' +
      '<div class="dv3-muted" style="text-align:right">Discard rate</div></div></div>' +
      '<div class="dv3-grid">' +
      '<div><div class="dv3-muted">Landings</div><div class="dv3-number">' + fmt(d.landings_kt, 2) + ' kt</div></div>' +
      '<div><div class="dv3-muted">Discards</div><div class="dv3-number">' + fmt(d.discards_kt, 2) + ' kt</div></div></div>' +
      '<div class="dv3-muted">Method: ' + safe(method) + '</div>' +
      '<div class="dv3-rule"></div>' +
      '<div class="dv3-coverage"><div class="dv3-row"><span class="dv3-section">Data coverage</span>' +
      '<b>' + safe(d.n_included) + ' / ' + safe(d.n_available) + ' stocks</b></div>' +
      '<div class="dv3-row dv3-muted"><span>Excluded share of known regional landings</span>' +
      '<b>' + pct(d.excluded_share_known_landings_pct, 1) + '</b></div>' +
      coverageBar +
      '<div class="dv3-muted" style="margin-top:7px">' + safe(d.n_excluded) + ' excluded stock(s)' +
      (d.n_missing_landings_ecoregion > 0 ? '; regional landings unknown for ' +
        safe(d.n_missing_landings_ecoregion) + ' stock(s)' : '') + '</div></div>' +
      '<div class="dv3-rule"></div>' +
      '<div class="dv3-row" style="align-items:center"><span class="dv3-section">Included stock composition</span>' +
      '<span class="dv3-legend"><span><i class="dv3-key dv3-L"></i>Landings</span>' +
      '<span><i class="dv3-key dv3-D"></i>Discards</span></span></div>' +
      '<div class="dv3-muted" style="margin-bottom:4px">Bar length = share of included guild landings + discards</div>';

    if (!Array.isArray(d.stocks) || d.stocks.length === 0) {
      html += '<div class="dv3-muted">No positive stock quantities</div>';
    } else {
      d.stocks.forEach(function(s) {
        html += '<div class="dv3-stock"><div class="dv3-row">' +
          '<span class="dv3-code">' + safe(s.stock) + '</span>' +
          '<span class="dv3-muted" style="white-space:nowrap">' + pct(s.share_pct, 1) + ' of guild</span></div>' +
          '<div class="dv3-track" style="margin-top:5px">' +
          '<div class="dv3-L" style="width:' + clamp(s.landings_share_pct) + '%"></div>' +
          '<div class="dv3-D" style="width:' + clamp(s.discards_share_pct) + '%"></div></div>' +
          '<div class="dv3-details">L: ' + fmt(s.landings_t, 0) + ' t  ·  D: ' +
          fmt(s.discards_t, 0) + ' t  ·  Stock discard rate: ' +
          (isNumber(s.rate_pct) ? pct(s.rate_pct, 1) : 'NA (incomplete)') + '</div></div>';
      });
    }
    if (d.additional_stocks > 0) {
      html += '<div class="dv3-muted" style="margin-top:7px">... and ' +
        safe(d.additional_stocks) + ' more stocks</div>';
    }
    return html;
  };

  let lastPointer = null;
  let hideTimer = null;
  function cancelHide() {
    if (hideTimer !== null) { clearTimeout(hideTimer); hideTimer = null; }
  }
  function hideSoon() {
    cancelHide();
    hideTimer = setTimeout(function() { tooltip.style.display = 'none'; }, 180);
  }
  function position(xPos, yPos) {
    if (!isNumber(xPos) || !isNumber(yPos)) return;
    const pad = 10, offset = 18;
    const tw = tooltip.offsetWidth, th = tooltip.offsetHeight;
    let left = xPos + offset;
    if (left + tw + pad > window.innerWidth) left = xPos - tw - offset;
    left = Math.max(pad, Math.min(left, window.innerWidth - tw - pad));
    let top = yPos + 12;
    if (top + th + pad > window.innerHeight) top = window.innerHeight - th - pad;
    top = Math.max(pad, top);
    tooltip.style.left = left + 'px';
    tooltip.style.top = top + 'px';
  }
  function onMove(e) {
    lastPointer = {x:e.clientX, y:e.clientY};
    if (tooltip.style.display !== 'none') position(e.clientX, e.clientY);
  }
  function onHover(evt) {
    const pt = evt && evt.points && evt.points[0];
    if (!pt) return;
    let payload = pt.customdata;
    if (payload == null && pt.data && pt.data.customdata) {
      payload = pt.data.customdata[pt.pointNumber];
    }
    if (Array.isArray(payload)) payload = payload[0];
    if (typeof payload !== 'string') { hideSoon(); return; }
    let d;
    try { d = JSON.parse(payload); } catch(e) { hideSoon(); return; }
    cancelHide();
    tooltip.innerHTML = tooltip.querySelector('style').outerHTML + build(d);
    tooltip.style.display = 'block';
    const mouse = evt.event && Number.isFinite(evt.event.clientX) ?
      {x:evt.event.clientX, y:evt.event.clientY} : lastPointer;
    if (mouse) position(mouse.x, mouse.y);
  }
  function onUnhover() { hideSoon(); }
  function onTipEnter() { cancelHide(); }
  function onTipLeave() { hideSoon(); }
  tooltip.addEventListener('mouseenter', onTipEnter);
  tooltip.addEventListener('mouseleave', onTipLeave);
  el.addEventListener('mousemove', onMove);
  el.on('plotly_hover', onHover);
  el.on('plotly_unhover', onUnhover);

  const cleanup = function() {
    cancelHide();
    el.removeEventListener('mousemove', onMove);
    if (typeof el.removeListener === 'function') {
      el.removeListener('plotly_hover', onHover);
      el.removeListener('plotly_unhover', onUnhover);
    }
    tooltip.removeEventListener('mouseenter', onTipEnter);
    tooltip.removeEventListener('mouseleave', onTipLeave);
    if (tooltip.parentNode) tooltip.parentNode.removeChild(tooltip);
    if (observer) observer.disconnect();
    if (el.__discardV3Cleanup === cleanup) delete el.__discardV3Cleanup;
  };
  el.__discardV3Cleanup = cleanup;
  // Shiny can remove/re-create the output without explicitly disposing it.
  const observer = new MutationObserver(function() {
    if (!document.body.contains(el)) cleanup();
  });
  observer.observe(document.body, {childList:true, subtree:true});
}
)---"

  htmlwidgets::onRender(p, js)
}
