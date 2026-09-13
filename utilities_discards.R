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