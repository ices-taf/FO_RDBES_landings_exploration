#' Plot landings (catch) trends with plotly
#'
#' Creates interactive landings trend plots using \pkg{plotly}, either as:
#' \itemize{
#'   \item a set of small-multiple plots (one per fisheries guild) when
#'     \code{type = "Common name"}, or
#'   \item a single plot stratified by \code{type} (e.g. country or guild).
#' }
#' The function ranks categories by total landings, keeps the top
#' \code{line_count}, and aggregates the rest into an \code{"other"} group.
#'
#' @param x A data frame with columns in the following order:
#'   \code{Year}, \code{Country}, \code{iso3}, \code{Fisheries guild},
#'   \code{Ecoregion}, \code{Species name}, \code{Species code},
#'   \code{Common name}, \code{Value}. These are renamed internally and
#' interpreted as landings (in tonnes).
#' @param type Character scalar, one of \code{"Common name"},
#'   \code{"Country"}, or \code{"Fisheries guild"}. Determines the
#'   grouping variable for the lines.
#' @param line_count Integer; maximum number of categories (within each
#'   grouping) to show as separate lines. Remaining categories are
#'   aggregated into \code{"other"}. Default is \code{10}.
#' @param dataUpdated Optional character string appended to the caption
#'   (e.g. a “data updated” date or note).
#' @param return_data Logical; if \code{TRUE}, the function returns the
#'   processed data used for plotting instead of the plotly object(s).
#'   When \code{type = "Common name"}, this is the combined data across
#'   fisheries guilds. Default is \code{FALSE}.
#' @param session Optional Shiny session object. If provided, the
#'   function reads \code{session$clientData[["output_landings_1-landings_layer_width"]]}
#'   to adapt font sizes to the available plot width. If \code{NULL}, a
#'   default width of 800px is assumed.
#' @param per_panel_height Numeric; height (in pixels) used for each
#'   panel when \code{type = "Common name"} and multiple guild plots are
#'   produced. Default is \code{380}.
#' @param ecoregion Optional character scalar used in plot titles and
#'   image file names (e.g. \code{"Greater North Sea"} or an acronym).
#'
#' @return
#' If \code{return_data = TRUE}, a data frame with yearly aggregated
#' landings (in thousand tonnes) by grouping variable is returned.
#'
#' If \code{return_data = FALSE}:
#' \itemize{
#'   \item for \code{type = "Common name"}, an \code{htmltools::tagList}
#'     of \pkg{plotly} objects (one per fisheries guild) is returned;
#'   \item for other \code{type} values, a single \pkg{plotly} object is
#'     returned.
#' }
#'
#' @details
#' For \code{type = "Common name"}, the function:
#' \enumerate{
#'   \item Normalises common names (e.g. collapsing variants such as
#'     “Sandeels …” to \code{"sandeel"}).
#'   \item Splits the data by \code{Fisheries guild}.
#'   \item Within each guild, ranks common names by total landings and
#'     keeps the top \code{line_count}, aggregating the rest into an
#'     \code{"other"} category.
#'   \item Aggregates landings by \code{Year} and \code{type_var} and
#'     converts them to thousand tonnes.
#'   \item Produces one plotly line chart per guild, using a discrete
#'     \code{hcl.colors(..., "Temps")} palette and a common caption that
#'     distinguishes historical (1950–2006) and official (2006–2023)
#'     catches.
#' }
#'
#' For other \code{type} values (e.g. \code{"Country"}, \code{"Fisheries guild"}),
#' the function aggregates across all guilds into a single data set and
#' produces one multi-line plot.
#'
#' In both cases, the y-axis shows landings in thousand tonnes, and the
#' plots are configured with a download-to-image button whose filename
#' includes \code{ecoregion} and the current date.
#'
#' @importFrom dplyr rename all_of filter group_by summarise arrange desc
#'   mutate inner_join pull
#' @importFrom grDevices hcl.colors
#' @importFrom plotly plot_ly add_trace layout highlight config attrs_selected
#' @importFrom htmltools tagList
#' @export
plot_catch_trends_plotly <- function(
  x,
  type = c("Common name", "Country", "Fisheries guild"),
  line_count = 10,
  selected_guild = NULL,
  dataUpdated = NULL,
  return_data = FALSE,
  session = NULL,
  ecoregion = NULL
) {
  type <- match.arg(type)

  # --- Responsive font sizes
  w <- tryCatch({
    if (!is.null(session)) {
      session$clientData[[paste0("output_", session$ns("landings_layer"), "_width")]]
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)

  if (is.na(w) || is.null(w)) w <- 800

  base_size         <- max(9,  min(18, round(w / 55)))
  axis_title_size   <- max(10, min(20, round(w / 50)))
  tick_size         <- max(9,  min(16, round(w / 55)))
  legend_title_size <- max(10, min(18, round(w / 55)))
  legend_text_size  <- max(9,  min(16, round(w / 65)))
  title_annot_size  <- max(12, min(22, round(w / 40)))
  caption_size      <- max(8,  min(14, round(w / 70)))

  # --- Dynamic bottom margin for caption
  caption_lines <- 3
  bottom_margin <- max(100, 20 + caption_lines * (caption_size + 10))

  # --- Expected columns
  names(x) <- c(
    "Year", "Country", "iso3", "Fisheries guild", "Ecoregion",
    "Species name", "Species code", "Common name", "Value"
  )

  cap_text <- paste0(
    "Historical Nominal Catches 1950–2005.<br>",
    "Official Nominal Catches 2006–2023.<br>",
    dataUpdated, ", ICES, Copenhagen."
  )

  sanitize_stub <- function(s) gsub("[^A-Za-z0-9]+", "_", s)
  date_stamp <- format(Sys.Date(), "%d-%b-%y")
  palette_vec <- function(n) grDevices::hcl.colors(max(n, 1), palette = "Temps")

  df <- x %>%
    dplyr::filter(!is.na(Year))

  if (type == "Common name") {
    if (!is.null(selected_guild) && nzchar(selected_guild)) {
      df <- df %>% dplyr::filter(`Fisheries guild` == selected_guild)
    }

    df <- df %>%
      dplyr::mutate(
        type_var = `Common name`,
        type_var = gsub("European ", "", type_var),
        type_var = gsub("Sandeels.*", "sandeel", type_var),
        type_var = gsub("Finfishes nei", "undefined finfish", type_var),
        type_var = gsub("Blue whiting.*", "blue whiting", type_var),
        type_var = gsub("Saithe.*", "saithe", type_var),
        type_var = ifelse(grepl("Norway", type_var), type_var, tolower(type_var))
      )
  } else if (type == "Country") {
    df <- df %>% dplyr::mutate(type_var = Country)
  } else if (type == "Fisheries guild") {
    df <- df %>% dplyr::mutate(type_var = `Fisheries guild`)
  }

  total_df <- df %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(total = sum(Value, na.rm = TRUE) / 1000, .groups = "drop")

  ranked <- df %>%
    dplyr::group_by(type_var) %>%
    dplyr::summarise(typeTotal = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(typeTotal)) %>%
    dplyr::filter(typeTotal >= 1) %>%
    dplyr::mutate(RANK = dplyr::row_number())

  plot_df <- df %>%
    dplyr::inner_join(ranked, by = "type_var") %>%
    dplyr::mutate(type_var = ifelse(RANK > line_count, "other", type_var)) %>%
    dplyr::group_by(type_var, Year) %>%
    dplyr::summarise(typeTotal = sum(Value, na.rm = TRUE) / 1000, .groups = "drop")

  type_levels <- plot_df %>%
    dplyr::group_by(type_var) %>%
    dplyr::summarise(tt = sum(typeTotal, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(tt)) %>%
    dplyr::pull(type_var)

  plot_df$type_var <- factor(plot_df$type_var, levels = type_levels)

  if (return_data) {
    return(list(series = plot_df, total = total_df))
  }

  n_types <- length(unique(plot_df$type_var))
  pal <- palette_vec(n_types)

  subtitle_part <- if (type == "Common name" && !is.null(selected_guild) && nzchar(selected_guild)) {
    paste0(" - ", selected_guild)
  } else {
    ""
  }

  file_stub <- paste0(
    sanitize_stub(ifelse(is.null(ecoregion), "ecoregion", ecoregion)),
    "_landings_",
    sanitize_stub(type),
    if (!is.null(selected_guild) && nzchar(selected_guild)) {
      paste0("_", sanitize_stub(selected_guild))
    } else {
      ""
    },
    "_",
    date_stamp
  )

  # Important: keyed data for click highlighting
  keyed_df <- plotly::highlight_key(plot_df, ~type_var)

  plotly::plot_ly(
    keyed_df,
    x = ~Year,
    y = ~typeTotal,
    color = ~type_var,
    colors = pal,
    showlegend = TRUE,
    type = "scatter",
    mode = "lines",
    line = list(width = 3),
    hovertemplate = paste0(
      "<b>", type, ":</b> %{fullData.name}<br>",
      "<b>Year:</b> %{x}<br>",
      "<b>Landings:</b> %{y:.2f} thousand tonnes<extra></extra>"
    ),
    source = "landings_trends"
  ) %>%
    plotly::add_trace(
      data = total_df,
      x = ~Year,
      y = ~total,
      type = "scatter",
      mode = "lines",
      inherit = FALSE,
      name = "Total",
      line = list(color = "black", width = 3, dash = "dash"),
      hovertemplate = paste0(
        "<b>Total</b><br>",
        "<b>Year:</b> %{x}<br>",
        "<b>Landings:</b> %{y:.2f} thousand tonnes<extra></extra>"
      )
    ) %>%
    plotly::layout(
      font = list(size = base_size),
      xaxis = list(
        title = list(text = "Year", font = list(size = axis_title_size)),
        tickfont = list(size = tick_size),
        automargin = TRUE
      ),
      yaxis = list(
        title = list(
          text = "Landings (thousand tonnes)",
          font = list(size = axis_title_size),
          standoff = 18
        ),
        tickfont = list(size = tick_size),
        automargin = TRUE
      ),
      margin = list(l = 80, r = 20, t = 70, b = bottom_margin),
      annotations = list(
        list(
          text = paste0("Landings trends (", ecoregion, subtitle_part, ")"),
          x = 0.01, y = 0.99,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          xanchor = "left", yanchor = "top",
          font = list(size = title_annot_size, color = "black")
        ),
        list(
          x = 1, y = 0,
          xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "top",
          yshift = -40,
          text = cap_text,
          showarrow = FALSE,
          align = "right",
          font = list(size = caption_size, color = "black")
        )
      ),
      legend = list(
        title = list(
          text = paste0("<b>", type, ":</b>"),
          font = list(size = legend_title_size)
        ),
        orientation = "h",
        x = 0.5, y = 1.08,
        xanchor = "center", yanchor = "bottom",
        font = list(size = legend_text_size),
        itemwidth = 50
      ),
      hoverlabel = list(font = list(size = base_size))
    ) %>%
    plotly::highlight(
      on = "plotly_click",
      off = "plotly_doubleclick",
      persistent = FALSE,
      dynamic = FALSE,
      selected = plotly::attrs_selected(
        opacity = 1,
        line = list(width = 5)
      )
    ) %>%
    plotly::config(
      responsive = TRUE,
      toImageButtonOptions = list(
        filename = file_stub,
        format = "png",
        scale = 3
      )
    )
}


p <- plot_catch_trends_plotly(
  x = BI,
  type = "Common name",
  line_count = 10,
  selected_guild = "Pelagic",
  dataUpdated = "Data updated: 2024-06-01",
  return_data = FALSE,
  session = NULL,
  ecoregion = "Bay of Biscay and the Iberian Coast"
)
