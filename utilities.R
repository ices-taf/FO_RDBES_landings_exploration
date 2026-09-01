library(dplyr)
library(icesTAF)
library(tidyr)
library(countrycode)


#' List of ICES ecoregions
ecoregions <- c(
  "Baltic Sea", "Bay of Biscay and the Iberian Coast", "Celtic Seas",
  "Greater North Sea", "Norwegian Sea", "Icelandic Waters", "Barents Sea",
  "Greenland Sea", "Faroes", "Oceanic Northeast Atlantic", "Azores"
)

#' Get acronym for an ICES ecoregion
#'
#' Translates a full ICES ecoregion name into the corresponding
#' three-letter acronym used in the app (e.g. `"Baltic Sea"` → `"BtS"`).
#'
#' @param ecoregion A single character string giving the full
#'   ecoregion name. Must be one of:
#'   `"Baltic Sea"`, `"Bay of Biscay and the Iberian Coast"`,
#'   `"Celtic Seas"`, `"Greater North Sea"`, `"Norwegian Sea"`,
#'   `"Icelandic Waters"`, `"Barents Sea"`, `"Greenland Sea"`,
#'   `"Faroes"`, `"Oceanic Northeast Atlantic"`, or `"Azores"`.
#'
#' @return A character string with the corresponding acronym:
#'   `"BtS"`, `"BI"`, `"CS"`, `"NrS"`, `"NwS"`, `"IS"`, `"BrS"`,
#'   `"GS"`, `"FO"`, `"ONA"`, or `"AZ"`.
#'
#' @details
#' If `ecoregion` does not match any of the supported names,
#' the function raises an error via [base::stop()].
#'
#' @examples
#' get_ecoregion_acronym("Baltic Sea")
#' get_ecoregion_acronym("Greater North Sea")
#'
#' @export
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


#' Load ASFIS species reference table
#'
#' Reads the local ASFIS ASFIS\_sp CSV file and returns a data frame with
#' English common name, scientific name, and FAO three-letter species code.
#'
#' @details
#' This function expects the file `./data-raw/ASFIS_sp_2025.csv` to be present
#' relative to the project root (or package root, if used inside a package).
#' Only the columns `English_name`, `Scientific_Name`, and `Alpha3_Code`
#' are kept.
#'
#' @return
#' A data frame (or tibble, if used with dplyr) with three character columns:
#' \describe{
#'   \item{English_name}{English common name of the species.}
#'   \item{Scientific_Name}{Scientific (Latin) name of the species.}
#'   \item{Alpha3_Code}{FAO three-letter species code.}
#' }
#'
#' @examples
#' \dontrun{
#' species <- load_asfis_species()
#' head(species)
#' }
#'
load_asfis_species <- function() {
#   url <- "http://www.fao.org/fishery/static/ASFIS/ASFIS_sp.zip"
    species <- read.csv("./Boot/ASFIS_sp_2025.csv", na.strings = "", stringsAsFactors = FALSE)
    species <- dplyr::select(species, English_name, Scientific_Name, Alpha3_Code)
    return(species)
}


#' Load ICES historical catches data
#'
#' Reads the local `ICES_historical_catches.csv` file and returns a data frame
#' with historical catches as provided by ICES.
#'
#' @details
#' This function expects the file `./data-raw/ICES_historical_catches.csv`
#' to be available relative to the project (or package) root. All columns
#' in the CSV file are returned unchanged.
#'
#' @return
#' A data frame containing the contents of `ICES_historical_catches.csv`.
#'
#' @examples
#' \dontrun{
#' hist <- load_historical_catches()
#' head(hist)
#' }
#'
#' @export
load_historical_catches<- function(){
        # url <- "http://ices.dk/data/Documents/CatchStats/HistoricalLandings1950-2010.zip"
        hist <- read.csv("./Boot/ICES_historical_catches.csv", header = TRUE)#, na.strings = "", stringsAsFactors = FALSE)
}



#' Load ICES official catches data
#'
#' Reads the local `ICESCatchDataset2006-2023_noConf.csv` file and returns
#' a data frame with official ICES catch statistics.
#'
#' @details
#' This function expects the file
#' `./data-raw/ICESCatchDataset2006-2023_noConf.csv` to be available
#' relative to the project (or package) root. All columns in the CSV file
#' are returned unchanged.
#'
#' @return
#' A data frame containing the contents of
#' `ICESCatchDataset2006-2023_noConf.csv`.
#'
#' @examples
#' \dontrun{
#' official <- load_official_catches()
#' head(official)
#' }
#'
#' @export
load_official_catches<- function(){
        # url <- "http://ices.dk/data/Documents/CatchStats/OfficialNominalCatches.zip"
        official <- read.csv("./Boot/ICESCatchDataset2006-2023_noConf.csv", header = TRUE)#, na.strings = "", stringsAsFactors = FALSE)
}




apply_manual_attributions <- function(dat, manual_attributions) {

  if ("COMMON_NAME" %in% names(dat)) {
    dat$COMMON_NAME <- normalise_common_name(dat$COMMON_NAME)
    common_name_key <- dat$COMMON_NAME
  } else {
    common_name_key <- NULL
  }

  if (is.null(manual_attributions)) {

    if ("GUILD" %in% names(dat)) {
      dat$GUILD <- normalise_guild(dat$GUILD)
    }

    return(dat)
  }

  required_cols <- c(
    "Order",
    "Target column",
    "Match column",
    "Match value",
    "New value / assignment"
  )

  missing_cols <- setdiff(required_cols, names(manual_attributions))

  if (length(missing_cols) > 0) {
    stop(
      "manual_attributions is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  rules <- manual_attributions %>%
    dplyr::mutate(
      Order = as.numeric(.data$Order),
      target_col = trimws(as.character(.data$`Target column`)),
      match_col  = trimws(as.character(.data$`Match column`)),
      match_val  = as.character(.data$`Match value`),
      new_val    = as.character(.data$`New value / assignment`),

      # Precompute normalised versions only once
      match_val_common = normalise_common_name(.data$match_val),
      new_val_common   = normalise_common_name(.data$new_val),

      match_val_guild = tolower(trimws(as.character(.data$match_val)))
    ) %>%
    dplyr::filter(
      !is.na(.data$Order),
      !is.na(.data$target_col),
      !is.na(.data$match_col),
      !is.na(.data$match_val),
      !is.na(.data$new_val),
      .data$target_col != "",
      .data$match_col != ""
    ) %>%
    dplyr::arrange(.data$Order)

  for (i in seq_len(nrow(rules))) {

    target_col <- rules$target_col[i]
    match_col  <- rules$match_col[i]
    match_val  <- rules$match_val[i]
    new_val    <- rules$new_val[i]

    if (!(target_col %in% names(dat))) {
      warning(
        "Skipping rule ",
        rules$Order[i],
        ": target column not found: ",
        target_col
      )
      next
    }

    if (!(match_col %in% names(dat))) {
      warning(
        "Skipping rule ",
        rules$Order[i],
        ": match column not found: ",
        match_col
      )
      next
    }

    if (match_col == "COMMON_NAME") {

      idx <- which(
        !is.na(common_name_key) &
          common_name_key == rules$match_val_common[i]
      )

    } else if (match_col == "GUILD") {

      idx <- which(
        !is.na(dat[[match_col]]) &
          tolower(trimws(as.character(dat[[match_col]]))) ==
          rules$match_val_guild[i]
      )

    } else {

      idx <- which(
        !is.na(dat[[match_col]]) &
          trimws(as.character(dat[[match_col]])) ==
          trimws(as.character(match_val))
      )
    }

    if (length(idx) > 0) {

      if (target_col == "COMMON_NAME") {
        new_val <- rules$new_val_common[i]
      }

      dat[[target_col]][idx] <- new_val

      # Keep the cached COMMON_NAME key in sync if a rule changes COMMON_NAME
      if (target_col == "COMMON_NAME") {
        common_name_key[idx] <- new_val
      }
    }
  }

  if ("GUILD" %in% names(dat)) {
    dat$GUILD <- normalise_guild(dat$GUILD)
  }

  if ("COMMON_NAME" %in% names(dat)) {
    dat$COMMON_NAME <- common_name_key
  }

  dat
}

normalise_guild <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x_lower <- tolower(x)

  dplyr::case_when(
    is.na(x) | x == "" ~ "Undefined",
    x_lower == "undefined" ~ "Undefined",
    x_lower == "pelagic" ~ "Pelagic",
    x_lower == "demersal" ~ "Demersal",
    x_lower == "benthic" ~ "Benthic",
    x_lower == "elasmobranch" ~ "Elasmobranch",
    x_lower %in% c("crustacean", "crustaceans", "shellfish") ~ "Shellfish",
    TRUE ~ x
  )
}


normalise_common_name <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- tolower(x)

  # Remove "european" only when it appears as a separate word
  x <- gsub("\\beuropean\\b", "", x)
  # Remove "european" only when it appears as a separate word
  x <- gsub("\\bnei\\b", "", x)

  # Clean repeated spaces created by removal
  x <- trimws(gsub("\\s+", " ", x))

  x
}

# -----------------------------
  # helper functions
  # -----------------------------
  clean_name <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- tolower(x)
    x
  }
  
  clean_code <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- toupper(x)
    x
  }
  
  year_cols <- function(df) {
    grep("^X\\d{4}$", names(df), value = TRUE)
  }




format_catches_dev_rdbes <- function(year,
                               ecoregion,
                               historical,
                               official,
                               preliminary = NULL,
                               species_list,
                               sid,
                               manual_attributions = NULL) {  
  
  
  # -----------------------------
  # lookups
  # -----------------------------
  fish_category <- sid %>%
    mutate(
      Alpha3_Code = substr(StockKeyLabel, 1, 3),
      Alpha3_Code = clean_code(Alpha3_Code)
    ) %>%
    select(Alpha3_Code, FisheriesGuild) %>%
    distinct()
  
  fish_category$FisheriesGuild[fish_category$Alpha3_Code == "POK"] <- "Demersal"
  
  species_lookup <- species_list %>%
    mutate(
      English_name_key = clean_name(English_name),
      Scientific_Name_key = clean_name(Scientific_Name),
      Alpha3_Code_key = clean_code(Alpha3_Code)
    )
  
  # -----------------------------
  # historical ecoregion definitions
  # -----------------------------
  historic_bs <- c(
    "III (not specified)", "III b  Baltic 23",
    "III b+c (not specified)", "III b-d (not specified)",
    "III c  Baltic 22", "III d  (not specified)",
    "III d  Baltic 24", "III d  Baltic 25",
    "III d  Baltic 26", "III d  Baltic 27",
    "III d  Baltic 28 (not specified)", "III d  Baltic 28-1",
    "III d  Baltic 28-2", "III d  Baltic 29",
    "III d  Baltic 30", "III d  Baltic 31",
    "III d  Baltic 32"
  )
  
  historic_ns <- c(
    "III a", "IIIa  and  IV  (not specified)",
    "IIIa  and  IVa+b  (not specified)", "IV (not specified)",
    "IV a", "IV a+b (not specified)",
    "IV b", "IV b+c (not specified)",
    "IV c", "VII d"
  )
  
  historic_bob <- c(
    "VIII a", "VIII b", "VIII c", "VIII d2", "VIII e2",
    "IX a", "IX b2", "VIII d (not specified)", "VIII (not specified)",
    "VIII e (not specified)", "IX (not specified)", "IX b (not specified)"
  )
  
  historic_cs <- c(
    "VI a", "VI b2", "VII a", "VII b", "VII c2", "VII f", "VII g", "VII h",
    "VII j2", "VII k2", "VII (not specified)", "VII b+c (not specified)",
    "VII c (not specified)", "VII d-k (not specified)", "VII f-k (not specified)",
    "VII g-k (not specified)", "VII j (not specified)"
  )
  
  historic_is <- c("V a (North-East)", "V a (South-West)", "V a1", "V a (not specified)", "V a2")
  historic_az <- c("X (not specified)", "X a (not specified)")
  historic_gs <- c("XII a3", "XIV (not specified)", "XIV a", "XIV b (not specified)", "XIV b2")
  
  historic_uk <- paste0(c("^UK", "^Channel", "^Isle of Man"), collapse = "|")
  
  historic_nw <- NULL
  historic_br <- NULL
  historic_fo <- NULL
  historic_nea <- NULL
  
  if (ecoregion == "Norwegian Sea") {
    historic_nw <- c(
      "II a1", "II b1", "I  and  IIa (not specified)", "II a (not specified)",
      "II (not specified)", "II a2", "II b (not specified)", "II b2", "XIV", "XIVa"
    )
  }
  
  if (ecoregion == "Barents Sea") {
    historic_br <- c(
      "I (not specified)", "I a", "I b", "I  and  IIa (not specified)",
      "II a (not specified)", "II (not specified)", "II a2",
      "II b (not specified)", "II b2"
    )
  }
  
  if (ecoregion == "Faroes") {
    historic_fo <- c("V b2", "V b (not specified)", "V b1 (not specified)", "V b1B")
  }
  
  if (ecoregion == "Oceanic Northeast Atlantic") {
    historic_nea <- c(
      "V b1A", "VI b1", "VII c1", "VII j1", "VII k1", "VIII d1", "VIII e1",
      "IX b1", "X b", "XII a1", "XII b", "XIV b1",
      "X (not specified)", "X a (not specified)", "XII (not specified)"
    )
  }
  
  # -----------------------------
  # historical special Faroes adjustment
  # -----------------------------
  fo_2020 <- historical %>%
    filter(Division == "ICES Area (not specified)", Country == "Faeroe Islands")
  
  if (nrow(fo_2020) > 0) {
    fo_2020$Division <- "V b1 (not specified)"
    fo_2020$Division[fo_2020$Species == "Atlantic mackerel"] <- "ICES Area (not specified)"
    fo_2020$Division[fo_2020$Species == "Atlantic horse mackerel"] <- "ICES Area (not specified)"
    fo_2020$Division[fo_2020$Species == "Atlantic herring"] <- "ICES Area (not specified)"
    
    historical <- bind_rows(historical, fo_2020)
  }
  
  # -----------------------------
  # historical catches
  # -----------------------------
  hist_years <- year_cols(historical)
  
  historical <- historical %>%
    mutate(across(all_of(hist_years), as.character))
  
  catch_dat_1950 <- historical %>%
    pivot_longer(
      cols = all_of(hist_years),
      names_to = "YEAR",
      values_to = "VALUE"
    ) %>%
    mutate(
      YEAR = as.numeric(gsub("X", "", YEAR)),
      VALUE = ifelse(VALUE == "<0.5", "0", VALUE),
      VALUE = as.numeric(VALUE),
      VALUE = ifelse(is.na(VALUE), 0, VALUE),
      Country = case_when(
        grepl(historic_uk, Country) ~ "United Kingdom",
        grepl("^Germany", Country) ~ "Germany",
        Country %in% c("Un. Sov. Soc. Rep.", "Russian Federation") ~ "Russia",
        grepl("Faeroe Islands", Country) ~ "Faroe Islands",
        grepl("Other nei", Country) ~ "OTHER",
        TRUE ~ Country
      ),
      ISO3 = countrycode(Country, "country.name", "iso3c", warn = FALSE),
      ECOREGION = case_when(
        Division %in% historic_bs ~ "Baltic Sea",
        Division %in% historic_ns ~ "Greater North Sea",
        Division %in% historic_bob ~ "Bay of Biscay and the Iberian Coast",
        Division %in% historic_cs ~ "Celtic Seas",
        Division %in% historic_is ~ "Icelandic Waters",
        Division %in% historic_az ~ "Azores",
        Division %in% historic_gs ~ "Greenland Sea",
        !is.null(historic_nea) & Division %in% historic_nea ~ "Oceanic Northeast Atlantic",
        !is.null(historic_fo) & Division %in% historic_fo ~ "Faroes",
        !is.null(historic_nw) & Division %in% historic_nw ~ "Norwegian Sea",
        !is.null(historic_br) & Division %in% historic_br ~ "Barents Sea",
        TRUE ~ "OTHER"
      ),
      Species_key = clean_name(Species)
    ) %>%
    filter(YEAR <= 2005) %>%
    left_join(
      species_lookup %>%
        select(English_name_key, Scientific_Name, Alpha3_Code, English_name),
      by = c("Species_key" = "English_name_key")
    ) %>%
    mutate(Alpha3_Code = clean_code(Alpha3_Code)) %>%
    left_join(fish_category, by = "Alpha3_Code") %>%
    transmute(
      YEAR,
      COUNTRY = Country,
      ISO3,
      GUILD = FisheriesGuild,
      ECOREGION,
      SPECIES_NAME = Scientific_Name,
      SPECIES_CODE = Alpha3_Code,
      COMMON_NAME = Species,
      VALUE
    )
  
  # -----------------------------
  # official catches
  # -----------------------------
  off_years <- year_cols(official)
  
  official <- official %>%
    mutate(across(all_of(off_years), as.character))
  
  catch_dat_2010 <- official %>%
    pivot_longer(
      cols = all_of(off_years),
      names_to = "YEAR",
      values_to = "VALUE"
    ) %>%
    mutate(
      YEAR = as.numeric(gsub("X", "", YEAR)),
      VALUE = as.numeric(VALUE),
      VALUE = ifelse(is.na(VALUE), 0, VALUE)
    ) %>%
    filter(Country != "") %>%
    mutate(
      Country = countrycode(Country, "iso2c", "country.name"),
      Country = ifelse(grepl("Guernsey|Isle of Man|Jersey", Country), "United Kingdom", Country),
      ISO3 = countrycode(Country, "country.name", "iso3c", warn = FALSE),
      Country = gsub("(United Kingdom) .*", "\\1", Country),
      Area = tolower(Area),
      Species_key = clean_code(Species),
      ECOREGION = case_when(
        Area %in% c("27.3.bc", "27.3.d", "27.3_nk") ~ "Baltic Sea",
        Area %in% c("27.3.a", "27.4", "27.7.d") ~ "Greater North Sea",
        Area %in% c("27.8.a", "27.8.b", "27.8.c", "27.8.d.2", "27.8.e.2", "27.9.a", "27.9.b.2") ~ "Bay of Biscay and the Iberian Coast",
        Area %in% c("27.6.a", "27.6.b.2", "27.7.a", "27.7.b", "27.7.c.2", "27.7.f", "27.7.g", "27.7.h", "27.7.j.2", "27.7.k.2") ~ "Celtic Seas",
        Area %in% c("27.5.a.1", "27.5.a.2", "27.5.a_NK", "27.5.a_nk", "27.12.a.4") ~ "Icelandic Waters",
        ecoregion == "Norwegian Sea" & Area %in% c("27.2.a.1", "27.2.a.2", "27.2.a_NK", "27.2.a_nk", "27.2.b.1", "27.2.b.2", "27.2.b_NK", "27.2.b_nk", "27.14.a", "27.14_NK", "27.14_nk") ~ "Norwegian Sea",
        ecoregion == "Azores" & Area %in% c("27.10.a.2", "27.10.a_NK", "27.10.a_nk", "27.10_NK", "27.10_nk") ~ "Azores",
        ecoregion == "Greenland Sea" & Area %in% c("27.12.a.3", "27.14.a", "27.14.b.2", "27.14.b_NK", "27.14.b_nk", "27.14_NK", "27.14_nk") ~ "Greenland Sea",
        ecoregion == "Faroes" & Area %in% c("27.5.b.2", "27.5.b.1.a", "27.5.b.1.b", "27.5.b.1_NK", "27.5.b_NK", "27.5.b.1_nk", "27.5.b_nk") ~ "Faroes",
        ecoregion == "Barents Sea" & Area %in% c("27.1.a", "27.1.b", "27.2.a.2", "27.2.a_NK", "27.2.a_nk", "27.2.b.2", "27.2.b_NK", "27.2.b_nk", "27.1_NK", "27.1_nk") ~ "Barents Sea",
        ecoregion == "Oceanic Northeast Atlantic" & Area %in% c("27.5.b.1.a", "27.6.b.1", "27.7.c.1", "27.7.j.1", "27.7.k.1", "27.8.d.1", "27.8.e.1", "27.9.b.1", "27.10.a.1", "27.10.b", "27.12_nk", "27.12_NK", "27.12.a.1", "27.12.b", "27.12.c", "27.14.b.1") ~ "Oceanic Northeast Atlantic",
        TRUE ~ "OTHER"
      )
    ) %>%
    left_join(
      species_lookup %>%
        select(Alpha3_Code_key, Scientific_Name, Alpha3_Code, English_name),
      by = c("Species_key" = "Alpha3_Code_key")
    ) %>%
    mutate(Alpha3_Code = clean_code(Alpha3_Code)) %>%
    left_join(fish_category, by = "Alpha3_Code") %>%
    transmute(
      YEAR,
      COUNTRY = Country,
      ISO3,
      GUILD = FisheriesGuild,
      ECOREGION,
      SPECIES_NAME = Scientific_Name,
      SPECIES_CODE = Alpha3_Code,
      COMMON_NAME = English_name,
      VALUE
    ) %>%
    group_by(YEAR, COUNTRY, ISO3, GUILD, ECOREGION, SPECIES_NAME, SPECIES_CODE, COMMON_NAME) %>%
    summarise(VALUE = sum(VALUE), .groups = "drop") %>% 
    filter(YEAR < 2021)
  
  # -----------------------------
  # preliminary catches
  # -----------------------------
  if (is.null(preliminary)) {
    df <- bind_rows(catch_dat_2010, catch_dat_1950)
    
  } else {
    # catch_dat_prelim <- preliminary %>%
    #   filter(Country != "")
    catch_dat_prelim <- preliminary %>%
        filter(!is.na(ISO3), ISO3 != "") %>%
        mutate(
            COUNTRY = countrycode(
            ISO3,
            origin = "iso2c",
            destination = "country.name",
            warn = FALSE
            ),
            ISO3 = countrycode(
            ISO3,
            origin = "iso2c",
            destination = "iso3c",
            warn = FALSE
            )
        )
    # catch_dat_prelim$VALUE <- catch_dat_prelim[, 7]
    # catch_dat_prelim <- catch_dat_prelim[, -grep("AMS", colnames(catch_dat_prelim)), drop = FALSE]
    # catch_dat_prelim <- catch_dat_prelim[, -grep("BMS", colnames(catch_dat_prelim)), drop = FALSE]
    # catch_dat_prelim$Species.Latin.Name <- catch_dat_prelim[, 3]
    
#     catch_dat_prelim <- catch_dat_prelim %>%
#       mutate(
#         # YEAR = Year,
#         # Country = countrycode(Country, "iso2c", "country.name"),
#         # Country = ifelse(grepl("Guernsey|Isle of Man|Jersey", Country), "United Kingdom", Country),
#         # ISO3 = COUNTRY,
#         # Country = gsub("(United Kingdom) .*", "\\1", Country),
#         Area = trimws(Area),
#         Species_key = clean_name(Scientific_Name),
#         ECOREGION = case_when(
#           Area %in% c("27_3_bc", "27_3_c_22", "27_3_d", "27_3_d_24", "27_3_d_25", "27_3_d_26", "27_3_d_30", "27_3_d_27", "27_3_d_31", "27_3_nk", "27_3_b_23", "27_3_d_28_2", "27_3_d_32", "27_3_d_29") ~ "Baltic Sea",
#           Area %in% c("27_3_a", "27_4_a", "27_4_b", "27_4_c", "27_7_d") ~ "Greater North Sea",
#           Area %in% c("27_8_a", "27_8_b", "27_8_c", "27_8_d_2", "27_8_e_2", "27_9_a", "27_9_b_2") ~ "Bay of Biscay and the Iberian Coast",
#           Area %in% c("27_6_a", "27_6_b_2", "27_7_a", "27_7_b", "27_7_c_2", "27_7.e", "27_7_f", "27_7_g", "27_7_h", "27_7_j_2", "27_7_k_2") ~ "Celtic Seas",
#           Area %in% c("5_a_1", "5_a_2", "12_a_4") ~ "Icelandic Waters",
#           Area %in% c("27_10_a_2", "27_10_A_2") ~ "Azores",
#           Area %in% c("27_1_a", "27_1_b", "27_2_b_2") ~ "Barents Sea",
#           Area %in% c("27_2_a_1", "27_2_a_2", "27_2_b_1", "27_2_b_2", "27_14_a", "27_2_a", "27_2_b") ~ "Norwegian Sea",
#           Area %in% c("27_5_b_1_A", "27_6_b_1", "27_7_c_1", "27_7_j_1", "27_7_k_1", "27_8_d_1", "27_8_e_1", "27_9_b_1", "27_10_a_1", "27_10_b", "27_12_a_1", "27_12_b", "27_12_c", "27_14_b_1") ~ "Oceanic Northeast Atlantic",
#           Area %in% c("27_14_B", "27_14", "27_14_B_2", "27_14_A", "27_14_NK") ~ "Greenland Sea",
#           Area %in% c("27_5_b", "27_5_b_1", "27_5_b_2", "27_5_b_1_b", " 27_5_b") ~ "Faroes",
#           TRUE ~ "OTHER"
#         )
#       ) %>%
#       filter(ECOREGION != "OTHER") %>%
#       left_join(
#         species_lookup %>%
#           select(Scientific_Name_key, Scientific_Name, Alpha3_Code, English_name),
#         by = c("Species_key" = "Scientific_Name_key")
#       ) %>%
#       mutate(
#         Alpha3_Code = clean_code(Alpha3_Code),
#         VALUE = as.numeric(VALUE)
#       ) %>%
#       left_join(fish_category, by = "Alpha3_Code") %>%
#     #   transmute(
#     #     YEAR,
#     #     COUNTRY = Country,
#     #     ISO3,
#     #     GUILD = FisheriesGuild,
#     #     ECOREGION,
#     #     SPECIES_NAME = Scientific_Name,
#     #     SPECIES_CODE = Alpha3_Code,
#     #     COMMON_NAME = English_name,
#     #     VALUE
#     #   )
#     transmute(
#   YEAR,
#   COUNTRY,
#   ISO3,
#   GUILD = FisheriesGuild,
#   ECOREGION,
#   SPECIES_NAME = Scientific_Name,
#   SPECIES_CODE = Alpha3_Code,
#   COMMON_NAME = English_name,
#   VALUE
# )
catch_dat_prelim <- catch_dat_prelim %>%
  mutate(
    Area = trimws(Area),
    Species_key = clean_code(CLspeciesFaoCode),
    ECOREGION = "Bay of Biscay and the Iberian Coast",
#     ECOREGION = case_when(
#       Area %in% c("27_3_bc", "27_3_c_22", "27_3_d", "27_3_d_24",
#                   "27_3_d_25", "27_3_d_26", "27_3_d_30", "27_3_d_27",
#                   "27_3_d_31", "27_3_nk", "27_3_b_23", "27_3_d_28_2",
#                   "27_3_d_32", "27_3_d_29") ~ "Baltic Sea",

#       Area %in% c("27_3_a", "27_4_a", "27_4_b", "27_4_c", "27_7_d") ~
#         "Greater North Sea",

#       Area %in% c("27_8_a", "27_8_b", "27_8_c", "27_8_d_2",
#                   "27_8_e_2", "27_9_a", "27_9_b_2") ~
#         "Bay of Biscay and the Iberian Coast",

#       Area %in% c("27_6_a", "27_6_b_2", "27_7_a", "27_7_b", "27_7_c_2",
#                   "27_7.e", "27_7_f", "27_7_g", "27_7_h",
#                   "27_7_j_2", "27_7_k_2") ~ "Celtic Seas",

#       Area %in% c("5_a_1", "5_a_2", "12_a_4") ~ "Icelandic Waters",

#       Area %in% c("27_10_a_2", "27_10_A_2") ~ "Azores",

#       Area %in% c("27_1_a", "27_1_b", "27_2_b_2") ~ "Barents Sea",

#       Area %in% c("27_2_a_1", "27_2_a_2", "27_2_b_1", "27_2_b_2",
#                   "27_14_a", "27_2_a", "27_2_b") ~ "Norwegian Sea",

#       Area %in% c("27_5_b_1_A", "27_6_b_1", "27_7_c_1", "27_7_j_1",
#                   "27_7_k_1", "27_8_d_1", "27_8_e_1", "27_9_b_1",
#                   "27_10_a_1", "27_10_b", "27_12_a_1", "27_12_b",
#                   "27_12_c", "27_14_b_1") ~
#         "Oceanic Northeast Atlantic",

#       Area %in% c("27_14_B", "27_14", "27_14_B_2", "27_14_A", "27_14_NK") ~
#         "Greenland Sea",

#       Area %in% c("27_5_b", "27_5_b_1", "27_5_b_2",
#                   "27_5_b_1_b", " 27_5_b") ~ "Faroes",

#       TRUE ~ "OTHER"
#     )
  ) %>%
#   filter(ECOREGION != "OTHER") %>%

  left_join(
    species_lookup %>%
      select(
        Alpha3_Code_key,
        lookup_Scientific_Name = Scientific_Name,
        Alpha3_Code,
        lookup_Common_Name = English_name
      ),
    by = c("Species_key" = "Alpha3_Code_key")
  ) %>%

  mutate(
    Alpha3_Code = clean_code(Alpha3_Code),
    VALUE = as.numeric(VALUE/ 1000)
  ) %>%

  left_join(fish_category, by = "Alpha3_Code") %>%

  transmute(
    YEAR,
    COUNTRY,
    ISO3,
    GUILD = FisheriesGuild,
    ECOREGION,

    # use canonical names from species_list
    SPECIES_NAME = lookup_Scientific_Name,
    SPECIES_CODE = Alpha3_Code,
    COMMON_NAME = lookup_Common_Name,

    VALUE
  )
    
    # catch_dat_prelim$COMMON_NAME[catch_dat_prelim$SPECIES_NAME == "Ammodytes"] <- "Sandeels(=Sandlances) nei"
    # catch_dat_prelim$SPECIES_CODE[catch_dat_prelim$SPECIES_NAME == "Ammodytes"] <- "SAN"
    
    df <- bind_rows(catch_dat_2010, catch_dat_1950, catch_dat_prelim)
  }
  
  # -----------------------------
  # final cleanup
  # -----------------------------
  df <- df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      GUILD = ifelse(is.na(GUILD), "Undefined", GUILD),
      COMMON_NAME = normalise_common_name(COMMON_NAME)
      # replace "crustacean" with "shellfish" in GUILD column
      # GUILD = ifelse(GUILD == "crustacean", "shellfish", GUILD)
    ) %>%
    dplyr::select(
      YEAR,
      COUNTRY,
      ISO3,
      GUILD,
      ECOREGION,
      SPECIES_NAME,
      SPECIES_CODE,
      COMMON_NAME,
      VALUE
    ) %>%
    dplyr::filter(ECOREGION %in% ecoregion)

  df <- apply_manual_attributions(df, manual_attributions)

  df <- df %>%
    dplyr::mutate(COMMON_NAME = normalise_common_name(COMMON_NAME)) %>%
    dplyr::group_by(
      YEAR,
      COUNTRY,
      ISO3,
      GUILD,
      ECOREGION,
      SPECIES_NAME,
      SPECIES_CODE,
      COMMON_NAME
    ) %>%
    dplyr::summarise(VALUE = sum(VALUE, na.rm = TRUE), .groups = "drop")
  
  return(df)

}



save_catch_trends_html <- function(
    data,
    guilds = NULL,
    types = "Common name",
    ecoregion,
    output_dir = "landings_RDBES_html",
    line_count = 10,
    dataUpdated = NULL
) {

  if (is.null(guilds)) {
    guilds <- data$GUILD %>%
      unique() %>%
      na.omit() %>%
      sort()
  }


sanitize_filename <- function(x) {
    gsub("[^A-Za-z0-9]+", "_", trimws(x))
  }

  for (type in types) {

    for (guild in guilds) {

      message("Creating: ", type, " | ", guild)

      p <- plot_catch_trends_plotly_rdbes(
        x = data,
        type = type,
        line_count = line_count,
        selected_guild = if (type == "Common name") guild else NULL,
        dataUpdated = dataUpdated,
        return_data = FALSE,
        session = NULL,
        ecoregion = ecoregion
      )

      file_name <- paste(
        sanitize_filename(type),
        sanitize_filename(guild),
        sanitize_filename(ecoregion),
        sep = "_"
      )

      htmlwidgets::saveWidget(
        widget = p,
        file = file.path("./output/landings_RDBES_html", paste0(file_name, ".html")),
        selfcontained = TRUE
      )
    }
  }
}

### plotting function for catch trends
plot_catch_trends_plotly_rdbes <- function(
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
    "Official Nominal Catches 2006–2020.<br>",
    "RDBES Landings 2021–2025.<br>",
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
