#' Canton Reference Data
#' This file provides a comprehensive reference for Swiss cantons including IDs, abbreviations, and names in three languages

#' Create the main canton reference dataframe
create_canton_reference <- function() {
  canton_ref <- data.frame(
    canton_id = 1:26,
    abbr = c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR",
             "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG",
             "TI", "VD", "VS", "NE", "GE", "JU"),
    name_de = c("Zürich", "Bern", "Luzern", "Uri", "Schwyz", "Obwalden", "Nidwalden",
                "Glarus", "Zug", "Freiburg", "Solothurn", "Basel-Stadt", "Basel-Landschaft",
                "Schaffhausen", "Appenzell Ausserrhoden", "Appenzell Innerrhoden",
                "St. Gallen", "Graubünden", "Aargau", "Thurgau", "Tessin", "Waadt",
                "Wallis", "Neuenburg", "Genf", "Jura"),
    name_fr = c("Zurich", "Berne", "Lucerne", "Uri", "Schwytz", "Obwald", "Nidwald",
                "Glaris", "Zoug", "Fribourg", "Soleure", "Bâle-Ville", "Bâle-Campagne",
                "Schaffhouse", "Appenzell Rhodes-Extérieures", "Appenzell Rhodes-Intérieures",
                "Saint-Gall", "Grisons", "Argovie", "Thurgovie", "Tessin", "Vaud",
                "Valais", "Neuchâtel", "Genève", "Jura"),
    name_it = c("Zurigo", "Berna", "Lucerna", "Uri", "Svitto", "Obvaldo", "Nidvaldo",
                "Glarona", "Zugo", "Friburgo", "Soletta", "Basilea Città", "Basilea Campagna",
                "Sciaffusa", "Appenzello Esterno", "Appenzello Interno",
                "San Gallo", "Grigioni", "Argovia", "Turgovia", "Ticino", "Vaud",
                "Vallese", "Neuchâtel", "Ginevra", "Giura"),
    stringsAsFactors = FALSE
  )
  
  return(canton_ref)
}

#' Get canton reference data
#' @return A dataframe containing canton reference information
get_canton_ref <- function() {
  # Cache the reference data in the global environment
  if (!exists("CANTON_REF", envir = .GlobalEnv)) {
    assign("CANTON_REF", create_canton_reference(), envir = .GlobalEnv)
  }
  return(get("CANTON_REF", envir = .GlobalEnv))
}

#' Get canton name from abbreviation
#' @param abbr Canton abbreviation (e.g., "ZH")
#' @param language Language code ("de", "fr", or "it")
#' @return Canton name in specified language
get_canton_name <- function(abbr, language = "de") {
  ref <- get_canton_ref()
  name_col <- paste0("name_", language)
  result <- ref[ref$abbr == toupper(abbr), name_col]
  if (length(result) == 0) return(NA)
  return(result)
}

#' Get canton abbreviation from ID
#' @param id Canton ID (1-26)
#' @return Canton abbreviation
get_canton_abbr <- function(id) {
  ref <- get_canton_ref()
  result <- ref[ref$canton_id == id, "abbr"]
  if (length(result) == 0) return(NA)
  return(result)
}

#' Get canton ID from abbreviation
#' @param abbr Canton abbreviation (e.g., "ZH")
#' @return Canton ID (1-26)
get_canton_id <- function(abbr) {
  ref <- get_canton_ref()
  result <- ref[ref$abbr == toupper(abbr), "canton_id"]
  if (length(result) == 0) return(NA)
  return(result)
}

#' Validate canton identifier
#' @param identifier Canton ID, abbreviation, or name
#' @return TRUE if valid, FALSE otherwise
is_valid_canton <- function(identifier) {
  ref <- get_canton_ref()
  
  # Check if it's an ID
  if (is.numeric(identifier)) {
    return(identifier %in% ref$canton_id)
  }
  
  # Check if it's an abbreviation
  if (nchar(identifier) == 2) {
    return(toupper(identifier) %in% ref$abbr)
  }
  
  # Check if it's a name in any language
  return(identifier %in% c(ref$name_de, ref$name_fr, ref$name_it))
}

#' Standardize canton identifier to abbreviation
#' @param identifier Canton ID, abbreviation, or name
#' @return Standardized canton abbreviation or NA if invalid
standardize_canton <- function(identifier) {
  ref <- get_canton_ref()
  
  # If it's already an abbreviation
  if (is.character(identifier) && nchar(identifier) == 2) {
    return(if (toupper(identifier) %in% ref$abbr) toupper(identifier) else NA)
  }
  
  # If it's an ID
  if (is.numeric(identifier)) {
    return(get_canton_abbr(identifier))
  }
  
  # If it's a name, check all language versions
  for (lang in c("de", "fr", "it")) {
    name_col <- paste0("name_", lang)
    idx <- which(ref[[name_col]] == identifier)
    if (length(idx) > 0) {
      return(ref$abbr[idx])
    }
  }
  
  return(NA)
}

#' Get all canton names for a given canton
#' @param identifier Canton ID, abbreviation, or name
#' @return A list of names in all languages
get_all_canton_names <- function(identifier) {
  ref <- get_canton_ref()
  
  # First standardize to abbreviation
  abbr <- standardize_canton(identifier)
  if (is.na(abbr)) return(NULL)
  
  # Get the row for this canton
  canton_row <- ref[ref$abbr == abbr, ]
  
  # Return a list of names
  return(list(
    abbr = canton_row$abbr,
    german = canton_row$name_de,
    french = canton_row$name_fr,
    italian = canton_row$name_it
  ))
} 