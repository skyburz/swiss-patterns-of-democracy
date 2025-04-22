# Canton name mapping
canton_mapping <- list(
  "ZH" = c("Zürich", "Zurich"),
  "BE" = c("Bern / Berne", "Bern", "Berne"),
  "LU" = c("Luzern", "Lucerne"),
  "UR" = c("Uri"),
  "SZ" = c("Schwyz", "Schwytz", "Schwyz"),
  "OW" = c("Obwalden"),
  "NW" = c("Nidwalden"),
  "GL" = c("Glarus", "Glaris"),
  "ZG" = c("Zug", "Zoug"),
  "FR" = c("Fribourg / Freiburg", "Fribourg", "Freiburg"),
  "SO" = c("Solothurn", "Soleure"),
  "BS" = c("Basel-Stadt", "Bâle-Ville"),
  "BL" = c("Basel-Landschaft", "Bâle-Campagne"),
  "SH" = c("Schaffhausen", "Schaffhouse"),
  "AR" = c("Appenzell Ausserrhoden", "Appenzell Rhodes-Extérieures"),
  "AI" = c("Appenzell Innerrhoden", "Appenzell Rhodes-Intérieures"),
  "SG" = c("St. Gallen", "Saint-Gall"),
  "GR" = c("Graubünden / Grigioni / Grischun", "Graubünden", "Grigioni", "Grischun"),
  "AG" = c("Aargau", "Argovie"),
  "TG" = c("Thurgau", "Thurgovie"),
  "TI" = c("Ticino", "Tessin"),
  "VD" = c("Vaud", "Waadt"),
  "VS" = c("Valais / Wallis", "Valais", "Wallis"),
  "NE" = c("Neuchâtel", "Neuenburg"),
  "GE" = c("Genève", "Genf"),
  "JU" = c("Jura")
)

# Function to get canton abbreviation from full name
get_canton_abbr <- function(full_name) {
  for (abbr in names(canton_mapping)) {
    if (full_name %in% canton_mapping[[abbr]]) {
      return(abbr)
    }
  }
  return(NULL)
}

# Function to get full canton name from abbreviation
get_canton_full_name <- function(abbr) {
  if (abbr %in% names(canton_mapping)) {
    return(canton_mapping[[abbr]][1])  # Return the first (primary) name
  }
  return(NULL)
} 