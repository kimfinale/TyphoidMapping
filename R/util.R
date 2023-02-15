tstamp <- function(year=TRUE, month=TRUE, day=TRUE,
                   hour=FALSE, minute=FALSE, second=FALSE) {
  stamp1 <- c()
  stamp2 <- c()
  if (year & !month & !day) {
    stamp <- format(Sys.time(), "%Y")
  } else if (year & month & !day) {
    stamp1 <- format(Sys.time(), "%Y%m")
  } else if (year & month & day) {
    stamp1 <- format(Sys.time(), "%Y%m%d")
  } else if (!year & month & day) {
    stamp1 <- format(Sys.time(), "%m%d")
  } else if (year & !month & day) {
    stamp1 <- format(Sys.time(), "%Y%d")
  } else if (!year & month & !day) {
    stamp1 <- format(Sys.time(), "%m")
  } else if (!year & !month & day) {
    stamp1 <- format(Sys.time(), "%d")
  } else{ stamp1 <- "You'd better select parameters well."}

  if (hour & !minute & !second) {
    stamp2 <- format(Sys.time(), "%H")
  } else if (hour & minute & !second) {
    stamp2 <- format(Sys.time(), "%H%M")
  } else if (hour & minute & second) {
    stamp2 <- format(Sys.time(), "%H%M%S")
  } else if (!hour & minute & !second) {
    stamp2 <- format(Sys.time(), "%M")
  } else if (!hour & !minute & second) {
    stamp2 <- format(Sys.time(), "%S")
  } else if (!hour & minute & second) {
    stamp2 <- format(Sys.time(), "%M%S")
  } else{}

  if (!is.null(stamp2)) {
    stamp1 <- paste0(stamp1, "T", stamp2)
  }
  return (stamp1)
}


#' Clean and standardize country names according to VIMC report templates
#'
#' The \code{clean_country_names()} is used to clean and standardize country names according to VIMC report templates
#' population for both sexes and incidence rate
#' @param country A vector of country names in character
#' @export
#' @examples
#' country <- clean_country_names(country = "DRC")
#' # Congo, the Democratic Republic of the
clean_country_names <- function(country) {
  for (i in 1:length(country)) {
    if (country[i] %in% c("DR Congo", "Democratic Republic of the Congo", "DRC", "Congo, Dem. Rep.", "Congo, DR",  "Congo, the Democratic Republic of the")){
      country[i] <- "Congo, Democratic Republic of the"
    }
    if (country[i] %in% c("Congo, Rep.", "Republic of the Congo", "Congo")){
      country[i] <- "Congo, Republic of the"
    }
    if (country[i] %in% c("São Tomé and Príncipe")){
      country[i] <- "Sao Tome e Principe"
    }
    if (country[i] %in% c("Iran", "Iran, Islamic Rep.", "Iran (Islamic Republic of)")){
      country[i] <- "Iran, Islamic Republic of"
    }
    if (country[i] %in% c("North Korea", "Korea:North", "Korea, DPR", "DPRK", "Democratic People's Republic of Korea", "Korea DPR")){
      country[i] <- "Korea, Democratic People's Republic of"
    }
    if (country[i] %in% c("South Korea", "Korea:South", "Korea, Rep.")){
      country[i] <- "Korea, the Republic of"
    }
    if (country[i] %in% c("Sudan: South")){
      country[i] <- "South Sudan"
    }
    if (country[i] %in% c("Sudan: North")){
      country[i] <- "Sudan"
    }
    if (country[i] %in% c("Venezuela", "Venezuela, RB", "Venezuela (Bolivarian Republic of)")){
      country[i] <- "Venezuela, Bolivarian Republic of"
    }
    if (country[i] %in% c("Tanzania", "United Republic of Tanzania")){
      country[i] <- "Tanzania, United Republic of"
    }
    if (country[i] %in% c("Syria")){
      country[i] <- "Syrian Arab Republic"
    }
    if (country[i] %in% c("Moldova", "Republic of Moldova")){
      country[i] <- "Moldova, Republic of"
    }
    if (country[i] %in% c("CAR")){
      country[i] <- "Central African Republic"
    }
    if (country[i] %in% c("Lao", "Laos", "Lao PDR")){
      country[i] <- "Lao People's Democratic Republic"
    }
    if (country[i] %in% c("US", "USA")){
      country[i] <- "United States of America"
    }
    if (country[i] %in% c("C?te d'Ivoire", "CÃ´te d'Ivoire", "Cì²™te d'Ivoire", "Côte d'Ivoire")){
      country[i] <- "Cote d'Ivoire"
    }
    if (country[i] %in% c("Bolivia", "Bolivia (Plurinational State of)")){
      country[i] <- "Bolivia, Plurinational State of"
    }
    if (country[i] %in% c("Cape Verde")){
      country[i] <- "Cabo Verde"
    }
    if (country[i] %in% c("Micronesia", "Micronesia (Federated States of)")){
      country[i] <- "Micronesia, Federated States of"
    }
    if (country[i] %in% c("Sao Tome e Principe")){
      country[i] <- "Sao Tome and Principe"
    }
    if (country[i] %in% c("Vietnam")){
      country[i] <- "Viet Nam"
    }
    if (country[i] %in% c("Eswatini")){ # to be consistent with other data files
      country[i] <- "Swaziland"
    }
  }
  return (country)
}

country_abbr <- function(x){
  x[x == "Central African Republic"] <- "CAR"
  x[x == "Congo, Republic of the"] <- "Congo"
  x[x == "Congo, Democratic Republic of the"] <- "DR Congo"
  x[x == "Tanzania, United Republic of"] <- "Tanzania"

  return(x)
}
