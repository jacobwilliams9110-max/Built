# Define @importFrom utils globalVariables
utils::globalVariables(c("MONTH", "STATE", "year", "n"))
#' Week 2 Functions Explanation
#'
#' @param filename A description of input filename
#' @return Decription of output
#' Read FARS CSV file
#'
#' Function tries to reach a CSV file about National Highway Safety
#' Administration data and loads it into R session as \code{tibble} or data
#' frame

#'
#' @return a data frame referring to \code{tibble} that contains FARS data
#'Give an error message "file filename does not exist"
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2014.csv.bz2")
#' }
#'#' @export
#' @param filename character string providing path and name of CSV file
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Make a standardized FARS filename
#'
#' Function creates a character string that shows the filename for a FARS Data
#' frame file giving the year. Example= \code{"accident_2015.csv.bz2"}.
#'
#' @param year provides a numeric value that represents the target year. It can
#' convert other formats into an integer.
#'
#' @return Returns a character string formatted as
#' \code{"accident_[year].csv.bz2"}
#'
#' @examples
#' make_filename(2015)
#' make_filename(2014)
#'
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Reads FARS data for multiple years at a time
#'
#' Function takes a vector, creates the corresponding FARS filenames, and uploads
#' data for the years.
#'
#' @param years numeric vector, list, or array of years
#'
#' @return A list of data frames containing the MONTH and year
#' columns \code{NULL} for list item
#'gives message saying "invalid year:year"
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{
#' fars_read_years(c(2014, 2015))
#' }
#'
#' @export

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarizes FARS data by year and month
#'
#' @param years is a numeric vector of years to be summarized
#'
#' @return A wide data frame (\code{tibble}) that summarizes number of accidents
#'  for that year (columns)
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2014, 2015))}
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plots FARS accident locations on the map
#'
#' Function will filter for specific state and year
#'
#' @param state.num integer that represents unique code
#' @param year numeric value specifying year
#'
#' @return Returns \code{invisible(NULL)} and gives a geographical plot
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' fars_map_state(36. 2015)
#' }
#'
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
