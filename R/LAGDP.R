#' @import dplyr purrr tidyxl readxl
library(readxl)
library(tidyxl)
library(purrr)
library(dplyr)

TEMP_PATH <- "./data/gdp_temp/"

#' Get GDP List from File
#'
#' `getGDP` returns a list of States and their associated Gross Domestic Product(GDP)
#'
#' @includeRmd vignettes/get-gdp.Rmd
#' @return A List of GDP data.frame data keyed by State
#' @export
getGDP <-
  function(filepath = "./data/lagdp1219.xlsx",
           sheet = "Table 1",
           save = FALSE) {
    # GDP Filename To Import
    GDP_XLSX_FILE_NAME <- filepath
    GDP_XLSX_SHEET_NAME <- sheet

    # Cells with Formatting Data
    cells <-
      tidyxl::xlsx_cells(GDP_XLSX_FILE_NAME, GDP_XLSX_SHEET_NAME)

    # Get a vector of bold characters from the XL sheet
    bold_formats <-
      tidyxl::xlsx_formats(GDP_XLSX_FILE_NAME, GDP_XLSX_SHEET_NAME)$local$font$bold

    # Map the Bold Formats in order to extract it's Cell Address and Character data
    bold_formats_map <-
      cells[cells$local_format_id %in% which(bold_formats),
            c("address", "character")]

    # Column Names for Raw Data
    col_names = c(
      "county",
      "2015",
      "2016",
      "2017",
      "2018",
      "rTotal",
      "c2016",
      "c2017",
      "c2018",
      "rChange"
    )

    # Raw XLSX File Data
    data <-
      readxl::read_excel(GDP_XLSX_FILE_NAME,
                         sheet = GDP_XLSX_SHEET_NAME,
                         col_names = col_names)

    # States List
    States <- getStateNames()

    # Keyed List of State Names
    Dictionary <- vector(mode = "list", length = length(States))
    names(Dictionary) <- States

    # Find the Raw Data Index from the Bold Formats Mapping. This matches
    # the character column to the state names vector and strips out the column id
    Start <- purrr::map(States, function(x) {
      # Always return an Integer of the Index in the Raw Data
      as.integer(# Remove the Column ID
        gsub("A", "", as.character(# Find the Current State in the Formats Mapping
          bold_formats_map[match(x, bold_formats_map[["character"]]), "address"])))

    })

    # Key the Vector on State Names
    names(Start) <- States

    # Reorder Start Indices
    Start <- Start[order(unlist(Start), decreasing = FALSE)]

    # Create Full Indices from the Start Index
    Indices <-
      as.data.frame(dplyr::tibble(
        State = names(Start),
        Start,
        Next = dplyr::lead(Start)
      ))

    # Assign the last Index's Next Column to the total length of the XLSX Data
    Indices[length(Start), 3] <- nrow(data)

    # For every Index, Assign the Data to the Dictionary for future lookup
    for (row in 1:nrow(Indices)) {
      s <- as.integer(Indices[row, "Start"][[1]])
      # Finish
      f <- as.integer(Indices[row, "Next"][[1]])

      # Get Names of the Indices
      iNames <- names(Indices)

      # Assign the data to the Dictionary, strip out state and tailing data
      Dictionary[[States[[row]]]] <-
        head(data %>% slice(s:f),-2)[-1, ]

    }

    # Return the Dictionary to the Caller
    Dictionary
  }

#' Get a State and their associated Gross Domestic Product(GDP)
#'
#' @return GDP data.frame
#' @export
getStateGDP <- function(state, save = FALSE) {
  if (save) {
    if (file.exists(getStatePath(state))) {
      answer <-
        readline(
          prompt = paste0(
            "File Already Exists!!! Are you sure you want to override: ",
            getStatePath(state),
            "? Y/N: "
          )
        )
      if (answer != "Y" & answer != "y") {
        stop("File exists, exited without saving.")
      }
    }
    return(saveStateGDP(state))
  }
  else if (file.exists(getStatePath(state))) {
    load(file = getStatePath(state))
    return(get(paste0(getStateShortCode(state), "_GDP")))
  }
  else {
    return(getGDP()[[state]])
  }
}

getStatePath <- function(stateName) {
  paste0(TEMP_PATH, getStateShortCode(stateName), "/", "GDP.Rda")
}

saveStateGDP <- function (stateName) {
  # Create Temp Directory
  if (!file.exists(paste0(TEMP_PATH, getStateShortCode(stateName)))) {
    dir.create(paste0(TEMP_PATH, getStateShortCode(stateName)))
  }


  # Assign a Variable Name (Used to reload ENV)
  varName <- paste0(getStateShortCode(stateName), "_GDP")

  assign(varName, getStateGDP(stateName, save = FALSE))

  save(list = varName,
       file = paste0(TEMP_PATH,
                     getStateShortCode(stateName),
                     "/",
                     "GDP.Rda"))
  return(get(varName))
}
