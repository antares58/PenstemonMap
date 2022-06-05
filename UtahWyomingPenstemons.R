library(data.table)
library(stringr)
#library(jsonlite)

#PENSTEMON_JSON = "UtahWyomingPenstemons.JSON"
PENSTEMON_DB_FILENAME = "Penstemons.txt"
PENSTEMON_NAMES_FILENAME = "PenstemonAlternateNames.txt"
COUNTY_FIPS_FILENAME = "CountyFIPSCodes.txt"
STATE_FIPS_FILENAME = "StateFIPSCodes.txt"

DB_PATH = paste0(getwd(), "/", PENSTEMON_DB_FILENAME)
ALT_NAMES_PATH = paste0(getwd(), "/", PENSTEMON_NAMES_FILENAME)
COUNTY_FIPS_PATH = paste0(getwd(), "/", COUNTY_FIPS_FILENAME)
STATE_FIPS_PATH = paste0(getwd(), "/", STATE_FIPS_FILENAME)

PLANT_DB <-NA
    PLANT_STATES <- data.table(PlantID = character(), StateCode = character())
    PLANT_COUNTIES <- data.table(PlantID = character(), CountyCode = character())
    BLOOM_COLORS <- data.table(PlantID = character(), BloomColor = character())
    BLOOM_MONTHS <- data.table(PlantID = character(), BloomMonth = integer())
PLANT_NAMES <- data.table(Name = character(), 
                          Language = character(),
                          PlantID = character())
COUNTY_FIPS_DATA <- NA
STATE_FIPS_DATA <- NA

# Control flow for reading data from disk
ERR <- FALSE

# Make sure the penstemon data file is accessible.
tryCatch(
    expr = {
        stopifnot(file.exists(DB_PATH))
    },
    error = function(e) {
        ERR <<- TRUE
        message(paste0("Penstemon data file \'", DB_PATH, "\' not found."))
    }
)
# Make sure the penstemon names configuration file is accessible

# Make sure the alternate names configuration file is accessible
tryCatch(
    expr = {
        stopifnot(file.exists(ALT_NAMES_PATH))
    },
    error = function(e) {
        ERR <<- TRUE
        message(paste0("Penstemon names file \'", ALT_NAMES_PATH, "\' not found."))
    }
)
# Check for county FIPS code data file.
tryCatch(
    expr = {
        stopifnot(file.exists(COUNTY_FIPS_PATH))
    },
    error = function(e) {
        ERR <<- TRUE
        message(paste0("County data file \'", COUNTY_FIPS_PATH, "\' not found."))
    }
)
# Check for state FIPS code data file.
tryCatch(
    expr = {
        stopifnot(file.exists(STATE_FIPS_PATH))
    },
    error = function(e) {
        ERR <<- TRUE
        message(paste0("State data file \'", STATE_FIPS_PATH, "\' not found."))
    }
)

# Read in state and county FIPS data
if(!ERR) {
    if(is.na(COUNTY_FIPS_DATA)) COUNTY_FIPS_DATA <<- fread(COUNTY_FIPS_PATH)
    if(is.na(STATE_FIPS_DATA)) STATE_FIPS_DATA <<- fread(STATE_FIPS_PATH)
    if( (is.null(COUNTY_FIPS_DATA) || is.na(COUNTY_FIPS_DATA) || 
        is.null(STATE_FIPS_DATA) || is.na(STATE_FIPS_DATA)) ) ERR <<- TRUE
}

# Read in the plant database file; expand, and cache certain list columns.
if((!ERR & is.na(PLANT_DB))) {
    PLANT_DB <<- fread(DB_PATH,
                           key = "PlantID")
    
    # Extract state distribution data into a tidy data table by PlantID
    extractStates <- function(id, stateString) {
        states <- trimws(unlist(str_split(stateString, ",")))
        PLANT_STATES <<- rbind(PLANT_STATES, 
              data.table(PlantID = id, StateCode = states))
    }
    mapply(extractStates, PLANT_DB$PlantID, PLANT_DB$StateCodes)
    
    # Extract county distribution data into a tidy data table by PlantID
    extractCounties <- function(id, countiesString) {
        if(any(nzchar(countiesString)))
            counties <- trimws(unlist(str_split(countiesString, ",")))
        else
            counties = "all"
        PLANT_COUNTIES <<- rbind(PLANT_COUNTIES, 
                               data.table(PlantID = id, CountyCode = counties))
    }
    mapply(extractCounties, PLANT_DB$PlantID, PLANT_DB$CountyCodes)
    
    # Extract bloom colors data into a tidy data table by PlantID
    extractColors <- function(id, colorsString) {
        colors <- trimws(unlist(str_split(colorsString, ",")))
        BLOOM_COLORS <<- rbind(BLOOM_COLORS, 
                               data.table(PlantID = id, BloomColor = colors))
    }
    mapply(extractColors, PLANT_DB$PlantID, PLANT_DB$BloomColors)
    
    # Extract bloom months data into a tidy data table by PlantID
    extractMonths <- function(id, monthsString) {
        months <- as.integer(unlist(str_split(monthsString, ",")))
        BLOOM_MONTHS <<- rbind(BLOOM_MONTHS, 
                               data.table(PlantID = id, BloomMonth = months))
    }
    mapply(extractMonths, PLANT_DB$PlantID, PLANT_DB$BloomMonths)
}

# Read in the plant names file and expand into a tidy data set.
if((!ERR & nrow(PLANT_NAMES) == 0)) {
    pnames <<- fread(ALT_NAMES_PATH)
    
    if((is.null(pnames) || is.na(pnames))) {
        ERR <<- TRUE
    }
    else {
        # Convert vector strings to vectors and store in tidy data table
        extractIDs <- function(name, language, idString) {
            plantIds <- trimws(unlist(str_split(idString, ",")))
            PLANT_NAMES <<- rbind(PLANT_NAMES, 
                                   data.table(Name = name,
                                              Language = language,
                                              PlantID = plantIds))
        }
        x <- mapply(extractIDs, pnames$Name, pnames$Language, pnames$PlantIDs)
        rm(x, pnames)
        
        # Add the preferred Latin names from the penstemon database
        PLANT_NAMES <<- rbind(data.table(Name = PLANT_DB$SpeciesName, 
                                   Language = rep_len("Latin", nrow(PLANT_DB)),
                                   PlantID = PLANT_DB$PlantID), PLANT_NAMES)
    }
}




