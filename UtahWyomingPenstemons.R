library(data.table)
library(stringr)
#library(jsonlite)

#PENSTEMON_JSON = "UtahWyomingPenstemons.JSON"
PENSTEMON_DB_FILENAME = "Penstemons.txt"
PENSTEMON_NAMES_FILENAME = "PenstemonAlternateNames.txt"
COUNTY_FIPS_FILENAME = "UTWY_CountyFIPSCodes.txt" #"CountyFIPSCodes.txt"
STATE_FIPS_FILENAME = "StateFIPSCodes.txt"

DB_PATH = paste0(getwd(), "/", PENSTEMON_DB_FILENAME)
ALT_NAMES_PATH = paste0(getwd(), "/", PENSTEMON_NAMES_FILENAME)
COUNTY_FIPS_PATH = paste0(getwd(), "/", COUNTY_FIPS_FILENAME)
STATE_FIPS_PATH = paste0(getwd(), "/", STATE_FIPS_FILENAME)

PLANT_DB <-NA
    PLANT_STATES <- data.table(PlantID = character(), StateCode = character())
    PLANT_COUNTIES <- data.table(PlantID = character(), FIPS = integer())
    BLOOM_COLORS <- data.table(PlantID = character(), BloomColor = character())
    BLOOM_MONTHS <- data.table(PlantID = character(), BloomMonth = integer())
PLANT_NAMES <- data.table(Name = character(), 
                          Language = character(),
                          PlantID = character())
COUNTY_DATA <- NA
COUNTY_DEFAULT <- 99999

WORKING_DATA <- NA # The set of PlantIDs that match the selection criteria
WORKING_FIPS <- NA # The set of county FIPS codes that match selection criteria

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
    if(is.na(COUNTY_DATA)) COUNTY_DATA <<- fread(COUNTY_FIPS_PATH)
    if( (is.null(COUNTY_DATA) || is.na(COUNTY_DATA)) ) ERR <<- TRUE
         
    if(!ERR) {
        state_fips <- fread(STATE_FIPS_PATH)
        if( (is.null(state_fips) || is.na(state_fips)) ) ERR <<- TRUE
        else {
            COUNTY_DATA[, StateName := state_fips[STUSAB == .BY, STATE_NAME], by = list(State)]
            COUNTY_DATA[, StateFIPS := state_fips[STUSAB == .BY, STATE], by = list(State)]
            COUNTY_DATA[, InputName := paste(State, ":", Name)]
            rm(state_fips)
        }
    }
}

# Read in the plant database file; expand, and cache certain list columns.
if((!ERR & is.na(PLANT_DB))) {
#    PLANT_DB <<- fread(DB_PATH,
#                           key = "PlantID")
    # Grab the column header
    header <- fread(DB_PATH, nrows = 1)
    
    # Retrieve only species found in Utah or Wyoming
    command <- sprintf(
        "grep -e 'UT' -e 'WY' %s", DB_PATH)
    
    PLANT_DB <<- fread(cmd = command,
                           col.names = names(header),
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
            counties <- as.integer(unlist(str_split(countiesString, ",")))
        else
            counties = COUNTY_DEFAULT
        PLANT_COUNTIES <<- rbind(PLANT_COUNTIES, 
                               data.table(PlantID = id, FIPS = counties))
    }
    mapply(extractCounties, PLANT_DB$PlantID, PLANT_DB$CountyCodes)
    
    # Restrict FIPS data to states/territories that have penstemon species.
    COUNTY_DATA <<- COUNTY_DATA[State %chin% unique(PLANT_STATES$StateCode)]
    
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

resetWorkingData <- function() {
    states <- sort(unique(PLANT_STATES$StateCode))
    workingData <- PLANT_DB[StateCode == states[1]]
}

languages <- function() {
    unique(PLANT_NAMES$Language)
}

states <- function() {
    return(c("Utah", "Wyoming"))
}

countiesForStates <- function(selectedStates) {
    if((is.na(selectedStates) || is.null(selectedStates))) {
        return(c("<All>"))
    } else {
        WORKING_FIPS_DATA <<- COUNTY_DATA[State %chin% selectedStates, .(FIPS, State)]
        return(COUNTY_DATA[State %chin% selectedStates, InputName])
    }
}

plantNames <- function() {
    WORKING_DB <<- PLANT_COUNTIES[CountyCode %in% WORKING_FIPS_DATA]
}