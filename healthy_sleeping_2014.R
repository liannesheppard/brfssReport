# ----------------------------------------------------------------------------
# Create a choropleth map for US states by prevalance of healthy sleeping 
# duration (>= 7 hs per 24 hour day) for adults in 2014 using CDC BRFSS data.
#
# Copyright: Brian High (https://github.com/brianhigh)
# License: GNU GPL v3 http://www.gnu.org/licenses/gpl.txt
# ----------------------------------------------------------------------------
#
# Tested to work on Windows 10, OSX Yosimite, and Ubuntu 14.04 LTS.
# Requires "pdftotext". See: http://www.foolabs.com/xpdf/download.html 
#
# Data source: http://www.cdc.gov/brfss/annual_data/annual_2014.html
#
# This is an effort to reproduce the choropleth map in this paper:
#
# Liu Y, Wheaton AG, Chapman DP, Cunningham TJ, Lu H, Croft JB. Prevalence of 
# Healthy Sleep Duration among Adults — United States, 2014. MMWR Morb Mortal 
# Wkly Rep 2016;65:137–141. DOI: http://dx.doi.org/10.15585/mmwr.mm6506a1.
#
# The figure to reproduce is titled:
#
# FIGURE. Age-adjusted percentage of adults who reported ≥7 hours of sleep 
# per 24-hour period, by state — Behavioral Risk Factor Surveillance System, 
# United States, 2014
#
# For maximum reproducibility and portability, data will be downloaded from
# the web and processed within this script, with no additional steps or other
# resources required. This script will get a zipped, SAS data file from the 
# CDC and extract and import it. It will also get the codebook for this dataset
# from the CDC as a PDF and extract states and their codes from it. Lastly, 
# 2000 US Standard Population figures for ages 0-99 will be extracted from an
# NIH webpage. Subsequent execution of this script will used cached files.

# Clear the workspace.
closeAllConnections()
rm(list=ls())

# Install packages (if necessary).
for (pkg in c("foreign", "data.table", "dplyr", "tm", "XML", "epitools", 
              "choroplethrMaps", "choroplethr", "ggplot2")) {
    if (! suppressWarnings(require(pkg, character.only=TRUE)) ) {
        install.packages(pkg, repos="http://cran.fhcrc.org", dependencies=TRUE)
        if (! suppressWarnings(require(pkg, character.only=TRUE)) ) {
            stop(paste(c("Can't load package: ", pkg, "!"), sep = ""))
        }
    }
}

# Create the data folder if it does not already exist.
datadir <- "data"
dir.create(file.path(datadir), showWarnings=FALSE, recursive=TRUE)

# Use cached data files, if present, or download and extract as needed.
allsleepersfile <- 'data/state_age_sleep_2014.csv'
if (! file.exists(allsleepersfile)) {
    # Download the BRFSS data for 2014 from the CDC as a zipped SAS file.
    dataurl <- 'http://www.cdc.gov/brfss/annual_data/2014/files/LLCP2014XPT.ZIP'
    datafile <- 'data/LLCP2014XPT.ZIP'
    if (! file.exists(datafile)) {
        download.file(dataurl, datafile, mode='wb')
    }
    
    # Extract the SAS data file from the zip file.
    sasfile <- 'data/LLCP2014.XPT'
    if (! file.exists(sasfile)) {
        unzip(datafile, exdir='data')
        # Remove the space character at the end of filename if necessary.
        # This was found to be an issue in the 2014 BRFSS SAS data file.
        if (file.exists(paste(sasfile, " ", sep=""))) {
            file.rename(from=paste(sasfile, " ", sep=""), to=sasfile)
        }
    }
    
    # Import SAS file. Use data.table for improved performance over data.frame.
    # Note:  For SAS variables that start with the underscore character, R will 
    # put an X at the beginning of the variable name as the data is imported.
    brfss <- as.data.table(read.xport(sasfile))
    
    # Subset by the data we need to create the choropleth and export to CSV.
    all.sleepers <- brfss[, list(X_STATE, X_AGE80, SLEPTIM1)]
    write.csv(all.sleepers, allsleepersfile, row.names=FALSE)
    
    # Close brfss data.table to conserve memory.
    rm(brfss)
} else {
    # Read the CSV into a data.table.
    all.sleepers <- as.data.table(read.csv(allsleepersfile, header=TRUE))
}

# Check the number of rows in the dataset. The BRFSS 2014 Survey Data page
# (http://www.cdc.gov/brfss/annual_data/annual_2014.html), under "Data Files"
# states, "There are 464,664 records for 2014."
nrow(all.sleepers)

# Count respondents who reported hours of sleep per day.
# Check that this is the same value as reported in the CDC codebook, page 14.
# http://www.cdc.gov/brfss/annual_data/2014/pdf/codebook14_llcp.pdf
# Codebook reports: 458172 ("Number of hours [1-24]", "Frequency")
all.sleepers[SLEPTIM1 <= 24, .N] 

# Count respondents who get at least 7 hours of sleep per day.
# Check that this is the same value as reported in the article, TABLE 1.
# http://www.cdc.gov/mmwr/volumes/65/wr/mm6506a1.htm
# Article reports: 444306 ("Total", "No.")
all.sleepers[SLEPTIM1 >= 7 & SLEPTIM1 <= 24, .N] 

# Order by state and age (imputed).
all.sleepers <- all.sleepers[order(X_STATE, X_AGE80)]

# Exctract the sleeping health (SLEPTIM1), excluding Guam (X_STATE == 66) and 
# Puerto Rico (X_STATE == 72), count respondents, and sum respondents who sleep
# at least 7 hours a day (SLEPTIM1 >= 7 & SLEPTIM1 <= 24) by state and age.
sleepers <- all.sleepers[X_AGE80 >= 18 & X_STATE != 66 & X_STATE != 72, 
                  list(Respondents = .N,
                       HealthySleepers = sum(SLEPTIM1 >= 7 & SLEPTIM1 <= 24, 
                                             na.rm = TRUE)),
                  by = list(X_STATE, X_AGE80)]

# Rename variables.
names(sleepers) <- c("StateNum", "Age", "Respondents", "HealthySleepers")

# Get some information about the dataset.
str(sleepers)

# View first and last rows.
sleepers

# View the healthy sleepers and respondents by age group and crude prevalence.
sleepers %>% transform(
    group=cut(Age, breaks=c(18, 25, 35, 45, 65, Inf), 
              right=FALSE, include.lowest=TRUE)) %>% 
    group_by(group) %>% 
    summarize_each(funs(sum), HealthySleepers, Respondents) %>% 
    rename(AgeGroup = group) %>%
    mutate(CrudePrevalence=HealthySleepers/Respondents)

# Download the BRFSS data for 2014 from the CDC as a zipped SAS file.
codebkurl <- 'http://www.cdc.gov/brfss/annual_data/2014/pdf/codebook14_llcp.pdf'
codebkfile <- 'data/codebook14_llcp.pdf'
if (! file.exists(codebkfile)) {
    download.file(codebkurl, codebkfile, mode='wb')
}

# Parse the codebook to get the state names and their codes.
# Note: readPDF() requires that you have pdftotext installed somewhere on your 
#       system in your PATH so that R will be able to execute it. You may find
#       pdftotext at: http://www.foolabs.com/xpdf/download.html
statesfile <- 'data/states_list.csv'
if (! file.exists(statesfile)) {
    
    # Read PDF into a list.
    pdf <- readPDF(control = list(text="-layout"))(elem=list(
        uri=codebkfile), language="en", id="id1")
    
    # Read the content into a vector of strings. (Adjust index as needed.)
    states.raw <- content(pdf)[19:89]
    
    # Filter out the lines which do not contain states and their codes.
    states.str <- states.raw[grepl("^\\s{2,6}\\d{1,}\\s+", states.raw)]
    
    # Parse the lines to get codes and state names into CSV format.
    states.csv <- gsub("^\\s+(\\d{1,2})\\s+(\\w+(?:\\s\\w+){,2}).*$", 
                       "\\1,\\2", states.str)
    
    # Read into a data.frame, label columns and write to a file for later use.
    states <- read.table(text=states.csv, sep=",", header=FALSE)
    names(states) <- c("StateNum", "State")
    write.csv(states, statesfile, row.names=FALSE)
    
} else {
    # Read the CSV into a data.frame.
    states <- read.csv(statesfile, header=TRUE)
}

# Get the 2000 US Standard Population (Census P25-1130), Single Ages to 99,
# to be used for age-adjustment. http://wonder.cdc.gov/wonder/help/faq.html#6
agesfile <- 'data/ages.csv'
if (! file.exists(agesfile)) {
    # Scrape a table from an NIH web page and save as CSV for later.
    agesurl <- 'http://seer.cancer.gov/stdpopulations/stdpop.singleages.html'
    agestbl <- readHTMLTable(agesurl)
    ages <- agestbl[[1]][2:102, c(1, 2)]
    row.names(ages) <- NULL
    names(ages) <- c("Age", "StdPop")
    ages$Age <- factor(as.numeric(
        sapply(ages$Age, function (x) gsub("[+]? years$", "", x))))
    ages$StdPop <- as.numeric(
        sapply(ages$StdPop, function (x) gsub(",", "", x)))
    write.csv(ages, agesfile, row.names=FALSE)
} else {
    # Read the CSV into a data.frame.
    ages <- read.csv(agesfile, header=TRUE)
}

# Merge with "sleepers" with "ages" to get standard population.
sleepers.pop <- merge(sleepers, ages, by="Age")

# View first and last rows.
sleepers.pop

# Create a wrapper function around the `ageadjust.direct()` function from
# the epitools package.
adjustAge <- function(state.num, data) {
    # Age-adjust for respondents in a specific state.
    adj <- as.vector(with(data[data$StateNum == state.num,],
                          ageadjust.direct(count=HealthySleepers,
                                           pop=Respondents, 
                                           stdpop=StdPop, 
                                           conf.level=0.95)))
    adj['StateNum'] <- state.num
    names(adj) <- c("crude.rate", "adj.rate", "lci", "uci", "StateNum")
    return(adj)
}

# Calculate the single-age adjustment for the US standard population, 2000.
sleepers.adj <- sapply(unique(sleepers.pop$StateNum),
                       function(x) adjustAge(x, sleepers.pop)) %>% t %>%
                       as.data.frame

# Store the adjusted prevalence as a percentage in a new column, "value".
sleepers.adj %>% mutate(value=adj.rate*100) %>% 
    select(StateNum, value) -> sleep.state.adj

# Prepare states data.frame for use with cloroplethr by adding "region" column.
states <- mutate(states, region=tolower(State))

# Merge in the state names for use when making the choropleth map.
merge(states, sleep.state.adj) %>% select(region, value) -> map.values

# View the first and last few rows and a summary.
head(map.values)
tail(map.values)
summary(map.values)

# Make the choropleth map with choroplethr, using 5 age-ranges.
plot.title <- paste("Age-adjusted percentage of adults who reported", "\n", 
                    "≥7 hours of sleep per 24-hour period, by state", "\n", 
                    "— Behavioral Risk Factor Surveillance System,", "\n",
                    "United States, 2014")
state.plot <- state_choropleth(map.values, num_colors=5, title=plot.title)

# Save map as a PNG file.
ggsave(filename="healthy_sleepers_by_state_2014.png", plot=state.plot)

# Display the map.
print(state.plot)
