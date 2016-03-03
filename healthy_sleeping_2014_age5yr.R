#:-----------------------------------------------------------------------------:
# Create a choropleth map for US states by prevalance of healthy sleeping
# duration (>= 7 hrs per 24 hour day) for adults in 2014 using CDC BRFSS data.
#
# Copyright: Brian High (https://github.com/brianhigh)
# License: GNU GPL v3, CC BY-SA 4.0 International, or GNU GFDL v1.3
#          You may choose any one of these, or higher versions thereof.
#:-----------------------------------------------------------------------------:
#
# Tested to work on OS X Yosemite (10.10.5), R version 3.2.3 (2015-12-10).
# Requires "pdftotext". See: http://www.foolabs.com/xpdf/download.html
#
# Data source: http://www.cdc.gov/brfss/annual_data/annual_2014.html
#
# This is an effort to reproduce the choropleth map in this paper:
#
# Liu Y, Wheaton AG, Chapman DP, Cunningham TJ, Lu H, Croft JB. Prevalence of
# Healthy Sleep Duration among Adults -- United States, 2014. MMWR Morb Mortal
# Wkly Rep 2016;65:137-141. DOI: http://dx.doi.org/10.15585/mmwr.mm6506a1.
#
# The figure to reproduce is titled:
#
# FIGURE. Age-adjusted percentage of adults who reported >=7 hours of sleep
# per 24-hour period, by state -- Behavioral Risk Factor Surveillance System,
# United States, 2014
#
# For maximum reproducibility and portability, data will be downloaded from
# the web and processed within this script, with no additional steps or other
# resources required. This script will get a zipped, SAS data file from the
# CDC and extract and import it. It will also get the codebook for this dataset
# from the CDC as a PDF and extract states and their codes from it. Lastly,
# 2000 US Standard Population "Distribution #9" will be extracted from an
# CDC pdf file. Repeated execution of this script will used cached files.

#:-----------------------------------------------------------------------------:
# Clear workspace and load packages
#:-----------------------------------------------------------------------------:

# Clear the workspace, unless you are running in knitr context.
# See: https://support.rstudio.com/hc/en-us/articles/200552276
# Example: rmarkdown::render("healthy_sleeping_2014.R", "pdf_document")
if (!isTRUE(getOption('knitr.in.progress'))) {
    closeAllConnections()
    rm(list = ls())
}

# Create a vector of package names for the packages we will need.
pkgs <- c(
    "foreign",
    "data.table",
    "dplyr",
    "tm",
    "XML",
    "epitools",
    "Hmisc",
    "sp",
    "maps",
    "choroplethrMaps",
    "choroplethr",
    "RColorBrewer",
    "ggplot2",
    "grid",
    "gridExtra"
)

# Install packages (if necessary).
for (pkg in pkgs) {
    if (!suppressWarnings(require(pkg, character.only = TRUE))) {
        install.packages(pkg, repos = "http://cran.fhcrc.org",
                         dependencies = TRUE)
        if (!suppressWarnings(require(pkg, character.only = TRUE))) {
            stop(paste(c(
                "Can't load package: ", pkg, "!"
            ), sep = ""))
        }
    }
}

# Create the data folder if it does not already exist.
datadir <- "data"
dir.create(file.path(datadir),
           showWarnings = FALSE,
           recursive = TRUE)

#:-----------------------------------------------------------------------------:
# Get the data
#:-----------------------------------------------------------------------------:

# Use cached data files, if present, or download and extract as needed.
# Note: If you want the script to get all the data directly from the sources
#       on the web, just delete the "data" folder before running this script.
allsleepersfile <- 'data/state_age5yr_sleep_2014.csv'
if (!file.exists(allsleepersfile)) {
    # Download the BRFSS data for 2014 from the CDC as a zipped SAS file.
    dataurl <-
        'http://www.cdc.gov/brfss/annual_data/2014/files/LLCP2014XPT.ZIP'
    datafile <- 'data/LLCP2014XPT.ZIP'
    if (!file.exists(datafile)) {
        download.file(dataurl, datafile, mode = 'wb')
    }
    
    # Extract the SAS data file from the zip file.
    sasfile <- 'data/LLCP2014.XPT'
    if (!file.exists(sasfile)) {
        unzip(datafile, exdir = 'data')
        # Remove the space character at the end of filename if necessary.
        # This was found to be an issue in the 2014 BRFSS SAS data file.
        if (file.exists(paste(sasfile, " ", sep = ""))) {
            file.rename(from = paste(sasfile, " ", sep = ""),
                        to = sasfile)
        }
    }
    
    # Import SAS file. Use data.table for improved performance over data.frame.
    # Note:  For SAS variables that start with the underscore character, R will
    # put an X at the beginning of the variable name as the data is imported.
    brfss <- as.data.table(read.xport(sasfile))
    
    # Subset by the data we need to create the choropleth and export to CSV.
    all.sleepers <- brfss[, list(X_STATE, X_AGEG5YR, SLEPTIM1)]
    write.csv(all.sleepers, allsleepersfile, row.names = FALSE,
              fileEncoding = "UTF-8")
    
    # Close brfss data.table to conserve memory.
    rm(brfss)
} else {
    # Read the CSV into a data.table.
    all.sleepers <-
        as.data.table(read.csv(allsleepersfile, header = TRUE))
}

#:-----------------------------------------------------------------------------:
# Perform sanity checks and subsetting. Compare respondent counts with article.
#:-----------------------------------------------------------------------------:

# Check the number of rows in the dataset. The BRFSS 2014 Survey Data page
# (http://www.cdc.gov/brfss/annual_data/annual_2014.html), under "Data Files"
# states, "There are 464,664 records for 2014."
nrow(all.sleepers)

# Subset the data.table for only those repondents which reported sleep time.
# In other words, exclude "Don't know/Not Sure" and "Refused".
all.sleepers <- all.sleepers[SLEPTIM1 <= 24,]

# Count respondents who reported hours of sleep per day.
# Check that this is the same value as reported in the CDC codebook, page 14.
# http://www.cdc.gov/brfss/annual_data/2014/pdf/codebook14_llcp.pdf
# Codebook reports: 458172 ("Number of hours [1-24]", "Frequency")
nrow(all.sleepers)

# Subset the data.table for only those respondents in the 50 states and DC.
# In other words, exclude Guam (X_STATE == 66) and Puerto Rico (X_STATE == 72).
all.sleepers <- all.sleepers[X_STATE != 66 & X_STATE != 72,]

# Count adult respondents in the 50 states and District of Columbia.
# The authors of the paper write, "CDC analyzed data from the 2014 Behavioral
# Risk Factor Surveillance System (BRFSS) to determine the prevalence of a
# healthy sleep duration (>=7 hours) among 444,306 adult respondents in all
# 50 states and the District of Columbia."
num.resp <- all.sleepers[X_AGEG5YR <= 13, .N]
num.resp

# Sanity checks show respondent counts match BRFSS products and the CDC paper.

#:-----------------------------------------------------------------------------:
# Aggregate by state and age to get counts and prevalence of healthy sleepers
#:-----------------------------------------------------------------------------:

# Exctract the sleeping health (SLEPTIM1), count total respondents and also the
# respondents who sleep at least 7 hours a day (SLEPTIM1 >= 7) by state and age.
sleepers <- all.sleepers[X_AGEG5YR <= 13,
                         list(Respondents = .N,
                              HealthySleepers = sum(SLEPTIM1 >= 7,
                                                    na.rm = TRUE)),
                         by = list(X_STATE, X_AGEG5YR)]

# Order by state and age.
sleepers <- sleepers[order(X_STATE, X_AGEG5YR)]

# Rename variables.
names(sleepers) <-
    c("StateNum", "AgeGrp", "Respondents", "HealthySleepers")

# Get some information about the dataset.
str(sleepers)

# View first and last rows.
sleepers

# View the healthy sleepers and respondents by age group and crude prevalence.
# Compare "Respondents" to Table 1., "Age group (yrs)", "No." in the CDC paper.
sleepers %>% transform(group = cut(
    AgeGrp,
    breaks = c(1, 2, 4, 6, 10, Inf),
    right = FALSE,
    include.lowest = TRUE
)) %>%
    group_by(group) %>%
    summarize_each(funs(sum), HealthySleepers, Respondents) %>%
    rename(AgeGroup = group) %>%
    mutate(CrudePrevalence = HealthySleepers / Respondents) -> prevalence
age.groups <- c("18-24", "25-34", "35-44", "45-64", ">=65")
levels(prevalence$AgeGroup) <- age.groups
prevalence

# Respondents matches the "Unweighted sample of respondents" by age group. 
# CrudePrevalence does *not* match the "%" column for age groups in Table 1.

#:-----------------------------------------------------------------------------:
# Get US standard population totals by age group to use for age-adjustment
#:-----------------------------------------------------------------------------:

# Get the 2000 US Standard Population (Census P25-1130), Distribution #9,
# to be used for age-adjustment. http://wonder.cdc.gov/wonder/help/faq.html#6

# Parse "Age Adjustment Using the 2000 Projected U.S. Population" by Richard J.
# Klein, M.P.H., and Charlotte A. Schoenborn, M.P.H. to get Distribution #9.
# See: http://www.cdc.gov/nchs/data/statnt/statnt20.pdf
#      "Table 2: Selected age distributions and age-adjustment weights
#      based on the 2000 projected U.S. population", Distribution #9.
#
# Note: readPDF() requires that you have pdftotext installed somewhere on your
#       system in your PATH so that R will be able to execute it. You may find
#       pdftotext at: http://www.foolabs.com/xpdf/download.html

# Download the age distribution tables from the CDC as a PDF file.
stdpopurl <-
    'http://www.cdc.gov/nchs/data/statnt/statnt20.pdf'
stdpopfile <- 'data/statnt20.pdf'
if (!file.exists(stdpopfile)) {
    download.file(stdpopurl, stdpopfile, mode = 'wb')
}

stdpopcsvfile <- 'data/stdpop.csv'
if (!file.exists(stdpopcsvfile)) {
    # Read PDF into a list.
    pdf <- readPDF(control = list(text = "-layout"))(
        elem = list(uri = stdpopfile),
        language = "en",
        id = "id1"
    )
    
    # Read the Distribution #9 table into a vector of strings.
    stdpop.raw <- content(pdf)[162:166]
    
    # Parse the lines to get age groups, population, and weight into CSV format.
    stdpop.csv <-
        gsub(
            "^(\\d+).*years.*\\D+(\\d+),(\\d+)\\D+([0-9.]+).*year.*$",
            "\\1,\\2\\3,\\4",
            stdpop.raw
        )
    
    # Read into a data.table, label columns and write to a file for later use.
    stdpop <- fread(paste(stdpop.csv, collapse = "\n"),
                    sep = ",",
                    header = FALSE)
    
    names(stdpop) <- c("AgeGroup", "StdPop", "Weight")
    stdpop$AgeGroup <- age.groups
    write.csv(stdpop, stdpopcsvfile, row.names = FALSE, fileEncoding = "UTF-8")
} else {
    # Read the CSV into a data.table.
    stdpop <- as.data.table(read.csv(stdpopcsvfile, header = TRUE))
}

# Set the variable type to factor.
stdpop$AgeGroup <- factor(stdpop$AgeGroup)

# Check age-adjusted prevalence of adult respondents who sleep >= 7 hours/day.
# The CDC authors say, "A total of 65.2% of respondents reported a healthy 
# sleep duration [>=7 hours]."
with(inner_join(prevalence, stdpop), 
     ageadjust.direct(count=HealthySleepers, pop=Respondents, stdpop=StdPop, 
                      conf.level = 0.95))
     
#:-----------------------------------------------------------------------------:
# Get state names to match up with the state codes in the BRFSS dataset
#:-----------------------------------------------------------------------------:

# Download the BRFSS codebook for 2014 from the CDC as a PDF file.
codebkurl <-
    'http://www.cdc.gov/brfss/annual_data/2014/pdf/codebook14_llcp.pdf'
codebkfile <- 'data/codebook14_llcp.pdf'
if (!file.exists(codebkfile)) {
    download.file(codebkurl, codebkfile, mode = 'wb')
}

# Parse the codebook to get the state names and their codes.
#
# Note: readPDF() requires that you have pdftotext installed somewhere on your
#       system in your PATH so that R will be able to execute it. You may find
#       pdftotext at: http://www.foolabs.com/xpdf/download.html
statesfile <- 'data/states_list.csv'
if (!file.exists(statesfile)) {
    # Read PDF into a list.
    pdf <- readPDF(control = list(text = "-layout"))(
        elem = list(uri = codebkfile),
        language = "en",
        id = "id1"
    )
    
    # Read the content into a vector of strings. (Adjust index as needed.)
    states.raw <- content(pdf)[19:89]
    
    # Filter out the lines which do not contain states and their codes.
    states.str <-
        states.raw[grepl("^\\s{2,6}\\d{1,}\\s+", states.raw)]
    
    # Parse the lines to get codes and state names into CSV format.
    states.csv <-
        gsub("^\\s+(\\d{1,2})\\s+(\\w+(?:\\s\\w+){,2}).*$",
             "\\1,\\2",
             states.str)
    
    # Read into a data.table, label columns and write to a file for later use.
    states <- fread(paste(states.csv, collapse = "\n"),
                    sep = ",",
                    header = FALSE)
    names(states) <- c("StateNum", "State")
    write.csv(states, statesfile, row.names = FALSE, fileEncoding = "UTF-8")
    
} else {
    # Read the CSV into a data.table.
    states <- as.data.table(read.csv(statesfile, header = TRUE))
}

#:-----------------------------------------------------------------------------:
# Calculate age-adjusted prevalence of healthy sleep duration by state
#:-----------------------------------------------------------------------------:

# The authors of the original paper report that:
#
# "The age-adjusted prevalence and 95% confidence interval (CI) of the
# recommended healthy sleep duration (>=7 hours) was calculated by state and
# selected characteristics, and adjusted to the 2000 projected U.S. population
# aged >= 18 years."
#
# There are several age variables in the dataset. Here is a summary of them
# taken from Summary Matrix of Calculated Variables (CV) in the 2014 Data File,
# http://www.cdc.gov/brfss/annual_data/2014/summary_matrix_14_version12.html
#
# Description or Result                          Output Variables
# of Calculation                                 (In Final Data Set)
# -------------------------------------------    -------------------
# 18-24 and then 5-year age groupings to 80+.    _AGEG5YR
# 18-64 and 65+ age groupings.                   _AGE65YR
# 18-64 and 65+ 10 year age groupings.           _AGE_G
# 18-80 age groupings (80=80+)                   _AGE80
#
# We will perform age-adjustment of the crude prevalence of healthy sleep
# duration among adults using the _AGEG5YR variable grouped by state (_STATE).
#
# Although the authors of the CDC article did not specify which age variable
# was used, we have shown (above) that using the _AGE5YR variable allows us to
# calculate matching respondent counts.

# Collapse the groups to match the standard population distribution.
sleepers %>% transform(group = cut(
    AgeGrp,
    breaks = c(1, 2, 4, 6, 10, Inf),
    right = FALSE,
    include.lowest = TRUE
)) %>% rename(AgeGroup = group) -> sleepers.grp
levels(sleepers.grp$AgeGroup) <- age.groups

# Compare the count of respondents and 2000 US Standard Population by age.
# Merge with standard population distribution and plot with ggplot.
sleepers.grp[, lapply(.SD, sum), by = AgeGroup] %>%
    inner_join(stdpop, by = "AgeGroup") %>%
    melt("AgeGroup", c("Respondents", "StdPop"), "Type", "Count") %>%
    ggplot(., aes(AgeGroup, Count, group = 1)) + geom_point() + geom_line() +
    facet_grid(Type ~ ., scales = "free_y") +
    ggtitle("Respondents and Standard Population by Age Group")

# Perform the merge again, but by state and age group.
sleepers.pop <- inner_join(sleepers.grp, stdpop, by = "AgeGroup")

# Create a wrapper function around the `ageadjust.direct()` function from
# the epitools package.
adjustAge <- function(state.num, data) {
    # Age-adjust for respondents in a specific state.
    adj <- as.vector(with(
        data[data$StateNum == state.num,],
        ageadjust.direct(
            count = HealthySleepers,
            pop = Respondents,
            stdpop = StdPop,
            conf.level = 0.95
        )
    ))
    adj['StateNum'] <- state.num
    names(adj) <-
        c("crude.rate", "adj.rate", "lci", "uci", "StateNum")
    return(adj)
}

# Calculate the age adjustment for the US standard population, 2000.
sleepers.adj <- sapply(unique(sleepers.pop$StateNum),
                       function(x)
                           adjustAge(x, sleepers.pop)) %>% t %>% as.data.table

# Store the adjusted prevalence as a percentage in a new column, "value".
sleepers.adj %>% mutate(value = adj.rate * 100) %>%
    select(StateNum, value) -> sleep.state

#:-----------------------------------------------------------------------------:
# Prepare map values data table and look it over before plotting
#:-----------------------------------------------------------------------------:

# Calculate crude prevalence of healthy sleepers per state as "value".
# Note: Uncomment the next command to plot the non-age-adjusted prevalence.
# sleep.state <-
#     sleepers[, list(value = 100 * sum(HealthySleepers) / sum(Respondents)),
#              by = StateNum]

# Prepare states data.table for use with cloroplethr by adding "region" column.
states[,region := tolower(State)]

# Merge in the state names for use when making the choropleth map.
inner_join(states, sleep.state, by = "StateNum") %>%
    select(region, value) -> map.values

# View the first and last few rows and a summary.
# Compare with the values in the State and % columns of TABLE 2 from
# the article: http://www.cdc.gov/mmwr/volumes/65/wr/mm6506a1.htm
head(map.values)
tail(map.values)
summary(map.values)

# List the groups that will be used in the 5-color choropleth map.
map.values[order(map.values$value),] %>%
    mutate(group = cut2(value, g = 5)) %>% group_by(group) %>%
    dplyr::summarize(regions = n(),
                     mean_value = round(mean(value), 1)) -> groups
groups

# D.C. will be too small on the map to see. Find it's group and print as text.
map.values %>% transform(group = cut2(value, g = 5)) %>%
    filter(region == "district of columbia") -> dc
dc

# Find color of DC as it would appear in the 5-level "Blues" choropleth map.
dc$color <-
    brewer.pal(5, "Blues")[which(groups$group %in% dc$group)]

#:-----------------------------------------------------------------------------:
# Create choropleth map
#:-----------------------------------------------------------------------------:

# Make the choropleth map with choroplethr, using 5 age-ranges.
choro <- StateChoropleth$new(map.values)
choro$set_num_colors(5)

# By default, the map labels states with values from the "region" variable.
# Remove the state labels to match the original map from the article.
choro$show_labels <- FALSE

# Render the map with reversed, relocated, and resized legend.
us.sleep.map <-
    choro$render() + guides(fill = guide_legend(reverse = TRUE)) +
    theme(legend.position = c(0.87, 0.17),
          legend.key.size = unit(.60, "cm"))

# Add DC to the map as a square floating offshore above the legend.
us.sleep.map <- us.sleep.map +
    geom_point(
        data = dc,
        shape = 15,
        size = 5,
        color = dc$color,
        aes(x = -73, y = 37)
    ) +
    geom_text(data = dc, aes(x = -71, y = 37, label = "DC"))

# Display the map.
print(us.sleep.map)

#:-----------------------------------------------------------------------------:
# Save the map as a PNG file with title and data source credit (as footer)
#:-----------------------------------------------------------------------------:

# Define plot title and data source (footer).
plot.title <- paste(
    "Age-adjusted percentage of adults who reported",
    "\n",
    "at least 7 hours of sleep per 24-hour period, by state"
)
data.src <-
    "Behavioral Risk Factor Surveillance System, United States, 2014"

# Layout the figure with source attribution string at bottom of plot area.
gmap <- arrangeGrob(
    us.sleep.map,
    top = textGrob(
        plot.title,
        x = 0,
        hjust = -.09,
        vjust = 1,
        gp = gpar(fontface = "plain", fontsize = 17)
    ),
    bottom = textGrob(
        data.src,
        x = 0,
        hjust = -.16,
        vjust = .25,
        gp = gpar(fontface = "italic", fontsize = 12)
    )
)

# Save map as a PNG file.
ggsave(
    filename = 'healthy_sleepers_by_state_2014.png',
    plot = gmap,
    width = 6.5,
    height = 5,
    units = 'in'
)
