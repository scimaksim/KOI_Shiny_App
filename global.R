# KOI Exploration
# global.R
# Authored by Maksim Nikiforov
# NCSU ST 558 - Fall, 2021

library(DT)
library(htmltools)
library(caret)
library(tidyverse)

# Read data from CSV file.
# Sourced from 
# https://exoplanetarchive.ipac.caltech.edu/docs/program_interfaces.html

# Optional API call - retrieve all available columns
# dataKOI <- read_csv("https://exoplanetarchive.ipac.caltech.edu/cgi-bin/nstedAPI/nph-nstedAPI?table=cumulative&select=*")

# Read in all data (default columns and otherwise) from CSV file
dataKOI <- read_csv("nph-nstedAPI_all.csv")

# Read in data only with default columns
defaultValKOI <- read_csv("nph-nstedAPI.csv")

# Subset columns to exclude "CANDIDATE", "NOT DISPOSITIONED" Kepler objects,
# remove columns containing "err" (most contains "NA"s) and columns with names.
# Drop rows with missing values. 
filteredKOI <- defaultValKOI %>% filter(koi_disposition == "CONFIRMED" | koi_disposition == "FALSE POSITIVE") %>%
  select(-contains("err"), -kepler_name, -koi_score, -koi_tce_delivname) %>% drop_na()

# Filter data to include only "CANDIDATE" objects. 
# These will be used as test data for supervised learning models
filteredCandidateKOI <- defaultValKOI %>% filter(koi_disposition == "CANDIDATE") %>%
  select(-contains("err"), -kepler_name, -koi_score, -koi_tce_delivname) %>% drop_na()

# Create dummy variables (0/1) for koi_disposition ("FALSE POSITIVE"/"CONFIRMED")
dummies <- dummyVars(" ~ koi_disposition", data = filteredKOI)
modelingData <- predict(dummies, newdata = filteredKOI)
modelingData <- as_tibble(modelingData)
filteredKOI$koi_disposition_binary <- as.factor(modelingData$koi_dispositionCONFIRMED)

# Create dummy variables (0/1) for koi_disposition ("CANDIDATE")
#dummyCandidates <- dummyVars(" ~ koi_disposition", data = filteredCandidateKOI)
#candidateData <- predict(dummyCandidates, newdata = filteredCandidateKOI)
#candidateData <- as_tibble(candidateData)
#filteredCandidateKOI$koi_disposition_binary <- as.factor(candidateData$koi_dispositionCANDIDATE)

# Include SearchBuilder extension 
dat <- data.frame(
  x = c(0, 1, 2, 3, 4),
  id = c("sub0", "sub0", "sub1", "sub1", "sub2")
)

# Optional search builder for data tables 
dtable <- datatable(
  dat,
  options = list(
    dom = "Qlfrtip",
    searchBuilder = list(
      columns = list(2) # 2 is the index of the 'id' column
    )
  )
)

# Path to the folder containing the two searchBuilder files
path_to_searchBuilder <- 
  normalizePath(".")

# Datatatble searchBuilder dependencies
dep <- htmlDependency(
  name = "searchBuilder",
  version = "1.0.0", 
  src = path_to_searchBuilder,
  script = "dataTables.searchBuilder.min.js",
  stylesheet = "searchBuilder.dataTables.min.css",
  all_files = FALSE
)

dtable$dependencies <- c(dtable$dependencies, list(dep))