# Read data from CSV file.
# Sourced from 
# https://exoplanetarchive.ipac.caltech.edu/docs/program_interfaces.html
dataKOI <- read_csv("nph-nstedAPI.csv")

library(DT)
library(htmltools)

dat <- data.frame(
  x = c(0, 1, 2, 3, 4),
  id = c("sub0", "sub0", "sub1", "sub1", "sub2")
)

dtable <- datatable(
  dat,
  options = list(
    dom = "Qlfrtip",
    searchBuilder = list(
      columns = list(2) # 2 is the index of the 'id' column
    )
  )
)

path_to_searchBuilder <- # path to the folder containing the two searchBuilder files
  normalizePath(".")

dep <- htmlDependency(
  name = "searchBuilder",
  version = "1.0.0", 
  src = path_to_searchBuilder,
  script = "dataTables.searchBuilder.min.js",
  stylesheet = "searchBuilder.dataTables.min.css",
  all_files = FALSE
)

dtable$dependencies <- c(dtable$dependencies, list(dep))

dtable

