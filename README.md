# NASA - Kepler Objects of Interest (KOI)

This application explores the [cumulative](https://exoplanetarchive.ipac.caltech.edu/docs/API_kepcandidate_columns.html) database of Kepler Objects of Interest (KOI) in the [NASA Exoplanet Archive](https://exoplanetarchive.ipac.caltech.edu/index.html).

## Package requirements

To install the requisite packages for this application, run

```
install.packages(c("shiny", "shinydashboard", "dashboardthemes",
"DT", "htmltools", "latex2exp", "tidyverse", "ggrepel", "grid",
"caret", "lares", "data.table", "rpart.plot"))
```

## Launch instructions

To launch this application in R, run 

```
shiny::runGitHub("NASA_KOI", "scimaksim", ref = "main")`
```