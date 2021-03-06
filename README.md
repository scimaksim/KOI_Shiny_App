# Exploring Kepler Objects of Interest (KOI)

This application provides summaries and statistical models for observations in the [cumulative](https://exoplanetarchive.ipac.caltech.edu/docs/API_kepcandidate_columns.html) table of Kepler Objects of Interest (KOI) in [NASA's Exoplanet Archive](https://exoplanetarchive.ipac.caltech.edu/index.html).

![screen-gif](https://github.com/scimaksim/NASA_KOI/blob/main/screen.gif)

## Package requirements

To install the requisite packages for this application, run

```
install.packages(c("shiny", "shinydashboard", "dashboardthemes",
"DT", "htmltools", "latex2exp", "tidyverse", "ggrepel", "grid",
"caret", "lares", "data.table", "rpart.plot", "randomForest", "glmnet", "Cairo", "ggplot2", "ggcorrplot"))
```

## Launch instructions

To launch this application in R, run 

```
shiny::runGitHub("KOI_Shiny_App", "scimaksim", ref = "main")
```

Alternatively, this application can be accessed at https://scimaksim.shinyapps.io/nasa_koi/.