library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(tidyverse)
library(ggcorrplot)
library(plotly)

### Data
wine <- read.csv("~/Documents/Masters/block5/551/WineVision-R-Group8/data/processed/wine_quality.csv")

### Get list of column names for dropdown
variables <- colnames(subset(wine, select = -c(Wine, Quality.Factor, Quality.Factor.Numeric)))

    # Subset out non-numeric columns
    winex <- subset(wine, select = -c(Wine, Quality.Factor, Quality.Factor.Numeric))
    # Create a correlation matrix
    corr <- cor(winex)
    p <-
      ggcorrplot(corr,
                 hc.order = TRUE,
                 type = "lower",
                 outline.color = "white",
                 color = c("darkred", "white", "darkred"))

app <-  Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      dccGraph(id = "graph", figure = ggplotly(p))
    )
  )
)

app$run_server(host = '0.0.0.0')