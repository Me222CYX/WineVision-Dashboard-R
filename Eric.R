library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(tidyverse)
library(ggcorrplot)
library(plotly)

### Data
wine <- read.csv("data/processed/wine_quality.csv")
corr_df <- read.csv("data/processed/correlation.csv")

### Get list of column names for dropdown
variables <- colnames(subset(wine, select = -c(Wine, Quality.Factor, Quality.Factor.Numeric)))

app = Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(dbcContainer(
  dbcRow(list(
    dbcCol(list(
      dbcRow(list(
        htmlH5("Quality"),
        dccChecklist(id = "quality",
                     options = list(
                       list("label" = "Below Average", "value" = 0),
                       list("label" = "Average", "value" = 1),
                       list("label" = "Above Average", "value" = 2)
                     ),
                     value = list(0,1,2)
        ),
        htmlH5("Wine Type"),
        dccChecklist(id = "winetype",
                     options = list(
                       list("label" = "White Wines", "value" = "white"),
                       list("label" = "Red Wines", "value" = "red")
                     ),
                     value=list("red", "white")
        ))),
      dccGraph(
        id = "matrix")
    )),
    dbcCol(list(
      htmlH5("x-axis"),
      dccDropdown(id = "x-axis",
                  options = list( #Couldn't figure out a list comprehension alternative
                    list("label" = variables[1], "value" = variables[1]),
                    list("label" = variables[2], "value" = variables[2]),
                    list("label" = variables[3], "value" = variables[3]),
                    list("label" = variables[4], "value" = variables[4]),
                    list("label" = variables[5], "value" = variables[5]),
                    list("label" = variables[6], "value" = variables[6]),
                    list("label" = variables[7], "value" = variables[7]),
                    list("label" = variables[8], "value" = variables[8]),
                    list("label" = variables[9], "value" = variables[9]),
                    list("label" = variables[10], "value" = variables[10]),
                    list("label" = variables[11], "value" = variables[11]),
                    list("label" = variables[12], "value" = variables[12])
                  ),
                  value = variables[2]),
      htmlH5("y-axis"),
      dccDropdown(
        id = "y-axis",
        options = list( #Couldn't figure out a list comprehension alternative
          list("label" = variables[1], "value" = variables[1]),
          list("label" = variables[2], "value" = variables[2]),
          list("label" = variables[3], "value" = variables[3]),
          list("label" = variables[4], "value" = variables[4]),
          list("label" = variables[5], "value" = variables[5]),
          list("label" = variables[6], "value" = variables[6]),
          list("label" = variables[7], "value" = variables[7]),
          list("label" = variables[8], "value" = variables[8]),
          list("label" = variables[9], "value" = variables[9]),
          list("label" = variables[10], "value" = variables[10]),
          list("label" = variables[11], "value" = variables[11]),
          list("label" = variables[12], "value" = variables[12])
        ),
        value = variables[1]
      ),
      dccGraph(
        id = "scatter",
        figure = {})
    ))
  ))
))

# Make Graphs

app$callback(
  output("matrix", "figure"),
  list(input("winetype", "value"),
       input("quality", "value")),
  function(winetype, quality){
    # Subset to our desired variable levels
    winex <- subset(wine, Wine %in% winetype)
    winex <- subset(winex, Quality.Factor.Numeric %in% quality)
    winex <- subset(winex, select = -c(Wine, Quality.Factor, Quality.Factor.Numeric))
    # Create a correlation matrix
    corr <- cor(winex)
    p <-
      ggcorrplot(corr,
        hc.order = TRUE,
        type = "lower",
        outline.color = "white",
        color = c("darkred", "white", "darkred"))
    ggplotly(p)
  }
)

app$callback(
  output("scatter", "figure"),
  params = list(input("x-axis", "value"),
       input("y-axis", "value"),
       input("winetype", "value"),
       input("quality", "value")),
  function(x, y, winetype, quality){
    # Subset to our desired variable levels
    winex <- subset(wine, Wine %in% winetype)
    winex <- subset(winex, Quality.Factor.Numeric %in% quality)
    p <- ggplot(winex, aes(x = !!sym(x), y = !!sym(y))) + geom_bin2d() +
      geom_smooth(method = lm)
    ggplotly(p)
  }
)


app$run_server(host = '0.0.0.0')
