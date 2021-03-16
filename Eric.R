library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(tidyverse)
library(ggcorrplot)
library(plotly)
library(GGally)

### Data
wine <- read.csv("data/processed/wine_quality.csv")
corr_df <- read.csv("data/processed/correlation.csv")

### R messed up column names so I'm trying to fix
variables <- colnames(subset(wine, select = -c(Wine, Quality.Factor, Quality.Factor.Numeric)))
variablesNoUnits <- gsub("\\..\\..*","", variables)
variablesNoUnits <- gsub("(..mg.dm)*","", variablesNoUnits)
variablesNoUnits <- gsub("\\.","", variablesNoUnits)

app = Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(dbcContainer(
  dbcRow(list(
    dbcCol(list(
      htmlH4("Choose Factor Levels"),
      dbcRow(list(
        dbcCol(list(
          htmlH5("Quality"),
          dccChecklist(id = "quality",
                       options = list(
                         list("label" = "Below Average", "value" = 0),
                         list("label" = "Average", "value" = 1),
                         list("label" = "Above Average", "value" = 2)
                       ),
                       value = list(0,1,2)
          )
        )),
        dbcCol(list(
          htmlH5("Wine Type"),
          dccChecklist(id = "winetype",
                       options = list(
                         list("label" = "White Wines", "value" = "white"),
                         list("label" = "Red Wines", "value" = "red")
                       ),
                       value=list("red", "white")
          )
        ))
      )),
      dccGraph(
        id = "matrix")
    )),
    dbcCol(list(
      htmlH4("Choose Scatterplot Axes"),
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
                    list("label" = "Alcohol Percent", "value" = variables[11]),
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
    if (quality == 1){ # The correlation plot breaks if only average quality chosen,
      # since there is only one value
      winex <- subset(winex, select = -c(Quality))
    }
    # Janky regex to remove periods and units to make things more readable
    colnames(winex) <- gsub("\\..\\..*","", colnames(winex))
    colnames(winex) <- gsub("(..mg.dm)*","", colnames(winex))
    colnames(winex) <- gsub("\\.","", colnames(winex))
    # Create a correlation matrix and reorder it alphabetically
    corr <- cor(winex)
    order <- corrplot::corrMatOrder(corr, "alphabet")
    corr <- corr[order,order]
    p <-
      ggcorrplot(corr,
                 hc.order = TRUE,
                 type = "lower",
                 outline.color = "white",
                 color = c("darkblue", "lightgray", "darkred"))
    ggplotly(p, height = 500, width = 500)
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
      scale_fill_gradient(low="lightgray", high = "darkred") +
      theme_minimal() +
      geom_smooth(method = lm)
    ggplotly(p, height = 500, width = 500)
  }
)


app$run_server(debug = T)
