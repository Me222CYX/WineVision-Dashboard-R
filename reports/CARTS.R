library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(tidyverse)
library(ggcorrplot)
library(plotly)
library(tree)
library(R3port)
library(ggplot2)
library(ggtree)
install.packages("ggtree")
wine <- read.csv("data/raw/wine_quality.csv")

variables <- colnames(subset(wine, select = -c(Wine, Quality.Factor)))
variablesNoUnits <- gsub("\\..\\..*","", variables)
variablesNoUnits <- gsub("(..mg.dm)*","", variablesNoUnits)
variablesNoUnits <- gsub("\\.","", variablesNoUnits)

head(wine)
#######3
wine
winex <- subset(wine, Wine == "red")
# Create subset df using only Quality.Factor and our chosen predictor variables
tree.variables<-c("pH","Alcohol")
preds <- wine[tree.variables]
Quality.Factor <- as.factor(wine$Quality.Factor)
winex <- cbind(Quality.Factor, preds)
# Create tree object using chosen predictors
wine.tree <- tree(Quality.Factor~., data = winex)
tree <- rtree(30)
wine.tree$terms 
ggtree(plot(wine.tree))
wine.tree
plot(wine.tree)
text(wine.tree )
########33

app = Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(dbcContainer(
  dbcRow(
    dbcCol(list( # Variable selection
      htmlH5("Physiochemical Properties"),
      dccDropdown(id = "variable-select",
                  options = colnames(wine)[2:12] %>% purrr::map(function(col) list(label = col, value = which(colnames(wine)==col))),
                  value = c(3,9, 12),
                  multi = T),
      htmlH5("Wine Type"),
      dccRadioItems(id = "winetype",
                    options = list(
                      list("label" = "White Wines", "value" = "white"),
                      list("label" = "Red Wines", "value" = "red")
                    ),
                    value="red"
      ),
      htmlImg(
        id = "treepng", src = "/assets/tree.png", width = "100%", height = "600px"
      )
    ))
  )
))


# I cant figure out how to regenerate the png image in a callback##
# Can't embed a graph because this tree package doesn't work with plotly##
app$callback(
  output("treepng", "srce"),
  params = list(input("winetype", "value"),
                input("variable-select", "value")),
  function(winetype, tree.variables){
    # Subset to our desired winetype
    winex <- subset(wine, Wine == winetype)
    # Create subset df using only Quality.Factor and our chosen predictor variables
    preds <- wine[tree.variables]
    Quality.Factor <- as.factor(wine$Quality.Factor)
    winex <- cbind(Quality.Factor, preds)
    # Create tree object using chosen predictors
    wine.tree <- tree(Quality.Factor~., data = winex)

    # If the tree is a single node tree (no splits) we get an error when we try to plot it
    if(wine.tree$frame$splits[1] == ""){
      return("information.jpeg")
    } else {
      plottree <- function(wine.tree){
        plot(wine.tree)
        text(wine.tree)
      }
      png(filename = "tree.png", width = 500, height = 500)
      plottree(wine.tree)
      dev.off()
    }

  }
)

app$run_server(debug = T)
