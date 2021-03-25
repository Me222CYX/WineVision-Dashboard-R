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
library(survMisc)
library(rpart)
library("grid")
library("ggplotify")
wine <- read.csv("data/raw/wine_quality.csv")

variables <- colnames(subset(wine, select = -c(Wine, Quality.Factor)))
variablesNoUnits <- gsub("\\..\\..*","", variables)
variablesNoUnits <- gsub("(..mg.dm)*","", variablesNoUnits)
variablesNoUnits <- gsub("\\.","", variablesNoUnits)

head(wine)
#######3
# wine
# winex <- subset(wine, Wine == "red")
# # Create subset df using only Quality.Factor and our chosen predictor variables
# tree.variables<-c("pH","Alcohol")
# preds <- wine[tree.variables]
# Quality.Factor <- as.factor(wine$Quality.Factor)
# winex <- cbind(Quality.Factor, preds)
# # Create tree object using chosen predictors
# wine.tree <- rpart(Quality.Factor~., data = winex)
# tree <- rtree(30)
# wine.tree$frame
# autoplot(wine.tree, compress=TRUE, branch=0.5, nspace=0.1,
#          title="Nodes show events / no. at risk")
# autoplot(wine.tree)
# as.Node(wine.tree$frame)
# ggtree(plot(wine.tree))
# wine.tree
# plot(wine.tree)
# text(wine.tree )


library(rpart)
library(survMisc)
library(ggdendro)




########33



app = Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(dbcContainer(
  dbcRow(
    dbcCol(list( # Variable selection
      htmlH5("Physiochemical Properties"),
      dccDropdown(id = "variable-select",
                  options = colnames(wine)[2:11] %>% purrr::map(function(col) list(label = col, value = which(colnames(wine)==col))),
                  value = c(3,9, 11),
                  multi = T),
      htmlH5("Wine Type"),
      dccRadioItems(id = "winetype",
                    options = list(
                      list("label" = "White Wines", "value" = "white"),
                      list("label" = "Red Wines", "value" = "red")
                    ),
                    value="red"
      ),
      
      htmlDiv(list(dccGraph(id = "tree"))),
      htmlDiv(id="tree2")

    ))
  )
)

)


colnames(wine)[2:11] %>% purrr::map(function(col) list(label = col, value = which(colnames(wine)==col)))

app$layout( dbcContainer(
  dbcRow(
    dbcCol(list( # Variable selection
      htmlH5("Physiochemical Properties"),
      dccDropdown(id = "variable-select",
                  options = colnames(wine)[2:11] %>% purrr::map(function(col) list(label = col, value = which(colnames(wine)==col))),
                  value = c(3,9, 11),
                  multi = T),
      htmlH5("Wine Type"),
      dccRadioItems(id = "winetype",
                    options = list(
                      list("label" = "White Wines", "value" = "white"),
                      list("label" = "Red Wines", "value" = "red")
                    ),
                    value="red"
      ),
      
      htmlDiv(list(dccGraph(id = "tree")))
      
    ))
  )
))

colnames(wine)[2:11] %>% purrr::map(function(col) list(label = col, value = which(colnames(wine)==col)))
# I cant figure out how to regenerate the png image in a callback##
# Can't embed a graph because this tree package doesn't work with plotly##


app$callback(
  output=list(id='tree2', property='children'),
  params=list(input(id='winetype', property='value')),
  function(input_value) {
    sprintf("You've entered \"%s\"", input_value)
  })

# coln<-colnames(wine)[2:11]
# names(coln)<-2:10
app$callback(
  output("tree", "figure"),
  params = list(input("winetype", "value"),
                input("variable-select", "value")),
  function(winetype, tree.variables){
    winetype<-unlist(winetype)
    tree.variables<-unlist(tree.variables)
    # Subset to our desired winetype
    winex <- subset(wine, Wine == winetype)
    coln<-colnames(wine)[2:11]
    names(coln)<-2:11
    coln[tree.variables-1]%>%as.vector()->sub
    # Create subset df using only Quality.Factor and our chosen predictor variables
    preds <- wine%>%select(sub)
    Quality.Factor <- as.factor(wine$Quality.Factor)
    winex <- cbind(Quality.Factor, preds)
    # Create tree object using chosen predictors

    wine.tree <- rpart(Quality.Factor~., data = winex)
    fitr <- dendro_data(wine.tree)
    p<-ggplot()+
        geom_segment(data = fitr$segments,
                     aes(x = x, y = y, xend = xend, yend = yend)
        ) +
        geom_text(data = fitr$labels, aes(x = x, y = y, label = label)) +
        geom_text(data = fitr$leaf_labels, aes(x = x, y = y, label = label)) +
        theme_dendro()
    
    plot <- ggplotly(p)
    plot <- plot %>% layout(
      paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
    
    plot

  }
)




app$run_server(debug = F)
