# Installs dashHtmlComponents, dashCoreComponents, and dashTable
# and will update the component libraries when a new package is released


# Installs dash bootstrap
library(devtools)
library(dash)
library(dashTable)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(dashCoreComponents)

library(plotly)
library(ggplot2)
library(tidyverse)
library(GGally)
library(ggcorrplot)
library(corrplot)
library(readr)
library(stringr)
library(plyr)
library(glue)
library(rpart)
library(ggdendro)


#############################################
## APP AND FUNCTIONAL APP OBJECTS
#############################################

#app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

app = Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$title(string = "Wine Vision")

app$config['suppress_callback_exceptions'] = TRUE


colors <- list(background = 'white', text = 'black')

pageTitle <- htmlH1('Wine Vision', style = list(textAlign = 'left', color = colors$text))

get_header <- function() {
  header = htmlDiv(
    list(
      htmlDiv(
        list(
          htmlDiv(
            htmlP("WineVision Dashboard"),
            className = "seven columns main-title", style = list(marginTop = '1em', background = 'rgba(0,0,0,0)')),
          htmlDiv(
            list(
              dccLink("Machine Learning",
                      href = "/WineVision/Prediction",
                      className = "learn-more-button")),
            className = "twelve columns")
        ),
        className = "twelve columns")
    ),
    className = "row"
  )
  return(header)
}


get_menu <- function() {
  menu = htmlDiv(
    list(
      dccLink(
        "Introduction",
        href="/WineVision/introduction",
        className="tab first"),
      dccLink(
        "Quality Distributions",
        href="/WineVision/Quality-Distributions",
        className="tab"),
      dccLink(
        "Correlation",
        href="/WineVision/Wine-Types",
        className="tab"),
      dccLink(
        "Exploration",
        href="/WineVision/Quality-Factors",
        className="tab"),
      dccLink(
        "Raw Data",
        href="/WineVision/Wine-table",
        className="tab")
    ),
    className="rowrow alltab "
  )
  return(menu)
}

Header <- htmlDiv(list(get_header(), htmlBr(), get_menu()))
Header_banner<-htmlDiv(list(get_header(), htmlBr(), get_menu(),htmlBr() ,htmlImg(
  # https://elite-brands.com/blog/wine-ratings-q3/
  src  =  "/assets/rating.png", width = "100%",
  className = "app__menu__img"
)))
Menu <- htmlDiv(list(get_menu()))

#############################################
## DATA
#############################################


source("data/Data_Compiler (r).R")


#############################################
## APP LAYOUT
#############################################

app$layout(
  htmlDiv(
    list(
      # URL
      dccLocation(id = 'url', refresh=FALSE), # Changed from false
      #Content
      htmlDiv(id='page-content')
    )
  )
)

################################
## Raw Data page

page_size <- 10

summarystats <- read.csv("reports/summarystats.csv")

table_layout<-htmlDiv(list(

  Header_banner,

  htmlBr(),

  htmlH4("Summary Statistics Table:", className = "graph__title"),

  htmlDiv(list(dbcCol(htmlDiv(
    dashDataTable(
      id = "summarystats table",
      style_table = list(overflowX = 'scroll'),
      columns = lapply(colnames(summarystats), 
                      function(colName){
                        list(
                          id = colName,
                          name = colName
                        )
                      }),
      data = df_to_list(summarystats))
  )))),

  htmlBr(),
  
  htmlH4("Raw Data Table:", className = "graph__title"),

  htmlBr(),

  htmlDiv(list(dbcCol(htmlDiv(
    dashDataTable(
      style_table = list(overflowX = 'scroll'),
      id = 'table-sorting-filtering',
      columns = lapply(sort(colnames(wine)), function(colName){list(id = colName,name = colName)}),
      page_current = 0,
      page_size = page_size,
      page_action = 'custom',
                  
      filter_action = 'custom',
      filter_query = '',
                  
      sort_action = 'custom',
      sort_mode = 'multi',
      sort_by = list())
    ), style=list(marginBottom = '15em')
  )))
))


app$callback(
  output = list(id = 'table-sorting-filtering', property = 'data'),
  params = list(input(id = 'table-sorting-filtering', property = 'page_current'),
                input(id = 'table-sorting-filtering', property = 'page_size'),
                input(id = 'table-sorting-filtering', property = 'sort_by'),
                input(id = 'table-sorting-filtering', property = 'filter_query')),
  function(page_current, page_size, sort_by, filters) {
    
    subdf <- wine
    # filter
    if(filters != "") {
      
      conditions <- strsplit(filters, split = "&&")[[1]]
      
      not_show <- lapply(conditions,
                         function(condition) {
                           
                           splited_condition <- strsplit(condition, split = " ")[[1]]
                           # len should be 3
                           len <- length(splited_condition)
                           
                           condition <- if('contains' %in% splited_condition) {
                             
                             splited_condition[which('contains' == splited_condition)] <- "=="
                             
                             if(!grepl("\"", splited_condition[len]) & !grepl("'", splited_condition[len])) {
                               splited_condition[len] <- paste0("'", splited_condition[len], "'")
                             }
                             
                             paste0(splited_condition, collapse = " ")
                           } else if('=' %in% splited_condition) {
                             gsub('=', '==', condition)
                           } else if ('datestartswith' %in% splited_condition) {
                             gsub('datestartswith', '>=', condition)
                           } else condition
                           
                           subdf <<- subdf %>%
                             dplyr::filter(eval(parse(text = condition)))
                         })
    }
    
    # sort
    if(length(sort_by) != 0) {
      
      index <- lapply(sort_by,
                      function(sort){
                        if(sort[['direction']] == "asc") {
                          subdf[, sort[['column_id']]]
                        } else {
                          -xtfrm(subdf[, sort[['column_id']]])
                        }
                      })
      
      # sort by multi columns
      subdf <- subdf[do.call(order, index), ]
    }
    
    start_id <- (page_current * page_size + 1)
    end_id <- ((page_current + 1) * page_size)
    subdf[start_id:end_id, ]
  }
)

################################
## Quality Distributions Page - Luka

Quality_Distribution_layout <- htmlDiv(
  list(
    Header_banner,
    htmlDiv(
      list(
        htmlBr(),
        dbcRow(
          list(
            dbcCol(htmlDiv(), width = 1),
            dbcCol(htmlDiv(htmlH4("Wine Selection:", className = "graph__title")), width = 3),
            dbcCol(htmlDiv(htmlH4("Variable Selection:", className = "graph__title")), width = 3),
            dbcCol(htmlDiv(htmlH4("Statistic Selection:", className = "graph__title")), width = 3),
            dbcCol(htmlDiv(), width = 1)
          ), justify ="center"
        ),
        htmlBr(),
        dbcRow(
          list(
            dbcCol(htmlDiv(), width = 1),
            dbcCol(htmlDiv(dccDropdown(
              id = 'wine-select',
              options = list(list(label = 'White Wine', value = 1), list(label = 'Red Wine', value = 2)),
              value = 1)), width = 3),
            dbcCol(htmlDiv(dccDropdown(
              id = 'col-select',
              options = colnames(wine)[2:12] %>% purrr::map(function(col) list(label = col, value = which(colnames(wine)==col))),
              value = 9)), width = 3),
            dbcCol(htmlDiv(dccDropdown(
              id = 'stat',
              options = list(list(label = 'Mean', value = 'Mean'), list(label = 'Median', value = 'Median'), list(label = 'Mode', value = 'Mode')),
              value = 'Mode')), width = 3),
            dbcCol(htmlDiv(), width = 1)
          ), justify ="center"
        ),
        htmlBr(), htmlBr(),
        dbcRow(
          list(
            dbcCol(dccGraph(id = 'density'), width = 8)
          ), justify="center"
        ),
        htmlBr(),
        dbcRow(
          list(
            dbcCol(dccGraph(id = 'stackeddensity'), width = 8)
          ), justify="center"
        ),
        htmlBr(),
        dbcRow(htmlDiv(style=list(marginBottom = '12em'))),
        htmlBr()
      )
    )
  )
)


app$callback(
  output(id = 'density', property = 'figure'),
  params = list(input(id = 'col-select', 'value'), 
                input(id = 'wine-select', 'value'),
                input(id = 'stat', 'value')),
  #input(id = 'dist-marginal', 'value'),
  
  function(variable, winetype, stat) {
    
    coln <- sym(colnames(wine)[variable])
    
    plot <- ggplot(wine_type[[winetype]], aes(x = !!coln, fill = `Quality Factor`)) + 
      geom_density(alpha = 0.4) + 
      geom_vline(data=stats[[stat]][[winetype]],
                 aes(xintercept=!!coln, color = `Quality Factor`),
                 linetype="dashed", size=0.7) +
      
      # Labels
      ggtitle(glue('Density Type: <b>Overlaid</b>')) +
      xlab(glue('{as.character(coln)} {units[variable]}')) +
      
      # Colour Scheme
      ggthemes::scale_fill_tableau() + scale_color_manual(values = c("#4E79A7", "#F28E2B", "#E15759")) +
      
      # Theme
      theme_classic() + 
      theme(plot.title = element_text(size=14, hjust = 0.01),
            axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.line.y = element_blank(),
            legend.title = element_blank(),
            text = element_text(size = 16),
            element_line(size = 1))
    
    
    plot <- ggplotly(plot)
    plot <- plot %>% layout(
      paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
      legend = list(title=list(text='<b> Quality Levels </b>\n'), x = 0.82, y = 1, itemwidth = 40, tracegroupgap = 12, bgcolor = 'rgba(0,0,0,0)'),
      autosize = FALSE)#,
    #width = 1100, height = 500)
    
    plot
  }
)

app$callback(
  output(id = 'stackeddensity', property = 'figure'),
  params = list(input(id = 'col-select', 'value'), 
                input(id = 'wine-select', 'value'),
                input(id = 'stat', 'value')),
  
  function(variable, winetype, stat) {
    
    coln <- sym(colnames(wine)[variable])
    
    plot <- ggplot(wine_type[[winetype]], aes(x = !!coln, fill = `Quality Factor`)) + 
      geom_density(alpha = 0.4, position="stack") + 
      geom_vline(data=stats[[stat]][[winetype]],
                 aes(xintercept=!!coln, color = `Quality Factor`),
                 linetype="dashed", size=0.7) +
      
      # Labels
      ggtitle(glue('Density Type: <b>Stacked</b>')) +
      xlab(glue('{as.character(coln)} {units[variable]}')) +
      
      # Colour Scheme
      ggthemes::scale_fill_tableau() + scale_color_manual(values = c("#4E79A7", "#F28E2B", "#E15759")) +
      
      # Theme
      theme_classic() + 
      theme(plot.title = element_text(size=14, hjust = 0.01),
            axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.line.y = element_blank(),
            legend.title = element_blank(),
            text = element_text(size = 16),
            element_line(size = 1))
    
    
    plot <- ggplotly(plot)
    plot <- plot %>% layout(
      paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
      legend = list(title=list(text='<b> Quality Levels </b>\n'), x = 0.82, y = 1, itemwidth = 40, tracegroupgap = 12, bgcolor = 'rgba(0,0,0,0)'),
      autosize = FALSE)#,
    #width = 1100, height = 500)
    
    plot
  }
)


## Quality.Factor
################################
## Quality Factor Analysis Page - RAIN


Quality_Factors_layout <- htmlDiv(
  list(
    Header,
    htmlDiv(
      list(
        htmlDiv(
          list(
            htmlDiv(
              list(
                htmlBr(),
                htmlImg(
                  # https://elite-brands.com/blog/wine-ratings-q3/
                  src  =  "/assets/rating.png", width = "100%",
                  className = "app__menu__img"
                )
              ), className = "app__header__logo"
            )
          ), className = "app__header"
        ),
        htmlBr(),
        htmlDiv(
          list(
            # scatter plot
            htmlDiv(
              list(
                htmlDiv(
                  list(
                    htmlH4('Select your variables:'),
                    htmlH5('X-axis'),
                    dccDropdown(
                      id='xcol-select',
                      options = colnames(wine)[2:12] %>% purrr::map(function(col) list(label = col, value = col)),
                      value='Alcohol'),
                    htmlH5('Y-axis'),
                    dccDropdown(
                      id='ycol-select',
                      options = colnames(wine)[2:12] %>% purrr::map(function(col) list(label = col, value = col)), 
                      value='Density'),
                    htmlBr(),
                    htmlH4("Interactive Plots:", className = "graph__title"),
                    htmlBr(),
                    htmlH5("Drag your mouse to select a range!"),
                    htmlBr() 
                  )
                ),
                dccGraph(
                  id = "plot-area"
                )
              ), className = "two-thirds column container"
            ),
            htmlDiv(
              list(
                # bar plot
                htmlDiv(
                  list(
                    htmlDiv(
                      list(
                        htmlH4(
                          "Select your wine type:",
                          className = "graph__title"
                        )
                      )
                    ),
                    htmlDiv(
                      list(
                        dccRadioItems(
                          id = 'wine-type',
                          options = list(list(label = 'White Wine', value = 'white'),
                                         list(label = 'Red Wine', value = 'red')),
                          value = 'white',
                          labelStyle = list(display = 'inline-block')#, class = 'radioItem'
                        )
                      ), className = "radioItem"
                    ),
                    dccGraph(
                      id = "bar-plot"
                    )
                  ), className = "graph__container first"
                ),
                # 2nd bar plot
                htmlBr(),
                htmlDiv(
                  list(
                    htmlDiv(list(htmlH4("% Quality Factors", className = "graph__title"))),
                    dccGraph(
                      id = "bar-plot2")
                  ), className = "graph__container second"
                )
              ), className = "one-third column histogram__direction", style=list(marginBottom = '15em')
            )
          ), className = "app__content"
        )
      ), className = "app__container"
    )
  )
)


app$callback(
  output = list(id='plot-area', property='figure'),
  params = list(input(id='xcol-select', property='value'),
                input(id='ycol-select', property='value'),
                input(id='wine-type', property='value')),
  
  function(xcol, ycol, type) {
    
    numx <- which(colnames(wine) == xcol)
    numy <- which(colnames(wine) == ycol)
    
    colnx <- sym(colnames(wine)[numx])
    colny <- sym(colnames(wine)[numy])
    
    wine_dif <- wine %>% subset(Wine == type)
    scatter <- ggplot(wine_dif) + 
      aes(x = !!sym(xcol), y = !!sym(ycol), color = `Quality Factor`, text = id) + 
      geom_point(alpha = 0.7) + 
      xlab(glue('{as.character(colnx)} {units[numx]}')) +
      ylab(glue('{as.character(colny)} {units[numy]}')) + 
      
      ggthemes::scale_color_tableau() +
      theme_minimal() + theme(text = element_text(size = 14), legend.title = element_blank())
    ggplotly(scatter, tooltip = 'text', height = 650) %>% layout(dragmode = 'select', paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
                                                                 legend = list(title=list(text='<b>Quality Factor</b>'), x = 0.82, y = 1.1, tracegroupgap = 1, bgcolor = 'rgba(0,0,0,0)'))
  }
)


app$callback(
  output = list(id='bar-plot', property='figure'),
  params = list(input(id='plot-area', property='selectedData'),
                input(id='wine-type', property='value')),
  
  function(selected_data, type) {
    wine_dif <- wine %>% subset(Wine == type)
    wine_id <- selected_data[[1]] %>% purrr::map_chr('text')
    p <- ggplot(wine_dif %>% filter(id %in% wine_id)) +
      aes(x = Quality,
          fill = `Quality Factor`) +
      geom_bar(width = 0.6, alpha = 0.7) +
      theme_minimal() +
      theme(legend.title=element_blank(), legend.position="none", text = element_text(size = 14), plot.title = element_text(size=12)) + # removed legends since it squishes the plot. Interactivity on bar plots is minimal to none 
      ggthemes::scale_fill_tableau() +
      ggtitle(glue('<b>Counts in Each Quality Factor</b>'))
    ggplotly(p, tooltip = 'text') %>% layout(dragmode = 'select', paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
  }
)


app$callback(
  output = list(id='bar-plot2', property='figure'),
  params = list(input(id='plot-area', property='selectedData'),
                input(id='wine-type', property='value')),
  
  function(selected_data, type) {
    wine_dif <- wine %>% subset(Wine == type)
    wine_id <- selected_data[[1]] %>% purrr::map_chr('text')
    
    b <- ggplot(wine_dif %>% filter(id %in% wine_id)) +
      aes(x = `Quality Factor`,
          fill = `Quality Factor`) +
      geom_bar(aes(y = (..count..)/sum(..count..)*100), alpha = 0.7) +
      theme_minimal() +
      ylab('% in the Selected Range') + 
      theme(axis.text.x=element_blank(), legend.title=element_blank(), legend.position="none", text = element_text(size = 14), plot.title = element_text(size=12)) + # removed legends since it squishes the plot. Interactivity on bar plots is minimal to none 
      ggthemes::scale_fill_tableau() + 
      ggtitle(glue('<b>Percentage for Each Quality Factor</b>'))
    ggplotly(b, tooltip = 'y') %>% layout(dragmode = 'select', paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
  }
)



################################
## Wine Type Comparison Page - Eric 

Wine_Types_layout <- htmlDiv(
  list(
    Header_banner,
    htmlDiv(
      list(
        htmlBr(),
        dbcContainer(
          dbcRow(list(
            dbcCol(list(
              htmlH4("Choose Factor Levels"),
              dbcRow(list(
                dbcCol(list(
                  htmlH5("Quality"),
                  dccRadioItems(id = "quality",
                                options = list(
                                  list("label" = "Below Average", "value" = 0),
                                  list("label" = "Average", "value" = 1),
                                  list("label" = "Above Average", "value" = 2),
                                  list("label" = "All Levels", "value" = 3)
                                ),
                                value = 3,
                                labelStyle = list("display" = "inline-block"),
                                className = "radioItem"
                  )
                )),
                dbcCol(list(
                  htmlH5("Wine Type"),
                  dccRadioItems(id = "winetype",
                                options = list(
                                  list("label" = "White Wines", "value" = 'white'),
                                  list("label" = "Red Wines", "value" = 'red')
                                ),
                                value= 'white',
                                labelStyle = list("display" = "inline-block"),
                                className = "radioItem"
                  )
                ))
              )),
              htmlBr(), htmlBr(),
              dccGraph(
                id = "matrix")
            )),
            dbcCol(list(
              htmlH4("Choose Scatterplot Axes"),
              htmlH5("X-axis"),
              dccDropdown(id = "x-axis",
                          options = colnames(wine)[2:12] %>% purrr::map(function(col) list(label = col, value = which(colnames(wine)==col))),
                          value = 3),
              htmlH5("Y-axis"),
              dccDropdown(
                id = "y-axis",
                options = colnames(wine)[2:12] %>% purrr::map(function(col) list(label = col, value = which(colnames(wine)==col))),
                value = 9
              ),
              htmlBr(),
              dccGraph(id = "scatter")
            ))
          ))
        ),
        htmlBr()
      ),
      className = "twelve columns", style=list(marginBottom = '15em')
    )
  )
)

# Make Graphs

app$callback(
  output("matrix", "figure"),
  list(input("winetype", "value"),
       input("quality", "value")),
  function(winetype, quality){
    # Subset to our desired variable levels
    winex <- subset(wine, Wine %in% winetype)
    if(quality != 3){ # Quality level 3 is all wine types
      winex <- subset(winex, `Quality Factor Numeric` %in% quality)
    }
    winex <- subset(winex, select = -c(Wine, `Quality Factor`, `Quality Factor Numeric`, `id`))
    if (quality == 1) { # The correlation plot breaks if only average quality chosen since there is only one value (six)
      winex <- subset(winex, select = -c(Quality))
    }
    
    # Create a correlation matrix and reorder it alphabetically
    corr <- cor(winex)
    order <- corrMatOrder(corr, "alphabet")
    corr <- corr[order,order]
    p <- ggcorrplot(corr,
                    hc.order = TRUE,
                    type = "lower",
                    outline.color = "white",
                    color = c("darkblue", "lightgray", "darkred")) + theme(text = element_text(size = 16))
    ggplotly(p, height = 550, width = 550) %>% layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')#margin())
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
    if(quality != 3){ # Quality level 3 is all wine qualities
      winex <- subset(winex, `Quality Factor Numeric` %in% quality)
    }
    
    colx <- sym(colnames(winex)[x])
    coly <- sym(colnames(winex)[y])
    
    p <- ggplot(winex, aes(x = !!colx, y = !!coly)) + geom_bin2d() +
      scale_fill_gradient(low="lightgray", high = "darkred") +
      theme_minimal() + theme(text = element_text(size = 12)) +
      geom_smooth(method = lm) +
      xlab(glue('{as.character(colx)} {units[x]}')) +
      ylab(glue('{as.character(coly)} {units[x]}'))
    
    ggplotly(p, height = 425, width = 425) %>% layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
  }
)

###############################
# Future Additions (Prediction )
###############################
wine233 <- read.csv("data/raw/wine_quality.csv")

tree_card<- dbcCard(
  dbcCardBody(list(
    htmlH4("Classification tree"),
    dbcButton(
      "How to use",
      id="collapse-button",
      className = "mb-3",
      color = "primary"
    ),
    dbcCollapse(
      id="tree-collapse",
      is_open =TRUE,
      dbcCard(dbcCardBody(
        list(
        htmlP("Interpretable machine learning!"),
        htmlP("Select variables of interest, It will fit categorical classification trees to the data."),
        htmlP("If no variables are selected, all variables will be considered as a default.")))
    )
  )
  )
))

classification<-dbcCard(dbcCardBody(list(
  
  dbcCol(list( # Variable selection
    htmlH5("Physiochemical Properties"),
    dccDropdown(id = "variable-select",
                options = colnames(wine233)[2:11] %>% purrr::map(function(col) list(label = col, value = which(colnames(wine233)==col))),
                value = c(3,9, 11),
                multi = T),
    htmlBr(),
    htmlH5("Wine Type"),
    dccRadioItems(id = "winetype",
                  options = list(
                    list("label" = "White Wines", "value" = "white"),
                    list("label" = "Red Wines", "value" = "red")
                  ),
                  value="red"
    ),
    
    htmlDiv(list(
      dbcCol(dccGraph(id = "tree"),width = 20)
    ))
  )
  
  )
  
)
  
)
  
)
prediction_layout<-htmlDiv(
  list(
    Header_banner,
    htmlBr(),
    tree_card,
    htmlBr(),
    classification
  )
)

app$callback(
  list(output("tree-collapse","is_open")),
  params = list(input("collapse-button","n_clicks"),
                state("tree-collapse","is_open")),
  function (n_clicks,is_open){
    return (list(!is_open))
  }
)


app$callback(
  output("tree", "figure"),
  params = list(input("winetype", "value"),
                input("variable-select", "value")),
  function(winetype, tree.variables){
    winetype<-unlist(winetype)
    tree.variables<-unlist(tree.variables)
    # Subset to our desired winetype
    winex <- subset(wine233, wine233 == winetype)
    coln<-colnames(wine233)[2:11]
    names(coln)<-2:11
    coln[tree.variables-1]%>%as.vector()->sub

    # Create subset df using only Quality.Factor and our chosen predictor variables
    preds <- wine233[sub]
    Quality.Factor <- as.factor(wine233$Quality.Factor)
    winex <- cbind(Quality.Factor, preds)
    # Create tree object using chosen predictors

    wine.tree <- rpart(Quality.Factor~., data = winex,method = "class")
    fitr <- dendro_data(wine.tree)
    p<-ggplot()+
      geom_segment(data = fitr$segments,
                   aes(x = x, y = y, xend = xend, yend = yend)
      ) +
      geom_text(data = fitr$labels, aes(x = x, y = y+0.004, label = label),size=4.5) +
      geom_text(data = fitr$leaf_labels, aes(x = x+0.05, y = y-0.009, label = label),size = 4)+
      theme_dendro()

    plot <- ggplotly(p)
    plot

  }
)





################################
## Introduction
################################


introduction_layout <- htmlDiv(
  list(
    # htmlBr(),
    # htmlDiv(
    #   list(
    #     htmlDiv(
    #       children = list(
    htmlDiv(
      id = "page-content",
      style = list(paddingTop = "0px", minHeight = "calc(100vh - 70px)"),
      className = "app-body",
      children = list(
        Header_banner,
        htmlBr(),
        htmlDiv(
          style=list(marginBottom = '8em'),
          id = "mhp-control-tabs",
          className = "control-tabs",
          children = list(
            dccTabs(
              id = "mhp-tabs",
              value = "what-is",
              children = list(
                dccTab(
                  label = "About",
                  value = "what-is",
                  children = htmlDiv(
                    className = "control-tab",
                    children = list(
                      htmlBr(),
                      htmlH4(
                        className = "what-is", 
                        children = "What is Our Motivation?",
                        style=list(marginTop = '1em', marginBottom = '1em')
                      ),
                      htmlBr(),
                      htmlP("With 36 billion bottles of wine produced each year, 
                                    wine makers are constantly looking for ways to outperform 
                                    the competition and create the best wines they can. Portugal 
                                    in particular is second in the world for per-capita wine 
                                    consumption and eleventh for wine production, creating over 
                                    600,000 litres per year.",
                            style = list(paddingLeft = "10px")
                      ),
                      htmlP("Given that physicochemical components 
                                    are fundamental to a wine's quality, those who understand this 
                                    aspect of wine will have a greater edge into crafting an enjoyable 
                                    and profitable product.",
                            style = list(paddingLeft = "10px")
                      ),
                      htmlBr(),
                      htmlDiv(
                        id = "app-page-header",
                        children = list(
                          htmlImg(
                            src = "/assets/GitHub-Mark-64px.png",
                            style = list(paddingLeft = "10px", marginBottom = '2em', float = 'center') # float not working
                          ),
                          htmlA(
                            id = "gh-link",
                            children = list("View on GitHub"),
                            href = "https://github.com/ubco-mds-2020-labs/WineVision-R-group8",
                            style = list(color = "grey", paddingLeft = "10px", float = 'center') # float not working
                          )
                        )
                      ),
                      htmlImg(
                        # https://www.heremagazine.com/articles/vinho-verde-is-the-new-rose/
                        src  =  "/assets/HereMag.png", width = "100%",
                        className = "app__menu__img",
                        style=list(marginTop = '1em')
                      )
                    )
                  )
                ),
                dccTab(
                  label = "Problem & Solution",
                  value = "what-is2",
                  children = htmlDiv(
                    className = "control-tab",
                    children = list(
                      htmlBr(),
                      htmlH4(
                        className = "what-is2", 
                        children = "What is Our Problem?",
                        style=list(marginTop = '1em', marginBottom = '1em')
                      ),
                      htmlP("Wine making has always been a traditional practice passed down 
                                    for many generations; yet, some of wine's secrets are still a 
                                    mystery to most people, even wine producers! So how are we supposed 
                                    to craft the perfect wine without knowing what makes it perfect 
                                    (speaking from both a consumer and business perspective)?",
                            style = list(paddingLeft = "10px")
                      ),
                      htmlP("In general, wine quality evaluation is assessed by physicochemical 
                                    tests and sensory analysis. It's the roadmap to improving a wine. 
                                    However the relationship between physicochemical structure and 
                                    subjective quality is complex and no individual component can be 
                                    used to accurately predict a wine's quality. The interactions are 
                                    as important as the components themselves.",
                            style = list(paddingLeft = "10px")
                      ),
                      htmlP("From a business perspective, producers are constantly looking for 
                                    ways to outperform the competition by creating the best wine they can.
                                    Those who understand the fundamental physiochemical aspects of wine 
                                    will have a greater edge into crafting an enjoyable and profitable 
                                    product. So, we introduce to you the Wine Vision Dashboard.",
                            style = list(paddingLeft = "10px")
                      ),
                      htmlBr(),
                      htmlH4(
                        className = "what-is2", 
                        children = "What is Our Solution?",
                        style=list(marginTop = '1em', marginBottom = '1em')
                      ),
                      htmlP("Our interactive dashboard will allow users to explore how a number 
                                    of physicochemical variables interact and determine the subjective 
                                    quality of a wine. Wine producers, wine enthusiasts, and curious 
                                    individuals can all make use of this dashboard to discover these 
                                    exclusive relationships.",
                            style = list(paddingLeft = "10px"),
                      ),
                      htmlImg(
                        # https://www.heremagazine.com/articles/vinho-verde-is-the-new-rose/
                        src  =  "/assets/HereMag.png", width = "100%",
                        className = "app__menu__img",
                        style=list(marginTop = '1em')
                      )
                    )
                  )
                ),
                dccTab(
                  label = "The Data",
                  value = "what-is3",
                  children = htmlDiv(
                    className = "control-tab",
                    children = list(
                      htmlBr(),
                      htmlH4(
                        className = "what-is3", 
                        children = "The Data We Are Using",
                        style=list(marginTop = '1em', marginBottom = '1em')
                      ),
                      htmlP("Data was collected from Vinho Verde wines originating from the 
                                    northwest regions of Portugal. These wines have a medium alcohol 
                                    content, and are particularly sought for their freshness in summer 
                                    months. Each wine sample was evaluated by at least three sensory 
                                    assessors (using blind tastes) who graded the wine from 0 (worst) 
                                    to 10 (best). The final quality score is given by the median of 
                                    these evaluations.",
                            style = list(paddingLeft = "10px")
                      ),
                      htmlP("The dataset consists of the physiochemical composition and sensory 
                                    test results for 4898 white and 1599 red wine samples which were 
                                    collected from May 2004 to February 2007. Each wine sample contains 
                                    12 variables that provide the acidity properties (fixed acidity, 
                                    volatile acidity, citric acid, pH), sulphides contents (free sulfur 
                                    dioxide, total sulfur dioxide, sulphates), density related 
                                    properties (residual sugar, alcohol, density), and salt content 
                                    (chlorides). It also contains quality as the response variable. 
                                    In order to improve classification analyses, we define a new 
                                    variable, quality_factor. Any wine with a quality score less than 
                                    six is classified as below average, a score of 6 is average, 
                                    and above 6 is above average.",
                            style = list(paddingLeft = "10px")
                      ),
                      htmlBr(),
                      htmlH4(
                        className = "what-is3", 
                        children = "Data Citation",
                        style=list(marginTop = '1em', marginBottom = '1em')
                      ),
                      htmlP("Paulo Cortez, University of Minho, Guimar�es, Portugal, 
                                    http://www3.dsi.uminho.pt/pcortez A. Cerdeira, F. Almeida, 
                                    T. Matos and J. Reis, Viticulture Commission of the Vinho Verde 
                                    Region(CVRVV), Porto, Portugal @2009",
                            style = list(paddingLeft = "10px")
                      ),
                      htmlImg(
                        # https://www.heremagazine.com/articles/vinho-verde-is-the-new-rose/
                        src  =  "/assets/HereMag.png", width = "100%",
                        className = "app__menu__img",
                        style=list(marginTop = '1em')
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
#     ))
# ))



#############################################
## APP PAGE CALLBACKS
#############################################

app$callback(output = list(id='page-content', property = 'children'),
             params = list(input(id='url', property = 'pathname')),
             display_page <- function(pathname) {
               if (pathname == '/WineVision/Quality-Factors') {
                 return(Quality_Factors_layout)
               }
               else if (pathname == "/WineVision/Quality-Distributions") {
                 return(Quality_Distribution_layout)
               }
               else if (pathname == "/WineVision/Wine-Types") {
                 return(Wine_Types_layout)
               }
               else if (pathname == "/WineVision/introduction") {
                 return(introduction_layout)
               }
               else if (pathname == "/WineVision/Wine-table") {
                 return(table_layout)
               }
               else if (pathname == "/WineVision/Prediction"){
                 return(prediction_layout)
               }
               else {
                 return(introduction_layout)
               }
             }
)


#############################################
## RUN APP
#############################################


app$run_server(host = '0.0.0.0') # 0.0.0.0 Needed for Heroku

#host = '0.0.0.0'