# Installs dashHtmlComponents, dashCoreComponents, and dashTable
# and will update the component libraries when a new package is released


# Installs dash bootstrap
library(devtools)
library(dash)
library(dashTable)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(dashCoreComponents)

library(readr)
library(stringr)
library(plyr)
library(glue)
library(ggplot2)
library(plotly)
fig <- plot_ly()

#############################################
## APP AND FUNCTIONAL APP OBJECTS
#############################################

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")
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
            className = "seven columns main-title"),
        htmlDiv(
          list(
            dccLink("Learn more",
                    href = "/WineVision/learn-more",
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
        "Red/White Wine Comparison",
        href="/WineVision/Wine-Types",
        className="tab first"),
      dccLink(
        "Wine Table",
        href="/WineVision/Wine-table",
        className="tab"),
      dccLink(
        "Quality Factor Analysis",
        href="/WineVision/Quality-Factors",
        className="tab ")
      ),
    className="rowrow alltab "
    )
  return(menu)
  }

Header <- htmlDiv(list(get_header(), htmlBr(), get_menu()))

Menu <- htmlDiv(list(get_menu()))

#############################################
## DATA
#############################################

df <- wine_quality <- read.csv("data/processed/wine_quality.csv")
gsub("(mg/dm^3)","",colnames(df),fixed = TRUE)->cn
gsub("(g/dm^3)","",cn,fixed = TRUE)->cn
gsub("(g/cm^3)","",cn,fixed = TRUE)->cn
gsub("(%)","",cn,fixed = TRUE)->cn
str_trim(cn, side = c("right"))->cn
colnames(df)<-cn

## Luka
# This could probably be done in the wrangling file
wine <- df
factors <- c(1, 13, 14, 15)
wine[, -factors] <- as.numeric(unlist(wine[, -factors]))
white <- wine[wine[,'Wine']=='white', ]
red <- wine[wine[,'Wine']=='red', ]
wine_type <- list('White' = white, 'Red' = red)

mu_white <- ddply(white, "Quality.Factor", numcolwise(mean))
mu_red <- ddply(red, "Quality.Factor",  numcolwise(mean))
mu_type <- list(mu_white, mu_red)

vars <- variable.names(wine)[-15] %>% as.vector()


#############################################
## APP LAYOUT
#############################################

app$layout(
  htmlDiv(
    list(
      # URL
      dccLocation(id = 'url', refresh=TRUE), # Changed from false
      #Content
      htmlDiv(id='page-content')
      )
    )
  )

################################
## Table page

page_size <- 10

table_layout<-htmlDiv(list(
  Header,
  dbcContainer(
    dbcRow(list(
      dbcCol(htmlDiv(
        dbcCard(
          dbcCardBody
          (list(
            htmlH5("WineVision dataset", className = "Card title"),
            htmlP("", className = "card-text")
          )
          )))),
      htmlBr(),
      dbcCol(htmlDiv(
        dashDataTable(
          style_table = list(overflowX = 'scroll'),
          id = 'table-sorting-filtering',
          columns = lapply(sort(colnames(df)),
                           function(colName){
                             list(
                               id = colName,
                               name = colName
                             )
                           }),
          page_current = 0,
          page_size = page_size,
          page_action = 'custom',
          
          filter_action = 'custom',
          filter_query = '',
          
          sort_action = 'custom',
          sort_mode = 'multi',
          sort_by = list()
        )
        
      ), width=3)
    )))))


app$callback(
  output = list(id = 'table-sorting-filtering', property = 'data'),
  params = list(input(id = 'table-sorting-filtering', property = 'page_current'),
                input(id = 'table-sorting-filtering', property = 'page_size'),
                input(id = 'table-sorting-filtering', property = 'sort_by'),
                input(id = 'table-sorting-filtering', property = 'filter_query')),
  function(page_current, page_size, sort_by, filters) {
    
    subdf <- df
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



#################################
## Quality Factor Analysis Page

Quality_Factors_layout <- htmlDiv(
  list(
    Header,
    htmlDiv(
      list(
        htmlBr(),
        dccMarkdown("hello graph.
                    may i ask why won't you FRIGGIN WORK"),
        dccDropdown(
          id = 'col-select',
          options = wine %>% colnames %>% purrr::map(function(col) list(label = col, value = which(colnames(wine)==col))),
          value = 8),
        dccDropdown(
          id = 'wine-select',
          options = list(list(label = 'Red Wine', value = 2),
                         list(label = 'White Wine', value = 1)),
          value = 1),

        htmlBr(),
        dccGraph(id = 'densityplot')
      ),
      className = "twelve columns"
    )
  )
)

app$callback(
  output(id = 'densityplot', property = 'figure'),
  params = list(input(id = 'col-select', 'value'), 
                input(id = 'wine-select', 'value')),
  function(variable, winetype) {
  
    coln <- sym(colnames(wine)[variable])

    plot <- ggplot(wine_type[[winetype]], aes(x = !!coln, fill = Quality.Factor)) + 
            geom_density(alpha = 0.4) + ylab('Density') + xlab(coln) +
            geom_vline(data=mu_type[[winetype]], aes(xintercept=!!coln, color=Quality.Factor), linetype="dashed", size=0.9) +
            ggtitle(glue('{coln} Quality Factor Distribution'), subtitle = glue('For {names(wine_type[winetype])} Wine'))# +
            theme_classic() +
            theme(legend.title = element_text('Quality Levels'),
                  legend.position = c(0.94,0.88),
                  text = element_text(size = 12),
                  element_line(size = 0.3),
                  panel.grid.major = element_line(color = '#ededed'))
    
    ggplotly(plot)
  }
)

################################
## Wine Type Comparison Page

Wine_Types_layout <- htmlDiv(
  list(
    Header,
    htmlDiv(
      list(
        htmlBr(),
        htmlBr()
        ),
      className = "twelve columns")
    )
  )

################################
## Learn More Page

learn_more_layout <- htmlDiv(
  list(
    Header,
    htmlDiv(
      list(
        # Row 3
        htmlDiv(
          list(
            htmlH3('Motivation'),
            htmlH6("With 36 billion bottles of wine produced each year, wine makers
                    are constantly looking for ways to outperform the competition and
                    create the best wines they can. Portugal in particular is second
                    in the world for per-capita wine consumption and eleventh for
                    wine production, creating over 600,000 litres per year.
                    Given that physicochemical components are fundamental to a wine's
                    quality, those who understand this aspect of wine will have a
                    greater edge into crafting an enjoyable and profitable product.")
            ),
          className="product"
          )
        ),
      className="twelve columns"
      ),
    htmlDiv(
      dccMarkdown(
        
    "
    ### Welcome!
    Hello and thank you for stopping by the Wine Vision App! 
    
    Feel free to visit out [GitHub homebase](https://github.com/ubco-mds-2020-labs/WineVision-R-group8) for more information on the project. 
    

    #### The problem
    Wine making has always been a traditional practice passed down for many generations; yet, some of wine's secrets are still a mystery to most people, even wine producers! So how are we supposed to craft the perfect wine without knowing what makes it perfect (speaking from both a consumer and business perspective)?
    
    In general, wine quality evaluation is assessed by physicochemical tests and sensory analysis. It's the roadmap to improving a wine. However the relationship between physicochemical structure and subjective quality is complex and no individual component can be used to accurately predict a wine's quality. The interactions are as important as the components themselves. 
    
    From a business perspective, producers are constantly looking for ways to outperform the competition by creating the best wine they can. Those who understand the fundamental physiochemical aspects of wine will have a greater edge into crafting an enjoyable and profitable product. So, we introduce to you the *Wine Vision Dashboard*.
    
    #### The solution
    **Our interactive dashboard will allow users to explore how a number of physicochemical variables interact and determine the subjective quality of a wine. Wine producers, wine enthusiasts, and curious individuals can all make use of this dashboard to discover these elusive relationships.** 


    #### App Description
    The dashboard has <_?_> pages:
    
    {{{{{{{{{{ ***Edit this, reduce words, compress***
    
    an overview, a comparison of red and white wines, and a comparison of  different wine quality levels.
    The Interactive Graphics page contains a number of graphis to explore the effects of physicochemical properties on wine quality. On the left hand side users are able to select the wine type (eg. red wine, white wine) as well as the physicochemical features of interest, with subset ranges if they so desire. Some possible visualizations are as follows:
    
    The Overview page provides information on how to use the app and includes some references and further information for those interested in wine aspects the dashboard does not cover.
    
    The second page is Wine Type, primarily intended to demonstrate differences between red and white wines. This page has a good layout, without leaving any large white spaces. It also demonstrates a cohesive narrative, with users able to distinguish high correlation variables from the correlation matrix and then investigating deeper using the scatter plot and density plot.
    
    The third page is Quality Factors, where users can explore features of wines in different quality groups. Users can subset the data range by selecting areas on the scatter plot, which immediately updates the other plots. The bar plot allows users to visualize quality factor proportions in their selections.. The drag and drop functionality makes this page particularly interactive.
    
    }}}}}}}}}}}
    
    
    #### The Data
    Portugal is second in the world for per-capita wine consumption [2](https://www.nationmaster.com/nmx/ranking/wine-consumption-per-capita) and eleventh for wine production [3](https://en.wikipedia.org/wiki/List_of_wine-producing_regions), so by no coincidence we built our dashboard on the famous Portuguese wine quality data set from Cortez et al., 2009. 
    
    Data was collected from Vinho Verde wines originating from the northwest regions of Portugal. These wines have a medium alcohol content, and are particularly sought for their freshness in summer months. Each wine sample was evaluated by at least three sensory assessors (using blind tastes) who graded the wine from 0 (worst) to 10 (best). The final quality score is given by the median of these evaluations.
    
    The dataset consists of the physiochemical composition and sensory test results for 4898 white and 1599 red wine samples which were collected from May 2004 to February 2007. Each wine sample contains 12 variables that provide the acidity properties (fixed acidity, volatile acidity, citric acid, pH), sulphides contents (free sulfur dioxide, total sulfur dioxide, sulphates), density related properties (residual sugar, alcohol, density), and salt content (chlorides). It also contains quality as the response variable. In order to improve classification analyses, we define a new variable, quality_factor. Any wine with a quality score less than six is classified as “below average”, a score of 6 is “average”, and above 6 is “above average”.


    #### A Fun Usage Scenario
    Alice is a winemaker in British Columbia's Okanagan Valley. She would like to create a new summer wine and hopes to take inspiration from the Vinho Verde wines, known for their refreshing qualities. Alice seeks our dashboard to better understand what wine attributes she should focus on to provide a tasting experience comparable to the very best Vinho Verde wines. However, there are some physicochemical properties she has little control over due to the soils and grape species available to her. Due to the above average alkalinity of Okanagan soil, she knows that her wines will typically be less acidic than true Vinho Verde wines, and the altitude means the chloride content will be lower as well. She wants to try to optimize the variables she has control over to make the best wine possible. She looks to our dashboard to see how Vinho Verde wines with higher pH and lower chloride content tend to fare. Looking at the interactive scatterplots, she sees that wines which have values within her possible ranges for these variables tend to be of very poor quality when they are also high in residual sugar, but less sweet wines are of good quality. She then consults the histograms and sees that there are not very many wines available that have these properties, so she knows that she will not have much direct competition should she go forward with this design. A few years later, she released this wine to broad critical acclaim and millions in profit.
    
    
    #### Get involved
    If you think you can help in any of the areas listed above (and we bet you can) or in any of the many areas that we haven't yet thought of (and here we're *sure* you can) then please check out our [contributors' guidelines](https://github.com/ubco-mds-2020-labs/WineVision/blob/main/CONTRIBUTING.md) and our [roadmap](https://github.com/ubco-mds-2020-labs/WineVision/pull/1).
    
    Please note that it's very important to us that we maintain a positive and supportive environment for everyone who wants to participate. When you join us we ask that you follow our [code of conduct](https://github.com/ubco-mds-2020-labs/WineVision/blob/main/CODE_OF_CONDUCT.md) in all interactions both on and offline.
    

    #### Contact us
    If you want to report a problem or suggest an enhancement we'd love for you to [open an issue](https://github.com/ubco-mds-2020-labs/WineVision/issues) at this github repository because then we can get right on it.
    

    #### Data Citation
    Paulo Cortez, University of Minho, Guimarães, Portugal, http://www3.dsi.uminho.pt/pcortez
    A. Cerdeira, F. Almeida, T. Matos and J. Reis, Viticulture Commission of the Vinho Verde Region(CVRVV), Porto, Portugal @2009
    "
),style = list("font-size"="1.925rem"))))


#############################################
## APP PAGE CALLBACKS
#############################################

app$callback(output = list(id='page-content', property = 'children'),
             params = list(input(id='url', property = 'pathname')),
             display_page <- function(pathname) {
               if (pathname == '/WineVision/Quality-Factors') {
                 return(Quality_Factors_layout)
               }
               else if (pathname == "/WineVision/Wine-Types") {
                 return(Wine_Types_layout)
               }
               else if (pathname == "/WineVision/learn-more") {
                 return(learn_more_layout)
               }
               else if (pathname == "/WineVision/Wine-table") {
                 return(table_layout)
               }
               else {
                 return(Wine_Types_layout)
               }
             }
)


#############################################
## RUN APP
#############################################

app$run_server(debug = T, dev_tools_hot_reload=FALSE)

