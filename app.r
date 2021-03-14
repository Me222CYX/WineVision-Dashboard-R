# Installs dashHtmlComponents, dashCoreComponents, and dashTable
# and will update the component libraries when a new package is released


# Installs dash bootstrap
library(devtools)
library(dash)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(dashCoreComponents)

library(ggplot2)
library(plotly)

#############################################
## APP AND FUNCTIONAL APP OBJECTS
#############################################

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

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




#############################################
## APP LAYOUT
#############################################

app$layout(
  htmlDiv(
    list(
      # URL
      dccLocation(id = 'url', refresh=FALSE),
      #Content
      htmlDiv(id='page-content')
      )
    )
  )

#################################
## Quality Factor Analysis Page

Quality_Factors_layout <- htmlDiv(
  list(
    Header,
    htmlDiv(
      list(
        htmlBr(),
        htmlBr()
      ),
      className = "twelve columns"
    )
  )
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
    ##### Hello and thank you for stopping by the Wine Vision App! 
    ##### Feel free to visit out [GitHub homebase](https://github.com/ubco-mds-2020-labs/WineVision-R-group8) for more information on the project. 
    
    #### The problem
    Wine making has always been a traditional practice passed down for many generations; yet, some of wine's secrets are still a mystery to most people, even wine producers! So how are we supposed to craft the perfect wine without knowing what makes it perfect (speaking from both a consumer and business perspective)?
    
    In general, wine quality evaluation is assessed by physicochemical tests and sensory analysis. It's the roadmap to improving a wine. However the relationship between physicochemical structure and subjective quality is complex and no individual component can be used to accurately predict a wine's quality. The interactions are as important as the components themselves. 
    
    From a business perspective, producers are constantly looking for ways to outperform the competition by creating the best wine they can. Those who understand the fundamental physiochemical aspects of wine will have a greater edge into crafting an enjoyable and profitable product. So, we introduce to you the *Wine Vision Dashboard*.
    
    #### The solution
    **Our interactive dashboard will allow users to explore how a number of physicochemical variables interact and determine the subjective quality of a wine. Wine producers, wine enthusiasts, and curious individuals can all make use of this dashboard to discover these elusive relationships.** 
    
    ![Wine chemistry](https://d2cbg94ubxgsnp.cloudfront.net/Pictures/480x270//9/2/5/139925_160412_PRS_AWRI_GAGO_0262.jpg)
    
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
    
    ![Vinho Verde Vineyard](https://blog.liebherr.com/appliances/us/wp-content/uploads/sites/3/2017/08/Vinho_Verde_Panther-753x493.jpg)
    
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
               else {
                 return(Wine_Types_layout)
               }
             }
)

#############################################
## RUN APP
#############################################

app$run_server(debug = T)

