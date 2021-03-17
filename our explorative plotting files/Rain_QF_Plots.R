library(devtools)
library(dash)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(readr)
library(ggplot2)
library(plotly)
library(stringr)
library(dashTable)
library(tidyverse)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

wine <- read.csv("data/processed/wine_quality.csv")
wine$id <- as.character(1:nrow(wine))

app$layout(htmlDiv(
  list(
    htmlDiv(
      list(
        htmlDiv(
          list(
            htmlImg(
              # https://elite-brands.com/blog/wine-ratings-q3/
              src  =  "assets/rating.png",
              className = "app__menu__img"
            )
          ), className = "app__header__logo"
        )
      ), className = "app__header"
    ),
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
                  options = wine %>% select_if(is.numeric) %>%
                    colnames %>%
                    purrr::map(function(xcol) list(label = xcol, value = xcol)), 
                  value='Chlorides..g.dm.3.'),
                htmlH5('Y-axis'),
                dccDropdown(
                  id='ycol-select',
                  options = wine %>% select_if(is.numeric) %>%
                    colnames %>%
                    purrr::map(function(ycol) list(label = ycol, value = ycol)), 
                  value='pH'),
                htmlBr(),
                htmlH4("Interactive Plots:", className = "graph__title"),
                htmlBr(),
                htmlH4("Drag your mouse to select a range!"))
            ),
            dccGraph(
              id = "plot-area"
            )
          ), className = "two-thirds column wind__speed__container"
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
                      labelStyle = list(display = 'inline-block')
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
                htmlDiv(
                  list(
                    htmlH4(
                      "% Quality Factors", className = "graph__title"
                    )
                  )
                ),
                dccGraph(
                  id = "bar-plot2")
              ), className = "graph__container second"
            )
          ), className = "one-third column histogram__direction"
        )
      ), className = "app__content"
    )
  ), className = "app__container"
))


app$callback(
  output = list(id='plot-area', property='figure'),
  params = list(input(id='xcol-select', property='value'),
                input(id='ycol-select', property='value'),
                input(id='wine-type', property='value')),
  
  function(xcol, ycol, type) {
    wine_dif <- wine %>% subset(Wine == type)
    scatter <- ggplot(wine_dif) + 
      aes(x = !!sym(xcol), y = !!sym(ycol), color = Quality.Factor, text = id) + 
      geom_point(alpha = 0.7) + ggthemes::scale_color_tableau()
    ggplotly(scatter, tooltip = 'text') %>% layout(dragmode = 'select')
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
          fill = Quality.Factor) +
      geom_bar(width = 0.6, alpha = 0.5) +
      ggthemes::scale_fill_tableau()
    ggplotly(p, tooltip = 'text') %>% layout(dragmode = 'select')
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
      aes(x = Quality.Factor,
          fill = Quality.Factor) +
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      theme(axis.text.x=element_blank()) +
      ggthemes::scale_fill_tableau()
    ggplotly(b, tooltip = 'y') %>% layout(dragmode = 'select')
  }
)

app$run_server(debug = T)
