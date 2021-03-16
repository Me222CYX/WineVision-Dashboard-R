library(devtools)
library(dash)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(dashTable)
library(ggplot2)
library(plotly)


#### data tables 


df <- wine_quality <- read_csv("data/processed/wine_quality.csv")

gsub("(mg/dm^3)","",colnames(df),fixed = TRUE)->cn
gsub("(g/dm^3)","",cn,fixed = TRUE)->cn
gsub("(g/cm^3)","",cn,fixed = TRUE)->cn
gsub("(%)","",cn,fixed = TRUE)->cn
str_trim(cn, side = c("right"))->cn
colnames(df)<-cn

##############################################3

app <- Dash$new()

page_size<-10

df%>%filter(Wine=='red')->red
df%>%filter(Wine=='white')->white
df

lapply(sort(colnames(df)), 
       function(colName){
         list(
           id = colName,
           name = colName
         )
       })

app$layout(htmlDiv(
  dbcContainer(
    dbcRow(list(
      dbcCol(
        dbcCard(
          dbcCardBody
          (list(
            htmlH5("WineVision dataset", className = "Card title"),
            htmlP("Select varibles", className = "card-text"),
            dccDropdown(
              id='select-var',
              options = lapply(colnames(df), function(col) list(label= col,value=col)),
              value = "pH",
              multi = TRUE),
            #hiden div update dataframe
          htmlDiv(id='data_1',style =list('display'="none")))
          )
        )
      ),
      htmlBr(),
      dbcCol(
        dashDataTable(
          style_table = list(overflowX = 'scroll'), 
          id = 'table-filtering',
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
          filter_query = ''
        )
        
      )
    )
    )
),style=list("width"="29rem")  ))


# 
# app$callback(
#   output = list(id = 'data_1', property = 'data'),
#   params = list(input(id = 'select-var', property = "value")),
#   function(values){
#     df%>%select(unlist(values))->df
#     return(df)
#   }
# )
#   

app$callback(
  output = list(id = 'table-filtering', property = 'data'),
  params = list(input(id = 'table-filtering', property = "page_current"),
                input(id = 'table-filtering', property = "page_size"),
                input(id = 'table-filtering', property = "filter_query"),
                input(id = 'select-var', property = "values")),
  function(page_current, page_size, filters,values) {
    
    df%>%select(unlist(values))->df
    subdf <- df
    

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

    start_id <- (page_current * page_size + 1)
    end_id <- ((page_current + 1) * page_size)
    subdf[start_id:end_id, ]
  }
)



# app$callback(
#   output = list(is='table', property='data'),
#   params = list(input(id='select-var',property = 'value')),
#   function(selected_col){
#     df%>%select(selected_col)->df_new
#     df_new
#   })




# app$layout(htmlDiv(
#   dccDropdown(
#     options = lapply(colnames(df), function(col) list(label= col,value=col)),
#     value = "pH",
#     multi = TRUE)))

# app$layout(htmlDiv(
#   dccDropdown(
#   options = lapply(colnames(df), function(col) list(label= col,value=col)),
#   value = "pH",
#   multi = TRUE)))






################################3
# page_size <- 15
# 
# app$layout(
#   dashDataTable(
#     id = 'table-sorting-filtering',
#     columns = lapply(sort(colnames(df)), 
#                      function(colName){
#                        list(
#                          id = colName,
#                          name = colName
#                        )
#                      }),
#     page_current = 0,
#     page_size = page_size,
#     page_action = 'custom',
#     
#     filter_action = 'custom',
#     filter_query = '',
#     
#     sort_action = 'custom',
#     sort_mode = 'multi',
#     sort_by = list()
#   )
# )
# 
# app$callback(
#   output = list(id = 'table-sorting-filtering', property = 'data'),
#   params = list(input(id = 'table-sorting-filtering', property = 'page_current'),
#                 input(id = 'table-sorting-filtering', property = 'page_size'),
#                 input(id = 'table-sorting-filtering', property = 'sort_by'),
#                 input(id = 'table-sorting-filtering', property = 'filter_query')),
#   function(page_current, page_size, sort_by, filters) {
#     
#     subdf <- df
#     # filter
#     if(filters != "") {
#       
#       conditions <- strsplit(filters, split = "&&")[[1]]
#       
#       not_show <- lapply(conditions,
#                          function(condition) {
#                            
#                            splited_condition <- strsplit(condition, split = " ")[[1]]
#                            # len should be 3
#                            len <- length(splited_condition)
#                            
#                            condition <- if('contains' %in% splited_condition) {
#                              
#                              splited_condition[which('contains' == splited_condition)] <- "=="
#                              
#                              if(!grepl("\"", splited_condition[len]) & !grepl("'", splited_condition[len])) {
#                                splited_condition[len] <- paste0("'", splited_condition[len], "'")
#                              }
#                              
#                              paste0(splited_condition, collapse = " ")
#                            } else if('=' %in% splited_condition) {
#                              gsub('=', '==', condition)
#                            } else if ('datestartswith' %in% splited_condition) {
#                              gsub('datestartswith', '>=', condition)
#                            } else condition
#                            
#                            subdf <<- subdf %>%
#                              dplyr::filter(eval(parse(text = condition)))
#                          })
#     }
#     
#     # sort
#     if(length(sort_by) != 0) {
#       
#       index <- lapply(sort_by, 
#                       function(sort){
#                         if(sort[['direction']] == "asc") {
#                           subdf[, sort[['column_id']]]
#                         } else {
#                           -xtfrm(subdf[, sort[['column_id']]])
#                         }
#                       })
#       
#       # sort by multi columns
#       subdf <- subdf[do.call(order, index), ]
#     }
#     
#     start_id <- (page_current * page_size + 1)
#     end_id <- ((page_current + 1) * page_size)
#     subdf[start_id:end_id, ]
#   }
# )



app$run_server()
