library(dplyr)
library(plyr)

wine <- read.csv("data/processed_python/wine_quality.csv")

## ID column for linking plots
wine$id <- as.character(1:nrow(wine))


## Remove units and replace dots with spaces
col_names <- colnames(wine)
colnames(wine) <- gsub("\\.", " ", col_names) 


##  Statistical Frames for the Distributions
factors <- c(1, 13, 14, 15)
wine[, -factors] <- as.numeric(unlist(wine[, -factors]))

  # Split types
  white <- wine[wine[,'Wine']=='white', ]
  red <- wine[wine[,'Wine']=='red', ]
  wine_type <- list('White' = white, 'Red' = red)

  # Mean
  mu_white <- ddply(white, "`Quality Factor`", numcolwise(mean))
  mu_red <- ddply(red, "`Quality Factor`",  numcolwise(mean))
  mu_type <- list(mu_white, mu_red)

  # Median
  med_white <- ddply(white, "`Quality Factor`", numcolwise(median))
  med_red <- ddply(red, "`Quality Factor`",  numcolwise(median))
  med_type <- list(med_white, med_red)

  # Mode (max peak)
  ## Function for mode on continuous variables
  contmode <- function(vector) {
    dens <- density(vector)
    maxx = dens$x[which.max(dens$y)]
    return(maxx)
  }

  mode_white <- ddply(white, "`Quality Factor`", numcolwise(contmode))
  mode_red <- ddply(red, "`Quality Factor`",  numcolwise(contmode))
  mode_type <- list(mode_white, mode_red)




## OBJECTS FOR APP

## Stat object for calling
stats <- list('Mean'=mu_type, 'Median'=med_type, 'Mode'=mode_type)

## Var object for calling
vars <- variable.names(wine)[-15] %>% as.vector()

## Units in order of variables for later calls
units <- c(' ', '(g/dm^3)', '(g/dm^3)', '(g/dm^3)', '(g/dm^3)', '(g/dm^3)', '(mg/dm^3)', '(mg/dm^3)', '(g/cm^3)', ' ', '(g/dm^3)', '(%)', ' ', ' ', ' ')
