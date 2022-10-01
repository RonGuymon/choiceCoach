# Choice Coach Functions
library(tidyverse)
library(magrittr)
library(lubridate)

# Points to weights function for converting the points dataframe to the weights and sd
ptw <- function(oPoints, nameCol = 'objectiveName1', obOp = 'objective'){
  # oPoints should contain the following columns: 
  # rowNum
  # points
  # column with names that will be used for column names
  oPointsOnly <- data.frame()
  for(r in 1:max(oPoints$rowNum)){
    tm <- oPoints %>% 
      dplyr::filter(rowNum == r) %>%
      dplyr::pull(points) %>%
      matrix(nrow = 1)
    oPointsOnly %<>% bind_rows(data.frame(tm))
  }
  names(oPointsOnly) <- unique(oPoints[,nameCol])
  
  # Calculate odds ratios
  oRatios <- oPointsOnly / (100-oPointsOnly)
  # Calculate normalized pcts by dividing odds ratios by sum of odds ratio for each column
  oNormalized <- apply(oRatios, 2, function(x) x/sum(x)) %>%
    data.frame()
  # Objective weights and standard deviations
  owsd <- data.frame(objective = names(oNormalized) %>% gsub('\\.', ' ', .)
                     , weights = apply(oNormalized, 1, mean)
                     , sd = apply(oNormalized, 1, sd)
  )
  names(owsd)[1] <- obOp
  return(owsd)
}