########### CHOICE COACH DATA ANALYSIS ##############
library(tidyverse)
library(lubridate)
library(magrittr)

# Objectives list----
objectives <- c('a', 'b', 'c', 'd', 'e')

# Options list----
options <- c('New Arena', 'Star Player', 'Rebrand')

# Pairwise comparison of objectives to get weights----
nobj <- length(objectives)
nObjPerms <- choose(nobj,2)
objPerm <- expand.grid(objectives, objectives, stringsAsFactors = F) %>% 
  dplyr::mutate(
    V1 = Var2
    , V2 = Var1
    , permutation = case_when(
      Var1 > Var2 ~ paste0(Var1,Var2)
      , T ~ paste0(Var2,Var1)
    )
    , order = row.names(.)
  ) %>%
  dplyr::select(-Var1, -Var2)

keyComps <- objPerm %>%
  dplyr::filter(V1 != V2) %>%
  .[!duplicated(.$permutation),] %>%
  group_by(V1) %>%
  dplyr::mutate(
    nobs = n()
  ) %>%
  ungroup() %>%
  dplyr::mutate(
    firstRow = ifelse(nobs == max(nobs), 1, 0)
  ) %>%
  dplyr::select(-nobs)


# Enter points for first row----
firstRow <- keyComps %>% dplyr::filter(firstRow == 1) %>% dplyr::select(-firstRow)
firstRow$points <- c(23,50,50,50)
firstRow %<>% dplyr::mutate(
  v1timesBetterThanV2 = points/(100-points)
)

# Infer points for the rest----
idf <- expand.grid(firstRow$V2, firstRow$V2, stringsAsFactors = F) %>% 
  dplyr::mutate(
    V1 = Var2
    , V2 = Var1
    , permutation = case_when(
      Var1 > Var2 ~ paste0(Var1,Var2)
      , T ~ paste0(Var2,Var1)
    )
  ) %>%
  dplyr::select(-Var1, -Var2) %>%
  dplyr::filter(V1 != V2) %>%
  .[!duplicated(.$permutation),] %>%
  dplyr::left_join(firstRow[,c('V2', 'v1timesBetterThanV2')], by = c('V1' = 'V2')) %>%
  dplyr::rename(baseToV1 = v1timesBetterThanV2) %>%
  dplyr::left_join(firstRow[,c('V2', 'v1timesBetterThanV2')], by = 'V2') %>%
  dplyr::rename(baseToV2 = v1timesBetterThanV2) %>%
  dplyr::mutate(
    v2ToV1 = baseToV2/baseToV1
    , inferredPoints = (100*v2ToV1)/(1+v2ToV1)
  ) %>%
  dplyr::select(V1, V2, permutation, inferredPoints)

weightsForKeyComps <- bind_rows(firstRow[,c('V1', 'V2', 'permutation', 'points')], idf)

keyComps %<>% left_join(weightsForKeyComps[, c('permutation', 'points', 'inferredPoints')], by = 'permutation')

# You can either stop here and show the user the implied points, or----
# ask the user to enter points to check for consistency. I'm going to just use the implied points.
keyComps %<>% dplyr::mutate(
  points = ifelse(is.na(points), inferredPoints, points)
)

objectiveWeights <- objPerm %>% 
  dplyr::left_join(keyComps[,c('V1', 'V2', 'points')], by = c('V1', 'V2')) %>%
  dplyr::arrange(permutation, points) %>%
  group_by(permutation) %>%
  dplyr::mutate(
    points = ifelse(!is.na(dplyr::lag(points)), 100 - dplyr::lag(points), points)
    , points = ifelse(is.na(points), 50, points)
    , order = as.numeric(order)
  ) %>%
  dplyr::arrange(order) %>%
  ungroup() %>%
  group_by(V2) %>%
  dplyr::mutate(
    ratio = points / (100-points)
    , normalizedRatio = ratio / sum(ratio)
  ) %>%
  ungroup() %>%
  group_by(V1) %>%
  dplyr::summarise(Weights = mean(normalizedRatio)) %>%
  ungroup() %>%
  dplyr::rename(Objectives = V1) %>%
  dplyr::arrange(desc(Weights), Objectives)

objectiveWeights$Objectives <- factor(objectiveWeights$Objectives, levels = objectiveWeights$Objectives)

# Visualize the weights of the objectives----
ggplot(objectiveWeights, aes(x = Objectives, y = Weights)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  theme_minimal() +
  labs(title = 'Objective Weights')

# Gather information about how the options compare for each objective----
for(ob in objectives){
  message('Compare each option for the ', ob, ' objective.')
  # Dataframe of permutations----
  opPerm <- expand.grid(options, options, stringsAsFactors = F) %>% 
    dplyr::mutate(
      V1 = Var2
      , V2 = Var1
      , permutation = case_when(
        Var1 > Var2 ~ paste0(Var1,Var2)
        , T ~ paste0(Var2,Var1)
      )
      , order = row.names(.)
    ) %>%
    dplyr::select(-Var1, -Var2)
  
  # The upper triangle----
  keyCompsOptions <- opPerm %>%
    dplyr::filter(V1 != V2) %>%
    .[!duplicated(.$permutation),] %>%
    group_by(V1) %>%
    dplyr::mutate(
      nobs = n()
    ) %>%
    ungroup() %>%
    dplyr::mutate(
      firstRow = ifelse(nobs == max(nobs), 1, 0)
    ) %>%
    dplyr::select(-nobs)
  
  # Get weights for the first row----  
  firstRowOptions <- keyCompsOptions %>% dplyr::filter(firstRow == 1) %>% dplyr::select(-firstRow)
  firstRowOptions$points <- sample(0:100, nrow(firstRowOptions))
  # firstRowOptions$points <- c(80,20) # Get these from the user
  firstRowOptions %<>% dplyr::mutate(
    v1timesBetterThanV2 = points/(100-points)
  )
  
  # Infer points for the remaining comparisons----
  idfOptions <- expand.grid(firstRowOptions$V2, firstRowOptions$V2, stringsAsFactors = F)%>% 
    dplyr::mutate(
      V1 = Var2
      , V2 = Var1
      , permutation = case_when(
        Var1 > Var2 ~ paste0(Var1,Var2)
        , T ~ paste0(Var2,Var1)
      )
    ) %>%
    dplyr::select(-Var1, -Var2) %>%
    dplyr::filter(V1 != V2) %>%
    .[!duplicated(.$permutation),] %>%
    dplyr::left_join(firstRowOptions[,c('V2', 'v1timesBetterThanV2')], by = c('V1' = 'V2')) %>%
    dplyr::rename(baseToV1 = v1timesBetterThanV2) %>%
    dplyr::left_join(firstRowOptions[,c('V2', 'v1timesBetterThanV2')], by = 'V2') %>%
    dplyr::rename(baseToV2 = v1timesBetterThanV2) %>%
    dplyr::mutate(
      v2ToV1 = baseToV2/baseToV1
      , inferredPoints = (100*v2ToV1)/(1+v2ToV1)
    ) %>%
    dplyr::select(V1, V2, permutation, inferredPoints)
  
  weightsForKeyCompsOptions <- bind_rows(firstRowOptions[,c('V1', 'V2', 'permutation', 'points')], idfOptions)
  
  keyCompsOptions %<>% left_join(weightsForKeyCompsOptions[, c('permutation', 'points', 'inferredPoints')], by = 'permutation') 
  # Here is where you could ask the user to validate their weights by making additional comparisons----
  keyCompsOptions %<>% 
    dplyr::mutate(
    points = ifelse(is.na(points), inferredPoints, points)
  )
  
  # Convert points to option weights----
  optionWeights <- opPerm %>% 
    dplyr::left_join(keyCompsOptions[,c('V1', 'V2', 'points')], by = c('V1', 'V2')) %>%
    dplyr::arrange(permutation, points) %>%
    group_by(permutation) %>%
    dplyr::mutate(
      points = ifelse(!is.na(dplyr::lag(points)), 100 - dplyr::lag(points), points)
      , points = ifelse(is.na(points), 50, points)
      , order = as.numeric(order)
    ) %>%
    dplyr::arrange(order) %>%
    ungroup() %>%
    group_by(V2) %>%
    dplyr::mutate(
      ratio = points / (100-points)
      , normalizedRatio = ratio / sum(ratio)
    ) %>%
    ungroup() %>%
    group_by(V1) %>%
    dplyr::summarise(weight = mean(normalizedRatio)) %>%
    ungroup()
  
  colnames(optionWeights) <- c('Options', paste0(ob, 'Strength'))
  if(ob == objectives[1]){
    opObjWeights <<- optionWeights
  }else{
    opObjWeights %<>% left_join(optionWeights, by = 'Options')
  }
}

# Summarize the results to see which option is the best----
resultData <- opObjWeights %>%
  tidyr::pivot_longer(cols = colnames(opObjWeights[,2:ncol(opObjWeights)])
                      , names_to = 'Objectives'
                      , values_to = 'ObjectiveStrength'
                      ) %>%
  dplyr::mutate(
    Objectives = gsub('Strength', '', Objectives)
  ) %>%
  left_join(objectiveWeights, by = 'Objectives') %>%
  dplyr::mutate(
    weightedStrength = ObjectiveStrength * 100 * Weights
  )
summaryData <- resultData %>%
  group_by(Options) %>%
  summarise(totalValue = sum(weightedStrength)) %>%
  ungroup() %>%
  dplyr::mutate(
    pctBest = round(totalValue / max(totalValue), 2)*100
  ) %>%
  dplyr::arrange(desc(totalValue)) %>%
  dplyr::mutate(Rank = row.names(.))

# Visualize the results----
resultData %<>% left_join(summaryData[,c('Options', 'Rank', 'pctBest')], by = 'Options') %>%
  dplyr::arrange(Rank)

resultData$Options <- factor(resultData$Options, levels = unique(resultData$Options))

ggplot(resultData, aes(x = Options, y = weightedStrength, fill = Objectives)) +
  geom_bar(stat = 'identity') +
  labs(title = paste0('Outcome: ', summaryData$Options[1], ' is the Top Choice')
       , subtitle = paste0(summaryData$Options[2], ' is ', summaryData$pctBest[2], '% as good as ', summaryData$Options[1])
       , y = 'Total Score'
       )
#


