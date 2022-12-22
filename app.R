# Choice Coach
# Libraries----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(magrittr)
library(lubridate)
library(plotly)
library(DT)
source('choiceCoachFunctions.R')

# Session timeout info----
# For this to work there are three parts:
# 1 the JavaScript code right here,
# 2 a tags$script(inactivity) in the body, and 
# 3 an observeEvent in the server
timeoutSeconds <- 60*15

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
Shiny.setInputValue('timeOut', '%ss')
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)

# UI----
header <- dashboardHeader(
  title = 'Choice Coach'#   span(tags$a(href = giggHomepageUrl
  #                     , tags$img(src='connorLogo.png'
  #                                , title = "Logo"
  #                                , height = "45px"
  #                                , width = "100px"
  #                                , position = "center")
  #                     # , style = "padding-left:30px;"
  # ))
  )
sidebar <- dashboardSidebar(
  # sidebarUserPanel(), 
  sidebarMenu(id = 'tabs'
                , menuItem('Define Question', tabName = 'defineQuestion')
                , menuItem('Objectives', tabName = 'objectives')
                , menuItem('Options', tabName = 'options')
  )
  , tags$head(
    tags$link(rel="shortcut icon"
              , href="favicon.ico")
  )
)
body <- dashboardBody(
  tags$script(inactivity)
    ,tabItems(
      tabItem(tabName = 'defineQuestion'
              , fluidRow(
                box(width = 12, title = 'What is Your Question'
                  , textInput('questionInput', label = 'Define Your Question')
                  , actionButton('setObjectives', label = 'Define Objective')
                ))
              )
      , tabItem(tabName='objectives'
                , fluidRow(
                  box(width = 12, title = 'Identify Objectives'
                      , tags$div(id = 'placeholder')
                      , actionButton('newObjectiveButton', label = 'Add Objective')
                      , p()
                      , actionButton('removeObjectiveButton', label = 'Remove Objective')
                      , p()
                      , actionButton('compareObjectivesButton', label = 'Compare Objectives')
                  )
                )
                , conditionalPanel(condition = 'input.compareObjectivesButton >= 1'
                                   , fluidRow(
                                     box(width = 12, title = 'Compare Objective Importance'
                                         , tags$div(id = 'placeholderSlider')
                                         # , dataTableOutput('dt')
                                         # , verbatimTextOutput('rt')
                                         , actionButton('createObjWeightButton', label = 'Calculate Objective Weights')
                                         # , dataTableOutput('objPts')
                                     )
                                   )
                                   
                )
                , conditionalPanel(condition = 'input.createObjWeightButton >= 1'
                                   , fluidRow(
                                     box(width = 12, title = 'Consider Objective Weights'
                                         , plotlyOutput('obWeightDonut')
                                         , actionButton('defineOptionsButton', 'Define Options')
                                     )
                                   )
                                   
                )
      )
      , tabItem(tabName = 'options'
                , fluidRow(
                  box(width = 12, title = 'Define Options'
                      , tags$div(id = 'placeholderOpt')
                      , actionButton('newOptionButton', label = 'Add Options')
                      , p()
                      , actionButton('removeOptionButton', label = 'Remove Option')
                      , p()
                      , actionButton('compareOptionsButton', label = 'Compare Options')
                  )
                )
                , conditionalPanel(condition = 'input.compareOptionsButton >= 1'
                                   , fluidRow(
                                     box(width = 12, title = 'Compare Options for Each Objective'
                                         , tags$div(id = 'placeholderSliderOpt')
                                         # , dataTableOutput('dtOpt')
                                         , actionButton('finalEvaluationButton', label = 'Make the final evaluation')
                                     )
                                   )
                )
                , conditionalPanel(condition = 'input.finalEvaluationButton >= 1'
                                   , fluidRow(
                                     box(width = 12, title = 'Overal Results'
                                         , plotlyOutput('finalPlotScaled')
                                         # , dataTableOutput('opPoints')
                                     )
                                   )
                )
      )
    )
)
ui <- dashboardPage(title = 'Choice Coach', header, sidebar, body
)
# SERVER----
server <- function(input, output, session) {
  # Timeout from session inactivity----
  observeEvent(input$timeOut, { 
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time()),
      footer = NULL
    ))
    session$close()
  })
  
  # The other stuff----
  observeEvent(input$setObjectives, {
    question <<- input$questionInput
    isolate({updateTabItems(session, "tabs", "objectives")}) # Selects the objectives tab after entering the question
  })
  observeEvent(input$defineOptionsButton, {
    isolate({updateTabItems(session, "tabs", "options")}) # Selects the options tab after entering the question
  })
  # Objective addition and removal----
  insertedObj <- c() # Keep track of objectives inserted and removed
  observeEvent(input$newObjectiveButton, {
    btn <- length(insertedObj)+1
    id <- paste0('objective_', btn)
    insertUI(
      selector = '#placeholder'
      # Wrap element in a div with id for ease of removal
      , ui = tags$div(
        textInput(inputId = paste0('objective_', btn)
                  , label = paste0('Objective ', btn))
        , id = id)
    )
    insertedObj <<- c(insertedObj, id)
  })
  observeEvent(input$removeObjectiveButton, {
    removeUI(
      # Pass in appropriate div id
      selector <- paste0('#', insertedObj[length(insertedObj)])
    )
    insertedObj <<- insertedObj[-length(insertedObj)]
  })
  
  # Objective sliders----
  insertedObjComp <- c()
  observeEvent(input$compareObjectivesButton,{
    # Remove objectives if they already exist
    if(length(insertedObjComp) > 0){
      for(l in 1:length(insertedObjComp)){
        removeUI(
          selector <- paste0('#', insertedObjComp[1])
        )
      }
      insertedObjComp <<- c()
    }
    # Create comparison df
    # numCompsObj <- choose(length(insertedObj), 2) # Number of unique pairwise comparisons
    objComps <- expand.grid(insertedObj, insertedObj, stringsAsFactors = F) %>%
      dplyr::rename(v1 = Var2, v2 = Var1) %>%
      dplyr::mutate(
        permutation = case_when(
          v2 > v1 ~ paste0(v1, '__', v2)
          , T ~ paste0(v2, '__', v1)
        )
        , order = 1:nrow(.)
      ) %>%
      dplyr::filter(v1 != v2) %>%
      .[!duplicated(.$permutation),] %>%
      dplyr::mutate(
        rowNum = gsub('.*_', '', v1) %>% as.numeric()
        , colNum = gsub('.*_', '', v2) %>% as.numeric()
      )
    
    # Use comparison df to create sliders
    # output$rt <- renderPrint({input$objective_1})
    newCol <- c()
    for(i in 1:nrow(objComps)){
      sliderName <- paste0(input[[objComps$v1[i]]], ' vs ', input[[objComps$v2[i]]])
      newCol <- c(newCol, sliderName)
      btnObjComp <- length(insertedObjComp)+1
      idObjComp <- paste0('objComp_', btnObjComp)
      insertUI(
        selector = '#placeholderSlider'
        , ui = tags$div(sliderInput(inputId = paste0('objectiveComparison_', i)
                                    , label = h3(sliderName)
                                    , min = 0, max = 100, value = 50)
                        , id = idObjComp
                        )
        )
      insertedObjComp <<- c(insertedObjComp, idObjComp)
    }
    objComps$sliderNames <- newCol
    objComps <<- objComps
    # output$dt <- renderDataTable({
    #   objComps
    # })
    
  })
  
  # Objective weights and donut chart----
  observeEvent(input$createObjWeightButton, {
    # Harvest comparison points
    objComps$points <- NA
    objComps$objectiveName <- NA
    for(r in 1:nrow(objComps)){
      objComps$points[r] <- 100 - input[[paste0('objectiveComparison_', r)]]
      objComps$objectiveName1[r] <- input[[objComps$v1[r]]]
      objComps$objectiveName2[r] <- input[[objComps$v2[r]]]
    }

    # Create point matrix
    oPoints <- objComps
    # Opposite cell
    for(r in 1:nrow(objComps)){
      newRow <- data.frame(v2 = objComps$v1[r]
                           , v1 = objComps$v2[r]
                           , permutation = paste0(objComps$v2[r], '__', objComps$v1[r])
                           , rowNum = objComps$colNum[r]
                           , colNum = objComps$rowNum[r]
                           , points = 100 - objComps$points[r]
                           , objectiveName1 = objComps$objectiveName2[r]
                           , objectiveName2 = objComps$objectiveName1[r]
                           )
      oPoints %<>% bind_rows(newRow)
    }
    # Diagonal
    for(d in 1:(max(objComps$rowNum)+1)){
      newRow <- data.frame(v2 = paste0('objective_', d)
                           , v1 = paste0('objective_', d)
                           , permutation = paste0('objective_', d, '__objective_', d)
                           , rowNum = d
                           , colNum = d
                           , points = 50
                           , objectiveName1 = input[[paste0('objective_', d)]]
                           , objectiveName2 = input[[paste0('objective_', d)]])
      oPoints %<>% bind_rows(newRow)
    }
    # Point only df
    oPoints %<>%
      dplyr::arrange(rowNum, colNum)
    
    # Calculate weights and sd
    owsdObjectives <<- ptw(oPoints, nameCol = 'objectiveName1', obOp = 'objective')
    
    # Plot the objective weights as a donut
    output$obWeightDonut <- renderPlotly({
      plot_ly(data = owsdObjectives
              , labels = ~objective
              , values = ~weights
              , textinfo = 'label+percent'
      ) %>%
        add_pie(hole = .6) %>%
        layout(title = "Objective Weights",  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
  })

  
  # Option addition and removal----
  insertedOpt <- c() # Keep track of options inserted and removed
  observeEvent(input$newOptionButton, {
    btnOpt <- length(insertedOpt)+1
    idOpt <- paste0('option_', btnOpt)
    insertUI(
      selector = '#placeholderOpt'
      # Wrap element in a div with id for ease of removal
      , ui = tags$div(
        textInput(inputId = paste0('option_', btnOpt)
                  , label = paste0('Option ', btnOpt))
        , id = idOpt)
    )
    insertedOpt <<- c(insertedOpt, idOpt)
  })
  observeEvent(input$removeOptionButton, {
    removeUI(
      # Pass in appropriate div id
      selector <- paste0('#', insertedOpt[length(insertedOpt)])
    )
    insertedOpt <<- insertedOpt[-length(insertedOpt)]
  })
  
  # Option sliders for each objective----
  insertedOptComp <- c()
  observeEvent(input$compareOptionsButton,{
    # Remove options if they already exist
    if(length(insertedOptComp) > 0){
      for(l in 1:length(insertedOptComp)){
        removeUI(
          selector <- paste0('#', insertedOptComp[1])
        )
      }
      insertedOptComp <<- c()
    }
    # Create comparison df for options
    optComps <- data.frame()
    for(obj in 1:length(insertedObj)){
      optCompsTemp <- expand.grid(insertedOpt, insertedOpt, stringsAsFactors = F) %>%
        dplyr::rename(v1 = Var2, v2 = Var1) %>%
        dplyr::mutate(
          permutation = case_when(
            v2 > v1 ~ paste0(v1, '__', v2)
            , T ~ paste0(v2, '__', v1)
          )
          , order = 1:nrow(.)
          , objective = obj
        ) %>%
        dplyr::filter(v1 != v2) %>%
        .[!duplicated(.$permutation),] %>%
        dplyr::mutate(
          rowNum = gsub('.*_', '', v1) %>% as.numeric()
          , colNum = gsub('.*_', '', v2) %>% as.numeric()
        )
      optComps %<>% bind_rows(optCompsTemp)
      
    }
    
    # Use optComps df to create sliders
    newCol <- c()
    optComps$optionName1 <- NA
    optComps$optionName2 <- NA
    optComps$objectiveName <- NA
    for(i in 1:nrow(optComps)){
      btnOptComp <- length(insertedOptComp)+1
      idOptComp <- paste0('optComp_', btnOptComp)
      sliderName <- paste0(input[[optComps$v1[i]]], ' vs ', input[[optComps$v2[i]]], ' for ', input[[paste0('objective_', optComps$objective[i])]])
      optComps$optionName1[i] <- input[[optComps$v1[i]]]
      optComps$optionName2[i] <- input[[optComps$v2[i]]]
      optComps$objectiveName[i] <- input[[paste0('objective_', optComps$objective[i])]]
      newCol <- c(newCol, sliderName)
      insertUI(
        selector = '#placeholderSliderOpt'
        , ui = tags$div(sliderInput(inputId = paste0('optionComparison_', i, '__objective_', optComps$objective[i])
                                    , label = h3(sliderName)
                                    , min = 0, max = 100, value = 50)
                        , id = idOptComp
        )
      )
      insertedOptComp <<- c(insertedOptComp, idOptComp)
    }
    optComps$sliderNames <- newCol
    
    optComps <<- optComps
    output$dtOpt <- renderDataTable({
      optComps
    })
    
  })
  
  # Option Comparisons and final chart----
  observeEvent(input$finalEvaluationButton, {
    # Harvest optioncomparison points
    optComps$points <- NA
    for(r in 1:nrow(optComps)){
      optComps$points[r] <- 100 - input[[paste0('optionComparison_', r, '__objective_', optComps$objective[r])]]
    }
    

    # Create option point matrix
    opPoints <- optComps
    # Opposite cell for each objective
    objectives <- unique(opPoints$objective)
    for(o in 1:length(objectives)){
      opPointsObj <- opPoints %>% dplyr::filter(objective == objectives[o])
      for(r in 1:nrow(opPointsObj)){
        newRow <- data.frame(v2 = opPointsObj$v1[r]
                             , v1 = opPointsObj$v2[r]
                             , permutation = paste0(opPointsObj$v2[r], '__', opPointsObj$v1[r])
                             , rowNum = opPointsObj$colNum[r]
                             , colNum = opPointsObj$rowNum[r]
                             , points = 100 - opPointsObj$points[r]
                             , objective = objectives[o]
                             , objectiveName = opPointsObj$objectiveName[r]
                             , optionName1 = opPointsObj$optionName2[r]
                             , optionName2 = opPointsObj$optionName1[r]
        )
        opPoints %<>% bind_rows(newRow)
      }
    }

    
    # Diagonal
    for(o in 1:length(objectives)){
      opPointsObj <- opPoints %>% dplyr::filter(objective == objectives[o])
      for(d in 1:(max(opPointsObj$rowNum))){
        newRow <- data.frame(v2 = paste0('option_', d)
                             , v1 = paste0('option_', d)
                             , permutation = paste0('option_', d, '__option_', d)
                             , rowNum = d
                             , colNum = d
                             , points = 50
                             , objective = objectives[o]
                             , objectiveName = opPointsObj$objectiveName[d]
                             , optionName1 = input[[paste0('option_', d)]]
                             , optionName2 = input[[paste0('option_', d)]]
                             )
        opPoints %<>% bind_rows(newRow)
      }
    }
    
    opPoints %<>% 
      dplyr::arrange(objective, rowNum, colNum)
    
    # Calculate weights and sd for each option
    # Point only df
    owsdOp <- data.frame()
    for(o in objectives){
      opPointsObj <- opPoints %>% dplyr::filter(objective == o)
      owsd <- ptw(opPointsObj, nameCol = 'optionName1', obOp = 'option')
      owsd$objective <- o
      owsdOp %<>% bind_rows(owsd)
    }
    
    # Join objective name to the owsdOP df
    owsdOp <- opPoints %>%
      select(objective, objectiveName) %>%
      left_join(owsdObjectives[,c('objective', 'weights')], by = c('objectiveName' = 'objective')) %>%
      rename(objectiveWeights = weights) %>%
      unique() %>%
      right_join(owsdOp, by = 'objective') %>%
      mutate(
        optionScore = objectiveWeights*weights
      ) %>%
      group_by(option) %>%
      mutate(
        totalScore = sum(optionScore)
      ) %>%
      ungroup() %>%
      mutate(
        totalScoreRank = rank(-totalScore, na.last = T, ties.method = 'first')
        , optionScoreScaled = optionScore / max(totalScore)
      ) %>%
      arrange(totalScoreRank) %>%
      mutate(
        option = fct_inorder(option)
      )
    
    # Plot
    output$finalPlotScaled <- renderPlotly({
      plot_ly(data = owsdOp, x = ~option, y = ~optionScoreScaled, type = 'bar'
              , name = ~objectiveName, color = ~objectiveName) %>%
        layout(yaxis = list(title = 'Percent of Best Option')
               , xaxis = list(title = 'Option')
               , barmode = 'stack')
    })
    
    
      
    
    # Group by option and 
    output$opPoints <- renderDataTable({
      datatable(owsdOp,
                options = list(scrollX = TRUE))
    })
  })
  
}

shinyApp(ui, server)
