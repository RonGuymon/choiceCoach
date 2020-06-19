# CHOICE COACH APPLICATION
library(shinydashboard)
library(shiny)
library(shinyBS) # For popovers
library(shinyjs) # For hiding the sidebar by default
library(tidyverse)
library(data.table)
library(lubridate)
library(magrittr)
library(plotly)
library(RColorBrewer)
library(shinycssloaders) # For loading spinners
library(rhandsontable)


# Options (like upload size)----
options(shiny.maxRequestSize=5000*1024^2)
# Colors----
giggColors <- data.frame(colorName = c("fireEngineRed", 
                                       "deepKoamaru", "persianGreen", "maximumYellowRed", 
                                       "graniteGray"
)
, hex = c("#D12229", "#28335C", "#00A499"
          , "#F3C454", "#63666A"
)
, rgb = c("rgb(209,34,41)", "rgb(40,51,92)", "rgb(0,164,153)"
          , "rgb(243,196,84)", "rgb(99,102,106)"
)
, stringsAsFactors = F)

pavementScoreColors <- c('#000000', "#FF0000", "#FF1100", "#FF2300", "#FF3400", "#FF6900"
                         , "#FFAF00", "#F7FF00", "#D4FF00", "#7CFF00", "#00FF00", "#D9D9D9")
pavementScoreLabels <- c(c(0:10, "Unknown"))

nysdotColors <- data.frame(colorName = c('NYSDOT Blue', 'Lighter Blue', 'NYSDOT Orange', 'Lighter Orange', 'Darker Orange')
                           , colorHex = c('#2471A7', '#5F97BF', '#F4AB2E', '#F9C06B', '#E8900D')
                           , stringsAsFactors = F)


# Trademark pictures----
customerTrademarkUrl <- "https://static-assets.ny.gov/sites/all/themes/ny_gov/images/nygov-logo.png"
customerHomepageUrl <- "https://www.dot.ny.gov/index"
giggTrademarkUrl <- "https://static.wixstatic.com/media/26e767_25b20366bac948fcb4ccb63b511a2147~mv2.png/v1/crop/x_232,y_154,w_407,h_163/fill/w_408,h_172,al_c,lg_1/26e767_25b20366bac948fcb4ccb63b511a2147~mv2.png"
giggHomepageUrl <- "https://www.datavinci.io/"
# wd <- getwd()
############## TEXT BOX STUFF ##############
##### UI #####
# Header----
# header <- dashboardHeader(uiOutput("header"))
header <- dashboardHeader(
  title = span(tags$a(href = giggHomepageUrl
                      , tags$img(src='nyLogo.png'
                                 , title = "New York Logo and Link"
                                 , height = "50px"
                                 , width = "94px"
                                 , position = "center")
                      # , style = "padding-left:30px;"
  )
  
  )
)
# Sidebar----
sidebar <- dashboardSidebar(uiOutput("sidebarpanel")
                            # , width = 350
                            , collapsed = T
                            )
# Body----
body <- dashboardBody(uiOutput("body")
                      , shinyjs::useShinyjs() # So that sidebar can open once you login, and to show messages from functions.
                      , tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "NYSDOT.css")
                      )
)   
# UI----
ui <- dashboardPage(title = "NYSDOT", header, sidebar, body)
# Login and loading boxes----
login <- fluidPage(
  tags$style(".skin-blue .content{background-image: url(\"nyLogo.png\");
             background-repeat: no-repeat;
             background-size: 55%;
             background-attachment: fixed;
             background-position: 45% 17%;
             background-color: #ffffff;
             }")
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , fluidRow(
    column(width = 8, offset = 4
           , textInput("userName", "Username")
           , passwordInput("passwd", "Password")
           , br()
           , actionButton("Login", "LOG IN",
                          style = "color: #FFF; 
                          background-color: #2471A7; 
                          border-color: #2471A7; 
                          border-radius: 0;")
           )
           )
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
  , br()
           )





##### SERVER #####
server <- function(input, output, session) {
  # output$testText <- renderText({wd})
  # To logout back to login page----
  login.page = paste0(
    isolate(session$clientData$url_protocol) # e.g., https
    , "//"
    , isolate(session$clientData$url_hostname) # e.g., 127.0.0.1 or 
    , isolate(session$clientData$url_pathname) # e.g., /
    , isolate(session$clientData$url_port) # e.g., 1234
    , isolate(session$clientData$search) # e.g., ?foo=123&bar=somestring
  )
  remoteAddress <- isolate(paste(session$clientData$url_hostname
                                 , session$clientDAta$url_pathname
                                 , session$clientData$url_port
                                 , session$clientData$search
                                 , sep = "_")) # Use this to get ip address of user
  ######### STEP 2: CREATE OBJECTIVE WEIGHTS ###########
  observeEvent(input$createObjWeights, {
    objectives <- c(input$objective1, input$objective2, input$objective3)
    isolate({updateTabItems(session, "tabs", "objWeights")}) # Automatically navigates to the next tab when the button is selected
    
    # Create pairs to compare
    USER$objPerm <- expand.grid(objectives, objectives, stringsAsFactors = F) %>% 
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
    
    USER$keyComps <- USER$objPerm %>%
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
    
    USER$firstRow <- USER$keyComps %>% dplyr::filter(firstRow == 1) %>% dplyr::select(-firstRow)
    
  })
  
  output$objComparisonSliders <- renderUI({
      lapply(1:nrow(USER$firstRow), function(i){
          sliderInput(paste0('objWeightComp_', i)
                      , label = paste0(USER$firstRow$V1[i], ' vs. ', USER$firstRow$V2[i])
                      , min = 0, max = 100
                      , value = 50
                      , step = 1
          )
      })
  })
  
  ######### STEP 3: VIEW OBJECTIVE WEIGHTS ###########
  observeEvent(input$viewObjWeights, {
    output$objWeightsChart <- renderPlotly({
      
      # Infer points for the rest----
      idf <- expand.grid(objComparisonSliders, stringsAsFactors = F) %>% 
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
      
      
      p <- ggplot(objectiveWeights, aes(x = Objectives, y = Weights)) +
        geom_bar(stat = 'identity', fill = 'steelblue') +
        theme_minimal() +
        labs(title = 'Objective Weights')
      ggplotly(p)
    })
  })
  
  
  
  ########### AUTHENTICATION ###########
  USER <- reactiveValues(Logged = F)

  # output$text1 <- renderText(ls())
  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- if(Username == ""){1}else{0} # put username in quotes
          Id.password <- if(Password == ""){1}else{0} # put pw in quotes
          # Notification of wrong unsername/password
          if(Id.username == 0 | Id.password == 0){
            loginNotification <<- showNotification(
              ui = "Incorrect Username/Password",
              # duration = 5, 
              closeButton = T,
              type = "error"
            )
          }
          if (Id.username > 0 & Id.password > 0){
            if (Id.username == Id.password) {
              loginNotification <<- showNotification(
                ui = "Loading Data",
                duration = 180,
                closeButton = T,
                type = "default"
              )
              
              # Give signal to set up the rest of the dashboard----
              USER$Logged <<- TRUE
              # cat(input$sidebarPanel, "\n")
              isolate({updateTabItems(session, "tabs", "importData")}) # Selects the importData tab after logging in
              # Remove the notification----
              removeNotification(loginNotification)
            }
          }
        }
      }
    }
  })
  # Dynamic sidebar config----
  output$sidebarpanel <- renderUI({
    if(USER$Logged == TRUE){
      div(
        sidebarUserPanel(
          h5(paste0(name = isolate(input$userName)))
          , subtitle = a(icon("usr"), icon("user"), "Logout", href = login.page)
          # , image = "https://www.secondcity.com/wp-content/uploads/2014/09/SC_Alumni_Farley_Chris_600x600_001.jpg" # Chris Farley
          , image = 'https://localtvkfsm.files.wordpress.com/2019/09/mgn_1280x960_50614k00-kzoku.jpg?quality=85&strip=all&w=300&h=225' # Batman
        )
        # , h6(paste0("Last update: ", as.character(maxDate)))
        ,br()
        # , verbatimTextOutput("testText")
        , sidebarMenu(id = "tabs",
          menuItem("Step 1: Define Objectives", tabName = "defineObjectives", icon = icon("arrow-alt-circle-up"))
          , menuItem("Step 2: Create Objective Weights", tabName = "objWeights", icon = icon("arrow-alt-circle-up"))
          , conditionalPanel(
            condition = "input.workPlanButton > 0"
            , radioButtons("projTenthRadio", "Summary Level"
                           , choices = list("Project" = "Project"
                                            , "Tenth" = "Tenth")
                           , selected = "Project"
                           )
            )
          )
        , br()
        , tags$a(href = giggHomepageUrl
                 , img(src = giggTrademarkUrl
                       , title = "Gigg Logo and Link"
                       , height = "40px"
                       , width = "100px")
                 , style = "padding-left:30px;"
        )
      )
    }
  })
  # Dynamic body config----
  output$body <- renderUI({
    if(USER$Logged == T){
      removeClass(selector = "body", class = "sidebar-collapse")
      tabItems(
        # Step 1: Define Objectives----
        tabItem(tabName = 'defineObjectives'
                , fluidRow(
                  box(width = 12
                      , collapsible = T
                      , title = 'Identify the Objectives'
                      , textInput("objective1", label = h3("Text input"), value = "Reliability")
                      , textInput("objective2", label = h3("Text input"), value = "Efficiency")
                      , textInput("objective3", label = h3("Text input"), value = "Cost")
                      , actionButton("createObjWeights", label = "Create Objective Weights")
                  )
                )
                )
        # Step 2: Objective Weights----
        , tabItem(tabName = 'objWeights'
                , fluidRow(
                  box(width = 12
                      , collapsible = T
                      , title = 'Create Objective Weights'
                      # , output$view <- renderTable({
                      #   USER$firstRow
                      # })
                      , uiOutput("objComparisonSliders")
                      , actionButton("viewObjWeights", label = "View Objective Weights")
                  )
                  , box(width = 12
                      , collapsible = T
                      , title = 'Visualize Objective Weights'
                      , plotlyOutput('objWeightsChart')
                      , actionButton("optWeights", label = "Compare Options")
                  )
                )
        )
        # Step 3: Option Weights---
        , tabItem(tabName = 'optComparison'
                  , fluidRow(
                    box(width = 12
                        , collapsible = T
                        , title = 'Option Comparison'
                        
                    )
                  )
        )

      )
    }else{
      login
    }
  })
  
}

#### RUN THE APP ####
shinyApp(ui = ui, server = server)

