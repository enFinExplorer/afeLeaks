library(shiny)
library(gentelellaShiny)
library(shinyWidgets)
#library(noncensus)
library(openintro)
library(tidyverse)
library(rdrop2)
library(dplyr)
library(shinyjs)
library(shinyBS)

token <- readRDS('./token1/droptoken.rds')

outputDir <- "responses"

saveData <- function(data) {
  #drop_acc(dtoken = token)
  #data <- t(data)
  # Create a unique file name
  fileName <- 'baseData.rds'
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  saveRDS(data, filePath)
  # Upload the file to Dropbox
  drop_upload(filePath, path = outputDir, dtoken = token)
}

saveData1 <- function(data) {
  #drop_acc(dtoken = token)
  #data <- t(data)
  # Create a unique file name
  fileName <- 'basePDF.rds'
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  saveRDS(data, filePath)
  # Upload the file to Dropbox
  drop_upload(filePath, path = outputDir, dtoken = token)
}

loadData <- function() {
  #drop_acc(dtoken = token)
  # Read all the files into a list
  #filesInfo <- drop_dir(outputDir, dtoken = token)
  filePaths <- '/responses/basedata.rds'
  #data <- lapply(filePaths, drop_download)
  data <- drop_download(filePaths, local_path = './responses/', dtoken = token,
                        overwrite = TRUE)
  # Concatenate all data together into one data.frame
  data <- readRDS('./responses/basedata.rds')
  data
}

loadData1 <- function() {
  #drop_acc(dtoken = token)
  # Read all the files into a list
  #filesInfo <- drop_dir(outputDir, dtoken = token)
  filePaths <- '/responses/basepdf.rds'
  #data <- lapply(filePaths, drop_download)
  data <- drop_download(filePaths, local_path = './responses/', dtoken = token,
                        overwrite = TRUE)
  # Concatenate all data together into one data.frame
  data <- readRDS('./responses/basepdf.rds')
  data
}

countyList <- readRDS('./data/countyList.rds')
#print(head(countyList))
options(shiny.jquery.version=1)

shinyApp(
  ui = gentelellaPageCustom(
    title = "AFE Leaks",
    navbar = gentelellaNavbar(
      
      # navbarItems = notif(
      #   
      #   id = "menunotif",
      #   icon = icon("info"),
      #   status = "primary",
      #   expanded = FALSE,
      #     notifItem(
      #       title = "John Doe",
      #       date = "1 min ago",
      #       img = paste0("https://image.flaticon.com/icons/svg/163/16382", 1,".svg"),
      #       ""
      #     )
      #   
      # )
    ),
    sidebar = gentelellaSidebar(
      site_title = shiny::HTML(paste(shiny::icon("gas-pump"),
                                     "AFE Leaks")),
      #uiOutput("profile"),
      sidebarDate(),
      sidebarMenu(
        sidebarItem(
          "AFE Upload",
          tabName = "tab1", 
          icon = tags$i(class = "fas fa-share"), 
          badgeName = "new",
          badgeStatus = "danger"
        ),
        sidebarItem(
          "AFE Viewer",
          tabName = "tab2", 
          icon = tags$i(class = "fas fa-archive")
        )
      )
    ),
    body = gentelellaBody(
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "tab1",
          fluidRow(
            column(
              width = 4,
              align = "center",
              fileInput('file_input', 'Upload PDF', accept = c('.pdf')),
              selectInput('state', 'State', choices = state2abbr(state.name)),
              selectInput('county', 'County', choices = unique(countyList$county_name)),
              textInput('operator', "Operator Name"),
              textInput('API', label = "API Number"),
              textOutput('apiDouble'),
              textInput('well', label = "Well Name"),
              dateInput('apiDate', label ='AFE Date'),
              numericInput("preSpud", "Pre-Spud Capex, $", value = 0),
              numericInput("drill", "Drill Cost, $", value = 0),
              numericInput('complete', "Completion Cost, $", value = 0),
              numericInput('facilities', 'Facilities Cost, $', value = 0),
              #actionBttn('upload', 'Upload to Database', icon = icon('table'),
              #           color = 'primary', style = 'jelly', size = 'sm')
              bsButton('upload', 'Upload to Database', icon = icon('table'),
                       size = 'small', tyle = 'primary')
            ),
            column(
              width = 8,
              align = "center",
              uiOutput("pdfview")
            )
          ),
          fluidRow(
            h6('Disclaimer:  All data is crowdsourced.  We do not correct the data.
               We do not provide investment advice and any 
               investment decisions based on this data is purely at the risk of the individual
               or business using it.')
          )
        ),
        tabItem(
          tabName = "tab2",
          fluidRow(
            column(
              4,
              selectInput('state1', 'State', choices = state2abbr(state.name)),
              selectInput('county1', 'County', choices = unique(countyList$county_name)),
              selectInput('operator1', "Operator Name", choices = ''),
              selectInput('well1', label = "Well Name", choices = ''),
              selectInput('API1', label = "API Number", choices = ''),
              textOutput('apiDate1'),
              textOutput('preSpud1'),
              textOutput('drill1'),
              textOutput('complete1'),
              textOutput('facilities1'),
              textOutput('total1')
            ),
            column(
              width = 8,
              align = "center",
              uiOutput("pdfview2")
            )
          ),
          fluidRow(
            h6('Disclaimer:  All data is crowdsourced.  We do not correct the data.
               We do not provide investment advice and any 
               investment decisions based on this data is purely at the risk of the individual
               or business using it.')
          )
        )
      )
    ),
    footer = gentelellaFooter(
      leftText = 'Energy FinTwit',
      rightText = '2020'
    )
  ),
  server = function(input, output, session) {
    
    values <- reactiveValues()
    
    afeData <- loadData()
    #print(head(afeData))
    afePDF <- loadData1()
    #print(head(afePDF))
    output$apiDouble <- renderText({
      if(input$API %in% afeData$API){
        shinyjs::disable('upload')
        'Well is already in database'
        
      } 
    })
    
    observe({
      print(input$state)
      print(input$county)
      print(input$operator)
      print(input$well)
      print(input$API)
      print(input$apiDate)
      
      
      if(is.null(input$state)|is.null(input$county)|
         is.null(input$operator)|is.null(input$well)|
         is.null(input$API)|is.null(input$file_input)|
         is.null(input$apiDate)|input$state == ''|
         input$county == ''|input$operator == ''|
         input$well == ''|input$API == ''){
        shinyjs::disable('upload')
      } else {
        shinyjs::enable('upload')
      }
    })
    
    observe({
      updateSelectInput(session, 'state1', 'State', unique(afeData$State))
      #updateSelectInput(session, 'operator1', 'Operator', unique(afeData$Operator))
      #updateSelectInput(session, 'well1', 'Well', unique(afeData$Well))
      #updateSelectInput(session, 'API1', 'API', unique(afeData$API))
    })
    # 
    # 
    # 
    # 
    observeEvent(input$state1, {
      #req(input$state)
      countyList1 <- countyList %>% filter(county_name %in% afeData$County)
      #print(head(countyList1))
      updateSelectInput(session, 'county1', label = 'County', choices = unique(countyList1$county_name))
    })
    # 
    observeEvent(input$county1, {
      afeData1 <- afeData %>% filter(State %in% input$state1) %>%
        filter(County %in% input$county1)

      updateSelectInput(session, 'operator1', label = 'Operator', unique(afeData1$Operator))
      #updateSelectInput(session, 'well1', unique(afeData1$Well))
      #updateSelectInput(session, 'AFE1', unique(afeData1$API))

    })
    # 
    observeEvent(input$operator1, {
      afeData1 <- afeData %>% filter(State %in% input$state1) %>%
        filter(County %in% input$county1) %>% filter(Operator %in% input$operator1)

      #updateSelectInput(session, 'operator1', unique(afeData1$Operator))
      updateSelectInput(session, 'well1', 'Well', unique(afeData1$Well))
      #updateSelectInput(session, 'AFE1', unique(afeData1$API))

    })
    # 
    observeEvent(input$well1, {
      afeData1 <- afeData %>% filter(State %in% input$state1) %>%
        filter(County %in% input$county1) %>% filter(Operator %in% input$operator1) %>%
        filter(Well %in% input$well1)

      #updateSelectInput(session, 'operator1', unique(afeData1$Operator))
      #updateSelectInput(session, 'well1', unique(afeData1$Well))
      updateSelectInput(session, 'API1', 'API Number', unique(afeData1$API))

    })
    # 
    
    output$apiDate1 <- renderText({
      afeData1 <- afeData %>% filter(API %in% input$API1)
      paste0('AFE Date: ', afeData1$Date)
    })
    
    
    
    output$preSpud1 <- renderText({
      afeData1 <- afeData %>% filter(API %in% input$API1)
      paste0('Pre-Spud, $: ', afeData1$PreSpud)
    })

    output$drill1 <- renderText({
      afeData1 <- afeData %>% filter(API %in% input$API1)
      paste0('Drill, $: ', afeData1$Drill)
    })
    output$complete1 <- renderText({
      afeData1 <- afeData %>% filter(API %in% input$API1)
      paste0('Complete, $: ', afeData1$Complete)
    })

    output$facilities1 <- renderText({
      afeData1 <- afeData %>% filter(API %in% input$API1)
      paste0('Facilities, $: ', afeData1$Facilities)
    })
    output$total1 <- renderText({
      afeData1 <- afeData %>% filter(API %in% input$API1)
      paste0('Total Capex, $: ', afeData1$Total)
    })
    
    observeEvent(input$upload, {
      shinyjs::disable('upload')
      updateButton(session, 'upload', label = 'Uploading....')
      df1 <- data.frame(State = input$state,
                        County = input$county,
                        Operator = input$operator,
                        API = input$API,
                        Well = input$well,
                        PreSpud = input$preSpud,
                        Complete = input$complete,
                        Drill = input$drill,
                        Facilities = input$facilities,
                        Total = input$preSpud + input$complete + input$drill + input$facilities,
                        Date = input$apiDate)
      
      df1 <- rbind(afeData, df1)
      print(head(df1))
      saveData(df1)
      afePDF1 <- list(values$test_file)
      names(afePDF1) <- input$API
      afeList <- append(afePDF, afePDF1)
      saveData1(afeList)
      
      #saveData(afeList)
      shinyjs::enable('upload')
      updateButton(session, 'upload', label = 'Upload to Database')
    })
    
    observe({
      #req(input$state)
      countyList1 <- countyList %>% filter(state %in% input$state)
      #print(head(countyList1))
      updateSelectInput(session, 'county', label = 'County', choices = unique(countyList1$county_name))
    })
    
    observe({
      req(input$file_input)
      test_file <- readBin(con=input$file_input$datapath,what = 'raw',n=input$file_input$size)
      values$test_file <- test_file
      writeBin(test_file,'www/myreport.pdf')
    })
    output$pdfview <- renderUI({
      if(is.null(input$file_input)){
        NULL
      } else {
      tags$iframe(style="height:600px; width:100%", src="myreport.pdf")
      }
    })
    
    observe({
      req(input$API1)
      #print(head(afePDF))
      test_file <- afePDF[[input$API1]]
      print(head(test_file))
      #test_file <- afePDF1()
      writeBin(test_file,'www/myreport1.pdf')
    })
    output$pdfview2 <- renderUI({
      if(is.null(input$API1)){
        NULL
      } else {
        tags$iframe(style="height:600px; width:100%", src="myreport1.pdf")
      }
    })
    
    


  }
)