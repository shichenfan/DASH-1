#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Shichen Fan 
# Atlanta Regional Commission
# June 2019
# Code for Equity Analysis page on ARC DASH page https://atlregional.github.io/DASH/Equity.html

library(c3)
library(classInt)
library(devtools)
library(DT)
library(dplyr)
library(fmsb)
library(ggvis)
library(ggplot2)
library(googlesheets)
library(htmlwidgets)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(maptools)
library(rgeos)
library(rgdal)
library(rmapshaper)
library(sf)
library(sp)
library(stringr)
library(stringi)
library(shinyWidgets)
library(shinycssloaders)
library(shinyalert)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(tidyr)
library(plotly)
library(flexdashboard)
library(highcharter)

load(file ="city_censustrack.RData")

######Feedback BOX
#--------------------------------
sheet <- gs_key('YOUR ACCESS KEY TO YOUR GOOGLE SHEET')

ffpal  <- colorNumeric(c('#ffffc9','#c7e9b4','#41b6c4','#225ea8','#081d58'), 0:4)
ffpal1 <- colorBin(c('#ffffc9','#c7e9b4','#41b6c4','#225ea8','#081d58'),bins = c(0,13.5,16.5,19.5,22.5,30))
# ------------------

shp<-readRDS('export_output.rds')
county_mean1 <- readRDS('county_mean.rds')
shp2<-readRDS('Counties_Atlanta_Region.rds')
shp3<-readRDS('City.rds')

zipcode <- spTransform(shp,  CRS('+proj=longlat +datum=WGS84'))
county  <- spTransform(shp2, CRS('+proj=longlat +datum=WGS84'))
city    <- spTransform(shp3, CRS('+proj=longlat +datum=WGS84'))


####zipcodedownload
zipcodeDL <- data.frame(
  zipcode$OBJECTID, zipcode$NAMELSAD, zipcode$NAMELSAD10, zipcode$IDDD, zipcode$MPO_BroadS,
  zipcode$MPO_Disabi, zipcode$MPO_Englis, zipcode$MPO_Ethnic, zipcode$MPO_Female,
  zipcode$MPO_Foreig, zipcode$MPO_Povert, zipcode$MPO_Race, zipcode$MPO_Senior,
  zipcode$MPO_Youth)

colnames(zipcodeDL) <- c('OBJECTID', 'NAMELSAD', 'NAMELSAD10', 'IDDD', 'Composite Results',
                       'DISABLILITY', 'ENGLISH', 'ETHINC', 'FEMALE', 'FOREIG', 'POVERT', 'RACE',
                       'SENIOR', 'YOUTH')
zipcode$countynum <- as.numeric(zipcode$COUNTYFP)

##### LEAFTLET MAP GENERATE #####

m <- leaflet(zipcode,options=leafletOptions(minZoom = 8, maxZoom = 20)) %>%
  clearBounds() %>%
  addProviderTiles(providers$CartoDB.Positron,    group = "Light") %>%
  addProviderTiles(providers$CartoDB.DarkMatter,  group = "Dark") %>%
  addProviderTiles(providers$Esri.WorldImagery,   group = "Satellite") %>%
  addSearchOSM() %>%
  addResetMapButton() %>%
  addEasyButton(easyButton(
    icon = 'fa-crosshairs', 
    title = 'Locate Me',
    onClick = JS("function(btn, map){ map.locate({setView: true});}"))
    ) %>%
  addScaleBar() %>%
  setView(lng = -84.3880,lat = 33.7490,zoom = 9) %>%
  addMapPane("base", zIndex = 410) %>%
  addMapPane("up", zIndex = 420) %>%
  addMapPane("subc", zIndex = 415) %>%
  addMapPane("highlight", zIndex = 430) %>%
  addPolygons(data = county,
              noClip = T,
              fill = F, 
              weight = 2,
              smoothFactor = 0.5,
              color = "#FFFFFF", 
              group = "county",
              options = pathOptions(pane = "up")
              ) %>%
  addLayersControl(
    baseGroups=c('Light','Dark','Satellite')  #base map choice : http://leaflet-extras.github.io/leaflet-providers/preview/index.html
  )


### LOADING ANIMATION ###
appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"
### LOADING ANIMATION END ###



# Load help contents-------------------------------
helpData <- read.csv("help1.csv")

# Load help contents-------------------------------


# FUNCTION
myfunction <- function(x,id){
  if(x[which(zipcode$OBJECTID == id)] == 0){
    print('Well Below average')
  }
  else if(x[which(zipcode$OBJECTID == id)] == 1){
    print('Below average')
  }
  else if(x[which(zipcode$OBJECTID == id)] == 2){
    print('Average')
  }
  else if(x[which(zipcode$OBJECTID == id)] == 3){
    print('Above Average')
  }
  else if(x[which(zipcode$OBJECTID == id)] == 4){
    print('Well Above Average')
  }
}
# Function end


########### GAUGE function ###########################

gauge1 <- function(value, min, max, sectors = gaugeSectors1(),
                   symbol = NULL, label = NULL,
                   abbreviate = TRUE, abbreviateDecimals = 1,
                   href = NULL) {
  
  x <- list(
    value = value,
    min = min,
    max = max,
    customSectors = I(resolveSectors1(sectors, min, max)),
    symbol = symbol,
    label = label,
    humanFriendly = abbreviate,
    humanFriendlyDecimal = abbreviateDecimals,
    href = href
  )
  
  # create widget
  htmlwidgets::createWidget(
    name = 'gauge',
    x,
    package = 'flexdashboard',
    dependencies = rmarkdown::html_dependency_jquery()
  )
}

#' @export
#' @rdname gauge
gaugeSectors1 <- function(wba = NULL, ba = NULL, average = NULL,aa = NULL,waa =NULL,
                          colors = c("wba", "ba", "average","aa","waa")) {
  list(wba = wba,
       ba = ba,
       average = average,
       aa=aa,
       waa=waa,
       colors = colors)
}

#' Shiny bindings for gauge
#'
#' Output and render functions for using gauge within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a gauge
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name gauge-shiny
#'
#' @export
gaugeOutput <- function(outputId, width = '100%', height = '100px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'gauge', width, height, package = 'flexdashboard')
}

#' @rdname gauge-shiny
#' @export
renderGauge <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, gaugeOutput, env, quoted = TRUE)
}

resolveSectors1 <- function(sectors, min, max) {
  
  # create default sectors if necessary
  if (is.null(sectors)) {
    sectors = sectors(
      wba = c(min, max),
      ba = NULL,
      average = NULL,
      aa = NULL,
      waa = NULL,
      colors = c("wba", "ba", "average","aa","waa")
    )
  }
  # provide default success range if only colors were specified
  if (is.null(sectors$wba) &&
      is.null(sectors$ba) &&
      is.null(sectors$average)  &&
      is.null(sectors$aa)  &&
      is.null(sectors$waa)) {
    sectors$success <- c(min, max)
  }
  # provide default colors if none were specified
  if (is.null(sectors$colors))
    sectors$colors <- c("wba", "ba", "average","aa","waa")
  
  # create custom sectors to pass to justgage
  customSectors <- list()
  addSector <- function(sector, color) {
    if (!is.null(sector)) {
      # validate
      if (!is.numeric(sector) || length(sector) != 2)
        stop("sectors must be numeric vectors of length 2", call. = FALSE)
      # add sector
      customSectors[[length(customSectors) + 1]] <<-
        list(lo = sector[[1]], hi = sector[[2]], color = color)
    }
  }
  sectors$colors <- rep_len(sectors$colors, 5)
  addSector(sectors$wba, sectors$colors[[1]])
  addSector(sectors$ba, sectors$colors[[2]])
  addSector(sectors$average, sectors$colors[[3]])
  addSector(sectors$aa, sectors$colors[[4]])
  addSector(sectors$waa, sectors$colors[[5]])
  
  
  # return
  customSectors
}

labs <- function(k){
  lapply(seq(nrow(zipcode)), function(i) {
  paste0( '<p>', zipcode$NAMELSAD[i], '<p></p>',
          zipcode$NAME10[i],'</p><p>',
          'Score:',#IPD
          k[i], '</p>' )
})
}

######################################
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("yeti"),    #'cosmo' works better...superhero
  tags$head(
    tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Roboto+Condensed|Cabin:400,700');"))
    ),
  list(
    tags$head(HTML('<link rel="icon" href="icon.png" type="image/png" />'))
    ),

  navbarPage(
    div(
     # img(src="logo.png",style=" height:28px; padding-right: 10px;"),
     # "DASH",
      # HELP function---------------------------------------------------------------
                    singleton(includeScript("www/annyang.min.js")),
                    singleton(includeScript("www/speechRecognition.js")),
                    singleton(includeScript("www/intro.min.js")),
                    singleton(includeCSS("www/introjs.min.css")),
                    singleton(includeCSS("www/app.css")),
                    singleton(includeScript("www/app.js")),
                    actionButton(inputId="startHelp", "help",style = "background-color:green;color:white;padding:1px;position:fixed;right:20px;top:5px;z-index:100000000000")
                    ),
    
       # ---------------------------------------------------------------------------------------
   windowTitle = "ATL DASH",id="navid",
    # source('navbar.R'),
               #---------------------------------------------------------------------------
               tabPanel("Map",
                        icon = icon("map"),
                        value = 1,
                        useShinyjs(),
                        inlineCSS(appCSS),
                        # tags$style(type = "text/css", "#plot {height: calc(100vh - 80 px) !important;}"),
                        # tags$style(type = "text/css", "#plot {height: calc(100vh - 80 px) !important;}"),
                        # leafletOutput("plot"
                        #               # ,
                        #               # height = '100%'
                        #               # ,
                        #               # height=670
                        # ) %>%
                        #   withSpinner()
                        # ,
                        fluidRow(
                          #---------------------------------------------------------------------------
                          column(7,
                                 leafletOutput("plot",height = 675  
                                 ) %>%
                                   withSpinner(),
                                 # verbatimTextOutput("clientdataText"),
                                 absolutePanel(
                                   top ='35%', left = '7%',
                                   fluidRow(id='slideropacity',
                                            noUiSliderInput(inputId ="slider1",
                                                            label =NULL,
                                                            min = 0, max = 1, value = 0.7,
                                                            orientation = "vertical",
                                                            width = "10%", height = "300px",
                                                            color = '#c6c9ce',
                                                            tooltips = F,
                                                            direction = 'rtl',
                                                            inline = T)
                                   )
                                 )
                          ),
                          #---------------------------------------------------------------------------
                          column(5,
                                 wellPanel(
                                   #------------------------------------
                                   fluidRow(
                                     #------------------------------------
                                     column(12,id='popgroupselection',
                                            pickerInput('popgroup',
                                                        'Equity Analysis Criteria',
                                                        choices =c(
                                                          # "Composite Results",
                                                          "Youth","Older Adults",
                                                          "Female",
                                                          "Racial Minority",
                                                          "Ethnic Minority",
                                                          'Foreign-Born',
                                                          'Limited English Proficiency',
                                                          'People with Disabilities',
                                                          'Low-Income'),
                                                        multiple = T,
                                                        options = list(`actions-box` = TRUE),
                                                        selected = c("Racial Minority",
                                                                     "Ethnic Minority",
                                                                     'Low-Income')
                                            )
                                     ),
                                     #------------------------------------
                                     column(6, id='countyselection',
                                            selectInput("countyselect", 
                                                        "Zoom to County:",
                                                        choices =c("All", sort(unique(as.character(zipcode$NAME10)))),
                                                        selected = 'All'
                                            )
                                     ),
                                     #------------------------------------
                                     column(6, id='cityselection',
                                            selectInput("cityselect", 
                                                        "Zoom to City:",
                                                        choices =c("NA", sort(unique(as.character(city$Name))))
                                            )
                                     )
                                   ),
                                   fluidRow(
                                     id = 'UISPIDER' , 
                                     uiOutput('UISpider',
                                              style = "height: 210px")
                                   ),
                                   #------------------------------------
                                   fluidRow(                     
                                     id = 'UIDETAIL' ,  
                                     uiOutput('UIDetail',
                                              style = "overflow-y:scroll; height: 275px")
                                   )
                                   #------------------------------------
                                   # hr(),
                                 )# wellpanel end
                          )
                          #---------------------------------------------------------------------------
                        )
               ),# map end
               #---------------------------------------------------------------------------
               tabPanel("Equity",icon = icon("info"),value=8,
                        fluidRow(
                          column(12,
                                 tags$h3("ARC\'s Equity Analysis",align='center' #introduction2
                                 ),
                                 fluidRow(
                                   column(1),
                                   column(5,
                                          p('ARC\'s Equity Analysis is widely used throughout the agency to demonstrate compliance with Federal Guidance, including Title VI of the Civil Rights Act of 1964, Limited English Proficiency Executive Order, Americans with Disabilities Act of 1990, Environmental Justice Executive Order, and FHWA and FTA\'s Title VI and Environmental Justice documents.',
                                            style= "font-family: 'Roboto Condensed', Light;", align='justify')
                                   ),
                                   column(1),
                                   column(4,
                                          wellPanel(
                                            h4('Additional Information'),
                                            tags$ul(
                                              tags$li(actionLink('al1','Methodology')),
                                              br(),
                                              tags$li(actionLink('al2','Data Disclaimer')),
                                              br(),
                                              tags$li(actionLink('al3','Contact Information'))
                                            ))
                                   )
                                 )
                          )
                        ),
                        #-------------
                        hr(),
                        #-------------
                        fluidRow(
                          column(6,
                                 actionButton('button1', 'VIEW MAP')
                          ),
                          column(6,
                                 actionButton('button2', 'DOWNLOAD DATA')
                          )
                        )
                        #-------------
               ),#  'intro 'End
               #---------------------------------------------------------------------------
               tabPanel("Methodology",value = 10,
                        tags$h3("Methodology",align='center'),
                        fluidRow(
                          column(1),
                          column(5,
                                 br(),
                                 tags$p('The Equity Analysis methodology generates a composite score based on the concentrations of
                                        the criteria selected, which is used to meet the nondiscrimination requirements and
                                        recommendations of Title VI and EJ for ARC\'s plans, programs, and decision-making processes.'
                                        ,style = "font-family: 'Roboto Condensed', Light;", align='justify'),
                                 tags$p('The score calculation is determined by standard deviations relative to a criteria\'s
                                        regional average. This score classifies the concentration of the populations of interest
                                        under Title VI and EJ present in every census tract in the region. These population groups
                                        are represented by the nine equity analysis criteria: youth, older adults, females, racial
                                        minorities, ethnic minorities, foreign-born, limited English proficiency, people with
                                        disabilities, and low-income.'
                                        ,style = "font-family: 'Roboto Condensed', Light;", align='justify'),
                                 tags$p('The data for each of the criteria in the equity analysis are split into five \"bins\" based on 
                                        the relative concentration across the region: well below average (score of 0); below average (score
                                        of 1); average (score of 2); above average (score of 3); and well above average (score of 4). See 
                                        Figure 1 below. A summary score of all nine indicators for each census tract (ranging from 0-36)
                                        is used to show regional concentrations of populations of interest under Title VI and EJ. A summary
                                        score of racial minority, ethnic minority, and low-income for each census tract is used in ARC\'s Project Evaluation Framework to prioritize projects in the Transportation Improvement Program (TIP).
                                        This view is the map default.',style = "font-family: 'Roboto Condensed', Light;", align='justify'),
                                 tags$p('Bin 2 for each indicator contains census tracts at or near (within a half standard deviation from) 
                                        the regional average (mean) for that indicator. Bins 4, 3, 1, and 0 are then built out from the regio
                                        nal average; Bins 1 and 3 go another full standard deviation out from bin 2, and bins 0 and 4 contain 
                                        any remaining tracts further out from 1 or 3, respectively. ',style = "font-family: 'Roboto Condensed', Light;", align='justify')
                                 ),
                          column(5, 
                                 HTML("<p style='text-align:center'><img src = 'method.png' style='height:200px;'> "),
                                 tags$p('This Equity Analysis supplants previous equity analysis iterations, including ARC\'s Equitable Targ
                                        et Areas (ETAs).',style = "font-family: 'Roboto Condensed', Light;", align='justify'),
                                 tags$p('The design of this methodology is supported by both FHWA\'s and FTA\'s Title VI recommendations to simply
                                        identify the protected classes using demographic data from the US Census Bureau as the first step in 
                                        conducting equity analyses. Additionally, ',
                                        tags$a(href = 'https://www.transit.dot.gov/regulations-and-guidance/environmental-programs/environmental-justice/environmental-justice-faqs','FTA\'s EJ guidance'), 
                                        'cautions recipients of federal funds to not be too reliant on population thresholds to determ
                                        ine the impact of a program, plan, or policy to a population group, but rather design a meaning
                                        ful measure to identify the presence of all protected and considered population groups and then 
                                        calculate the possibility of discrimination or disproportionately high and adverse effect on these 
                                        populations.',style = "font-family: 'Roboto Condensed', Light;", align='justify'),
                                 tags$p('ARC plans to continue the conversation with its staff, partners, and Transportation Equity Advisory Group (TEAG) about measuring and evaluating transportation benefits and burdens, as well as layering the Equity Analysis with supplemental analyses such as access to essential services, affordability, and displacement. '
                                        ,style = "font-family: 'Roboto Condensed', Light;", align='justify')
                                 ),
                          column(1)
                                 )
                        ),
               #---------------------------------------------------------------------------
               tabPanel("Data Disclaimer",value = 11,
                        tags$h3("Data Disclaimer",align='center'), #introduction2
                        br(),
                        fluidRow(
                          column(3),
                          column(6,
                                 tags$p('This webpage is a public resource using ACS data. 
                                        The Atlanta Regional Commission (ARC) makes no warranty, representation, or guarantee as to the content, 
                                        sequence, accuracy, timeliness, or completeness of any of the spatial data or database information provided 
                                        herein. ARC and partner state, regional, local, and other agencies shall assume no liability for errors, omissions, 
                                        or inaccuracies in the information provided regardless of how caused, or any decision made or action taken or not 
                                        taken by any person relying on any information or data furnished within.',
                                        style = "font-family: 'Roboto Condensed', Light;", align='justify'),
                                 tags$p(
                                   'ARC is committed to enforcing the provisions of Title VI of the Civil Rights Act of 1964 and taking positive and realistic affirmative steps to ensure the protection of rights and opportunities for all persons affected by its programs, services, and activities.'
                                   ,style= "font-family: 'Roboto Condensed', Light;", align='justify')
                          ),
                          column(3)
                          )
               ),
               #---------------------------------------------------------------------------
               tabPanel("Contact Info",value = 12,
                        tags$h5(HTML("<u>Equity Analysis Contact Info:</u>"), 
                                style= "font-family: 'Roboto Condensed', Light;", 
                                align='justify'),
                        tags$p('Aileen Daney', 
                               style= "font-family: 'Roboto Condensed', Light;", 
                               align='justify'),
                        tags$p('Senior Planner', 
                               style= "font-family: 'Roboto Condensed', Light;", 
                               align='justify'),
                        tags$p('Transportation Access & Mobility Group', 
                               style= "font-family: 'Roboto Condensed', Light;", 
                               align='justify'),
                        tags$p('470.378.1579', 
                               style= "font-family: 'Roboto Condensed', Light;", 
                               align='justify'),
                        tags$a(href = 'mailto:adaney@atlantaregional.org', 
                               'adaney@atlantaregional.org', 
                               style= "font-family: 'Roboto Condensed', Light;", 
                               align='justify'),
                        hr(),
                        tags$h5(HTML("<u>Title VI Policy and Complaint Contact Info:</u>"), 
                                style= "font-family: 'Roboto Condensed', Light;", align='justify'),
                        tags$p('Brittany Zwald', 
                               style= "font-family: 'Roboto Condensed', Light;", align='justify'),
                        tags$p('Title VI Officer/Grants and Contracts Analyst', 
                               style= "font-family: 'Roboto Condensed', Light;", align='justify'),
                        tags$p('Finance Group', style= "font-family: 'Roboto Condensed', Light;", align='justify'),
                        tags$p('470.378.1494', style= "font-family: 'Roboto Condensed', Light;", align='justify'),
                        tags$a(href = 'mailto:bzwald@atlantaregional.org','bzwald@atlantaregional.org', 
                               style= "font-family: 'Roboto Condensed', Light;", align='justify'),
                        hr(),
                        tags$div(
                          HTML("<p>For more information on ARC\'s Title VI program or to obtain a Title VI Policy and Complaint Form please visit:</p>")
                        ),
                        tags$a(href = 'https://atlantaregional.org/leadership-and-engagement/guidelines-compliance/title-vi-plan-and-program/','https://atlantaregional.org/leadership-and-engagement/guidelines-compliance/title-vi-plan-and-program/',
                               style= "font-family: 'Roboto Condensed', Light;", align='justify')
               ),
               #---------------------------------------------------------------------------
               tabPanel("Download Data",value = 13,
                        icon = icon('download'),
                        fluidRow(
                          column(9,
                                 selectInput("countyd",
                                             "County:",
                                             choices =c("All",sort(unique(as.character(zipcode$NAME10))))
                                 ),
                                 selectInput('cityd',
                                             "City:",
                                             choices = c('NA',sort(unique(as.character(city$Name)))))
                          ),
                          column(3,
                                 downloadButton('downloaddata','Download')
                          ),
                          DT::dataTableOutput('mytable')
                          # New add multiple selection request by GDOT
                          , fluidRow(
                            #---------------------------------------------------------------------------
                            column(7,
                                   leafletOutput("plot_ms",height = 675  
                                   ) %>%
                                     withSpinner(),
                                   # verbatimTextOutput("clientdataText"),
                                   absolutePanel(
                                     top ='35%', left = '7%',
                                     fluidRow(id='slideropacity_ms',
                                              noUiSliderInput(inputId ="slider1_ms",
                                                              label =NULL,
                                                              min = 0, max = 1, value = 0.7,
                                                              orientation = "vertical",
                                                              width = "10%", height = "300px",
                                                              color = '#c6c9ce',
                                                              tooltips = F,
                                                              direction = 'rtl',
                                                              inline = T)
                                     )
                                   )
                            ),
                            #---------------------------------------------------------------------------
                            column(5,
                                   wellPanel(
                                     #------------------------------------
                                     fluidRow(
                                       #------------------------------------
                                       column(12,id='popgroupselection_ms',
                                              pickerInput('popgroup_ms',
                                                          'Equity Analysis Criteria',
                                                          choices =c(
                                                            # "Composite Results",
                                                            "Youth","Older Adults",
                                                            "Female",
                                                            "Racial Minority",
                                                            "Ethnic Minority",
                                                            'Foreign-Born',
                                                            'Limited English Proficiency',
                                                            'People with Disabilities',
                                                            'Low-Income'),
                                                          multiple = T,
                                                          options = list(`actions-box` = TRUE),
                                                          selected = c("Racial Minority",
                                                                       "Ethnic Minority",
                                                                       'Low-Income')
                                              )
                                       ),
                                       #------------------------------------
                                       column(6, id='countyselection_ms',
                                              selectInput("countyselect_ms", 
                                                          "Zoom to County:",
                                                          choices =c("All", sort(unique(as.character(zipcode$NAME10)))),
                                                          selected = 'All'
                                              )
                                       ),
                                       #------------------------------------
                                       column(6, id='cityselection_ms',
                                              selectInput("cityselect_ms", 
                                                          "Zoom to City:",
                                                          choices =c("NA", sort(unique(as.character(city$Name))))
                                                          )
                                              )
                                       ,column(12,  dataTableOutput('table_ms'))
                                       )
                                     #------------------------------------
                                     # hr(),
                                     )# wellpanel end
                                   )
                            #---------------------------------------------------------------------------
                          )
                        )# map end
                        
               ),#downloads end
               #---------------------------------------------------------------------------
               tabPanel("Feedback",value = 14,
                        fluidRow(
                          column(7,
                                 DT::dataTableOutput('feedbackdt')),
                          column(5,
                                 textAreaInput('feedback','Feedback:',placeholder='Please leave your feedback here,Thanks!',height = 400),
                                 actionButton('feedbackbt', 'SUBMIT'))
                        )
               )
               
                )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  # Simulate work being done for 1 second
  Sys.sleep(1)
  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")
  
  datasetInput <- reactive({
    ninput <- length(input$popgroup)
    indexselect <- c(rep(0,nrow(zipcode)))
    for(i in 1:ninput){
      a<-switch(input$popgroup[i],
                "Composite Results"             = zipcode$MPO_BroadS,
                "Youth"                         = zipcode$MPO_Youth,
                "Older Adults"                  = zipcode$MPO_Senior,
                "Female"                        = zipcode$MPO_Female,
                "Racial Minority"               = zipcode$MPO_Race,
                "Ethnic Minority"               = zipcode$MPO_Ethnic,
                'Foreign-Born'                  = zipcode$MPO_Foreig,
                'Limited English Proficiency'   = zipcode$MPO_Englis,
                'People with Disabilities'      = zipcode$MPO_Disabi,
                'Low-Income'                    = zipcode$MPO_Povert
                )
      indexselect<-cbind.data.frame(a,indexselect)
      }
    rowSums(indexselect)
    })
  datasetInput_ms <- reactive({
    ninput <- length(input$popgroup_ms)
    indexselect <- c(rep(0,nrow(zipcode)))
    for(i in 1:ninput){
      a<-switch(input$popgroup_ms[i],
                "Composite Results"             = zipcode$MPO_BroadS,
                "Youth"                         = zipcode$MPO_Youth,
                "Older Adults"                  = zipcode$MPO_Senior,
                "Female"                        = zipcode$MPO_Female,
                "Racial Minority"               = zipcode$MPO_Race,
                "Ethnic Minority"               = zipcode$MPO_Ethnic,
                'Foreign-Born'                  = zipcode$MPO_Foreig,
                'Limited English Proficiency'   = zipcode$MPO_Englis,
                'People with Disabilities'      = zipcode$MPO_Disabi,
                'Low-Income'                    = zipcode$MPO_Povert
      )
      indexselect<-cbind.data.frame(a,indexselect)
    }
    rowSums(indexselect)
  })
  
  dbInput <- reactive({
    switch(input$popgroup[1],
           "Composite Results"             = c("MPO_BroadS","MPO_BroadS"),
           "Youth"                         = c('YOUTH','MPO_Youth'),
           "Older Adults"                  = c('SENIORS','MPO_Senior'),
           "Female"                        = c('FEMALE','MPO_Female'),
           "Racial Minority"               = c('RACE','MPO_Race'),
           "Ethnic Minority"               = c('ETHNICMINO','MPO_Ethnic'),
           'Foreign-Born'                  = c('FOREIGNBOR','MPO_Foreig'),
           'Limited English Proficiency'   = c('ENGLISHPRO','MPO_Englis'),
           'People with Disabilities'      = c('DISABILI_1','MPO_Disabi'),
           'Low-Income'                    = c('POVERTY','MPO_Povert')
           )
    })

  dbInput_ms <- reactive({
    switch(input$popgroup_ms[1],
           "Composite Results"             = c("MPO_BroadS","MPO_BroadS"),
           "Youth"                         = c('YOUTH','MPO_Youth'),
           "Older Adults"                  = c('SENIORS','MPO_Senior'),
           "Female"                        = c('FEMALE','MPO_Female'),
           "Racial Minority"               = c('RACE','MPO_Race'),
           "Ethnic Minority"               = c('ETHNICMINO','MPO_Ethnic'),
           'Foreign-Born'                  = c('FOREIGNBOR','MPO_Foreig'),
           'Limited English Proficiency'   = c('ENGLISHPRO','MPO_Englis'),
           'People with Disabilities'      = c('DISABILI_1','MPO_Disabi'),
           'Low-Income'                    = c('POVERTY','MPO_Povert')
    )
  })
  
  id2 <- reactive({
    # if(is.null(input$plot_shape_click)){
      (input$plot_shape_click)$id
    })
  id2_ms <- reactive({
    # if(is.null(input$plot_shape_click)){
    (input$plot_ms_shape_click)$id
  })
  
  initial <- rowSums(
    cbind.data.frame(
      zipcode$MPO_Race,zipcode$MPO_Ethnic,zipcode$MPO_Povert
      )
  )

  ccc9 <- classIntervals(initial,n=5 ,style='jenks')
  cccc9 <- c(0,ccc9$brks[2:6]+0.5)
  ffpal9 <- colorBin(c('#ffffc9','#c7e9b4','#41b6c4','#225ea8','#081d58'),bins = cccc9)
  clr9 <- ~ffpal9(initial)
  labeltitle9 <- paste0('Concentration of 3 criteria')
  
  # Output plot___the main map------------------------------------------------------------------------------
  output$plot <- renderLeaflet({
    m%>%
   
    addLegend(colors = ffpal(c(0:4)),
              labels = c('Least Concentrated','','','','Most Concentrated'),
              opacity = 0.7,
              position = "bottomright",
              group = 'census',
              title = labeltitle9) %>%
      addPolygons(
        label = lapply(labs(initial), HTML),
        labelOptions = labelOptions(direction = 'auto'),
        weight = 1.3, 
        color = clr9, 
        opacity = 0.7,
        fillColor = clr9, 
        fillOpacity = 0.7,
        stroke = F,
        smoothFactor = 0.5,
        highlightOptions = highlightOptions(
          stroke = T,
          color ='#ed3704', 
          weight = 5,
          bringToFront = TRUE,
          sendToBack = TRUE),
        layerId = ~zipcode$OBJECTID,group='census',options = pathOptions(pane = "base"))
    }
    )
  
  output$plot_ms <- renderLeaflet({
    m%>%
      addLegend(colors = ffpal(c(0:4)),
                labels = c('Least Concentrated','','','','Most Concentrated'),
                opacity = 0.7,
                position = "bottomright",
                group = 'census',
                title = labeltitle9) %>%
      addPolygons(
        label = lapply(labs(initial), HTML),
        labelOptions = labelOptions(direction = 'auto'),
        weight = 1.3, 
        color = clr9, 
        opacity = 0.7,
        fillColor = clr9, 
        fillOpacity = 0.7,
        stroke = F,
        smoothFactor = 0.5,
        highlightOptions = highlightOptions(
          stroke = T,
          color ='#ed3704', 
          weight = 5,
          bringToFront = TRUE,
          sendToBack = TRUE),
        layerId = ~zipcode$OBJECTID,group='census',options = pathOptions(pane = "base"))
    }
    )
  
  observe({
    if(length(input$popgroup) == 0){
      leafletProxy("plot", data = zipcode) %>% 
        clearControls() %>%
        removeShape(layerId = ~zipcode$OBJECTID)
      }
    else if(length(input$popgroup) == 1){
      if(input$popgroup[1] == "Composite Results"){
        clr <- ~ffpal1(datasetInput())
        leafletProxy("plot", data = zipcode) %>%
          clearControls() %>%
          addLegend(colors = ffpal(c(0:4)),
                    labels = c('Least Concentrated','','','','Most Concentrated'),
                    opacity = 0.7,
                    position = "bottomright",
                    group = 'census',
                    title = "Concentration of Criteria") %>%
          addPolygons(
            label = lapply(labs(datasetInput()), HTML),
            labelOptions = labelOptions(direction = 'auto'),
            weight = 1.3, 
            color = clr,
            opacity = input$slider1,
            fillColor = clr,
            fillOpacity = input$slider1,
            stroke = F,
            smoothFactor = 0.5,
            highlightOptions = highlightOptions(
              stroke = T,
              color ='#ed3704', 
              weight = 5,
              bringToFront = TRUE,
              sendToBack = TRUE
              ),
            layerId = ~zipcode$OBJECTID, group='census', options = pathOptions(pane = "base"))
        }else{
          clr <- ~ffpal(datasetInput())
          leafletProxy("plot",data = zipcode)%>%
            clearControls()%>%
            addLegend(colors = ffpal(c(0:4)),
                      labels = c('Well below average',
                                  'Below average',
                                  'Average',
                                  'Above average', 
                                  'Well above average'), 
                      opacity = 0.7,
                      position = "bottomright",
                      group = 'census',
                      title = paste0(input$popgroup[1], " Analysis")) %>%
            addPolygons(
              label = lapply(labs(datasetInput()), HTML),
              labelOptions = labelOptions(direction = 'auto'),
              weight = 1.3, 
              color = clr,
              opacity = input$slider1,
              fillColor = clr,fillOpacity = input$slider1,
              stroke = F,
              smoothFactor = 0.5,
              highlightOptions = highlightOptions(
                stroke = T,
                color = '#ed3704', 
                weight = 5,
                bringToFront = TRUE,
                sendToBack = TRUE
                ),
              layerId = ~zipcode$OBJECTID,
              group = 'census',
              options = pathOptions(pane = "base")
            )
          }
      }else{
        ### Multi  select Index ###-------------------------------------------------------------
        ccc <- classIntervals(
          datasetInput(),
          n = 5,
          style = 'jenks')
        cccc <- c(0, ccc$brks[2:6]+0.5)
        ffpal2 <- colorBin(
          c('#ffffc9','#c7e9b4','#41b6c4','#225ea8','#081d58'),
          bins = cccc)
        clr <- ~ffpal2(datasetInput())
        labeltitle <- paste0('Concentration of ',length(input$popgroup), ' criteria')
        
        leafletProxy("plot",data = zipcode)%>%
          clearControls()%>%
          addLegend(colors = ffpal(c(0:4)),
                    labels  = c('Least Concentrated','','','','Most Concentrated'),
                    opacity = 0.7,
                    position = "bottomright",
                    group = 'census',
                    title = labeltitle)%>%
          addPolygons(
            label = lapply(labs(datasetInput()), HTML),
            labelOptions = labelOptions(direction = 'auto'),
            weight = 1.3, 
            color = clr,
            opacity = input$slider1,
            fillColor = clr,
            fillOpacity = input$slider1,
            stroke = F,
            smoothFactor = 0.5,
            highlightOptions = highlightOptions(
              stroke = T,
              color ='#ed3704', 
              weight = 5,
              bringToFront = TRUE,
              sendToBack = TRUE),
            layerId = ~zipcode$OBJECTID,group ='census',options = pathOptions(pane = "base")
            )
        }
})
  
  observe({
    if(length(input$popgroup_ms) == 0){
      leafletProxy("plot_ms", data = zipcode) %>% 
        clearControls() %>%
        removeShape(layerId = ~zipcode$OBJECTID)
    }
    else if(length(input$popgroup_ms) == 1){
      if(input$popgroup_ms[1] == "Composite Results"){
        clr_ms <- ~ffpal1(datasetInput_ms())
        leafletProxy("plot_ms", data = zipcode) %>%
          clearControls() %>%
          addLegend(colors = ffpal(c(0:4)),
                    labels = c('Least Concentrated','','','','Most Concentrated'),
                    opacity = 0.7,
                    position = "bottomright",
                    group = 'census',
                    title = "Concentration of Criteria") %>%
          addPolygons(
            label = lapply(labs(datasetInput_ms()), HTML),
            labelOptions = labelOptions(direction = 'auto'),
            weight = 1.3, 
            color = clr_ms,
            opacity = input$slider1_ms,
            fillColor = clr_ms,
            fillOpacity = input$slider1_ms,
            stroke = F,
            smoothFactor = 0.5,
            highlightOptions = highlightOptions(
              stroke = T,
              color ='#ed3704', 
              weight = 5,
              bringToFront = TRUE,
              sendToBack = TRUE
            ),
            layerId = ~zipcode$OBJECTID, group='census', options = pathOptions(pane = "base"))
      }else{
        clr_ms <- ~ffpal(datasetInput_ms())
        leafletProxy("plot_ms",data = zipcode)%>%
          clearControls()%>%
          addLegend(colors = ffpal(c(0:4)),
                    labels = c('Well below average',
                               'Below average',
                               'Average',
                               'Above average', 
                               'Well above average'), 
                    opacity = 0.7,
                    position = "bottomright",
                    group = 'census',
                    title = paste0(input$popgroup_ms[1], " Analysis")) %>%
          addPolygons(
            label = lapply(labs(datasetInput_ms()), HTML),
            labelOptions = labelOptions(direction = 'auto'),
            weight = 1.3, 
            color = clr_ms,
            opacity = input$slider1_ms,
            fillColor = clr_ms,fillOpacity = input$slider1_ms,
            stroke = F,
            smoothFactor = 0.5,
            highlightOptions = highlightOptions(
              stroke = T,
              color = '#ed3704', 
              weight = 5,
              bringToFront = TRUE,
              sendToBack = TRUE
            ),
            layerId = ~zipcode$OBJECTID,
            group = 'census',
            options = pathOptions(pane = "base")
          )
      }
    }else{
      ### Multi  select Index ###-------------------------------------------------------------
      ccc_ms <- classIntervals(
        datasetInput_ms(),
        n = 5,
        style = 'jenks')
      cccc_ms <- c(0, ccc_ms$brks[2:6]+0.5)
      ffpal2_ms <- colorBin(
        c('#ffffc9','#c7e9b4','#41b6c4','#225ea8','#081d58'),
        bins = cccc_ms)
      clr_ms <- ~ffpal2_ms(datasetInput_ms())
      labeltitle_ms <- paste0('Concentration of ',length(input$popgroup_ms), ' criteria')
      
      leafletProxy("plot_ms",data = zipcode)%>%
        clearControls()%>%
        addLegend(colors = ffpal(c(0:4)),
                  labels  = c('Least Concentrated','','','','Most Concentrated'),
                  opacity = 0.7,
                  position = "bottomright",
                  group = 'census',
                  title = labeltitle_ms)%>%
        addPolygons(
          label = lapply(labs(datasetInput_ms()), HTML),
          labelOptions = labelOptions(direction = 'auto'),
          weight = 1.3, 
          color = clr_ms,
          opacity = input$slider1_ms,
          #opacity  =input$slider1,
          fillColor = clr_ms,
          fillOpacity = input$slider1_ms,
          stroke = F,
          smoothFactor = 0.5,
          highlightOptions = highlightOptions(
            stroke = T,
            color ='#ed3704', 
            weight = 5,
            bringToFront = TRUE,
            sendToBack = TRUE),
          layerId = ~zipcode$OBJECTID,group ='census',options = pathOptions(pane = "base")
        )
    }
  })
  # ### zoomlevel ###-----------------------------------------------------------------------------------


  
  ### highlight county selected ### -----------------------------------
  observe({
    if(input$countyselect == 'All'){
      leafletProxy("plot",data=zipcode)%>%
        removeShape(layerId = 'Hightlightcounty')
    }
    else{
      datalselect11 <- county[county$NAME10 == input$countyselect, ]
      leafletProxy("plot", data = zipcode) %>%
        removeShape(layerId = 'Hightlightcounty') %>%
        addPolygons(data = datalselect11, 
                    noClip = T,
                    fill = F,
                    smoothFactor = 0.5,
                    weight = 2, 
                    color = "#f91313", 
                    group = "county", 
                    layerId ='Hightlightcounty',
                    options = pathOptions(pane = "up"))
      }
    })
  ### highlight county selected MS ### -----------------------------------
  observe({
    if(input$countyselect_ms == 'All'){
      leafletProxy("plot_ms",data=zipcode)%>%
        removeShape(layerId = 'Hightlightcounty')
    }
    else{
      datalselect11_ms <- county[county$NAME10 == input$countyselect_ms, ]
      leafletProxy("plot_ms", data = zipcode) %>%
        removeShape(layerId = 'Hightlightcounty') %>%
        addPolygons(data = datalselect11_ms, 
                    noClip = T,
                    fill = F,
                    smoothFactor = 0.5,
                    weight = 2, 
                    color = "#f91313", 
                    group = "county", 
                    layerId ='Hightlightcounty',
                    options = pathOptions(pane = "up"))
    }
  })
  # hightlight city selected-------------------------------------------
  observe({
    if(input$cityselect == 'NA'){
      leafletProxy("plot",data = zipcode) %>%
        removeShape(layerId='Hightlightcity')
    }
    else{
      datalselect12<-city[city$Name==input$cityselect, ]
      leafletProxy("plot",data = zipcode) %>%
        removeShape(layerId = 'Hightlightcity') %>%
        addPolygons(data = datalselect12,
                    smoothFactor = 0.5,
                    noClip = T,
                    fill = F, 
                    weight = 2, 
                    color = "#f91313", 
                    layerId ='Hightlightcity',
                    options = pathOptions(pane = "up"))
    }
  })
  # hightlight city selected ms-------------------------------------------
  observe({
    if(input$cityselect_ms == 'NA'){
      leafletProxy("plot_ms",data = zipcode) %>%
        removeShape(layerId='Hightlightcity')
    }
    else{
      datalselect12_ms<-city[city$Name==input$cityselect_ms, ]
      leafletProxy("plot_ms",data = zipcode) %>%
        removeShape(layerId = 'Hightlightcity') %>%
        addPolygons(data = datalselect12_ms,
                    smoothFactor = 0.5,
                    noClip = T,
                    fill = F, 
                    weight = 2, 
                    color = "#f91313", 
                    layerId ='Hightlightcity',
                    options = pathOptions(pane = "up"))
    }
  })
  
### Highlight selected area ###-------------------------------------------------------------
  observeEvent(input$plot_shape_click, {
    click <- input$plot_shape_click
    
    if(is.null(click))
      return()   
    
    #pulls lat and lon from shiny click event
    lat <- click$lat
    lon <- click$lng
    
    #puts lat and lon for click point into its own data frame
    coords <- as.data.frame(cbind(lon, lat))
    
    #  
    proj4string(shp) <- CRS("+proj=longlat +datum=WGS84")
    #converts click point coordinate data frame into SP object, sets CRS
    point <- SpatialPoints(coords)
    proj4string(point) <- CRS("+proj=longlat +datum=WGS84")
    
    #retrieves country in which the click point resides, set CRS for country
    selected <- shp[point,]
    proj4string(selected) <- CRS("+proj=longlat +datum=WGS84")

    proxy <- leafletProxy("plot",data=zipcode)
    if(click$id == "Selected"){
      proxy %>% removeShape(layerId = "Selected")
    } else {
      proxy %>% addPolygons(data = selected, 
                            fill = F,
                            color = 'red',
                            # fillColor = "red",
                            # fillOpacity = 1, 
                            # color = "red",
                            weight = 3, 
                            smoothFactor = 0.5,
                            stroke = T,
                            layerId = "Selected",options = pathOptions(pane = "highlight"))
    } 
  })
  
RV <- reactiveValues(Clicks=c(),LAT=c(),LNG = c())
output$table_ms<- renderDataTable({
  if (is.null(RV$LAT)){
    Guide <- 'Please choose census tract from the map by click'
    data.frame(Guide)
  }else{
  lat_ms <- RV$LAT
  lon_ms <- RV$LNG
coords_ms <- as.data.frame(cbind(lon_ms, lat_ms))
shp <- spTransform(shp, CRS("+proj=longlat +datum=WGS84"))
 point_ms <- SpatialPoints(coords_ms)
  proj4string(point_ms) <- CRS("+proj=longlat +datum=WGS84")

  selected_ms <- shp[point_ms,]
  selected_ms <- zipcodeDL[which(zipcodeDL$OBJECTID %in% selected_ms$OBJECTID), ]
  datatable(data =selected_ms
            ,extensions = "Buttons"
            ,options=list(
              scrollX = TRUE,
              dom = "Blfrtip"
              , buttons = 
                list("copy", list(
                  extend = "collection"
                  , buttons = c("csv", "excel")
                  , text = "Download"
                ))
            ))

}} )  

### Highlight selected area ms###-------------------------------------------------------------
  observeEvent(input$plot_ms_shape_click, {
    click_ms <- input$plot_ms_shape_click
      if(is.null(click_ms))
        return() 
    if(click_ms$id %in% RV$Clicks){
        RV$LAT  <- RV$LAT[-which(RV$Clicks == click_ms$id)]
        RV$LNG  <- RV$LNG[-which(RV$Clicks == click_ms$id)]
        RV$Clicks <- RV$Clicks[-which(RV$Clicks == click_ms$id)]
        
    }
    else{
    RV$Clicks <- c(RV$Clicks, click_ms$id)
    RV$LAT    <- c(RV$LAT, click_ms$lat)
    RV$LNG    <- c(RV$LNG, click_ms$lng)
    }
    
    
    #pulls lat and lon from shiny click event
    lat_ms <- RV$LAT
    lon_ms <- RV$LNG
    
    #puts lat and lon for click point into its own data frame
    coords_ms <- as.data.frame(cbind(lon_ms, lat_ms))
    shp <- spTransform(shp, CRS("+proj=longlat +datum=WGS84"))
    
    # shp <- spTransform(shp, CRS = CRS("+proj=longlat +datum=WGS84") )
    #converts click point coordinate data frame into SP object, sets CRS
    point_ms <- SpatialPoints(coords_ms)
    proj4string(point_ms) <- CRS("+proj=longlat +datum=WGS84")
    #retrieves country in which the click point resides, set CRS for country
    selected_ms <- shp[point_ms,]
    if(is.null(selected_ms))
      return() 

    selected_ms_A <- selected_ms[1,]
    for (i in 1:length(selected_ms)){
      selected_ms_A <- gUnion(selected_ms_A,selected_ms[i,])
    }
    selected_ms_A <- spTransform(selected_ms_A, CRS("+proj=longlat +datum=WGS84"))
    proxy_ms <- leafletProxy("plot_ms",data=selected_ms_A)
        proxy_ms %>% 
        removeShape(layerId = "Selected_ms") %>%
        addPolygons(data = selected_ms_A, 
                            fill = F,
                            color = 'red',
                            # fillColor = "red",
                            # fillOpacity = 1, 
                            # color = "red",
                            weight = 3, 
                            smoothFactor = 0.5,
                            stroke = T,
                            layerId = "Selected_ms" ,group='multiselect'
                            ,options = pathOptions(pane = "up")
                            )
  })
  
  
  
  # DOWNLOAD TABLE---------------------------------------------------------------------------------
  output$mytable <- DT::renderDataTable(
    ({datak <<- zipcodeDL
    if(input$countyd !='All'){
      datak <<- datak[datak$NAMELSAD10 == paste0(input$countyd, ' County'), ]
    }
    if (input$cityd !='NA') {
    censustractlist <- city_censustrack$censustract_id[which(city$Name == input$cityd)]
      datak <<- datak[which(datak$OBJECTID %in% censustractlist[[1]]), ]
    }
      datak
    }),
    extensions = 'Buttons',
    options = list(
      # dom='Bfrtip',
      # button=c('copy','csv','excel','pdf','print'),
      pageLength = 6,
      lengthMenu = list(c(6,48, 72, 96, -1), 
                        list('6', '48', '72','96', 'All')), 
      paging = T),
    style = 'bootstrap'
  )

 
  output$downloaddata <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      # write.csv(zipcodeDL, file)
      write.csv(datak, file)
      
    }
  )
  
  
  
  ###### Update selectinput: Can only select city level or county level;---------------------------------
  
  observe({
    x <- input$cityselect
    y <- input$countyselect
    if(input$countyselect !='All'){
      updateSelectInput(session, 'cityselect',
                        label = 'Zoom to City:',choices = 'NA')}
    else{
      updateSelectInput(session,'cityselect',label = 'Zoom to City:',
                          choices = c("NA",
                                     sort(unique(as.character(city$Name)))
                          ),selected = x
      )
                        }
      if(input$cityselect !='NA'){
        updateSelectInput(session, 'countyselect',
                          label ='Zoom to County:',choices = 'All')
      }
    else{
        updateSelectInput(session, "countyselect", label = "Zoom to County:",
         choices = c("All",
                    sort(unique(as.character(zipcode$NAME10)))
         ),selected = y
        )
      }
  })
  
  observe({
    x_ms <- input$cityselect_ms
    y_ms <- input$countyselect_ms
    if(input$countyselect_ms !='All'){
      updateSelectInput(session, 'cityselect_ms',
                        label = 'Zoom to City:',choices = 'NA')}
    else{
      updateSelectInput(session,'cityselect_ms',label = 'Zoom to City:',
                        choices = c("NA",
                                    sort(unique(as.character(city$Name)))
                        ),selected = x_ms
      )
    }
    if(input$cityselect_ms !='NA'){
      updateSelectInput(session, 'countyselect_ms',
                        label ='Zoom to County:',choices = 'All')
    }
    else{
      updateSelectInput(session, "countyselect_ms", label = "Zoom to County:",
                        choices = c("All",
                                    sort(unique(as.character(zipcode$NAME10)))
                        ),selected = y_ms
      )
    }
  })
 
  ### 'Detail UI Page' ###---------------------------------------------------------------------------
  output$UIDetail<-renderUI({
    if(is.null(id2()#input$plot_shape_click
               )
       ){
      list(textOutput('warninig'))
      }else{
        list(
          fluidRow(
            column(5,
                   actionButton("youth4",tags$h4('Youth')),
                   tags$h4(textOutput('youth')),
                   tags$p('of residents are under 18')
                   ),
            column(7, div(gaugeOutput("youth3"), style = "height:100px;"),
                   tags$i(textOutput('youth1'), style = 'text-align:center;')
                   # plotlyOutput('youth2',height =150)
                   )
            ),
          hr(),
          fluidRow(
            column(5,
                   # tags$h4('Older Adults'),
                   actionButton("old4", tags$h4("Older Adult")),
                   tags$h4(textOutput('old')),
                   tags$p('of residents are 65 years or older')
                   ),
            column(7, div(gaugeOutput("old3"), style = "height:100px;"),
                   tags$i(textOutput('old1'), style='text-align:center;')
                   # plotlyOutput('old2',width ="auto",height =150)
                   )
            ),
          hr(),
          fluidRow(
            column(5,
                   # tags$h4('Female'),
                   actionButton("female4", tags$h4("Female")),
                   tags$h4(textOutput('female')),
                   tags$p('of residents are female')
            ),
            column(7,div(gaugeOutput("female3"),style = "height:100px;"),
                   tags$i(textOutput('female1'),style='text-align:center;')
                   # plotlyOutput('female2',width ="auto",height =150)
            )
          ),
          hr(),
          fluidRow(
            column(5,
                   actionButton("disable4", tags$h4("Disabled")),
                   tags$h4(textOutput('disable')),
                   tags$p('of residents are disables')
            ),
            column(7,
                   div(gaugeOutput("disable3"),style = "height:100px;"),
                   tags$i(textOutput('disable1'),style='text-align:center;')
            )
          ),
          hr(),
          fluidRow(
            column(5,
                   actionButton("race4", tags$h4("Racial Minority")),
                   tags$h4(textOutput('race')),
                   tags$p('of residents identify as one or more racial minority')
            ),
            column(7,
                   div(gaugeOutput("race3"),style = "height:100px;"),
                   tags$i(textOutput('race1'),style='text-align:center;')
            )
          ),
          hr(),
          fluidRow(
            column(5,
                   actionButton("ethnic4", tags$h4("Ethnic Minority")),
                   tags$h4(textOutput('ethnic')),
                   tags$p('of residents identified themselves as being of Hispanic or Spanish origin.')
            ),
            column(7,
                   div(gaugeOutput("ethnic3"),style = "height:100px;"),
                   tags$i(textOutput('ethnic1'),style='text-align:center;')
            )
          ),
          hr(),
          fluidRow(
            column(5,
                   actionButton("foreig4", tags$h4("Foreign Born")),
                   tags$h4(textOutput('foreig')),
                   tags$p('of residents were born outside of the United States')
            ),
            column(7,
                   div(gaugeOutput("foreig3"),style = "height:100px;"),
                   tags$i(textOutput('foreig1'),style='text-align:center;')
            )
          ),
          hr(),
          fluidRow(
            column(5,
                   actionButton("englis4", tags$h4("Limited English Proficiency")),
                   tags$h4(textOutput('englis')),
                   tags$p('of residents report having English proficiency below "very well"')
            ),
            column(7,
                   div(gaugeOutput("englis3"),style = "height:100px;"),
                   tags$i(textOutput('englis1'),style='text-align:center;')
            )
          ),
          hr(),
          fluidRow(
            column(5,
                   actionButton("povert4", tags$h4("Low Income")),
                   tags$h4(textOutput('povert')),
                   tags$p('live in households with an income below 200% of the national poverty level')
            ),
            column(7,
                   div(gaugeOutput("povert3"),style = "height:100px;"),
                   tags$i(textOutput('povert1'),style='text-align:center;')
            )
      )#####fluid row end
     )
    }
  })
  
  output$UISpider <- renderUI({
    if(is.null(input$plot_shape_click)){
tags$p('Please choose a Census Tract on map')}
    else{
      list(
        highchartOutput('radarplot', height = 220)
      )
    }
  })

  #-----------------------------------------------------------------------
  ####'Please Choose a Census on Map'
output$warninig <- renderText({
  paste0('Please choose a Census Tract on map')
})
  ####'
  # greater or less than average
  
  output$youth <- renderText({
    paste(round(zipcode$YOUTH[which(zipcode$OBJECTID==id2())],2),"%")
  })
  output$youth1 <- renderText({
    myfunction(zipcode$MPO_Youth, id2())
  })

  output$youth3 <- renderGauge({
    gauge1(
      round(zipcode$YOUTH[which(zipcode$OBJECTID==id2())],2),
      min = 0,
      max = round(max(zipcode$YOUTH), 2),
      symbol = '%',
      gaugeSectors1(
        wba = c(0, max(zipcode$YOUTH[which(zipcode$MPO_Youth==0)])),
        ba = c(max(zipcode$YOUTH[which(zipcode$MPO_Youth==0)]), max(zipcode$YOUTH[which(zipcode$MPO_Youth==1)])),
        average = c(max(zipcode$YOUTH[which(zipcode$MPO_Youth==1)]), max(zipcode$YOUTH[which(zipcode$MPO_Youth==2)])),
        aa = c(max(zipcode$YOUTH[which(zipcode$MPO_Youth==2)]), max(zipcode$YOUTH[which(zipcode$MPO_Youth==3)])),
        waa = c(max(zipcode$YOUTH[which(zipcode$MPO_Youth==3)]), 100),
        colors = c('#ffffc9','#c7e9b4','#41b6c4','#225ea8','rgb(29,48,93)')
      )
    )
  })
  ##
  output$old <- renderText({
    paste(round(zipcode$SENIORS[which(zipcode$OBJECTID==id2())],2),"%")
  })
  
  output$old1 <- renderText({
    myfunction(zipcode$MPO_Senior,id2())
    
  })
  
  output$old3 <- renderGauge({
    gauge1(
      round(zipcode$SENIORS[which(zipcode$OBJECTID==id2())],2),
      min = 0,
      max = round(max(zipcode$SENIORS), 2),
      symbol = '%',
      gaugeSectors1(
        wba = c(0, max(zipcode$SENIORS[which(zipcode$MPO_Senior==0)])),
        ba = c(max(zipcode$SENIORS[which(zipcode$MPO_Senior==0)]), max(zipcode$SENIORS[which(zipcode$MPO_Senior==1)])),
        average = c(max(zipcode$SENIORS[which(zipcode$MPO_Senior==1)]), max(zipcode$SENIORS[which(zipcode$MPO_Senior==2)])),
        aa = c(max(zipcode$SENIORS[which(zipcode$MPO_Senior==2)]), max(zipcode$SENIORS[which(zipcode$MPO_Senior==3)])),
        waa = c(max(zipcode$SENIORS[which(zipcode$MPO_Senior==3)]), 100),
        colors = c('##ffffc9','#c7e9b4','#41b6c4','#225ea8','rgb(29,48,93)')
      )
    )
  })
  ###
  output$female <- renderText({
    paste(round(zipcode$FEMALE[which(zipcode$OBJECTID==id2())],2),"%")
  })
  
  output$female1 <- renderText({
    myfunction(zipcode$MPO_Female, id2())
    
  })
  
  output$female3 <- renderGauge({
    gauge1(
      round(zipcode$FEMALE[which(zipcode$OBJECTID==id2())], 2),
      min = 0, 
      max = round(max(zipcode$FEMALE), 2), 
      symbol = '%',
      gaugeSectors1(
        wba = c(0, max(zipcode$FEMALE[which(zipcode$MPO_Female == 0)])),
        ba = c(max(zipcode$FEMALE[which(zipcode$MPO_Female == 0)]), 
               max(zipcode$FEMALE[which(zipcode$MPO_Female == 1)])),
        average = c(max(zipcode$FEMALE[which(zipcode$MPO_Female == 1)]), 
                    max(zipcode$FEMALE[which(zipcode$MPO_Female == 2)])),
        aa = c(max(zipcode$FEMALE[which(zipcode$MPO_Female == 2)]), 
               max(zipcode$FEMALE[which(zipcode$MPO_Female == 3)])),
        waa = c(max(zipcode$FEMALE[which(zipcode$MPO_Female == 3)]), 100),
        colors = c('#ffffc9', '#c7e9b4', '#41b6c4', '#225ea8', 'rgb(29,48,93)')
      )
    )
  })
  ###
  output$disable <- renderText({
    paste(round(zipcode$DISABILI_1[which(zipcode$OBJECTID == id2())], 2), "%")
  })
  output$disable1 <- renderText({
    myfunction(zipcode$MPO_Disabi, id2())
  })
  
  output$disable3 <- renderGauge({
    gauge1(
      round(zipcode$DISABILI_1[which(zipcode$OBJECTID==id2())], 2),
      min = 0,
      max = round(max(zipcode$DISABILI_1), 2),
      symbol = '%',
      gaugeSectors1(
        wba = c(0, max(zipcode$DISABILI_1[which(zipcode$MPO_Disabi == 0)])),
        ba = c(max(zipcode$DISABILI_1[which(zipcode$MPO_Disabi == 0)]), 
               max(zipcode$DISABILI_1[which(zipcode$MPO_Disabi == 1)])),
        average = c(max(zipcode$DISABILI_1[which(zipcode$MPO_Disabi == 1)]), 
                    max(zipcode$DISABILI_1[which(zipcode$MPO_Disabi == 2)])),
        aa = c(max(zipcode$DISABILI_1[which(zipcode$MPO_Disabi == 2)]), 
               max(zipcode$DISABILI_1[which(zipcode$MPO_Disabi == 3)])),
        waa = c(max(zipcode$DISABILI_1[which(zipcode$MPO_Disabi == 3)]), 100),
        colors = c('#ffffc9','#c7e9b4','#41b6c4','#225ea8','rgb(29,48,93)')
      )
    )
  })
  ###
  output$race <- renderText({
    paste(round(zipcode$RACE[which(zipcode$OBJECTID==id2())], 2), "%")
  })
  output$race1 <- renderText({
    myfunction(zipcode$MPO_Race, id2())
  })
  
  output$race3 <- renderGauge({
    gauge1(
      round(zipcode$RACE[which(zipcode$OBJECTID==id2())], 2),
      min = 0,
      max = round(max(zipcode$RACE), 2), 
      symbol = '%',
      gaugeSectors1(
        wba = c(0, max(zipcode$RACE[which(zipcode$MPO_Race == 0)])),
        ba = c(max(zipcode$RACE[which(zipcode$MPO_Race == 0)]),
               max(zipcode$RACE[which(zipcode$MPO_Race == 1)])),
        average = c(max(zipcode$RACE[which(zipcode$MPO_Race == 1)]), 
                    max(zipcode$RACE[which(zipcode$MPO_Race == 2)])),
        aa = c(max(zipcode$RACE[which(zipcode$MPO_Race == 2)]), 
               max(zipcode$RACE[which(zipcode$MPO_Race == 3)])),
        waa = c(max(zipcode$RACE[which(zipcode$MPO_Race == 3)]), 100),
        colors = c('#ffffc9','#c7e9b4','#41b6c4','#225ea8','rgb(29,48,93)')
      )
    )
  })
  
  ###
  output$ethnic <- renderText({
    paste(round(zipcode$ETHNICMINO[which(zipcode$OBJECTID == id2())], 2), "%")
  })
  output$ethnic1 <- renderText({
    myfunction(zipcode$MPO_Ethnic, id2())
  })
  
  output$ethnic3 <- renderGauge({
    gauge1(
      round(zipcode$ETHNICMINO[which(zipcode$OBJECTID == id2())],2),
      min = 0,
      max = round(max(zipcode$ETHNICMINO), 2),
      symbol = '%',
      gaugeSectors1(
        wba = c(0, max(zipcode$ETHNICMINO[which(zipcode$MPO_Ethnic == 0)])),
        ba = c(max(zipcode$ETHNICMINO[which(zipcode$MPO_Ethnic == 0)]), 
               max(zipcode$ETHNICMINO[which(zipcode$MPO_Ethnic == 1)])),
        average = c(max(zipcode$ETHNICMINO[which(zipcode$MPO_Ethnic == 1)]), 
                    max(zipcode$ETHNICMINO[which(zipcode$MPO_Ethnic == 2)])),
        aa = c(max(zipcode$ETHNICMINO[which(zipcode$MPO_Ethnic == 2)]), 
               max(zipcode$ETHNICMINO[which(zipcode$MPO_Ethnic == 3)])),
        waa = c(max(zipcode$ETHNICMINO[which(zipcode$MPO_Ethnic == 3)]), 100),
        colors = c('#ffffc9', '#c7e9b4', '#41b6c4', '#225ea8', 'rgb(29,48,93)')
      )
    )
  })
  ###
  output$foreig <- renderText({
    paste(round(zipcode$FOREIGNBOR[which(zipcode$OBJECTID == id2())], 2), "%")
  })
  output$foreig1 <- renderText({
    myfunction(zipcode$MPO_Foreig, id2())
  })
 
  output$foreig3 <- renderGauge({
    gauge1(
      round(zipcode$FOREIGNBOR[which(zipcode$OBJECTID == id2())], 2),
      min = 0,
      max = round(max(zipcode$FOREIGNBOR), 2),
      symbol = '%',
      gaugeSectors1(
        wba = c(0, max(zipcode$FOREIGNBOR[which(zipcode$MPO_Foreig == 0)])),
        ba = c(max(zipcode$FOREIGNBOR[which(zipcode$MPO_Foreig == 0)]),
               max(zipcode$FOREIGNBOR[which(zipcode$MPO_Foreig == 1)])),
        average = c(max(zipcode$FOREIGNBOR[which(zipcode$MPO_Foreig == 1)]),
                    max(zipcode$FOREIGNBOR[which(zipcode$MPO_Foreig == 2)])),
        aa = c(max(zipcode$FOREIGNBOR[which(zipcode$MPO_Foreig == 2)]),
               max(zipcode$FOREIGNBOR[which(zipcode$MPO_Foreig == 3)])),
        waa = c(max(zipcode$FOREIGNBOR[which(zipcode$MPO_Foreig == 3)]), 100),
        colors = c('#ffffc9', '#c7e9b4', '#41b6c4', '#225ea8', 'rgb(29,48,93)')
      )
    )
  })
  ###
  output$englis <- renderText({
    paste(round(zipcode$ENGLISHPRO[which(zipcode$OBJECTID==id2())], 2), "%")
  })
  output$englis1 <- renderText({
    myfunction(zipcode$MPO_Englis, id2())
  })
 
  output$englis3 <- renderGauge({
    gauge1(
      round(zipcode$ENGLISHPRO[which(zipcode$OBJECTID == id2())], 2),
      min = 0,
      max = round(max(zipcode$ENGLISHPRO), 2),
      symbol = '%',
      gaugeSectors1(
        wba = c(0, max(zipcode$ENGLISHPRO[which(zipcode$MPO_Englis == 0)])),
        ba = c(max(zipcode$ENGLISHPRO[which(zipcode$MPO_Englis == 0)]),
               max(zipcode$ENGLISHPRO[which(zipcode$MPO_Englis == 1)])),
        average = c(max(zipcode$ENGLISHPRO[which(zipcode$MPO_Englis == 1)]), 
                    max(zipcode$ENGLISHPRO[which(zipcode$MPO_Englis == 2)])),
        aa = c(max(zipcode$ENGLISHPRO[which(zipcode$MPO_Englis == 2)]), 
               max(zipcode$ENGLISHPRO[which(zipcode$MPO_Englis == 3)])),
        waa = c(max(zipcode$ENGLISHPRO[which(zipcode$MPO_Englis == 3)]), 100),
        colors = c('#ffffc9','#c7e9b4','#41b6c4','#225ea8','rgb(29,48,93)')
      )
    )
  })
  ###
  output$povert <- renderText({
    paste(round(zipcode$POVERTY[which(zipcode$OBJECTID == id2())],2),"%")
  })
  output$povert1 <- renderText({
    myfunction(zipcode$MPO_Povert,id2())
  })
 
  output$povert3 <- renderGauge({
    gauge1(
      round(zipcode$POVERTY[which(zipcode$OBJECTID == id2())],2),
      min = 0,
      max = round(max(zipcode$POVERTY), 2),
      symbol = '%',
      gaugeSectors1(
        wba = c(0, max(zipcode$POVERTY[which(zipcode$MPO_Povert == 0)])),
        ba = c(max(zipcode$POVERTY[which(zipcode$MPO_Povert == 0)]), 
               max(zipcode$POVERTY[which(zipcode$MPO_Povert == 1)])),
        average = c(max(zipcode$POVERTY[which(zipcode$MPO_Povert == 1)]), 
                    max(zipcode$POVERTY[which(zipcode$MPO_Povert == 2)])),
        aa = c(max(zipcode$POVERTY[which(zipcode$MPO_Povert == 2)]), 
               max(zipcode$POVERTY[which(zipcode$MPO_Povert == 3)])),
        waa = c(max(zipcode$POVERTY[which(zipcode$MPO_Povert == 3)]), 100),
        colors = c('#ffffc9','#c7e9b4','#41b6c4','#225ea8','rgb(29,48,93)')
      )
    )
  })
  
  # radar chart by Highchart
  output$radarplot <- renderHighchart({
      value1 <- c(zipcode$MPO_Youth[which(zipcode$OBJECTID==id2())],zipcode$MPO_Senior[which(zipcode$OBJECTID==id2())],
                zipcode$MPO_Race[which(zipcode$OBJECTID==id2())],zipcode$MPO_Povert[which(zipcode$OBJECTID==id2())],
                zipcode$MPO_Foreig[which(zipcode$OBJECTID==id2())],zipcode$MPO_Female[which(zipcode$OBJECTID==id2())],
                zipcode$MPO_Ethnic[which(zipcode$OBJECTID==id2())],zipcode$MPO_Englis[which(zipcode$OBJECTID==id2())],
                zipcode$MPO_Disabi[which(zipcode$OBJECTID==id2())])
      h1 <- highchart()%>%
        hc_chart(polar=TRUE,
                 type = 'line',
                 marginTop = 30 
        )%>%
        hc_pane(size = '80%')%>%
        hc_xAxis(categories = c("Youth", "Older Adults", "Racial Minority",
                                "Low Income", "Foreign Born", "Female",
                                "Ethnic Minority", "Limited English Proficiency",
                                "Disabled"),
                 tickmarkPlacement = 'on',
                 lineWidth = 0,
                 labels = list(style=list(color = '#000000'))
        )%>%
        hc_yAxis(
          gridLineInterpolation = 'polygon',
          lineWidth = 0,
          min = -0.05,
          showFirstLabel = FALSE
        )%>%
        
        hc_tooltip(
          shared = TRUE,
          pointFormat='<span style="color:{series.color}">
        {series.name}: <b>score: {point.y}</b><br/>'
        )%>%
        hc_legend(
          align = 'left',
          verticalAlign = 'top',
          layout = 'horizontal',
          y = -5
        )%>%
        hc_add_series(data =value1,
                      name = paste0(zipcode$NAMELSAD[which(zipcode$OBJECTID==id2())]),
                      pointPlacement = 'on',
                      color ='rgba(255,165,0,0.5)'
        )%>%
        hc_add_series(data = c(2, 2, 2, 2, 2, 2, 2, 2, 2),
                      name = 'Regional Average',
                      pointPlacement = 'on',
                      color = "rgba(0,0,255,0.5)")
      # if (input$countyselect != 'All' ){ # show county average
        label_county_city <- zipcode$NAME10[which(zipcode$OBJECTID==id2())]
        value_county_city <- county_mean1[ ,zipcode$NAME10[which(zipcode$OBJECTID==id2())]]
        h1 %>% hc_add_series( data = value_county_city,
                              name = paste0(label_county_city, " Average"),
                              pointPlacement = 'on',
                              color = "rgba(255,0,0,0.5)")
      # } 
      # else{ h1 }
  })
  #_____________________________________________________________________________________
  #
  dataModal <- function(failed = FALSE) {
    modalDialog(
      fluidPage(
        titlePanel('Concentrations of Populations Identified for Equity Analyses'),
        navlistPanel(
          tabPanel('Youth',
                   p('The Youth indicator addresses FHWA\'s EJ recommendation to include children 
as a traditionally underserved\' population group when conducting equity analyses and FHWA\'s Additional Nondiscrimination Requirement 
under the Age Discrimination Act of 1975 to not discriminate based on age.',
                     style= "font-family: 'Roboto Condensed', Light;"),
                   br(),
p('This indicator uses age data from the ACS and includes all persons in the region under 18 years old.',
  style= "font-family: 'Roboto Condensed', Light;")
                   ),
          tabPanel('Older Adults',
                   p('The Older Adults indicator addresses the populations included in FHWA\'s Additional Nondiscrimination Requirement under the Age Discrimination 
Act of 1975 and FHWA\'s Environmental Justice recommendations to not discriminate 
based on age.',style= "font-family: 'Roboto Condensed', Light;"),br(),
                   p('This indicator uses age data from the ACS and includes all persons in the region 65 years and older.', style= "font-family: 'Roboto Condensed', Light;")),
          tabPanel('Female',
                   p('The Female indicator addresses FHWA\'s Additional Nondiscrimination Requirement under Section 162 (a) of the Federal-Aid Highway Act of 1973 (23 USC 324)
that no person shall be subject to discrimination on the basis of sex under any program or activity receiving Federal assistance. This additional Nondiscrimination 
Requirement is connected to Title IX of the Civil Rights Act, which designates women 
as a protected class.',
                     style= "font-family: 'Roboto Condensed', Light;"),br(),
p("This indicator uses sex data from the ACS and captures the 
  Census Bureau's estimate of all persons identifying as female when given the choice of male or female on the survey form.",style= "font-family: 'Roboto Condensed', Light;")
                   ),
          tabPanel('Racial Minority',
                   p('The Racial Minority indicator addresses the populations included in Title VI of 
the Civil Rights Act, FHWA\'s Title VI and Additional Nondiscrimination Requirements ,
and FTA\'s Title VI requirements and guidelines , all of which prohibit discrimination
of persons in the United States based on race.',
                     style= "font-family: 'Roboto Condensed', Light;"),br(),
                   p('This indicator uses race data from the ACS and includes all persons in the region who identified themselves as one or more of the following races in their Census form: Black or African American, American Indian, Alaskan Native, Asian Indian, Japanese, Native Hawaiian, Chinese, Korean, Guamanian or Chamorro, Filipino, Vietnamese, Samoan, Other Asian, and/or Other Pacific Islander.',
                     style= "font-family: 'Roboto Condensed', Light;")
                   ),
          tabPanel('Ethnic Minority',
p('The Ethnic Minority indicator addresses the populations included in Title VI of the Civil Rights Act, FHWA\'s Title VI and Additional Nondiscrimination Requirements
, and FTA\'s Title VI requirements and guidelines , and the recommendation to 
consider minority under the Executive Order on Environmental Justice .',
  style= "font-family: 'Roboto Condensed', Light;"),br(),p('This indicator uses ethnicity data from the ACS and includes all persons in the region who identified themselves as being of Hispanic, Latino, Spanish, Mexican, Chicano, Cuban, Puerto Rican, or Other Hispanic origin.
                                       ', style= "font-family: 'Roboto Condensed', Light;") ),
          tabPanel('Foreign-Born',
p('The foreign-born indicator addresses the populations included in Title VI of the 
Civil Rights Act, FHWA\'s Title VI and Additional Nondiscrimination Requirements , 
and FTA\'s Title VI requirements and guidelines , all of which prohibit discrimination
of persons in the United States based on national origin.',style= "font-family: 'Roboto Condensed', Light;"),br(),p('This indicator uses national origin data from the ACS and includes all persons in the region who indicated they were born outside of the United States in their Census form.
                                    ',  style= "font-family: 'Roboto Condensed', Light;") ),
          tabPanel('Limited English Proficiency',
p('The Limited English Proficiency indicator addresses the populations included in Title VI of the Civil Rights Act, Executive Order 13166, "Improving 
Access to Services for Persons with Limited English Proficiency", FHWA\'s Title VI and Additional Nondiscrimination Requirements , and FTA\'s Title VI requirements and guidelines, all of which prohibit discrimination of persons in the United States based on race and national origin.',
  style= "font-family: 'Roboto Condensed', Light;"),
br(),
p('This indicator uses language data from the ACS and includes all persons in the region who indicated they speak English less than \'very well\'.
                     ',style= "font-family: 'Roboto Condensed', Light;") 
                   ),
          tabPanel('Disabled',
                   p('The Disabled indicator addresses the populations included in FH
WA\'s Title VI and Additional Nondiscrimination Requirements under Section 504 of the 
Rehabilitation Act of 1973 and Americans With Disabilities Act of 1990, which protect 
persons with a disability against discrimination.',
                     style= "font-family: 'Roboto Condensed', Light;"),br(),
                   p('This indicator uses disability data from the ACS and includes all persons in 
                     the region who indicated they experience one or more physical and/or mental 
                     disabilities.', style= "font-family: 'Roboto Condensed', Light;") ),
          tabPanel('Low-income',
p('The Low-Income indicator addresses the populations included in Executive Order on Environmental Justice,
FHWA Environmental Justice recommendations, and FTA\'s Environmental Justice policy 
guidance, all of which encourage agencies to consider their impact on low-income 
persons.',style= "font-family: 'Roboto Condensed', Light;"),br(),p('This indicator uses income data from the ACS and includes all 
persons in the region who have a household income below 200% of the national poverty 
level. ', style= "font-family: 'Roboto Condensed', Light;") )


      ))
      
    )
  }
  # 
  observeEvent(input$youth4,    {showModal(dataModal())})
  observeEvent(input$old4,      {showModal(dataModal())})
  observeEvent(input$female4,   {showModal(dataModal())})
  observeEvent(input$disable4,  {showModal(dataModal())})
  observeEvent(input$race4,     {showModal(dataModal())})
  observeEvent(input$ethnic4,   {showModal(dataModal())})
  observeEvent(input$foreig4,   {showModal(dataModal())})
  observeEvent(input$englis4,   {showModal(dataModal())})
  observeEvent(input$povert4,   {showModal(dataModal())})
  ###########Plotly#############------------------------------------------------------------------------------------------------------------------------------------
  ###############################
  # Drag event for the scatterplot; will grab tractids of selected points
  
  sub <- reactive({
    eventdata <- event_data('plotly_selected',source = "A")
    if (is.null(eventdata)) {
      return(NULL) # do nothing
    } else {
      tracts <- eventdata[['key']]
      if (length(tracts) == 0) {
        tracts <- 'abcdefg' # a hack but it's working - set to something that can't be selected
      }
      if (!(tracts %in% zipcode$OBJECTID)) {
        return(NULL) # if there is not a match, do nothing as well
      } else {
        # Give back a sp data frame of the selected tracts
        sub <- zipcode[zipcode$OBJECTID %in% tracts,"YOUTH" ]
        return(sub)
      }
    }
  })
  
  cou <- reactive({
    if(input$countyselect !='All'){
      cou <- zipcode[zipcode$NAME10 == input$countyselect, ]
    }else{
      cou <- zipcode
    }
    return(cou)
  })
  
  
  observe({
    req(cou()) # Do this if cou() is not null
    proxy <- leafletProxy('plot',data=zipcode)
    # Clear old selection on map, and add new selection
    proxy %>%
      fitBounds(lng1 = bbox(cou())[1],
                lat1 = bbox(cou())[2],
                lng2 = bbox(cou())[3],
                lat2 = bbox(cou())[4])
  })
  cit <- reactive({
    if(input$cityselect !='NA'){
      cit <- city[city$Name == input$cityselect, ]
    }else{
      cit <- city
    }
    return(cit)
  })
  
  
  observe({
    req(cit()) # Do this if cou() is not null
    proxy <- leafletProxy('plot',data=zipcode)
    # Clear old selection on map, and add new selection
    proxy %>%
      fitBounds(lng1 = bbox(cit())[1],
                lat1 = bbox(cit())[2],
                lng2 = bbox(cit())[3],
                lat2 = bbox(cit())[4])
  })
  # zoom to ms
 
  cou_ms <- reactive({
    if(input$countyselect_ms !='All'){
      cou_ms <- zipcode[zipcode$NAME10 == input$countyselect_ms, ]
    }else{
      cou_ms <- zipcode
    }
    return(cou_ms)
  })
  
  
  observe({
    req(cou_ms()) # Do this if cou() is not null
    proxy_ms <- leafletProxy('plot_ms',data=zipcode)
    # Clear old selection on map, and add new selection
    proxy_ms %>%
      fitBounds(lng1 = bbox(cou_ms())[1],
                lat1 = bbox(cou_ms())[2],
                lng2 = bbox(cou_ms())[3],
                lat2 = bbox(cou_ms())[4])
  })
  cit_ms <- reactive({
    if(input$cityselect_ms !='NA'){
      cit_ms <- city[city$Name == input$cityselect_ms, ]
    }else{
      cit_ms <- city
    }
    return(cit_ms)
  })
  
  
  observe({
    req(cit_ms()) # Do this if cou() is not null
    proxy_ms <- leafletProxy('plot_ms',data=zipcode)
    # Clear old selection on map, and add new selection
    proxy_ms %>%
      fitBounds(lng1 = bbox(cit_ms())[1],
                lat1 = bbox(cit_ms())[2],
                lng2 = bbox(cit_ms())[3],
                lat2 = bbox(cit_ms())[4])
  })

  ######Button-navigation ###---------------------------------------------------------------------------
  observeEvent(input$button1, {
    # updateNavbarPage(session, "navid", "Map")
    updateNavbarPage(session, "navid", "1")
    
    
  })
  observeEvent(input$button2, {
    updateNavbarPage(session, "navid", "13")
  })
  observeEvent(input$al1, {
    updateNavbarPage(session, "navid", "10")
  })
  observeEvent(input$al2, {
    updateNavbarPage(session, "navid", "11")
  })
  observeEvent(input$al3, {
    updateNavbarPage(session, "navid", "12")
  })
  ###BUtton-navigation end

# #   #______________________________________________________________________________________________________________________________________________
  ####feed back----------------------------------------------------------------------------
  ##submit button
  ##

  # sheet <-gs_url('https://docs.google.com/spreadsheets/d/1TLmjgzWx2eAJFXz8qWpTylxWvnQzdYSL7e8m57dBB-Q/edit?usp=sharing')
  feedback<-as.data.frame(gs_read_csv(sheet))
  # feedback<-as.data.frame(read.csv(file='feedback.csv'))
  output$feedbackdt <- DT::renderDataTable(
    ({
      feedback()
    }),
    style = 'bootstrap'
  )

  feedback <- reactiveVal({value=feedback})

  observeEvent(input$feedbackbt, {
    addfeedback <- input$feedback
     feedback1 <- rbind.data.frame(feedback(), addfeedback)
     feedback(feedback1)
    gs_add_row(sheet, input=addfeedback)

      showModal(FEEDSUCCESS())

    })
###fEED SUCCESS
  FEEDSUCCESS <- function(failed = FALSE) {
    modalDialog(
      fluidPage(tags$p('SUBMIT SUCCESS!'))
    )
    }
  #############################################
# HELP FUNCTION----------------------------------------------------------
observe({
 
  # if (input$navid == '1'){
    indTab <- helpData$tab==input$navid
    # nextPage <- 1
  # }
    
    
  # set help content
  session$sendCustomMessage(type = 'setHelpContent', message = list(steps = toJSON(helpData[indTab,]) 
                                                                    # ,nextPage=nextPage
                                                                    ))
})

observeEvent(input$autoStartHelp,{
  if(input$autoStartHelp==0)return()

  # Auto start help if we press nextpage
  session$sendCustomMessage(type = 'startHelp', message = list(""))
})
# # listen to the action button
observeEvent(input$startHelp,{
  # on click, send custom message to start help
  session$sendCustomMessage(type = 'startHelp', message = list(""))

})
# auto start
observe({
  # on click, send custom message to start help
  session$sendCustomMessage(type = 'startHelp', message = list(""))
})

}

# Run the application 
shinyApp(ui = ui, server = server)
##### WAY TO SAVE .httr_?
# getwd()
# install.packages('googleAuthR')
# library(googleAuthR)
# googleAuthR::gar_auth()
# googleAuthR::gar_auth(new_user=TRUE)
# setwd('C:/Users/shichen/Documents')
