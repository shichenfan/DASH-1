#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# Shichen Fan 
# Atlanta Regional Commission
# Jun 2019
# Code for Congestion Management Process page on ARC DASH tool https://atlregional.github.io/DASH/Congestion_Management.html

library(shiny)
library(sp)
library(rgeos)
library(rgdal)
library(leaflet)
library(DT)
library(leaflet.extras)
library(htmltools)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)

# load taz file --------------------------------------------------------------------------------------------------
spdcompare <- readRDS('2018join2.rds')
spdtti <- readRDS('2018speed1.rds')

# labels_tti_AM <- sprintf(
#   "<strong>TMC: %s</strong><br/>TTI: %g ",
#   spdtti$TMC, spdtti$tti.AM) %>% lapply(htmltools::HTML)
# saveRDS(labels_tti_AM,'labels_tti_AM.rds')
# labels_tti_PM <- sprintf(
#   "<strong>TMC: %s</strong><br/>TTI: %g ",
#   spdtti$TMC, spdtti$tti.PM) %>% lapply(htmltools::HTML)
# saveRDS(labels_tti_PM,'labels_tti_PM.rds')
# labels_speed_AM <- sprintf(
#   "<strong>TMC: %s</strong><br/>Speed: %g mph",
#   spdtti$TMC, spdtti$speed.AM) %>% lapply(htmltools::HTML)
# saveRDS(labels_speed_AM,'labels_speed_AM.rds')
# labels_speed_PM <- sprintf(
#   "<strong>TMC: %s</strong><br/>Speed: %g mph",
#   spdtti$TMC, spdtti$speed.PM) %>% lapply(htmltools::HTML)
# saveRDS(labels_speed_PM,'labels_speed_PM.rds')

labels_tti_AM <- readRDS('labels_tti_AM.rds')
labels_tti_PM <- readRDS('labels_tti_PM.rds')
labels_speed_AM <- readRDS('labels_speed_AM.rds')
labels_speed_PM <- readRDS('labels_speed_PM.rds')
### LOADING ANIMATION ###

### LOADING ANIMATION END ###


# spdtti <- readRDS('W:/Shichen/CMP/CMP_leaflet/2018speed.rds')
# spdtti <- spdtti[, c("TMC", "RoadName", "County", "Direction","speed.AM","tti.AM","speed.PM","tti.PM" )]
# saveRDS(spdtti,'W:/Shichen/CMP/CMP_leaflet/2018speed1.rds')
# spdtti <- st_as_sf(spdtti)
# spdtti1 <-spdtti
pal <- colorBin(
  palette = "RdYlGn",na.color = "#808080",bins = c(0,10,20,30,40,50,75),
  domain = NULL) # speed 

paltti <- colorBin(
  palette = "RdYlGn",na.color = "#808080",bins = c(0,1.1,1.3,1.6,2,2.5,10),
  domain = NULL,reverse = T) # tti 

# spdcompare <- spdcompare[order(spdcompare$Base.Impact,decreasing = T),]
# saveRDS(spdcompare, '2018join1.rds')
# colnames(network@data)

# data_need <-    data.frame(spdcompare$County.y,
#                            spdcompare$Head.Location..approximate.,
#                            spdcompare$Average.max.length..miles.,
#                            spdcompare$Average.daily.duration.minutes.,
#                            spdcompare$Total.duration.hours.,
#                            spdcompare$All.Events.Incidents,
#                            # spdcompare$`Volume Estimate`,
#                            spdcompare$Base.Impact,
#                            spdcompare$TMC
#                            # spdcompare$`Speed differential`,
#                            # spdcompare$Congestion,
#                            # spdcompare$`TOTAL DELAY`,
# 
# )
# colnames(data_need) <-c(  "County",  "Location", "Average max length (miles)", "Average daily duration (minutes)",
#                           "Total duration (hours)","All Events/Incidents","Base Impact" ,   "TMC"
#                           # "Speed differential", "Congestion", "TOTAL DELAY",
#                           )
# saveRDS(data_need,'W:/Shichen/CMP/CMP_leaflet/data_need.rds')
data_need <- readRDS('data_need.rds')

# Define UI 
ui <- tabsetPanel(
  tabPanel("Bottleneck",  
           fluidPage(
             fluidRow(
               leafletOutput('map',height = 500 ))
             , DTOutput('tb')
             , fluidRow(
               column(4, actionButton(inputId = "page", label = "select curent page"))
               , column(4, actionButton(inputId = "help", label = "Help"))
               )
             )
           ),
  tabPanel("Regional Speed",
           fluidPage(
             fluidRow(
               column(4, awesomeRadio(
                 inputId = "tti",
                 label = "Switch between speed and TTI", 
                 choices = c('speed', 'tti'),
                 selected = "tti"
               )),
               column(4, awesomeRadio(
                 inputId = "gif",
                 label = "Switch between map and GIF", 
                 choices = c('map', 'GIF'),
                 selected = "map"
               )),
               column(4, actionButton(inputId = "help2", label = "Help"))
             ),
             uiOutput('gif')
             )
           ),
  tabPanel("Transit Congestion"
           )
  )




# Define server logic required to draw a histogram
server <- function(input, output) {

  showModal(modalDialog(
    title = paste0("Welcome"),
    HTML('
           <h4>INTRODUCTION </h4>
                  <p><q>Congestion management is the application of strategies to improve transportation system performance and reliability by reducing the adverse impacts of congestion on the movement of people and goods. A congestion management process (CMP) is a systematic and regionally-accepted approach for managing congestion that provides accurate, up-to-date information on transportation system performance and assesses alternative strategies for congestion management that meet state and local needs.</q> <i>FHWA Congestion Management Process: A Guidebook</i></p>
                  <p>As part of the Atlanta Regional Commission&apos;s Congestion Management Process (CMP), staff has developed visualizations to quickly and effectively communicate congestion-related data for decision-makers and planners. These congestion visualizations also enable ARC to understand the status of congestion and promote context-specific solutions for the region.</p>
                  <h4>HOW TO USE THIS PAGE</h4>
                  <p>The map and table below defaults to the Bottleneck Tab which highlights the top 10 bottlenecks in the Atlanta region. You can navigate the Congestion Management Page by selecting the other tabs.</p>
                  <p>In addition, you can modify the bottleneck map by selecting column headers and then selecting the "map tabular data" button below the table display. This sorts the data into low-to-high or high-to-low order and refreshes the data displayed on the map. You can also select rows within the table and the map will automatically refresh to display only those selected.</p>
                  <p>If at any time you need to revisit this information box, select the HELP button.</p>

                  <h4>DATA DICTIONARY</h4>
                  <p>Source: This data is derived from INRIX Probe Data Analytics Suite<a href="https://pda.ritis.org/suite/" target="_blank">(https://pda.ritis.org/suite/)</a></p>
                  <p>Definitions:</p>
                  <ul>
                <li>Average maximum length: The average distance (in miles) from the incident where slowed conditions could be experienced</li>
                <li>Average daily duration: The average length of time (in minutes) that congested conditions persisted on the roadway segment</li>
                <li>Total duration: The total amount of time (in minutes) each congestion location persisted</li>
                <li>All Events/Incidents: The number of traffic events and incidents that occurred within the space of the bottleneck during the time searched. Events/incidents include things like collisions, roadwork, medical emergencies, police activity, and road closures. For a complete description visit: <a href="https://pda.ritis.org/suite/help/#icon-legend" target="_blank">https://pda.ritis.org/suite/help/#icon-legend</a>
                <li>Base Impact: The sum of queue lengths over the duration of the bottleneck. This is the default bottleneck ranking.</li>
                </ul>

                <p>Limitations: INRIX uses cell phone, vehicle tracking, and GPS data to estimate speeds. INRIX defines free-flow speeds using real traffic data and defines congestion as travel speeds that fall below 65% of free-flow speeds. This means that the baseline for determining congestion is derived from observed speeds that may exceed legal speed limits, potentially skewing higher baselines and exaggerating actual congestion. This limitation is considered in the decision-making process.</p>

         ')
    ,
    easyClose = TRUE,
    footer = NULL,
    size = c("l")
    ))
  
  observeEvent(input$help, {
    showModal(modalDialog(
      title = paste0("Welcome"),
      HTML('
           <h4>INTRODUCTION </h4>
             <p><q>Congestion management is the application of strategies to improve transportation system performance and reliability by reducing the adverse impacts of congestion on the movement of people and goods. A congestion management process (CMP) is a systematic and regionally-accepted approach for managing congestion that provides accurate, up-to-date information on transportation system performance and assesses alternative strategies for congestion management that meet state and local needs.</q> <i>FHWA Congestion Management Process: A Guidebook</i></p>
             <p>As part of the Atlanta Regional Commission&apos;s Congestion Management Process (CMP), staff has developed visualizations to quickly and effectively communicate congestion-related data for decision-makers and planners. These congestion visualizations also enable ARC to understand the status of congestion and promote context-specific solutions for the region.</p>
             <h4>HOW TO USE THIS PAGE</h4>
             <p>The map and table below defaults to the Bottleneck Tab which highlights the top 10 bottlenecks in the Atlanta region. You can navigate the Congestion Management Page by selecting the other tabs.</p>
             <p>In addition, you can modify the bottleneck map by selecting column headers and then selecting the "map tabular data" button below the table display. This sorts the data into low-to-high or high-to-low order and refreshes the data displayed on the map. You can also select rows within the table and the map will automatically refresh to display only those selected.</p>
             <p>If at any time you need to revisit this information box, select the HELP button.</p>
             
             <h4>DATA DICTIONARY</h4>
             <p>Source: This data is derived from INRIX Probe Data Analytics Suite<a href="https://pda.ritis.org/suite/" target="_blank">(https://pda.ritis.org/suite/)</a></p>
             <p>Definitions:</p>
             <ul>
             <li>Average maximum length: The average distance (in miles) from the incident where slowed conditions could be experienced</li>
             <li>Average daily duration: The average length of time (in minutes) that congested conditions persisted on the roadway segment</li>
             <li>Total duration: The total amount of time (in minutes) each congestion location persisted</li>
             <li>All Events/Incidents: The number of traffic events and incidents that occurred within the space of the bottleneck during the time searched. Events/incidents include things like collisions, roadwork, medical emergencies, police activity, and road closures. For a complete description visit: <a href="https://pda.ritis.org/suite/help/#icon-legend" target="_blank">https://pda.ritis.org/suite/help/#icon-legend</a>
             <li>Base Impact: The sum of queue lengths over the duration of the bottleneck. This is the default bottleneck ranking.</li>
             </ul>
             
             <p>Limitations: INRIX uses cell phone, vehicle tracking, and GPS data to estimate speeds. INRIX defines free-flow speeds using real traffic data and defines congestion as travel speeds that fall below 65% of free-flow speeds. This means that the baseline for determining congestion is derived from observed speeds that may exceed legal speed limits, potentially skewing higher baselines and exaggerating actual congestion. This limitation is considered in the decision-making process.</p>
             
           ')
    ,easyClose = TRUE
    ,footer = NULL,
    size = c("l")
    ))
  })
  
  
  observeEvent(input$help2, {
    showModal(modalDialog(
      title = "How to Start",
      HTML('<p>Click tti or speed to show the map <br/></p>
            <p>The map on left is AM map while the map on right is PM map. The view of PM map is  synchronized with AM map<br/></p>
           <p>Data Source: Probe Data Analytics Suite<a href="https://pda.ritis.org/suite/"target="_blank">https://pda.ritis.org/suite/</a></p>
           <p>Travel Time Index(TTI) :Travel time represented as a percentage of the ideal travel time (Travel Time / Free-flow Travel Time)<br/></p>
           
           <p>AM: Average data of INRIX hourly data from 6:00:00 AM to 10:00:00 AM in 2018 weekdays </p>
           
           <p>PM: Average data of INRIX hourly data from 4:00:00 PM to 8:00:00 PM in 2018 weekdays </p>
           ')
      ,easyClose = TRUE
      ,footer = NULL
      ))
  })
  
  output$map <- renderLeaflet({
    m <-leaflet(spdcompare[1:10, ],options=leafletOptions(minZoom = 8, maxZoom = 14)) %>%
      clearBounds() %>%
      addProviderTiles(providers$CartoDB.Positron,    group = "Light") %>%
      addProviderTiles(providers$CartoDB.DarkMatter,  group = "Dark") %>%
      addProviderTiles(providers$Esri.WorldImagery,   group = "Satellite") %>%
      addResetMapButton() %>%
      addScaleBar() %>%
      setView(lng = -84.3880,lat = 33.7490,zoom = 9) %>%
      addLayersControl(
        baseGroups=c('Light','Dark','Satellite')
        )
    m %>% addPolylines(
      label = ~htmlEscape(Head.Location..approximate.),
      highlightOptions = highlightOptions(
        stroke = T,
        # color ='#ed3704', 
        weight = 10,
        bringToFront = TRUE,
        sendToBack = TRUE)
    )
  }
  )
    
  output$tb <- renderDT(
    data_need, 
    filter = 'top',
    rownames = FALSE,
    callback = JS("table.on('click.dt', 'td', function() {
            var row_=table.cell(this).index().row;
            var col=table.cell(this).index().column;
            var rnd= Math.random();
            var data = [row_, col, rnd];
            Shiny.onInputChange('rows',data );
            });"),
    options = list( pageLength = 10,
                    autoWidth = TRUE,
                    lengthMenu = c(5, 10, 15, 20)
                    )
    )
  
  observeEvent({input$rows},{
    if (length(input$tb_rows_selected)){
      leafletProxy("map", data = spdcompare[input$tb_rows_selected, ]) %>% 
      # clearControls() %>%
        clearShapes() %>%
      addPolylines(
        label = ~htmlEscape(Head.Location..approximate.),
        highlightOptions = highlightOptions(
          stroke = T,
          # color ='#ed3704', 
          weight = 10,
          bringToFront = TRUE,
          sendToBack = TRUE))
      }else{}
  })
  
  
  observeEvent({input$page},{
    leafletProxy("map", data =  spdcompare[input$tb_rows_current,]) %>% 
      # clearControls() %>%
      clearShapes() %>%
      addPolylines(
        label = ~htmlEscape(Head.Location..approximate.),
        highlightOptions = highlightOptions(
          stroke = T,
          # color ='#ed3704', 
          weight = 10,
          bringToFront = TRUE,
          sendToBack = TRUE)
      )
  })
  

  
  
  output$map2 <- renderLeaflet({
    m <-leaflet(spdtti,options=leafletOptions(minZoom = 8, maxZoom = 18)) %>%
      clearBounds() %>%
      addProviderTiles(providers$CartoDB.Positron,    group = "Light") %>%
      addProviderTiles(providers$CartoDB.DarkMatter,  group = "Dark") %>%
      addProviderTiles(providers$Esri.WorldImagery,   group = "Satellite") %>%
      addSearchOSM() %>%
      addResetMapButton() %>%
      addScaleBar() %>%
      setView(lng = -84.3880,lat = 33.7490,zoom = 9) %>%
      addLayersControl(
        baseGroups=c('Light','Dark','Satellite')
      )

    m %>%  addPolylines(
      label = labels_tti_AM,
      # color = ~pal(speed.AM),
      color = eval(parse(text=paste0('~paltti(','tti','.','AM',')'))),
      highlightOptions = highlightOptions(
        stroke = T,
        # color ='#ed3704',
        weight = 10,
        bringToFront = TRUE,
        sendToBack = TRUE))%>%
        addLegend("bottomright",  pal = paltti, values = ~tti.AM,
                                    title = paste0("INRIX Average AM Travel Time Index 2018"),
                                    opacity = 0.8,
                   layerId = 1)        
  }
  )

  output$map3 <- renderLeaflet({

    m <-leaflet(spdtti,options=leafletOptions(minZoom = 8, maxZoom = 18)) %>%
      clearBounds() %>%
      addProviderTiles(providers$CartoDB.Positron,    group = "Light") %>%
      addProviderTiles(providers$CartoDB.DarkMatter,  group = "Dark") %>%
      addProviderTiles(providers$Esri.WorldImagery,   group = "Satellite") %>%
      addSearchOSM() %>%
      addResetMapButton() %>%
      addScaleBar() %>%
      setView(lng = -84.3880,lat = 33.7490,zoom = 9) %>%
      addLayersControl(
        baseGroups=c('Light','Dark','Satellite')
      )
    
    m %>%  addPolylines(
      label = labels_tti_PM,
      # color = ~pal(speed.AM),
      color = eval(parse(text=paste0('~paltti(','tti','.','PM',')'))),
      highlightOptions = highlightOptions(
        stroke = T,
        # color ='#ed3704',
        weight = 10,
        bringToFront = TRUE,
        sendToBack = TRUE))%>%
      addLegend("bottomright",  pal = paltti, values = ~tti.PM,
                title = paste0("INRIX Average PM Travel Time Index 2018"),
                opacity = 0.8,
                layerId = 1)        
  }
  )

  observeEvent({input$tti},{
    if(input$tti == "tti"){
    leafletProxy("map2",data=spdtti) %>%
      # clearControls() %>%
      clearShapes() %>%
        removeControl(layerId = 1) %>%
      addPolylines(
        label = labels_tti_AM,
        # color = ~pal(speed.AM),
        color = eval(parse(text=paste0('~paltti(','tti','.AM)'))),
        highlightOptions = highlightOptions(
          stroke = T,
          color ='#ed3704',
          weight = 5,
          bringToFront = TRUE,
          sendToBack = TRUE))%>%
        addLegend("bottomright", pal = paltti, values = eval(parse(text=paste0('~tti.AM'))),
                  title = paste0("INRIX Average AM Travel Time Index 2018"),
                  opacity = 0.8,
                  layerId = 1)
      
      leafletProxy("map3",data=spdtti) %>%
        # clearControls() %>%
        clearShapes() %>%
        removeControl(layerId = 1) %>%
        addPolylines(
          label = labels_tti_PM,
          # color = ~pal(speed.AM),
          color = eval(parse(text=paste0('~paltti(','tti','.PM)'))),
          highlightOptions = highlightOptions(
            stroke = T,
            color ='#ed3704',
            weight = 5,
            bringToFront = TRUE,
            sendToBack = TRUE))%>%
        addLegend("bottomright", pal = paltti, values = eval(parse(text=paste0('~tti.PM'))),
                  title = paste0("INRIX Average PM Travel Time Index 2018"),
                  opacity = 0.8,
                  layerId = 1)
    }else{
      leafletProxy("map2",data=spdtti) %>%
        # clearControls() %>%
        clearShapes() %>%
        removeControl(layerId = 1) %>%
        
        addPolylines(
          label = labels_speed_AM,
          # color = ~pal(speed.AM),
          color = eval(parse(text=paste0('~pal(speed.AM)'))),
          highlightOptions = highlightOptions(
            stroke = T,
            color ='#ed3704',
            weight = 5,
            bringToFront = TRUE,
            sendToBack = TRUE)) %>%
        addLegend("bottomright", pal = pal, values = eval(parse(text=paste0('~speed.AM'))),
                  title = paste0("INRIX Average AM Speed 2018 (MPH)"),
                  opacity = 0.8,
                  layerId = 1)
      
      leafletProxy("map3",data=spdtti) %>%
        # clearControls() %>%
        clearShapes() %>%
        removeControl(layerId = 1) %>%
        
        addPolylines(
          label = labels_speed_PM,
          # color = ~pal(speed.AM),
          color = eval(parse(text=paste0('~pal(speed.PM)'))),
          highlightOptions = highlightOptions(
            stroke = T,
            color ='#ed3704',
            weight = 5,
            bringToFront = TRUE,
            sendToBack = TRUE)) %>%
        addLegend("bottomright", pal = pal, values = eval(parse(text=paste0('~speed.PM'))),
                  title = paste0("INRIX Average PM Speed 2018 (MPH)"),
                  opacity = 0.8,
                  layerId = 1)
    }
  })


######################################
### Synchronize Two Map ##############
####################################
  observe({ # Observer to respond to zoom / pan of map1 and apply to map2
    coords <- input$map2_bounds
    
    if (!is.null(coords)) {
      tproxy <- leafletProxy("map3") %>% 
        fitBounds(coords$west,
                  coords$south,
                  coords$east,
                  coords$north)
    }
  })
  
  
# renderUI switch GIF and map
  output$gif <- renderUI({
    if(input$gif=='map' ){
      tagList(
      h3('INRIX Speed and Travel Time Index'),

      fluidRow(   column(6,leafletOutput('map2', height = 700) %>% withSpinner()),
                  column(6,leafletOutput('map3', height = 700) %>% withSpinner())
                  
      )
      )
    }else if(input$tti =='tti'){
      tagList(
        h3('Travel Time Index GIF: 6:00 AM - 10:00 AM, 4:00 PM - 8:00 PM   2018 weekday average'),
        HTML('<img class="center"  src="2018tti.gif" alt="ttimap" style="width:900px;height:750px;">') 
          
      )
    } else{
      tagList(
        h3('Speed GIF: 6:00 AM - 10:00 AM, 4:00 PM - 8:00 PM   2018 weekday average'),
        HTML('<img class="center"  src="2018speed.gif" alt="ttimap" style="width:900px;height:750px;">') 
      )
    }
  })

}
# Run the application 
shinyApp(ui = ui, server = server)
