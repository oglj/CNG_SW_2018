
load("prod_data.RData")

# To all embarking on this journey:
# I highly recommend watching Joe Chen's seminar
# on reactive Shiny programming
# https://www.rstudio.com/resources/webinars/shiny-developer-conference/
# In particular, setting:
# options(shiny.reactlog = TRUE)
# and pressing CTRL + F3 after running the app
# will provide a visual means of exploring
# connections between inputs, outputs,
# observers, and reactive objects

# Near the end of this process, I got lazy with documenting every single change. 
# Hopefully this gives you the overall app structure. The data processing 
# markdown file has more about how to structure the spatial data. 

##### libraries #####

# # Shiny libraries

library(shiny)
library(shinydashboard)
library(shinyjs)
library(V8) # required by shinyjs
library(shinyTree) # generating an expandable/collapsed checkboxes (nested checkboxes)
# 
# 
# # Connecting to Remote Database
library(rsconnect)
library(mongolite) # connecting to mongo lab, a web-based mongodb
library(RCurl)
# 
# 
# # Spatial + mapping packages
# # webmapping
library(leaflet)
# 
# # spatial file formats in R
# # goal is to keep everything in sf so that it's tidyverse-compatible
# library(sp)
library(sf)
# library(geojsonsf)
# 
# # packages used for simplifying spatial lines dataframes
# library(spdplyr) # for manipulating the attribute data inside the spatial data frame
# library(rmapshaper) # for manipulating the geometry (polygon, line, marker) part of the GeoJSON data
# library(sf)
# library(RColorBrewer)
library(htmltools) # for popups
library(httr)
# library(stringr)
library(tidyverse)
library(glue)
library(DT)
library(dplyr)
library(ggvis)
library(ggplot2)

# We need the packages 'remotes' to install the 'openrouteservice-r'
# package since the 'openrouteservice-r' package is not on CRAN

# library(remotes)
# remotes::install_github("GIScience/openrouteservice-r")

# library(openrouteservice)

##### Debug packages #####

library(reactlog)
options(shiny.reactlog = TRUE)


# 
# # calculating shortest distance between any pair of origin and destination through using Google API
# 
# # We're only using the XML library for the distance call
# 
# library(XML)
# 
# # We can accomplish the same with jsonlite
# 
# library(jsonlite)
# 
# library(scales) # formatting percentage
# 


##### set global options #####
options(
  mongodb = list(
    "host" = "ds159025.mlab.com:59025",
    "username" = "collablocation",
    "password" = "Geodesign2018A"
  ),
  DT.options = list(
    autoWidth = TRUE
  )
)

#### connection to MongoDB #####

# set name of database

# workshop_dbName <- "h2_ct_hartford_2019"

# connect to mLab.com and access the home base for a mongoDB account

# mongoURL <- glue(
#   "mongodb://{un}:{pw}@{h}/{db}",
#   un = options()$mongodb$username,
#   pw = options()$mongodb$password,
#   h = options()$mongodb$host,
#   db = workshop_dbName
# )

##### category & ID assignment #####
# 
# # selected sites need a type 'selected'
# # We'll make this happen when someone clicks on a marker in the
# # shiny reactive environment
# 


# o-d locations need a type: 'poi' maybe
# or 'centroid', something that makes sense

# model locations need a type '____model' where ____ is the model name/abbr

# So that way when we hit the 'remove' button it just changes the type.
# What if we did: click on a site to add it, click again to remove
# that'd be easier than the whole remove process.
# UMMMMM Big thing here: This means that EVERY point that could change type
# needs a unique ID number.  So that we don't end up with 'candidate001' and 
# 'selected001' existing simultaneously, then trying to change 'candidate001' to
# 'selected001' while 'selected001' already exists.
# I don't think that's a problem so long as we start with no selected candidates.
# So what does that mean for loading previous sessions? I imagine we could 
# maintain a database of selected sites
# and switch any sites with category 'candidate' to category 'selected'
# Maybe include in the mouseover a list of the loaded sessions that selected that candidate?
# Or, here's a thought, opacity varies based on the number of sessions that selected that candidate
# so selected sites need an entry for number of sessions.
# which, think about this.  We need a counter for the number of loaded sessions.  
# Then, for each session that selectes a candidate, increment that counter by 1
# Note: 27.09.2019 - that idea is deprecated.  We've decided to go with simple display

# # The Google Maps API Key needs the Distance Matrix active
# 
# # Please go to https://developers.google.com/maps/documentation/ for more info
# 
# # googleMapsAPIKey <- "AIzaSyBAEus4_qur-fkzm2A0PQ2mYoIK7Qra26c"

##### global variables #####

# horrible idea to make these global variables. They'll vary based on the summary stats that we assemble at the very end
# when comparing groups
# 
# xAxis_vars <- c(
#   "Group and Stage" = "groupStageIterInfo",
#   "Average Name" = "averageName",
#   "Station Count" = "stationCount"
# )
# 
# yAxis_vars <- c(  
#   "Average Name" = "averageName",
#   "Station Count" = "stationCount"
# )

##### header #####

header <- dashboardHeader(
  titleWidth = 400,
  title = tags$div(
    style = "font-size:0",
    p(
      span(
        "COL", style = "color:white; font-size: 20px"
      ),
      span(
        "LAB", style = "color:red; font-size: 20px"
      ),
      span(
        "LOCATION: hydrogen stations", style = "color: white; font-size: 20px"
      )
    )
  ),
  
  # This is the global metric distance setting
  
  tags$li(
    sliderInput(
      inputId = "metricDist", 
      label = NULL, 
      min = 1, 
      max = 10, 
      value = 2, 
      step = 1, 
      ticks = FALSE
    ),
    class = "dropdown",
    style = "
      padding-top: 5px;
      padding-bottom: 5px;
      padding-right: 20px;
    "
  ),
  
  # This will display group, stage, and iteration info
  tags$li(
    uiOutput(
      "gSI_header",
      inline = TRUE
    ),
    class = "dropdown",
    style = "
      font-size: 20px;
      color: white;
      vertical-align: middle;
      margin-right: 20px;
      margin-top: 12px
    "
  )
)

##### body #####

body <- dashboardBody(
  
  # call Javascript for use in Shiny
  
  useShinyjs(),
  
  # reload the page when calling function 'shinyjs.refresh'
  
  extendShinyjs(
    text = "shinyjs.refresh = function() { location.reload(); }"
  ),
  
  # settings for style
  
  tags$style(
    type = "text/css",
    "#map {height: calc(100vh - 122px) !important;}"
  ),
  
  # make a fluid row in the body. This makes the platform scaleable across screenwidths
  
  fluidRow(
    
    # Access the HTML <head> element
    
    tags$head(
      
      # Create an HTML <style> element
      
      tags$style(
        
        # Write HTML code and set styles for elements that we'll construct later.
        # This could be done in CSS as well.  Might be cleaner that way.
        
        # slider customization:
        # https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
        
        HTML(
          "
          .form-group{
          margin-top: 0px;
          margin-bottom: 0px;
          }
          .selectize-control{
          margin-top: 0px;
          margin-bottom: 0px;
          }
          .checkbox{
          margin-top: 0px;
          margin-bottom: 0px;
          }
          
          /* These settings change the colors of the slider input in the header */
          
          .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
          background: red
          }
          
         .js-irs-0 .irs-from, .js-irs-0 .irs-to, .js-irs-0 .irs-single, .js-irs-0 .irs-min, .js-irs-0 .irs-max { 
         background: #367fa9
         }
         
         .irs-min {
         color: #fff;
         } 
         
         .irs-max {
         visibility: hidden !important
         }
         
         .js-irs-0 .irs .irs-max:after{
         content: '10 mi.' !important;
         }
         
         .irs-max:after{
         visibility: visible !important;
        display: block;
        background: rgba(0, 0, 0, 0.1) none repeat scroll 0 0;
        border-radius: 3px;
        color: #fff;
        font-size: 10px;
        line-height: 1.333;
        padding: 1px 3px;
        text-shadow: none;
        top: 0;
        cursor: default;
        display: block;
        left: 0;
        position: absolute;
        width: 35px;
        text-align: center;
         }
         
         #addPreviousGroups {
         background-color: #3a9ed8;
         color: #fff;
         width: 100%;
         }
         
         #stageSubmit {
         background-color: #3edc5f;
         color: #fff;
         width: 100%
         }
         
         #groupNameModal {
         font-weight: normal
         }
         
         "
        )
      )
    ),
    
    # Create a tabset panel, or a set of tabs
    # assign it an ID
    # We'll populate it with our working tabs
    
    tabsetPanel(
      id = "Geodesign",
      type = "tabs",
      
      # Main tab ####
      # This is the Main tab
      # we'll put the Leaflet map in here.
      
      tabPanel(
        title = strong("Main"),
        id = "mainTab",
        value = "mainTab",
        
        
        # This is the section of the Main tab holding the Leaflet widget
        
        column(
          width = 10,
          leafletOutput(
            "map",
            height = 800
          )
        ),
        
        # This is the sidebar of the Main tab holding controls
        
        column(
          width = 2,
          height = 1200,
          style = "
          padding:0px 5px 0px 5px;
          ",
          # wellPanel(
          #   
          #   # This div contains the group name
          #   
          #   div(
          #     style = "
          #     padding:0px 0px 5px 0px;
          #     ",
          #     title = "Group: ",
          #     textOutput(
          #       outputId = "groupName"
          #     ),
          #     p(
          #       "Stage: ",
          #       textOutput(
          #         outputId = "stageNum"
          #       ),
          #       br(),
          #       "Iteration: ",
          #       textOutput(
          #         outputId = "iterNum"
          #       )
          #     )
          #   ),
          #   style = "
          #   padding: 0px 5px 0px 5px;
          #   margin-bottom: 10px;
          #   "
          # ),
          
          # This panel contains the button to be clicked whenever a group wants to save their current selection of 
          # stations.  It will also update the trees for 
          
          wellPanel(
            
            # FW: adding a submit button "Evaluate" to get results and generate a table (inlcuding loading and saving data to a remote database)
            
            tags$div(
              style = "
        padding: 5px 0px 5px 0px;
        ",
              title = "
        Click this button to submit selected stations for comparison to other submissions
        ",
              actionButton(
                inputId = "stageSubmit",
                label = strong("Save current selection"),
                # setting "white-space" to "normal" allows for flexible
                # multiline action button labels
                style = "
                  font-size: 1.5em;
                  white-space: normal;
                "
              )
            ),
            
            style = "
      padding: 0px 5px;
      margin-bottom: 10px;
      border-width: 3px;
      border-color: #3edc5f;
      "
          ),
          
          # This panel contains layer controls for the Leaflet Map
          # Performance might be improved using the solution in https://stackoverflow.com/questions/42658225/put-leaflet-controls-outside-of-map-div-in-shiny-environment
          
          wellPanel(
            div(
              title = "
              Click the checkbox next to each layer to display or hide it on the map.1&#013;
              Hover over each layer name for more information.
              ",
              p(
                style = "
          padding-left: 1em;
          margin-bottom: 5px;
          font-size: 1.5em;
          background-color: #ccc;
          color: #fff;
          border-radius: 3px
          ",
                
                strong(
                  "Layer Controls"
                )
              )
            ),
            HTML("<span style = 'padding-left: 20px;'><img src = 'selected_icon.png' width = '18'>selected candidate</span>"),
            div(
              title = "
              Existing or planned hydrogen stations with fuel for retail to the public | source: DOE Alt. Fuel Data Center
              ",
              checkboxInput(
                inputId = "ct_stationsH2_button",
                label = HTML("H<sub>2</sub> stations<br><img src = 'stationH2_existing_icon.png' width = '18'>existing<br><img src = 'stationH2_planned_icon.png' width = '18'>planned"),
                TRUE
              )
            ),
            # div(img(src = 'candidate_icon.png', height = 18),
            div(
              title = "
              gas stations within the Hartford metropolitan area | source: Google Maps
              ",
              checkboxInput(
                inputId = "ct_candidates_button",
                label = HTML("<img src = 'candidate_icon.png' width = '18'>candidate sites (gas stations)"),
                TRUE
              )
            ),
            # styling for horizontal lines lifed from this example: http://www.iraqtimeline.com/maxdesign/basicdesign/principles/prinhorizontal.html
            HTML("<hr style = 'display: block; position: relative; padding: 0; margin: 8px auto; height: 0; max-height: 0; font-size: 1px; line-height: 0; clear: both;border: none; border-top: 1px solid #aaaaaa; border-bottom: 1px solid #ffffff'>"),
            HTML("<span><b>Annual Average Daily Traffic (AADT)</b><br>line width corresponds to AADT</span>"),
            div(
              title = "
              source: Connecticut Department of Transportation
              ",
              checkboxInput(
                inputId = "ct_aadt_1_button",
                label = HTML("<img src = 'aadt_icon.png' height = '18'>interstate"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_aadt_2_button",
                label = HTML("<img src = 'aadt_icon.png' height = '18'>other freeway"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_aadt_3_button",
                label = HTML("<img src = 'aadt_icon.png' height = '18'>major arterial"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_aadt_4_button",
                label = HTML("<img src = 'aadt_icon.png' height = '18'>minor arterial"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_aadt_5_button",
                label = HTML("<img src = 'aadt_icon.png' height = '18'>major collector"),
                FALSE
              )
            ),
            # styling for horizontal lines lifed from this example: http://www.iraqtimeline.com/maxdesign/basicdesign/principles/prinhorizontal.html
            HTML("<hr style = 'display: block; position: relative; padding: 0; margin: 8px auto; height: 0; max-height: 0; font-size: 1px; line-height: 0; clear: both;border: none; border-top: 1px solid #aaaaaa; border-bottom: 1px solid #ffffff'>"),
            HTML("<span><b>Other Metrics</b><br>note: circle size corresponds to value<br>last-selected layer appears on top</span>"),
            div(
              title = "
              2015 populations for aggregated traffic analysis zones | source: CRCOG
              ",
              checkboxInput(
                inputId = "mpo_pop_button",
                label = HTML("<img src = 'pop_icon.png' height = '18'>population"),
                FALSE
              )
            ),
            div(
              title = "
              # of trips originating from OR terminating at traffic analysis zones | source: CRCOG
              ",
              checkboxInput(
                inputId = "mpo_trip_button",
                label = HTML("<img src = 'trip_icon.png' height = '18'>trips (to or from)"),
                FALSE
              )
            ),
            # styling for horizontal lines lifed from this example: http://www.iraqtimeline.com/maxdesign/basicdesign/principles/prinhorizontal.html
            HTML("<hr style = 'display: block; position: relative; padding: 0; margin: 8px auto; height: 0; max-height: 0; font-size: 1px; line-height: 0; clear: both;border: none; border-top: 1px solid #aaaaaa; border-bottom: 1px solid #ffffff'>"),
            HTML("<span><b>Optimization Model Results</b></span>"),
            div(
              title = "
              Station locations minimize travel time from roadways based on vehicle-miles traveled (VMT) | Lin et al. 2008
              ",
              checkboxInput(
                inputId = "ct_05ftBack_button",
                label = HTML("<img src = 'ftb05_icon.png' height = '30px'>fuel-travel-back | 5"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_10ftBack_button",
                label = HTML("<img src = 'ftb10_icon.png' height = '30px'>fuel-travel-back | 10"),
                FALSE
              )
            ),
            div(
              title = "
              Station locations minimize travel time from population nodes | Nicholas et al. 2004
              ",
              checkboxInput(
                inputId = "ct_05dflrm_button",
                label = HTML("<img src = 'frlm05_icon.png' height = '24px'>FRLM | 5"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_10dflrm_button",
                label = HTML("<img src = 'frlm10_icon.png' height = '24px'>FRLM | 10"),
                FALSE
              )
            ),
            div(
              title = "
              Station locations maximize the number of trips that can be made along the shortest paths given a 100 mi. driving range | Kuby et al. 2013
              ",
              checkboxInput(
                inputId = "ct_05pMed_button",
                label = HTML("<img src = 'pMed05_icon.png' height = '13px'>p-Median | 5"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_10pMed_button",
                label = HTML("<img src = 'pMed10_icon.png' height = '13px'>p-Median | 10"),
                FALSE
              )
            ),
            
            # styling for horizontal lines lifed from this example: http://www.iraqtimeline.com/maxdesign/basicdesign/principles/prinhorizontal.html
            HTML("<hr style = 'display: block; position: relative; padding: 0; margin: 8px auto; height: 0; max-height: 0; font-size: 1px; line-height: 0; clear: both;border: none; border-top: 1px solid #aaaaaa; border-bottom: 1px solid #ffffff'>"),
            HTML("<span><b>Miscellaneous Info</b></span>"),
            div(
              title = "
              Economically-distressed communities where new investments, under certain conditions, may be eligible for preferential tax treatment | source: U.S. Department of the Treasury
              ",
              checkboxInput(
                inputId = "oppZone_button",
                label = HTML("<img src = 'oppZone_icon.png' width = '18px'>opportunity zones"),
                FALSE
              )
            ),
            div(
              title = "
              Natural gas pipelines in Connecticut | source: U.S. Energy Information Administration
              ",
              checkboxInput(
                inputId = "ct_natGasPipes_button",
                label = HTML("<img src = 'natGas_icon.png' height = '18px'>natural gas pipelines"),
                FALSE
              )
            ),
            HTML("<hr style = 'display: block; position: relative; padding: 0; margin: 8px auto; height: 0; max-height: 0; font-size: 1px; line-height: 0; clear: both;border: none; border-top: 1px solid #aaaaaa; border-bottom: 1px solid #ffffff'>"),
            HTML("<span>note: last-selected layer appears on top</span>"),
            div(
              title = "
              Car dealerships in Connecticut | source: Google Maps
              ",
              checkboxInput(
                inputId = "ct_autoLots_button",
                label = HTML("<img src = 'autoLots_icon.png' width = '18px'>auto dealers"),
                FALSE
              )
            ),
            
            div(
              title = "
              Warehouses in Connecticut | source: Connecticut Hydrogen Fuel Cell Coalition
              ",
              checkboxInput(
                inputId = "ct_warehouse_button",
                label = HTML("<img src = 'warehouse_icon.png' width = '18px'>warehouses"),
                FALSE
              )
            ),
            div(
              title = "
              Industries that use hydrogen somewhere in their supply line | source: Connecticut Hydrogen Fuel Cell Coalition
              ",
              checkboxInput(
                inputId = "ct_coldStorage_button",
                label = HTML("<img src = 'coldStorage_icon.png' width = '18px'>cold storage"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_foodP_button",
                label = HTML("<img src = 'foodP_icon.png' width = '18px'>food processing"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_metalF_button",
                label = HTML("<img src = 'metalFinish_icon.png' width = '18px'>metal finish shops"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_transit_button",
                label = HTML("<img src = 'transit_icon.png' width = '18px'>transit companies"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_truckP_button",
                label = HTML("<img src = 'truckP_icon.png' width = '18px'>truck parking"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_wholesale_button",
                label = HTML("<img src = 'wholesale_icon.png' width = '18px'>wholesale"),
                FALSE
              )
            ),
            
            
            style = "
      padding: 5px;
      margin-bottom: 10px;
      border-color: #ccc;
      border-width: 3px;
      "
          ),
          
          # debug test mike
          
          # # This well panel contains the slider for distance buffers
          # wellPanel(
          #   div(
          #     p(
          #       style = "
          # padding: 0px;
          # margin-bottom: 5px;
          # ",
          #       strong(
          #         "Metric calculation distance (mi)"
          #       )
          #     )
          #   ),
          #   sliderInput(
          #     inputId = "metricDist", 
          #     label = NULL, 
          #     min = 1, 
          #     max = 10, 
          #     value = 2, 
          #     step = 1, 
          #     ticks = FALSE
          #   )
          # ),
          #     
          #     # This well panel contains the slider and checkbox for visualizing
          #     # long-distance travel
          #     
          #     wellPanel(
          #       
          #       # This div contains the title
          #       
          #       div(
          #         p(
          #           style = "
          #     padding: 0px;
          #     margin-bottom: 5px;
          #     ",
          #           
          #           strong(
          #             "Long-Distance Visualization"
          #           )
          #         )
          #       ),
          #       sliderInput(
          #         inputId = "longDistVis",
          #         label = "range (mi)",
          #         min = 250,
          #         max = 450,
          #         value = 300
          #       ),
          #       
          #       checkboxInput(
          #         inputId = "longDistVis_button",
          #         label = "visualize long-distance travel",
          #       ),
          #       
          #       style = "
          # padding: 0px 5px 0px 5px;
          # margin-bottom: 5px;
          # "
          #     ),
          
          # This well panel contains the previous group selections for viewing
          
          wellPanel(
            
            # The action button functions as the title
            div(
              title = "
              Click this button to add the candidates from previous sessions to the current selection
              ",
              actionButton(
                inputId = "addPreviousGroups",
                label = span(
                  HTML("<img src = 'prevSelecAdd_icon.png' height = '18'>"),
                  strong("Add previous selections to current selection")
                ),
                # setting "white-space" to "normal" allows for flexible
                # multiline action button labels
                style = "
                  font-size: 1.1em;
                  white-space: normal;
                "
              )
            ),
            shinyTree(
              outputId = "previousGroupList",
              checkbox = TRUE,
              theme = "default",
              themeIcons = FALSE,
              sort = TRUE
            ),
            style = "
      padding: 5px;
      margin-bottom: 5px;
      border-color: #3a9ed8;
      border-width: 3px;
      "
          )
          
          
          
          
          # This panel contains the distance calculation tool
          # Reminder that the distance calculation currently uses a pre-calculated matrix of distances between o-d pairs that requires the user to input origin & destination ID #s for the table look-up. 
          
          # wellPanel(
          #   bootstrapPage(
          #     
          #     # This div contains the title
          #     div(
          #       p(
          #         style = "
          #         padding: 0px;
          #         margin-bottom: 5px;
          #         ",
          #         strong(
          #           "Distance Calculation"
          #         )
          #       )
          #   ),
          #   
          #   # This div contains origin & destination ID# inputs
          #   
          #   div(
          #     style = "
          #     display: inline-block
          #     ",
          #     textInput(
          #       inputId = "from",
          #       label = NULL,
          #       value = "",
          #       width = 90,
          #       placeholder = "Enter ID"
          #     )
          #   ),
          #   div(
          #     style = "
          #     display: inline-block
          #     ",
          #     "To"
          #   ),
          #   div(
          #     style = "
          #     display: inline-block;
          #     padding: 0px 15px 0px 0px
          #     ",
          #     textInput(
          #       inputId = "to",
          #       label = NULL,
          #       value = "",
          #       width = 90,
          #       placeholder = "Enter ID"
          #     )
          #   )
          # ),
          # bootstrapPage(
          #   
          #   # This div contains the button that initiates distance calculation
          #   
          #   div(
          #     style = "
          #     display: inline-block;
          #     padding: 5px 5px 5px 0px;
          #     ",
          #     title = "
          #     Distance Calculation
          #     ",
          #     actionButton(
          #       "distCal",
          #       "Calculate"
          #     )
          #   ),
          #   
          #   # This div contains the button that clears labels from previously measured points
          #   
          #   div(
          #     style = "
          #     display: inline-block;
          #     padding: 5px 0px 5px 0px;
          #     ",
          #     title = "
          #     Remove Labels
          #     ",
          #     actionButton(
          #       "removeLabels",
          #       "Remove Labels"
          #     )
          #   )
          #           ),
          # 
          # # Distance look-up result display
          # 
          # verbatimTextOutput(
          #   outputId = "distanceResult"
          # ),
          # 
          # style = "
          # padding: 0px 5px 0px 5px;
          # margin-bottom: 10px;
          # "
          # ),
          
          # Panel containing driving range input and layer control for displaying gaps in tonnage quartiles
          # Since we're not using tonnage in the CT workshop, we could replace this with VMT
          
          # wellPanel(
          #   
          #   # This div contains the title
          #   
          #   div(
          #     p(
          #       style = "
          #       padding: 0px;
          #       margin-bottom: 5px;
          #       ",
          #       strong(
          #         "Driving Range"
          #       )
          #     )
          # ),
          # 
          # # This div contains input for the driving range
          # # Why does it need to be text input? Why not number input?
          # # Maybe radio buttons to select ranges of existing vehicles, with an "other" option for inputing a different range
          # 
          # div(
          #   style = "
          #   display: inline-block;
          #   padding: 0px 0px 5px 0px;
          #   ",
          #   textInput(
          #     "driving_range",
          #     label = NULL,
          #     value = "",
          #     placeholder = "Enter driving range"
          #   )
          # ),
          # 
          # # div containing literally just the word "miles"
          # div(
          #   style = "
          #   display: inline-block;
          #   padding: 0px 0px 5px 0px;
          #   ",
          #   "miles"
          # ),
          # 
          # # div containing title for layer control section
          # 
          # div(
          #   p(
          #     style = "
          #     padding: 3px 0px 0px 0px;
          #     margin-bottom: 0px
          #     ",
          #     strong(
          #       "Show Coverage Gaps"
          #     )
          #   )
          #   ),
          # 
          # # layer controls
          # 
          # checkboxInput(
          #   inputId = "gaps_top_quartile",
          #   label = "Top Quartile"
          # ),
          # checkboxInput(
          #   inputId = "gaps_2nd_quartile",
          #   label = "2nd Quartile"
          # ),
          # checkboxInput(
          #   inputId = "gaps_3rd_quartile",
          #   label = "3rd Quartile"
          # ),
          # checkboxInput(
          #   inputId = "gaps_4th_quartile",
          #   label = "4th Quartile"
          # ),
          # 
          # 
          # # # Input for specific O-D pair that displays path on map
          # # 
          # # div(
          # #   style = "
          # #     padding: 5px 0px 5px 0px
          # #   ",
          # #   selectInput(
          # #     inputId = "odPairs",
          # #     label = "Origin-Destination Pairs",
          # #     choices = c(
          # #       Choose = "",
          # #       OD_paths$PAIR
          # #     ),
          # #     selectize = FALSE
          # #   )
          # # ),
          # 
          # style = "
          # padding: 0px 5px 0px 5px;
          # margin-bottom: 10px;
          # "
          #     ),
          
          
          
          
          
          
        )
      ),
      
      
      # Selected Station Data Tab ####
      
      # This tab panel contains the performance metrics table for the selected new candidates
      
      tabPanel(
        title = strong(
          "Selected Station Data"
        ),
        id = "formTabSelections",
        value = "formTabSelections",
        br(),
        
        # This div contains the performance metric table
        
        div(
          style = "
        overflow-x: scroll;
        ",
          DT::dataTableOutput(
            outputId = "selectionsTable",
            width = "100%",
            height = "100%"
          )
        ),
        
        # Button for downloading the table
        
        downloadButton(
          outputId = "downloadGroupSelectionData",
          label = "Download this table"
        )
      ),
      
      # Iteration Performance Metrics ####
      
      # This tab panel contains the performance metrics table for each group, by stage
      
      tabPanel(
        title = strong(
          "Iteration Performance Metrics"
        ),
        id = "formTabGroup",
        value = "formTabGroup",
        br(),
        
        # This div contains the performance metric table
        
        div(
          style = "
        overflow-x: scroll;
        ",
          DT::dataTableOutput(
            outputId = "measuresTable",
            width = "100%",
            height = "100%"
          )
          
        ),
        
        # Button for downloading the table
        
        downloadButton(
          outputId = "downloadGroupMeasureData",
          label = "Download this table"
        )
      ),
      
      # Spatial Group Comparison Tab #####
      
      # this tab panel contains the performance metric comparison chart for all group candidate sets by stage 
      
      tabPanel(
        title = strong(
          "Comparing to Other Groups"
        ),
        id = "formTabAllGroups",
        value = "formTabAllGroups",
        
        # This column contains the comparison charts
        
        column(
          width = 10,
          wellPanel(
            
            # This div contains the comparison chart
            div(
              style = "
            overflow-x: scroll
            ",
              DT::dataTableOutput(
                "allMeasuresTable"
              )
            ),
            
            bootstrapPage(
              
              # This div contains the button for uploading the table to include recent submissions from all groups
              
              # div(
              #   style = "
              # display: inline-block
              # ",
              #   title = "
              # Update table to include recent inputs from other groups
              # ",
              #   actionButton(
              #     inputId = "updateTable",
              #     label = "Update the table"
              #   )
              # ),
              
              # this div contains the button to download the comparison table as a .csv file
              
              div(
                style = "
              display: inline-block;
              ",
                title = "
              Download this table as a .csv file
              ",
                downloadButton(
                  outputId = "downloadAllGroupMeasureData",
                  label = "Download the table"
                )
              )
            )
          ),
          
          wellPanel(
            plotOutput(
              outputId = "groupComparison_plot",
              height = 600
            ),
            tags$div(
              title = "
        Download the plot as a .png file
        ",
              downloadButton(
                outputId = "downloadPlot",
                label = "Download the plot"
              )
            )
          )
        ),
        
        # This column contains the selection controls for group candidate set comparison
        
        column(
          width = 2,
          style = "
      padding: 0px 5px 0px 5px;
      ",
          wellPanel(
            
            # This div instructs users to select groups and stages for comparison
            
            div(
              p(
                style = "
            padding: 0px 0px 0px 5px;
            margin-bottom: 0px;
            ",
                strong(
                  "
              Select group(s) & stage(s) for comparison
              "
                )
              )
            ),
            
            # This div contains a collapsing tree of checkboxes that allow for selection of specific groups and stages
            
            tags$div(
              style = "
          font-size: 10pt;
          padding: 0px;
          margin-bottom: 0px;
          margin-top: 0px;
          ",
              class = ".jstree-node",
              shinyTree(
                outputId = "groupstagePlot",
                checkbox = TRUE,
                theme = "default",
                themeIcons = FALSE,
                sort = TRUE
              )
            ),
            
            style = "
        padding: 0px 0px 20px 0px;
        margin-bottom: 0px;
        height: 800px;
        overflow-y: scroll;
        "
          ),
          
          # This panel contains the button that updates the list of groups & stages displayed to include those checked in the Tree structure
          
          wellPanel(
            tags$div(
              style = "
          padding: 5px 0px 5px 0px;
          ",
              title = "
          Update list of groups & stages
          ",
              actionButton(
                inputId = "updatePlot",
                label = "Update list"
              )
            ),
            
            style = "
        padding: 0px 0px 0px 5px;
        margin-bottom: 20px
        "
          ),
          
          # This panel contains the x & y variable selection inputs for variables to display in the comparison chart
          
          wellPanel(
            
            selectInput(
              inputId = "xVarPlot",
              label = "X-axis variable",
              choices = xAxis_vars
            ),
            
            selectInput(
              inputId = "yVarPlot",
              label = "Y-axis variable",
              choices = yAxis_vars
            ),
            
            # This is a note containing information for users
            
            tags$small(
              paste0(
                "Note: A bar chart will be created to ",
                "compare one performance measure between ",
                "selected groups when GROUP_STAGE is ",
                "selected as X-axis variable. Otherwise, ",
                "a scatter plot will be generated to compare ",
                "two performance measures between groups."
              )
            ),
            
            style = "
      padding: 0px 5px 0px 5px;
      "
            
          )
        )
      ),
      
      # this tab panel contains the leaflet widget that allows for comparisons between different candidate sets
      
      tabPanel(
        title = strong(
          "Spatial Comparison"
        ),
        id = "formTabAllGroupSelections",
        value = "formTabAllGroupSelections",
        
        # This column contains the leaflet widget
        
        column(
          width = 10,
          leafletOutput(
            "map_compare",
            height = 900
          )
        ),
        
        # This column contains the layer controls for the leaflet map
        
        column(
          width = 2,
          style = "
        padding: 0px 5px 0px 5px
        ",
          # This panel contains layer controls for the Leaflet Map
          # Performance might be improved using the solution in https://stackoverflow.com/questions/42658225/put-leaflet-controls-outside-of-map-div-in-shiny-environment
          wellPanel(
            div(
              title = "
              Click the checkbox next to each layer to display or hide it on the map.1&#013;
              Hover over each layer name for more information.
              ",
              p(
                style = "
          padding-left: 1em;
          margin-bottom: 5px;
          font-size: 1.5em;
          background-color: #ccc;
          color: #fff;
          border-radius: 3px
          ",
                
                strong(
                  "Layer Controls"
                )
              )
            ),
            HTML("<span style = 'padding-left: 20px;'><img src = 'selected_icon.png' width = '18'>selected candidate</span>"),
            div(
              title = "
              Existing or planned hydrogen stations with fuel for retail to the public | source: DOE Alt. Fuel Data Center
              ",
              checkboxInput(
                inputId = "ct_stationsH2_button_spC",
                label = HTML("H<sub>2</sub> stations<br><img src = 'stationH2_existing_icon.png' width = '18'>existing<br><img src = 'stationH2_planned_icon.png' width = '18'>planned"),
                FALSE
              )
            ),
            # div(img(src = 'candidate_icon.png', height = 18),
            div(
              title = "
              gas stations within the Hartford metropolitan area | source: Google Maps
              ",
              checkboxInput(
                inputId = "ct_candidates_button_spC",
                label = HTML("<img src = 'candidate_icon.png' width = '18'>candidate sites (gas stations)"),
                FALSE
              )
            ),
            # styling for horizontal lines lifed from this example: http://www.iraqtimeline.com/maxdesign/basicdesign/principles/prinhorizontal.html
            HTML("<hr style = 'display: block; position: relative; padding: 0; margin: 8px auto; height: 0; max-height: 0; font-size: 1px; line-height: 0; clear: both;border: none; border-top: 1px solid #aaaaaa; border-bottom: 1px solid #ffffff'>"),
            HTML("<span><b>Annual Average Daily Traffic (AADT)</b><br>line width corresponds to AADT</span>"),
            div(
              title = "
              source: Connecticut Department of Transportation
              ",
              checkboxInput(
                inputId = "ct_aadt_1_button_spC",
                label = HTML("<img src = 'aadt_icon.png' height = '18'>interstate"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_aadt_2_button_spC",
                label = HTML("<img src = 'aadt_icon.png' height = '18'>other freeway"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_aadt_3_button_spC",
                label = HTML("<img src = 'aadt_icon.png' height = '18'>major arterial"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_aadt_4_button_spC",
                label = HTML("<img src = 'aadt_icon.png' height = '18'>minor arterial"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_aadt_5_button_spC",
                label = HTML("<img src = 'aadt_icon.png' height = '18'>major collector"),
                FALSE
              )
            ),
            # styling for horizontal lines lifed from this example: http://www.iraqtimeline.com/maxdesign/basicdesign/principles/prinhorizontal.html
            HTML("<hr style = 'display: block; position: relative; padding: 0; margin: 8px auto; height: 0; max-height: 0; font-size: 1px; line-height: 0; clear: both;border: none; border-top: 1px solid #aaaaaa; border-bottom: 1px solid #ffffff'>"),
            HTML("<span><b>Other Metrics</b><br>note: circle size corresponds to value<br>last-selected layer appears on top</span>"),
            div(
              title = "
              2015 populations for aggregated traffic analysis zones | source: CRCOG
              ",
              checkboxInput(
                inputId = "mpo_pop_button_spC",
                label = HTML("<img src = 'pop_icon.png' height = '18'>population"),
                FALSE
              )
            ),
            div(
              title = "
              # of trips originating from OR terminating at traffic analysis zones | source: CRCOG
              ",
              checkboxInput(
                inputId = "mpo_trip_button_spC",
                label = HTML("<img src = 'trip_icon.png' height = '18'>trips (to or from)"),
                FALSE
              )
            ),
            # styling for horizontal lines lifed from this example: http://www.iraqtimeline.com/maxdesign/basicdesign/principles/prinhorizontal.html
            HTML("<hr style = 'display: block; position: relative; padding: 0; margin: 8px auto; height: 0; max-height: 0; font-size: 1px; line-height: 0; clear: both;border: none; border-top: 1px solid #aaaaaa; border-bottom: 1px solid #ffffff'>"),
            HTML("<span><b>Optimization Model Results</b></span>"),
            div(
              title = "
              Station locations minimize travel time from roadways based on vehicle-miles traveled (VMT) | Lin et al. 2008
              ",
              checkboxInput(
                inputId = "ct_05ftBack_button_spC",
                label = HTML("<img src = 'ftb05_icon.png' height = '30px'>fuel-travel-back | 5"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_10ftBack_button_spC",
                label = HTML("<img src = 'ftb10_icon.png' height = '30px'>fuel-travel-back | 10"),
                FALSE
              )
            ),
            div(
              title = "
              Station locations minimize travel time from population nodes | Nicholas et al. 2004
              ",
              checkboxInput(
                inputId = "ct_05dflrm_button_spC",
                label = HTML("<img src = 'frlm05_icon.png' height = '24px'>FRLM | 5"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_10dflrm_button_spC",
                label = HTML("<img src = 'frlm10_icon.png' height = '24px'>FRLM | 10"),
                FALSE
              )
            ),
            div(
              title = "
              Station locations maximize the number of trips that can be made along the shortest paths given a 100 mi. driving range | Kuby et al. 2013
              ",
              checkboxInput(
                inputId = "ct_05pMed_button_spC",
                label = HTML("<img src = 'pMed05_icon.png' height = '13px'>p-Median | 5"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_10pMed_button_spC",
                label = HTML("<img src = 'pMed10_icon.png' height = '13px'>p-Median | 10"),
                FALSE
              )
            ),
            
            # styling for horizontal lines lifed from this example: http://www.iraqtimeline.com/maxdesign/basicdesign/principles/prinhorizontal.html
            HTML("<hr style = 'display: block; position: relative; padding: 0; margin: 8px auto; height: 0; max-height: 0; font-size: 1px; line-height: 0; clear: both;border: none; border-top: 1px solid #aaaaaa; border-bottom: 1px solid #ffffff'>"),
            HTML("<span><b>Miscellaneous Info</b></span>"),
            div(
              title = "
              Economically-distressed communities where new investments, under certain conditions, may be eligible for preferential tax treatment | source: U.S. Department of the Treasury
              ",
              checkboxInput(
                inputId = "oppZone_button_spC",
                label = HTML("<img src = 'oppZone_icon.png' width = '18px'>opportunity zones"),
                FALSE
              )
            ),
            div(
              title = "
              Natural gas pipelines in Connecticut | source: U.S. Energy Information Administration
              ",
              checkboxInput(
                inputId = "ct_natGasPipes_button_spC",
                label = HTML("<img src = 'natGas_icon.png' height = '18px'>natural gas pipelines"),
                FALSE
              )
            ),
            HTML("<hr style = 'display: block; position: relative; padding: 0; margin: 8px auto; height: 0; max-height: 0; font-size: 1px; line-height: 0; clear: both;border: none; border-top: 1px solid #aaaaaa; border-bottom: 1px solid #ffffff'>"),
            HTML("<span>note: last-selected layer appears on top</span>"),
            div(
              title = "
              Car dealerships in Connecticut | source: Google Maps
              ",
              checkboxInput(
                inputId = "ct_autoLots_button_spC",
                label = HTML("<img src = 'autoLots_icon.png' width = '18px'>auto dealers"),
                FALSE
              )
            ),
            
            div(
              title = "
              Warehouses in Connecticut | source: Connecticut Hydrogen Fuel Cell Coalition
              ",
              checkboxInput(
                inputId = "ct_warehouse_button_spC",
                label = HTML("<img src = 'warehouse_icon.png' width = '18px'>warehouses"),
                FALSE
              )
            ),
            div(
              title = "
              Industries that use hydrogen somewhere in their supply line | source: Connecticut Hydrogen Fuel Cell Coalition
              ",
              checkboxInput(
                inputId = "ct_coldStorage_button_spC",
                label = HTML("<img src = 'coldStorage_icon.png' width = '18px'>cold storage"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_foodP_button_spC",
                label = HTML("<img src = 'foodP_icon.png' width = '18px'>food processing"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_metalF_button_spC",
                label = HTML("<img src = 'metalFinish_icon.png' width = '18px'>metal finish shops"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_transit_button_spC",
                label = HTML("<img src = 'transit_icon.png' width = '18px'>transit companies"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_truckP_button_spC",
                label = HTML("<img src = 'truckP_icon.png' width = '18px'>truck parking"),
                FALSE
              ),
              checkboxInput(
                inputId = "ct_wholesale_button_spC",
                label = HTML("<img src = 'wholesale_icon.png' width = '18px'>wholesale"),
                FALSE
              )
            ),
            
            
            style = "
      padding: 5px;
      margin-bottom: 10px;
      border-color: #ccc;
      border-width: 3px;
      "
          ),
          
          # Panel containing controls for group selection
          
          wellPanel(
            
            # this div contains the title
            
            div(
              p(
                style = "
              padding: 0px;
              margin-bottom: 0px,
              ",
                strong(
                  "Select group(s) & stage(s)"
                )
              )
            ),
            
            # This div contains a collapsing tree of groups and stages for spatial comparison
            
            tags$div(
              style = "
          font-size: 10pt;
          padding: 0px;
          margin-bottom: 0px;
          margin-top: 0px;
          ",
              class = ".jstree-node",
              shinyTree(
                outputId = "spatialGroupSelectionTree",
                checkbox = TRUE,
                theme = "default",
                themeIcons = FALSE,
                sort = TRUE
              )
            ),
            
            style = "
        padding: 0px 0px 20px 0px;
        margin-bottom: 0px;
        height: 800px;
        overflow-y: scroll;
        "
          ),
          
          # This div contains a button that updates the list of groups / stages to include recently input ones
          
          wellPanel(
            tags$div(
              style = "
            padding: 5px 0px 15px 0px;
            ",
              title = "
            Update list of groups & stages
            ",
              actionButton(
                inputId = "updateSpatial",
                label = "Update list"
              )
            ),
            
            bootstrapPage(
              
              # This div contains a button for visualizing selected groups & stages
              
              div(
                style = "
              display: inline-block;
              padding: 0px 5px 15px 0px
              ",
                title = "
              Visualize selected groups / stages
              ",
                actionButton(
                  "visSpatial",
                  "Visualize Points"
                )
              ),
              
              div(
                style = "
              display: inline-block;
              padding: 0px 5px 15px 0px
              ",
                title = "
              Remove selected points on map
              ",
                actionButton(
                  "hideSelectedGroups_sp",
                  "Hide Points"
                )
              ),
              
              div(
                style = "
              display: inline-block;
              padding: 0px 5px 15px 0px
              ",
                title = "
              Download selected data as .csv file
              ",
                downloadButton(
                  "downloadSelectedData_sp",
                  "Download selected data"
                )
              )
            ),
            
            style = "
          padding: 0px 5px 0px 5px;
          margin-bottom: 10px;
          "
          )
        )
      ),
      
      # This tab panel contains the .pdf that contains the collablocation information
      
      tabPanel(
        title = strong("Help"), id = "about", value = "help",
        br(),
        htmlOutput("quickStartGuide") # show a pdf with default size
      )
    )
  )
)

##### sidebar #####

sidebar <- dashboardSidebar(
  disable = TRUE
)

##### ui #####

ui <- dashboardPage(
  title = "Collablocation: Geodesign Platform",
  header = header,
  sidebar = sidebar,
  body = body
)

##### server #####

server <- function(
  input,
  output,
  session
) {
  
  # print(" debug server ")
  ##### MongoDB connections #####
  
  
  # connect to allGroupInfo_db
  
  allGroupInfo_db <- mongo(
    collection = "allGroupInfo_db",
    url = mongoURL
  )
  
  allGroupInfo <- reactive({
    input$stageSubmit
    return(
      allGroupInfo_db$find('{}')
    )
  }
  )
  
  # connect to ct_candidates_db
  
  ct_candidates_db <- mongo(
    collection = "ct_candidates_db",
    url = mongoURL
  )
  
  # connect to groupSelected_joinId_db
  
  groupSelected_joinId_db <- mongo(
    collection = "groupSelected_joinId_db",
    url = mongoURL
  )
  
  groupSelected_joinId <- reactive(
    groupSelected_joinId_db$find('{}')
  )
  
  # tempSelected_db
  
  tempSelected_db <- reactive(
    return(
      mongo(
        collection = glue(
          "{gN}_temp_db",
          gN = req(
            expr = input$groupName
          )
        ),
        url = mongoURL
      )
    )
  )
  
  ##### Initial Group Name Input #####
  
  # So this is going to set up a pop-up that asks for group name as input
  # It sets up input Id's in the global namespace
  
  showModal(
    modalDialog(
      textInput(
        inputId = "groupName",
        label = span(
          "Please enter your group name:",
          textOutput(
            outputId = "groupNameModal",
          )
        ),
        value = "",
        placeholder = "(20 characters max)"
      ),
      
      radioButtons(
        inputId = "stageNum",
        label = "Please select a workshop stage:",
        choices = c(
          "1" = 1,
          "2" = 2,
          "3" = 3
        ),
        inline = TRUE
      ),
      
      footer = tagList(
        span(
          strong(
            "Please wait while the ColLabLocation map loads, then click to begin:"
          ),
          style = '
          padding-right: 1em
          '
        ),
        actionButton(
          inputId = "groupName_submit",
          label = "Begin"
        )
      ),
      easyClose = FALSE
    )
  )
  
  output$groupNameModal <- renderText(
    validate(
      
      need(
        expr =  {
          if(nchar(input$groupName) > 0) {
            nchar(input$groupName) < 20
          } else(TRUE)
        },
        message = "twenty (20) characters or fewer, please"
      ),
      need(
        expr = {
          if(nchar(input$groupName) > 0) {
            str_detect(
              string = input$groupName,
              pattern = validInputPattern
            ) 
          } else(TRUE)
        },
        message = HTML("alphanumeric characters, periods, dashes, and underscores only
  A-Z a-z 0-9 . - _")
      ),
      need(
        expr = input$groupName,
        message = "group name cannot be blank"
      )
    )
  )
  
  
  
  observeEvent(
    eventExpr = input$groupName_submit,
    handlerExpr = {
      
      # This precludes the begin button working prior to submitting a group name
      
      validate(
        
        need(
          expr =  {
            if(nchar(input$groupName) > 0) {
              nchar(input$groupName) < 20
            } else(TRUE)
          },
          message = "twenty (20) characters or fewer, please"
        ),
        need(
          expr = {
            if(nchar(input$groupName) > 0) {
              str_detect(
                string = input$groupName,
                pattern = validInputPattern
              ) 
            } else(TRUE)
          },
          message = HTML("alphanumeric characters, periods, dashes, and underscores only
  A-Z a-z 0-9 . - _")
        ),
        need(
          expr = input$groupName,
          message = "group name cannot be blank"
        )
      )
      removeModal()
    }
  )
  
  ##### Group Stage Iteration Info #####
  
  # This sets the iterNum for the active session once users click input$submitStage
  iterNum_activeSesh <- reactiveVal(
    val = NULL
  )
  
  iterNum <- reactive({
    if({
      nchar(input$groupName) > 0 
      nchar(input$groupName) < 20
      str_detect(
        string = input$groupName,
        pattern = validInputPattern
      ) 
    }) {
      ifelse(
        test = is.null(iterNum_activeSesh()),
        
        # if iterNum_activeSesh is null, query the database for current group & stage info
        yes = {
          allGroupNameStageIter <- allGroupInfo_db$find(
            
            query = glue(
              '{{
                "groupName" : "{gN}",
                "stageNum" : "{sN}"
              }}',
              gN = req(
                expr = input$groupName
              ),
              sN = req(
                expr = input$stageNum
              )
            ),
            fields = '{
              "_id" : false,
              "groupStageIterInfo" : true,
              "stageNum" : true,
              "iterNum" : true
            }'
          )
          
          # identify the maximum iteration number among iterations in current group & stage, then
          # return that + 1
          
          return(
            ifelse(
              test = nrow(allGroupNameStageIter) > 0,
              no = 1,
              yes = {max(as.integer(allGroupNameStageIter$iterNum)) + 1}
            )
          )
        },
        
        # if iterNum_activeSesh is not null, use iterNum_activeSesh as the current iteration number
        no = {
          iterNum_activeSesh()
        }
      )
    }
  })
  
  # sometimes we'll want the padded iteration number
  paddedIter <- reactive({
    return(
      str_pad(
        string = iterNum(),
        
        # NOTE: the padded 2 is hard-coded, we assume that we will never get to 100 iterations
        # in one stage.
        
        width = 2,
        pad = "0"
      )
    )
  })
  
  # This sets the iteration number to 1, which we assume any given stage begins with.
  # We increment this whenever a group hits the submit button.
  # We might not need this if we auto-evaluat a given network of stations when
  # a group switches to the performance tab
  
  # We'll use this to construct a unique ID for each 
  
  groupStageIterInfo <- reactive({
    
    if({
      nchar(req(input$groupName)) > 0 
      nchar(req(input$groupName)) < 20
      str_detect(
        string = req(input$groupName),
        pattern = validInputPattern
      ) 
    }) {
      return(
        glue(
          "{groupName}_{stageNum}_{iterNum}",
          groupName = input$groupName,
          stageNum = input$stageNum,
          iterNum = paddedIter()
        )
      )
    }
  })
  
  
  
  # create an entry for the current session in the allGroupSelections table:
  
  # Display the input groupName
  
  # TODO Validate group name
  
  
  
  # https://shiny.rstudio.com/reference/shiny/latest/validate.html
  
  # display group, stage, and iteration at the top of the page
  output$gSI_header <- renderUI({
    
    
    glue(
      "{gN} | Stage: {sN}.{pIN}",
      gN = input$groupName,
      sN = input$stageNum,
      pIN = paddedIter()
    )
  })
  
  ##### server functions #####
  
  # here are some functions that we want to use later.
  
  # Function for showing / hiding layers in a Shiny Leaflet widget using Shiny input controls outside of the widget
  # As mentioned above, this function might be improved performance-wise using the 
  # This function takes a spatial dataframe, a Shiny input object, and the group name assigned to the spatial dataframe.
  # If input = TRUE, show layer.  If input = FALSE, hide layer.
  
  
  layerToggle <- function(
    mapId,
    button,
    group
  ){
    ifelse(
      button,
      leafletProxy(
        mapId = mapId
      ) %>%
        showGroup(
          group = group
        ),
      leafletProxy(
        mapId = mapId
      ) %>%
        hideGroup(
          group = group
        )
    )
  }
  
  # query-building function creates a JSON object for querying MongoDB
  # using a list of values and a field to check for those values in the db
  # If we made a function to auto retrieve info using the joining table this
  # would be essential
  
  mongo_buildInListQuery <- function(listToQuery, field = as.character()) {
    
    x <- glue_collapse(
      double_quote(
        listToQuery
      ),
      sep = ", "
    )
    
    glue(
      '{{
        "{field}" : {{
          "$in": [
            {listToQuery}
          ]
        }}
      }}',
      listToQuery = x,
      field = field
    )
  }
  
  ##### global reactive objects #####
  
  # create a reactive list comprised of the candidate stations
  # All candidate sites need to be located within the same spatial dataframe
  
  # In this iteration of Collablocation, all of our candidate sites are gas stations
  
  # sf stands for spatial feature
  
  ct_candidates <- reactiveValues(
    sf = ct_candidates_4326_sf 
  )
  
  # subset the candidates with category "selected"
  # to a reactive value called selectedCandidates
  
  ct_selected <- reactive(
    return(
      ct_candidates$sf[ct_candidates$sf$category == "selected",]
    )
  )
  
  ct_selectedMarkers <- reactive({
    selectedMarkers <- ct_selected() %>%
      mutate(
        "layerId" = str_replace(
          string = layerId,
          pattern = "selected",
          replacement = "marker"
        )
      )
    return(
      selectedMarkers
    )
  })
  
  # This fetches the set of station label texts that correspond to the 
  # user-selected metric buffer distance
  
  stationLabels <- eventReactive(
    eventExpr = input$metricDist,
    valueExpr = {
      
      stationLabels_xx <- get(
        glue(
          "stationLabels_{xx}",
          xx = str_pad( # paddedId <- str_pad(
            string = as.character(input$metricDist),
            width = 2,
            pad = "0"
          )
        )
      )
      
      return(
        stationLabels_xx
      )
    }
  )
  
  # This returns the name or symbol of the flow metric column
  # corresponding to the user-selected metric buffer distance
  # it needs to be called as a column name for a give dataframe
  # e.g. ct_candidates$sf$totFlowMetric()
  
  totFlowMetric <- eventReactive(
    eventExpr = input$metricDist,
    valueExpr = {
      totFlowxxmi <- glue(
        "totFlow{xx}mi",
        xx = str_pad(
          string = as.character(input$metricDist),
          width = 2,
          pad = "0"
        )
      )
      return(
        totFlowxxmi %>% as.symbol()
      )
    }
  )
  
  # This returns the name or symbol of the flow metric column
  # corresponding to the user-selected metric buffer distance
  # it needs to be called as a column name for a give dataframe
  # e.g. ct_candidates$sf$totPopMetric()
  
  totPopMetric <- eventReactive(
    eventExpr = input$metricDist,
    valueExpr = {
      totPopxxmi <- glue(
        "totPop{xx}mi",
        xx = str_pad(
          string = as.character(input$metricDist),
          width = 2,
          pad = "0"
        )
      )
      return(
        totPopxxmi %>% as.symbol()
      )
    }
  )
  
  # This returns the name or symbol of the trip metric column
  # corresponding to the user-selected metric buffer distance
  # it needs to be called as a column name for a give dataframe
  # e.g. ct_candidates$sf$totTripMetric()
  
  totTripMetric <- eventReactive(
    eventExpr = input$metricDist,
    valueExpr = {
      totTripxxmi <- glue(
        "totTrip{xx}mi",
        xx = str_pad(
          string = as.character(input$metricDist),
          width = 2,
          pad = "0"
        )
      )
      return(
        totTripxxmi %>% as.symbol()
      )
    }
  )
  
  
  ##### global reactive settings #####
  
  # attempt to make dynamically sized icons dependent on zoom level
  
  # markerSizesZoomHigh <- c(
  #   candidate <- c(
  #     x = 30,
  #     y = 34
  #   ),
  #   selected <- c(
  #     x = 28,
  #     y = 66
  #   ),
  #   stationH2 <- c(
  #     x = 45,
  #     y = 58
  #   )
  # )
  # 
  # markerSizesZoomLow <- markerSizesZoomHigh %>% modify_depth(
  #   1,
  #   function(dim){
  #     dim / 2
  #   }
  # ) #MAKE THIS A PURRR THING THAT HALVES THE VALUES IN MARKERSIZESZOOMHIGH
  # print(markerSizesZoomLow)
  #   
  #   # WAIT DO THIS FOR THE MARKER ICON LIST.  THEN APPLY EITHER IN A FUNCTION THAT ACCOUNTS FOR ZOOM LEVEL
  #   
  # markerIconList_byCategory <- reactive({
  #   
  #   # zoomLessThan11 <- 
  #   # if(else(
  #   #   input$
  #   # ))
  #   markerSizes <- c(
  #     candidate = c(
  #       x = ifelse(
  #         test = input$map_zoom > 11,
  #         yes = 30,
  #         no = 16 # 15
  #       ),
  #       y = ifelse(
  #         test = input$map_zoom > 11,
  #         yes = 34,
  #         no = 18 #17
  #       )
  #     ),
  #     selected = c(
  #         x = ifelse(
  #         test = input$map_zoom > 11,
  #         yes = 48,
  #         no = 24
  #       ),
  #       y = ifelse(
  #         test = input$map_zoom > 11,
  #         yes = 66,
  #         no = 34 #33
  #       )
  #     ),
  #     stationH2_x <- ifelse(
  #       test = input$map_zoom > 11,
  #       yes = 46, #45
  #       no = 58
  #     ),
  #     stationH2_y <- ifelse(
  #       test = input$map_zoom > 11,
  #       yes = 58,
  #       no = 30 #29
  #     )
  #   )
  #   print(class(markerSizes))
  #   print(str(markerSizes))
  #   print(is.atomic(markerSizes))
  #   return(
  #     iconList(
  #   candidate = makeIcon(
  #     iconUrl = "www/candidate_icon.png",
  #     iconWidth = markerSizes$candidate["x"],
  #     iconHeight = markerSizes$candidate["y"],
  #     iconAnchorX = markerSizes$candidate["x"] / 2,
  #     iconAnchorY = markerSizes$candidate["y"] / 2
  #   ),
  #   selected = makeIcon(
  #     iconUrl = "www/selected_icon.png",
  #     iconWidth = markerSizes$selected["x"],
  #     iconHeight = markerSizes$selected["y"],
  #     iconAnchorX = markerSizes$candidate["x"] / 2,
  #     iconAnchorY = markerSizes$candidate["y"] / 2
  #   ),
  #   stationH2_planned = makeIcon(
  #     iconUrl = "www/stationH2_planned_icon.png",
  #     iconWidth = markerSizes$stationH2["x"],
  #     iconHeight = markerSizes$stationH2["y"],
  #     iconAnchorX = markerSizes$stationH2["x"] / 2,
  #     iconAnchorY = markerSizes$stationH2["y"] / 2
  #   ),
  #   stationH2_existing = makeIcon(
  #     iconUrl = "www/stationH2_existing_icon.png",
  #     iconWidth = markerSizes$selected["x"],
  #     iconHeight = markerSizes$selected["y"],
  #     iconAnchorX = markerSizes$candidate["x"] / 2,
  #     iconAnchorY = markerSizes$canddidate["y"] / 2
  #   ),
  #   prevSelec_q1 = makeIcon(
  #     iconUrl = "www/prevSelecq1_icon.png",
  #     iconWidth = 48,
  #     iconHeight = 34,
  #     iconAnchorX = 1,
  #     iconAnchorY = 34
  #   ),
  #   prevSelec_q2 = makeIcon(
  #     iconUrl = "www/prevSelecq2_icon.png",
  #     iconWidth = 48,
  #     iconHeight = 34,
  #     iconAnchorX = 1,
  #     iconAnchorY = 34
  #   ),
  #   prevSelec_q3 = makeIcon(
  #     iconUrl = "www/prevSelecq3_icon.png",
  #     iconWidth = 48,
  #     iconHeight = 34,
  #     iconAnchorX = 1,
  #     iconAnchorY = 34
  #   ),
  #   prevSelec_q4 = makeIcon(
  #     iconUrl = "www/prevSelecq4_icon.png",
  #     iconWidth = 48,
  #     iconHeight = 34,
  #     iconAnchorX = 1,
  #     iconAnchorY = 34
  #   ),
  #   frlm05 = makeIcon(
  #     iconUrl = "www/frlm05_icon.png",
  #     iconWidth = 29,
  #     iconHeight = 43,
  #     iconAnchorX = 14,
  #     iconAnchorY = 43
  #   ),
  #   frlm10 = makeIcon(
  #     iconUrl = "www/frlm10_icon.png",
  #     iconWidth = 32,
  #     iconHeight = 45,
  #     iconAnchorX = 16,
  #     iconAnchorY = 45
  #   ),
  #   pMed05 = makeIcon(
  #     iconUrl = "www/pMed05_icon.png",
  #     iconWidth = 29,
  #     iconHeight = 43,
  #     iconAnchorX = 14,
  #     iconAnchorY = 43
  #   ),
  #   pMed10 = makeIcon(
  #     iconUrl = "www/pMed10_icon.png",
  #     iconWidth = 32,
  #     iconHeight = 45,
  #     iconAnchorX = 16,
  #     iconAnchorY = 45
  #   ),
  #   ftb05 = makeIcon(
  #     iconUrl = "www/ftb05_icon.png",
  #     iconWidth = 29,
  #     iconHeight = 43,
  #     iconAnchorX = 14,
  #     iconAnchorY = 43
  #   ),
  #   ftb10 = makeIcon(
  #     iconUrl = "www/ftb10_icon.png",
  #     iconWidth = 32,
  #     iconHeight = 45,
  #     iconAnchorX = 16,
  #     iconAnchorY = 45
  #   )
  #   ))
  # })
  
  ###### Main tab ######
  
  ##### render leaflet ####
  
  # create a leaflet map to be rendered in the Main tab
  
  # TODO https://stackoverflow.com/questions/37862467/leaflet-legend-for-custom-markers-in-r
  
  output$map <- renderLeaflet({
    leaflet() %>%
      
      # set initial viewpoint
      
      setView(
        lng = -72.674167,
        lat = 41.7625,
        zoom = 13
      ) %>%
      
      # add Scale bar
      
      addScaleBar(
        position = "bottomright",
        options = scaleBarOptions(
          maxWidth = 100,
          metric = FALSE,
          imperial = TRUE,
          updateWhenIdle = TRUE
        )
      ) %>%
      
      # add basemap
      
      addTiles(
        group = "OpenStreetMap"
      ) %>%
      
      # add theme tiles
      
      addProviderTiles(
        "Stamen.Toner",
        group = "High Contrast"
      ) %>%
      addProviderTiles(
        "CartoDB.DarkMatter",
        group = "Dark Theme"
      ) %>%
      addProviderTiles(
        "Esri.WorldImagery",
        group = "Satellite View"
      ) %>%
      
      # Add Map panes that will control the rendering of candidates in R
      
      
      addMapPane(
        name = "lines",
        zIndex = 410
      ) %>%
      addMapPane(
        name = "popTrip",
        zIndex = 420
      ) %>%
      addMapPane(
        name = "oppZones",
        zIndex = 430
      ) %>%
      addMapPane(
        name = "h2Industry",
        zIndex = 445
      ) %>%
      addMapPane(
        name = "candidates",
        zIndex = 440
      ) %>%
      # addMapPane(
      #   name = "candidateData",
      #   zIndex = 421
      # ) %>%
      addMapPane(
        name = "h2Stations",
        zIndex = 450
      ) %>%
      addMapPane(
        name = "optiModelPane_10",
        zIndex = 460
      ) %>%
      addMapPane(
        name = "optiModelPane_05",
        zIndex = 470
      ) %>%
      addMapPane(
        name = "selectedPane",
        zIndex = 480
      ) %>%
      
      # add layer control
      
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap",
          "High Contrast",
          "Dark Theme",
          "Satellite View"
        ),
        
        options = layersControlOptions(
          collapsed = TRUE,
          autoZIndex = TRUE
        )
      ) %>%
      addPolygons(
        data = ct_oppZones_ctcc_4326_sf,
        group = "ct_oppZones",
        fillColor = "orange",
        fillOpacity = .5,
        color = "orange",
        opacity = 1,
        options = pathOptions(
          pane = "oppZones"
        )
      ) %>%
      addPolylines(
        data = ct_aadtInterstate_4326_sf,
        group = "ct_aadt_1",
        color = aadt_line_color,
        opacity = 1,
        weight = .0001 * ct_aadtInterstate_4326_sf$AADT_AADT,
        popup = ~ct_aadtInterstate_4326_sf$AADT_AADT,
        options = pathOptions(
          pane = "lines"
        )
      ) %>%
      addPolylines(
        data = ct_aadtOtherFreeway_4326_sf,
        group = "ct_aadt_2",
        color = aadt_line_color,
        opacity = 1,
        weight = .0001 * ct_aadtOtherFreeway_4326_sf$AADT_AADT,
        popup = ~ct_aadtOtherFreeway_4326_sf$AADT_AADT,
        options = pathOptions(
          pane = "lines"
        )
      ) %>%
      
      addPolylines(
        data = ct_aadtMajorArterial_4326_sf,
        group = "ct_aadt_3",
        color = aadt_line_color,
        opacity = 1,
        weight = .0001 * ct_aadtMajorArterial_4326_sf$AADT_AADT,
        popup = ~ct_aadtMajorArterial_4326_sf$AADT_AADT,
        options = pathOptions(
          pane = "lines"
        )
      ) %>%
      
      addPolylines(
        data = ct_aadtMinorArterial_4326_sf,
        group = "ct_aadt_4",
        color = aadt_line_color,
        opacity = 1,
        weight = .0001 * ct_aadtMinorArterial_4326_sf$AADT_AADT,
        popup = ct_aadtMinorArterial_4326_sf$AADT_AADT,
        options = pathOptions(
          pane = "lines"
        )
      ) %>%
      
      addPolylines(
        data = ct_aadtMajorCollector_4326_sf,
        group = "ct_aadt_5",
        color = aadt_line_color,
        opacity = 1,
        weight = .0001 * ct_aadtMajorCollector_4326_sf$AADT_AADT,
        options = pathOptions(
          pane = "lines"
        )
      ) %>% 
      
      addPolylines(
        data = ct_natGasPipes_4326_sf,
        group = "ct_natGasPipes",
        color = "purple",
        opacity = 1,
        weight = 1.5,
        options = pathOptions(
          pane = "lines"
        )
      ) %>%
      
      addMarkers(
        data = ct_candidates_4326_sf,
        group = "ct_candidates",
        layerId = ct_candidates_4326_sf$layerId,
        icon = ~markerIconList_byCategory["candidate"],
        # make this correspond to starting slider distance value
        label = lapply(stationLabels_02, htmltools::HTML),
        options = pathOptions(
          pane = "candidates"
        )
      ) %>%
      
      addMarkers(
        data = ct_optimalModelResults_4326_sf %>% filter(
          model == "frlm05"
        ),
        group = "ct_frlm05",
        layerId = ct_optimalModelResults_4326_sf,
        icon = ~markerIconList_byCategory["frlm05"],
        options = pathOptions(
          pane = "optiModelPane_05"
        )
      ) %>%
      
      addMarkers(
        data = ct_optimalModelResults_4326_sf %>% filter(
          model == "frlm10"
        ),
        group = "ct_frlm10",
        layerId = ct_optimalModelResults_4326_sf,
        icon = ~markerIconList_byCategory["frlm10"],
        options = pathOptions(
          pane = "optiModelPane_10"
        )
      ) %>%
      
      addMarkers(
        data = ct_optimalModelResults_4326_sf %>% filter(
          model == "ftb05"
        ),
        group = "ct_ftb05",
        layerId = ct_optimalModelResults_4326_sf,
        icon = ~markerIconList_byCategory["ftb05"],
        options = pathOptions(
          pane = "optiModelPane_05"
        )
      ) %>%
      
      addMarkers(
        data = ct_optimalModelResults_4326_sf %>% filter(
          model == "ftb10"
        ),
        group = "ct_ftb10",
        layerId = ct_optimalModelResults_4326_sf,
        icon = ~markerIconList_byCategory["ftb10"],
        options = pathOptions(
          pane = "optiModelPane_10"
        )
      ) %>%
      
      addMarkers(
        data = ct_optimalModelResults_4326_sf %>% filter(
          model == "pMed05"
        ),
        group = "ct_pMed05",
        layerId = ct_optimalModelResults_4326_sf,
        icon = ~markerIconList_byCategory["pMed05"],
        options = pathOptions(
          pane = "optiModelPane_05"
        )
      ) %>%
      
      addMarkers(
        data = ct_optimalModelResults_4326_sf %>% filter(
          model == "pMed10"
        ),
        group = "ct_pMed10",
        layerId = ct_optimalModelResults_4326_sf,
        icon = ~markerIconList_byCategory["pMed10"],
        options = pathOptions(
          pane = "optiModelPane_10"
        )
      ) %>%
      addCircleMarkers(
        data = hartford_demandNodes_mpo_utm18N_sf,
        group = "mpo_pop",
        fillColor = "#0000CD",
        fillOpacity = 1,
        radius = {
          sqrt(
            hartford_demandNodes_mpo_utm18N_sf$Population / max(hartford_demandNodes_mpo_utm18N_sf$Population)
          ) * 30
        },
        opacity = 0,
        options = pathOptions(
          pane = "popTrip"
        )
      ) %>%
      addCircleMarkers(
        data = hartford_demandNodes_mpo_utm18N_sf,
        group = "mpo_pop",
        fillColor = "cyan",
        fillOpacity = 1,
        radius = 1,
        opacity = 0,
        options = pathOptions(
          pane = "popTrip"
        )
      ) %>%
      addCircleMarkers(
        data = hartford_demandNodes_mpo_utm18N_sf,
        group = "mpo_trip",
        fillColor = "#00FF00",
        fillOpacity = 1,
        radius = {
          sqrt(
            hartford_demandNodes_mpo_utm18N_sf$Flow / max(hartford_demandNodes_mpo_utm18N_sf$Flow)
          ) * 30
        },
        opacity = 0,
        options = pathOptions(
          pane = "popTrip"
        )
      ) %>%
      addCircleMarkers(
        data = hartford_demandNodes_mpo_utm18N_sf,
        group = "mpo_trip",
        fillColor = "#008000",
        fillOpacity = 1,
        radius = 1,
        opacity = 0,
        options = pathOptions(
          pane = "popTrip"
        )
      ) %>%
      
      addMarkers(
        data = ct_stationsH2Public_4326_sf,
        layerId = ct_stationsH2Public_4326_sf$layerId,
        group = "ct_stationsH2",
        icon = ~markerIconList_byCategory[{
          ifelse(
            test = ct_stationsH2Public_4326_sf$`Status Code` == 'P',
            yes = "stationH2_planned",
            no = "stationH2_existing"
          )
        }],
        options = pathOptions(
          pane = "h2Stations"
        )
      ) %>%
      ##### misc layers ####
    addMarkers(
      data = ct_autoLots_4326_sf,
      group = "ct_autoLots",
      icon = ~markerIconList_byCategory["autoLots"],
      options = pathOptions(
        pane = "h2Industry"
      )
    ) %>%
      
      addMarkers(
        data = ct_h2Users_wholesale_4326_sf,
        group = "ct_wholesale",
        icon = ~markerIconList_byCategory["wholesale"],
        options = pathOptions(
          pane = "h2Industry"
        )
      ) %>%
      addMarkers(
        data = ct_h2Users_coldStorage_4326_sf,
        group = "ct_coldStorage",
        icon = ~markerIconList_byCategory["coldStorage"],
        options = pathOptions(
          pane = "h2Industry"
        )
      ) %>%
      addMarkers(
        data = ct_truckStops,
        group = "ct_truckStops",
        icon = ~markerIconList_byCategory["truckPark"],
        options = pathOptions(
          pane = "h2Industry"
        )
      ) %>%
      addMarkers(
        data = ct_h2Users_transit_4326_sf,
        group = "ct_transit",
        icon = ~markerIconList_byCategory["transit"],
        options = pathOptions(
          pane = "h2Industry"
        )
      ) %>%
      addMarkers(
        data = ct_h2Users_metalFinishing_4326_sf,
        group = "ct_metalF",
        icon = ~markerIconList_byCategory["metalF"],
        options = pathOptions(
          pane = "h2Industry"
        )
      ) %>%
      addMarkers(
        data = ct_h2Users_foodProcessing_4326_sf,
        group = "ct_foodP",
        icon = ~markerIconList_byCategory["foodP"],
        options = pathOptions(
          pane = "h2Industry"
        )
      ) %>% 
      addMarkers(
        data = ct_warehouseLocations,
        group = "ct_warehouse",
        icon = ~markerIconList_byCategory["warehouse"],
        options = pathOptions(
          pane = "h2Industry"
        )
        # ) %>% 
        # hideGroup(
        #   c(
        #     "ct_aadt_1",
        #     "ct_aadt_2",
        #     "ct_aadt_3",
        #     "ct_aadt_4",
        #     "ct_aadt_5",
        #     "ct_aadt_N",
        #     "ct_natGasPipes",
        #     "ct_candidates",
        #     "ct_frlm05",
        #     "ct_frlm10",
        #     "ct_ftb05",
        #     "ct_ftb10",
        #     "ct_pMed05",
        #     "ct_pMed10",
        #     "ct_stationsH2",
        #     "ct_foodP",
        #     "ct_warehouse",
        #     "ct_transit",
        #     "ct_metalF",
        #     "ct_truckStops",
        #     "ct_coldStorage",
        #     "ct_wholesale",
        #     "ct_autoLots",
        #     "mpo_trip",
        #     "mpo_pop",
        #     "ct_oppZones"
        #   )
      )
    
  })
  
  # Add layerToggle calls for each feature set here
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$oppZone_button,
      group = "ct_oppZones"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_transit_button,
      group = "ct_transit"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_warehouse_button,
      group = "ct_warehouse"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_metalF_button,
      group = "ct_metalF"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_foodP_button,
      group = "ct_foodP"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_wholesale_button,
      group = "ct_wholesale"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_export_button,
      group = "ct_export"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_truckP_button,
      group = "ct_truckStops"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_coldStorage_button,
      group = "ct_coldStorage"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_stationsH2_button,
      group = "ct_stationsH2"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_candidates_button,
      group = "ct_candidates"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_aadt_1_button,
      group = "ct_aadt_1"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_aadt_2_button,
      group = "ct_aadt_2"
    )
  )
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_aadt_3_button,
      group = "ct_aadt_3"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_aadt_4_button,
      group = "ct_aadt_4"
    )
  )
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_aadt_5_button,
      group = "ct_aadt_5"
    )
  )
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_05dflrm_button,
      group = "ct_frlm05"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_10dflrm_button,
      group = "ct_frlm10"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_05ftBack_button,
      group = "ct_ftb05"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_10ftBack_button,
      group = "ct_ftb10"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_05pMed_button,
      group = "ct_pMed05"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_10pMed_button,
      group = "ct_pMed10"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_natGasPipes_button,
      group = "ct_natGasPipes"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$ct_autoLots_button,
      group = "ct_autoLots"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$mpo_pop_button,
      group = "mpo_pop"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map",
      button = input$mpo_trip_button,
      group = "mpo_trip"
    )
  )
  
  # observe({
  #   checkedBoxes <- c(
  #     
  #     # these need to be in the order corresponding to their
  #     # placement in mainMap_legend_html
  #     
  #     # this will return a list of TRUE and FALSE that can
  #     # subset the list of html-formatted strings
  #     
  #     input$ct_candidates_button,
  #     input$ct_stationsH2_button,
  #     input$ct_05dflrm_button,
  #     input$ct_10dflrm_button,
  #     input$ct_05pMed_button,
  #     input$ct_10pMed_button,
  #     input$ct_05ftBack_button,
  #     input$ct_10ftBack_button
  #   )
  #   
  #   # glue the selected html-formateted strings together
  #   mainMap_legend_checked <- mainMap_legend_list[checkedBoxes] %>% glue_collapse()
  #   
  #   # pass the single html-formatted string to the map as a legend
  #   leafletProxy(
  #     "map"
  #   ) %>% removeControl(
  #     layerId = "legend"
  #   ) %>% addControl(
  #     html = mainMap_legend_checked,
  #     position = "topright",
  #     layerId = "legend"
  #   )
  # })
  # 
  
  
  observeEvent(
    eventExpr = input$metricDist,
    handlerExpr = {
      
      leafletProxy(
        mapId =  "map"
      ) %>%
        clearGroup(
          "ct_candidates"
        ) %>%
        addMarkers(
          data = ct_candidates$sf,
          group = "ct_candidates",
          layerId = ct_candidates$sf$layerId,
          icon = ~markerIconList_byCategory["candidate"],
          label = lapply(stationLabels(), htmltools::HTML),
          options = pathOptions(
            pane = "candidates"
          )
        )
    }
  )
  
  # ##### long distance visualization #####
  # 
  # longDistAccess <- eventReactive(
  #   eventExpr = input$longDistVis,
  #   valueExpr = {
  #     # Driving range needs to be in meters
  #     # so here we'll convert from miles
  #     
  #     driveRange_meters <- input$longDistVis * 1609.344
  #     
  #     selectedCoords <- ct_selected() %>% st_coordinates()
  #     
  #     selectedCoords <- selectedCoords[,-2]
  #     
  #     ldAccess <- ors_isochrones(
  #       locations = selectedCoords,
  #       range = driveRange_meters,
  #       interval = driveRange_meters / 2,
  #       range_type = "distance",
  #       output = "sf",
  #       api_key = "5b3ce3597851110001cf6248f7fce179711c4a3fa87a06c2789e310a"
  #     )
  #     
  #     return(ldAccess)
  #       
  #     
  #   }
  # )
  # 
  # observeEvent(
  #   eventExpr = input$longDistVis,
  #   handlerExpr = {
  #     if(
  #       input$longDistVis_button == TRUE
  #     ) {
  #       print("debug")
  #       print(longDistAccess())
  #       print(class(longDistAccess()))
  #       
  #     }
  #   }
  # )
  
  
  #
  # #### Add Previous GroupStage #####
  # 
  # The goal here is to make leaflet display the points that have been previously
  # selected by the groups chosen by the user in the input list.
  # 
  # create the input list
  # 
  # print("debug create prevGroup Input List")
  # 
  # prevGroups_namedList <- reactive({
  #   
  #   allGroupInfo <- allGroupInfo_db$find('{}')
  #     
  #   prevGroupNamesFormatted <- allGroupInfo %>% 
  #     filter(
  #       nrow(.) > 0
  #     ) %>% glue_data(
  #     "{groupName} {stageNum}.{paddedIterNum}",
  #     paddedIterNum = str_pad(
  #       string = iterNum,
  #       
  #       # NOTE: the padded 2 is hard-coded, we assume that we will never get to 100 iterations
  #       # in one stage.
  #       
  #       width = 2,
  #       pad = "0"
  #     )
  #   )
  #   
  #   prevGroups_listWithNames <- setNames(
  #     as.list(
  #       allGroupInfo$groupStageIterInfo
  #     ),
  #     prevGroupNamesFormatted
  #   )
  #   
  #   return(
  #     prevGroups_listWithNames
  #   )
  # })
  # 
  # print("debug prevgroup Input List created")
  # 
  # # output$previousGroupList <- renderUI({
  # #   selectizeInput(
  # #     inputId = "prevGroupsToAdd",
  # #     label = "Retrieve previously-submitted selections",
  # #     choices = prevGroups_namedList(),
  # #     multiple = TRUE,
  # #     options = list(
  # #       placeholder = "Select one or more scenarios"
  # #     )
  # #   )
  # # })
  
  nestedListOfPrevGroupsToRender_byStage <- reactive({
    
    # just to make it run on input$stageSubmit
    
    input$stageSubmit
    
    # So what we want to do is assign to allGroupInfo_byStage_List
    # lists named "Stage #" where # is the stageNum that block of 
    # elements in the list will contain
    
    # We want each list named "Stage #" to have the name "{groupName}"
    # and be composed of a list of groupStageIterInfo for each iteration,
    # with each element named "{groupName} {stageNum}_{paddedIter}
    
    allGroupInfo_byStage <- allGroupInfo() %>% split(
      
      # Split each stageNum by groupName
      
      .$stageNum,
      .$groupName
    ) %>% set_names(
      
      # set the names of the top levels to "Stage #"
      
      nm = glue(
        "Stage {sN}",
        sN = names(.)
      )
      
    ) %>% map(
      
      # in each group that we already split by stageNum and groupName, 
      # split it again by groupName
      # I don't quite know how this works but it does
      
      .f = function(x) split(
        x,
        x$groupName
      )
    ) %>% map_depth(
      
      # at depth level 2, split by "{groupName} {stageNum}.{paddedIterNum}"
      # hold groupStageIterInfo as factor
      
      .depth = 2,
      .f = function(x) {
        split( 
          x$groupStageIterInfo,
          glue(
            "{gN} {sN}.{paddedIterNum}",
            gN = x$groupName,
            sN = x$stageNum,
            paddedIterNum = str_pad(
              string = as.vector(x$iterNum),
              
              # NOTE: the padded 2 is hard-coded, we assume that we will never get to 100 iterations
              # in one stage.
              
              width = 2,
              pad = "0"
            )
          )
        )
      }
    ) 
    
    return(
      allGroupInfo_byStage
    )
  })
  
  output$previousGroupList <- renderTree({
    
    # using the same variable name as in the last environment don't @ me
    
    allGroupInfo_byStage <- nestedListOfPrevGroupsToRender_byStage()
    
    ifelse(
      test = {
        length(allGroupInfo_byStage) == 0
      },
      yes = {
        
        # if no previous groups exist, return an empty list
        return(
          list()
        )
      },
      no = {
        return(
          allGroupInfo_byStage
        )
      }
    )
    # print("debug renderTree completed")
  })
  
  observeEvent(
    eventExpr = nestedListOfPrevGroupsToRender_byStage(),
    handlerExpr = {
      
      # We're getting problems here I think.  This is updating before anything can even load.  I think that setting it to trigger only when
      # input$stageSubmit changes is ideal. Let's find out.
      
      if(!is.null(iterNum_activeSesh())){
        updateTree(
          session = session,
          treeId = "previousGroupList",
          data = req(nestedListOfPrevGroupsToRender_byStage())
        )
      }
    }
  )
  
  # This returns a formatted list of names for every groupStageIteration
  # There's possibly a way to streamline this to support the tree creation where we do the same thing
  # well it works for now
  
  # Oh right because we made the names for the tree on the fly, then after clicking them we need a way
  # to attribute that name to a groupStageIterInfo unique identifier
  
  # TODO: generate formatted names beforehand, then assign them for the tree
  
  prevGroups_all_listNamesGSII <- reactive({
    
    # get all groups 
    
    allGroupInfo <- allGroupInfo()
    
    prevGroupNamesFormatted <- allGroupInfo %>% glue_data(
      
      # format the groupName, stageNum, and iteration number (padded)
      
      "{groupName} {stageNum}.{paddedIterNum}",
      paddedIterNum = str_pad(
        string = iterNum,
        
        # NOTE: the padded to two digits is hard-coded, we assume 
        # that we will never get to 100 iterations in one stage.
        
        width = 2,
        pad = "0"
      )
    )
    
    # set the names of allGroupInfo$groupStageIterInfo to the formatted names
    
    prevGroups_listWithNames <- setNames(
      as.list(
        allGroupInfo$groupStageIterInfo
      ),
      prevGroupNamesFormatted
    )
    
    return(
      prevGroups_listWithNames
    )
  })
  
  # This fetches the selected groups in classid format
  prevGroups_selected <- reactive({
    selectedGroups <- unlist(
      get_selected(
        tree = input$previousGroupList,
        format = "classid"
      )
    )
    
    # this selects the groupStageIterInfo by name, where that name is in the list of selected groups
    
    prevGroups_selectedMatched <- prevGroups_all_listNamesGSII()[
      selectedGroups
      ]
    
    # This returns the unnamed list as a list of groupStageIterInfo selected
    
    return(
      unname(prevGroups_selectedMatched)
    )
    
  })
  
  # This generates the list of pairs to add
  
  groupStationPairsToAdd <- reactive({
    prevGroups_selected <- prevGroups_selected()
    
    # if the list of selected groups is not null:
    
    ifelse(
      test = {
        !is.null(prevGroups_selected())
      },
      yes = {
        
        # first we need to create a list of previous groups to add
        # that is a string of groupStageIteration unique identifiers
        # We'll use this to build the mongolite query as a JSON object
        
        previousGroupsToAdd_charList <- glue_collapse(
          double_quote(
            prevGroups_selected
          ),
          sep = ", "
        )
        
        # Then we construct the query for mongolite
        
        previousGroupsToAdd_query <- glue(
          
          # remember that glue() needs double brackets to represent single
          # bracket characters
          
          '{{ 
            "groupStageIterInfo" : {{ 
              "$in": [
                {selected_gSII}
              ] 
            }} 
          }}',
          
          selected_gSII = previousGroupsToAdd_charList
          
        )
        
        # Then we're using the query to call from the joining database
        # This gets us the uniqueIds for stations that were in
        # previous groups
        
        groupStationUniqueIDPairs <- groupSelected_joinId_db$find(
          query = previousGroupsToAdd_query,
          fields = '{"_id" : false}'
        ) %>% rename(
          "uniqueId" = "ct_selected_uniqueId"
        )
        
        return(
          groupStationUniqueIDPairs
        )
      },
      no = {
        return(data.frame())
      }
    )
  })
  
  # observeEvent(
  #   eventExpr = input$previousGroupList,
  #   handlerExpr = {
  #         ifelse(
  #           test = {
  #             length(prevGroups_selected()) > 0
  #           },
  #           yes = {
  # 
  #             # if any previous groups are selected, change their entries in
  #             # ct_candidates$sf from 'candidate' to 'selected'
  #             
  #             ifelse(
  #               ifelse(
  #                 ct_candidates$sf[ct_candidates$sf$uniqueId == clickedUniqueId,]$category == "candidate",
  #                 ct_candidates$sf[ct_candidates$sf$uniqueId == clickedUniqueId,]$category <- "selected",
  #                 ct_candidates$sf[ct_candidates$sf$uniqueId == clickedUniqueId,]$category <- "candidate"
  #               )
  #             )
  #           },
  #           no = {
  #             leafletProxy(
  #               mapId = "map"
  #             ) %>% clearGroup(
  #               group = "prevSelec"
  #             )
  #           }
  #         )
  #   }
  # )
  # 
  # observeEvent(
  #   input$previousGroupList,
  #   {
  # 
  #   print(groupStationPairsToAdd())
  #   print(str(groupStationPairsToAdd))
  # })
  
  # This segment is only necessary if we're including the ability to count view how many times a station was chosen
  
  
  # Now we need to construct a dataframe containing those stations
  # Along with a counter for the number of times they appear
  
  # we could summarise this thing, then join the count to the candidate
  # dataframe filtered to include only stations in that group
  
  
  
  stationsToAdd <- reactive({
    groupStationPairsToAdd_df <- groupStationPairsToAdd()
    
    groupStationPairsToAdd_df <- groupStationPairsToAdd_df %>% group_by(
      uniqueId
    ) %>% summarise(
      selecCount = n()
    ) %>% inner_join(
      ct_candidates$sf
    )  %>% mutate(
      "category" = "prevSelec",
      "layerId" = str_replace(
        string = layerId,
        pattern = "candidate|selected",
        replacement = "prevSelec"
      )
    )
    
    return(
      st_as_sf(groupStationPairsToAdd_df)
    )
  })
  
  
  
  
  # Now when someone hits input$addPrevious, add the selected stations to the map
  
  observeEvent(
    eventExpr = input$previousGroupList,
    handlerExpr = {
      
      ifelse(
        test = {
          length(prevGroups_selected()) > 0
        },
        yes = {
          
          # if any previous groups are selected, clear the existing displayed
          # groups and redisplay the new selection
          
          stationsToAddOnClick <- stationsToAdd()
          
          leafletProxy(
            mapId = "map"
          ) %>% clearGroup(
            group = "prevSelec"
          ) %>% addMarkers(
            
            data = stationsToAddOnClick,
            group = "prevSelec",
            icon = ~markerIconList_byCategory["prevSelecAdd"],
            layerId = stationsToAddOnClick$layerId,
            options = pathOptions(
              pane = "optiModelPane_05"
            )
          )
        },
        no = {
          leafletProxy(
            mapId = "map"
          ) %>% clearGroup(
            group = "prevSelec"
          )
        }
      )
    }
  )
  
  observeEvent(
    eventExpr = input$addPreviousGroups,
    handlerExpr = {
      
      if(
        length(prevGroups_selected()) > 0
      ) {
        
        # if any previous groups are selected, change their entries in
        # ct_candidates$sf from 'candidate' to 'selected'
        
        # make a vector of unique ids
        
        groupStationPairsToAdd()$uniqueId %>% unique(
        ) %>% map(
          
          # for each uniqueId:
          .f = function(x) {
            
            if(
              # if the category of the corresponding row in ct_candidates$sf is currently 'candidate'
              ct_candidates$sf[ct_candidates$sf$uniqueId == x,]$category == "candidate"
            ) {
              # make it 'selected'
              ct_candidates$sf[ct_candidates$sf$uniqueId == x,]$category <- "selected"
              
              # create a new layerId 
              ct_candidates$sf[ct_candidates$sf$uniqueId == x,]$layerId <- glue(
                "selected{uniqueId}",
                uniqueId = x
                
                # ct_selected() will update, thus triggering another observer and adding these to the selection
              )
            }
          }
        )
      }
    }
  )
  
  # clear selected groups displayed on map on submit click
  
  observeEvent(
    eventExpr = input$stageSubmit,
    handlerExpr = {
      leafletProxy(
        "map"
      )  %>% clearGroup(
        group = "prevSelec"
      )
    }
  )
  
  # print("debug Add Previous Stations Complete")
  
  
  ##### candidate selection #####
  
  ##### observe map_marker_click #####
  
  # This observeEvent function changes the category of a feature in 
  # ct_candidates from 'candidate' to 'selected' whenever its marker is clicked
  # and from 'selected' to 'candidate' whenever it is clicked a second time.
  # It also changes the layerId attribute to reflect the category change.
  
  observeEvent(
    
    # Observe for inputs that are clicks on map markers
    
    eventExpr = input$map_marker_click,
    
    handlerExpr = {
      
      # just for readability
      
      clicked <- input$map_marker_click
      
      # if the strings "candidate" or "selected" or are in the clicked point's layerId attribute
      # then execute the observeEvent code
      
      if(
        
        grepl(
          "candidate|selected|marker", 
          clicked$id
        )
        
      ) {
        
        # In the ct_candidates$sf row where layerId == clickedId, test if the category == "candidate"
        # if so, change it to "selected"
        # if not, change it to "candidate"
        
        clickedUniqueId <- str_remove_all(
          string = clicked$id,
          pattern = "candidate|selected|marker"
        )
        
        ifelse(
          ct_candidates$sf[ct_candidates$sf$uniqueId == clickedUniqueId,]$category == "candidate",
          ct_candidates$sf[ct_candidates$sf$uniqueId == clickedUniqueId,]$category <- "selected",
          ct_candidates$sf[ct_candidates$sf$uniqueId == clickedUniqueId,]$category <- "candidate"
        )
        
        # create a new layerId based on the new category value and the point's uniqueId
        
        ct_candidates$sf[ct_candidates$sf$uniqueId == clickedUniqueId,]$layerId <- glue(
          "{category}{uniqueId}",
          category = ct_candidates$sf[ct_candidates$sf$uniqueId == clickedUniqueId,]$category,
          uniqueId = ct_candidates$sf[ct_candidates$sf$uniqueId == clickedUniqueId,]$uniqueId
          
        )
      }
    }
  )
  
  # This observeEvent function updates the leaflet map whenever a candidate point is clicked
  # to reflect the selected network of stations
  
  observeEvent(
    
    # # This event triggers after the observeEvent that toggles the point's category and layerId
    # 
    # priority = 0,
    
    # This event triggers when a marker on the map is clicked
    
    eventExpr = ct_selected(),
    
    handlerExpr = {
      
      # # for readability
      # 
      # clicked <- input$map_marker_click
      # 
      # # if the strings "candidate" or "selected" are in the clicked point's layerId attribute
      # # then execute the observeEvent code
      # 
      # if(
      #   grepl(
      #     "candidate|selected|marker", 
      #     clicked$id
      #   )
      # ) {
      #   
      #   # Retrieve the uniqueId from the layerId by removing the "candidate" or "selected" characters
      #   # and assign the id to a new variable 'clickedUniqueId'
      #   
      #   # we need to do this because after the first ObserveEvent, the layerId has changed and we need to
      #   # reference the layerId of the point to remove its old marker prior to replacement.
      #   
      #   clickedUniqueId <- str_remove_all(
      #     string = clicked$id,
      #     pattern = "candidate|selected|marker"
      #   )
      #   
      #   # get the clicked candidate from ct_candidates$sf where its uniqueId is equal to the clicked point's
      #   # uniqueId
      #   
      #   clickedCandidate <- ct_candidates$sf[ct_candidates$sf$uniqueId == clickedUniqueId,]
      
      # access the leaflet map within a reactive environment
      
      leafletProxy("map") %>%
        
        # remove the existing marker for the clicked point
        clearGroup(
          group = "candidates"
        ) %>%
        addMarkers(
          data = ct_candidates$sf,
          group = "ct_candidates",
          layerId = ct_candidates$sf$layerId,
          icon = ~markerIconList_byCategory["candidate"],
          label = lapply(stationLabels(), htmltools::HTML),
          options = pathOptions(
            pane = "candidates"
          )
        ) %>%
        clearGroup(
          group = "selected"
        ) %>% addMarkers(
          data = ct_selectedMarkers(),
          group = "selected",
          layerId = ct_selectedMarkers()$layerId,
          icon = ~markerIconList_byCategory["selected"],
          options = pathOptions(
            pane = "selectedPane"
          )
        )
      
      # removeMarker(
      #   
      #   layerId = clicked$id
      #   
      # ) %>%
      # 
      # # replace it with a marker that matches its toggled state
      # 
      # addCircleMarkers(
      #   
      #   data = clickedCandidate,
      #   lng = clicked$lng,
      #   lat = clicked$lat,
      #   group = "ct_candidates",
      #   color = ~toggleSelectedMarkerColor[[category]],
      #   opacity = candidate_marker_opacity,
      #   radius = candidate_marker_radius,
      #   fill = candidate_marker_fill,
      #   fillColor = ~toggleSelectedMarkerColor[[category]],
      #   fillOpacity = candidate_marker_opacity,
      #   layerId = clickedCandidate$layerId
      #   
      # ) %>%
      # 
      # # redraw the markers for selected candidates
      # 
      # addCircleMarkers(
      #   data = ct_selected(),
      #   group = "ct_candidates",
      #   color = selected_marker_color,
      #   opacity = candidate_marker_opacity,
      #   radius = candidate_marker_radius,
      #   fill = candidate_marker_fill,
      #   fillColor = selected_marker_color,
      #   fillOpacity = candidate_marker_opacity,
      #   layerId = ct_selected()$layerId
      # )
      # }
    }
  )
  
  ##### temporary selection #####
  
  observeEvent(
    
    # TODO Make this selectable in the main tab
    
    # Sorry for not commenting more here.  Just making a group-specific database that 
    # saves the current groupStageIteration's gSI & candidate uniqueId pairs
    
    eventExpr = ct_selected(),
    handlerExpr = {
      
      ct_S <- ct_selected()
      
      gSII <- groupStageIterInfo()
      
      # Now we create a dataframe comprising the groupStageIterInfo and the
      # selected stations' uniqueId values:
      
      # repreat the string for the number of selected stations
      
      gSIInfo_nTimes <- rep( gSII, nrow(ct_S) )
      
      # join the groupStageIterInfo unique ID with the ct_selected unique ID
      # so that each row contains a unique combination of groupStageIterInfo
      # and uniqueId
      
      currentTempSelection<- data.frame(
        groupStageIterInfo = gSIInfo_nTimes,
        ct_selected_uniqueId = ct_S$uniqueId
      )
      
      tempSelected <- tempSelected_db()
      
      tempSelected$remove('{}')
      tempSelected$insert(currentTempSelection)
      
    }
  )
  
  ##### observe submit click #####
  
  observeEvent(
    eventExpr = input$stageSubmit,
    handlerExpr = {
      
      if(
        nrow(ct_selected()) > 0
      ) {
        
        # create a JSON called currentSession that contains:
        # groupStageIterInfo : uniqueId for this dataframe.  Would it
        #   be better to call this field 'uniqueId'?
        # dateCreated : the current date
        # timeCreated : the time the button is pressed
        # groupName : the name of the group
        # stageNum : the current stage number
        # iterNum : the current iteration
        # ct_selected : a vector containing the uniqueIds of the currently
        #   selected candidates
        
        # note: refer to ?glue for rules on constructing strings with glue()
        # note: we're doing this as a JSON instead of as a single-row dataframe because
        # ct_selected needs to be an array.  If there's a good way to make an
        # array an entry in a dataframe, you let me know.  Either way we get
        # the same result.
        
        gSII <- groupStageIterInfo()
        ct_S <- ct_selected()
        
        ct_S_csv <- glue_collapse(
          x = double_quote(
            ct_S$uniqueId
          ),
          sep = ", "
        )
        
        currentGroupSelection <- c(
          glue('{{
          "groupStageIterInfo" : "{gSII}",
          "dateCreated" : "{dC}",
          "timeCreated" : "{tC}",
          "groupName" : "{gN}",
          "stageNum" : "{sN}",
          "iterNum" : "{iN}",
          "ct_selected" : [{ct_S_csv}]
          }}',
               
               gSII = gSII,
               dC = Sys.Date(),
               tC = Sys.time(),
               gN = input$groupName,
               sN = input$stageNum,
               iN = iterNum(),
               ct_S_csv = ct_S_csv
          )
        )
        
        allGroupInfo_db$insert(currentGroupSelection)
        
        # Now we create a dataframe comprising the groupStageIterInfo and the
        # selected stations' uniqueId values:
        
        # repreat the string for the number of selected stations
        
        gSIInfo_nTimes <- rep( gSII, nrow(ct_S) )
        
        # join the groupStageIterInfo unique ID with the ct_selected unique ID
        # so that each row contains a unique combination of groupStageIterInfo
        # and uniqueId
        
        currentGroupSelected_joinId <- data.frame(
          groupStageIterInfo = gSIInfo_nTimes,
          ct_selected_uniqueId = ct_S$uniqueId
        )
        
        # upload it to the database
        
        groupSelected_joinId_db$insert(currentGroupSelected_joinId)
        
        # increment the iteration number
        
        newIterNum <- iterNum() + 1
        
        iterNum_activeSesh(
          iterNum() + 1
        )
      } else {
        showModal(
          modalDialog(
            title = "Invalid submission",
            "Please select at least one candidate site",
            easyClose = TRUE
          )
        )
      }
    }# ,
    # priority = 1
  )
  
  observeEvent(
    input$stageSubmit,
    {
      if(nrow(ct_selected()) > 0) {
        showModal(
          modalDialog(
            title = "Selection submitted",
            "You can now compare your network's performance to other submissions",
            easyClose = TRUE
          )
        )
      }
    },
    priority = -1
  )
  
  # clear the temporary selection
  
  observeEvent(
    eventExpr = input$stageSubmit,
    handlerExpr = {
      
      tempSelected_db()$drop()
      
    }
  )
  
  ##### Selected Station Data Tab #####
  
  # Populate the output$selectionsTable with a rendered data table
  # using the expression downloadGroupSelections() 
  
  # TODO change default ordering to stage then iteration
  # https://datatables.net/reference/option/order
  # Here's a link to change the arrow to the left
  # https://stackoverflow.com/questions/28575810/jquery-datatables-left-align-sort-icon
  # 
  # output$selectionsTable <- renderDataTable(
  #   expr = downloadGroupSelections()#, #%>% formatRound(
  #   # columns = c(
  #   #   5,6,7
  #   # ),
  #   # digits = 0
  #   # ),
  #   # options = list(
  #   #   columnDefs = list(
  #   #     list(
  #   #       className = 'dt-right',
  #   #       targets = c(1,2)
  #   #     ),
  #   #     list(
  #   #       visible = FALSE,
  #   #       targets = c(0)
  #   #     )
  #   #   ),
  #   #   pageLength = 20,
  #   #   order = list(list(1,'desc'),list(2,'desc'))
  #   # )
  # )
  # 
  # # create a reactive object called downloadGroupSelections 
  # 
  # # This section DEFINITELY falls into the fw trap of doing wayyy too much in one reactive object
  # 
  # downloadGroupSelections <- eventReactive(
  #   
  #   # I tried to make this update whenever we changed tabs to input$formTabSelections.  Probably doing it wrong but it'd be clean
  #   
  #   eventExpr = {
  #     input$stageSubmit
  #     input$metricDist
  #     ct_selected()
  #     # input$formTabSelections
  #     
  #     },
  #   valueExpr = {
  #     
  #     # select all submitted entries for the current group based on group name
  #     
  #     currentGroup_allStageIter <- allGroupInfo_db$find(
  #       query = glue(
  #         '{{
  #           "groupName" : "{gN}"
  #         }}',
  #         gN = input$groupName
  #       ),
  #       fields = '{
  #         "_id" : false,
  #         "groupStageIterInfo" : true,
  #         "stageNum" : true,
  #         "iterNum" : true,
  #         "ct_selected" : true
  #       }'
  #     ) %>% 
  #       
  #       # transpose it so that it is a list of lists
  #       # This way we can iterate through using map()
  #       
  #       transpose()
  #     
  #     # Do the same thing with currently selected, yet unsubmitted stations
  #     
  #     # unsubmitted_df <- ct_selected() %>%
  #     # mutate(
  #     #   stage = stageNum(),
  #     #   iteration = i
  #     # )
  #     
  #     # make an empty dataframe for collating all selected station information
  #     
  #     # currentGroup_allSelections <- data.frame()
  #     currentGroup_allSelections <- ct_selected() %>%
  #       mutate(
  #         stage = input$stageNum,
  #         iteration = iterNum() 
  #       ) %>% 
  #       as.data.frame(
  #       )
  #     
  # 
  #       
  #     
  #     # get the metric column names that correspond with the selected metric buffer distance
  #     
  #     tFM <- totFlowMetric()
  #     tPM <- totPopMetric()
  #     tTM <- totTripMetric()
  # 
  #     currentGroup_allStageIter %>% map(
  #       .f = function(x) {
  #         
  #         # make an empty dataframe for collating current stage_iter selected information
  #         
  #         stageIter_selectedInfo <- data.frame()
  #         
  #         # for each entry in ct_selected:
  #         
  #         x$ct_selected %>% map(
  #           .f = function(y) {
  #             
  #             # get the info that applies to the station we're looking at
  #             
  #             stationInfo <- ct_candidates$sf %>% filter(
  #               uniqueId == y
  #             ) %>% 
  #               
  #               # add stage and iteration info
  #               
  #               mutate(
  #                 stage = as.integer(x$stageNum),
  #                 iteration = as.integer(x$iterNum)
  #               ) %>%
  #               
  #               # convert it to data frame to merge without dealing with crs info
  #               # We might convert this back to sf later
  #               
  #               as.data.frame()
  #             
  #             # create a data frame entry containing that information, along with 
  #             
  #             # selectedInfo <- data.frame(
  #             #   name = stationInfo$Name,
  #             #   stage = x$stageNum,
  #             #   iteration = x$iterNum,
  #             #   uniqueId = y
  #             # )
  #             
  #             
  #             # add it to the collating data frame
  #             
  #             stageIter_selectedInfo <<- rbind(
  #               stageIter_selectedInfo,
  #               stationInfo
  #               # selectedInfo
  #             )
  #           }
  #         )
  #         
  #         # add the collated rows to the collating data frame
  #         
  #         currentGroup_allSelections <<- rbind(
  #           currentGroup_allSelections,
  #           stageIter_selectedInfo
  #         )
  #       }
  #     )
  #     
  # currentGroup_allSelections <- currentGroup_allSelections %>%
  # mutate(
  #   name = currentGroup_allSelections$Name,
  #   town = currentGroup_allSelections$municipality,
  #   `VMT served` = currentGroup_allSelections[[tFM]],
  #   `population served` = currentGroup_allSelections[[tPM]],
  #   `trips served` = currentGroup_allSelections[[tTM]]
  # ) %>% select(
  #   stage,
  #   iteration,
  #   name,
  #   town,
  #   `VMT served`,
  #   `population served`,
  #   `trips served`
  # )
  #     
  #     return(
  #       currentGroup_allSelections
  #     )
  #   }
  # )
  output$selectionsTable <- renderDataTable(
    expr = downloadGroupSelections(),
    options = list(
      columnDefs = list(
        list(
          className = 'dt-right',
          targets = c(1,2)
        ),
        list(
          visible = FALSE,
          targets = c(0)
        )
      ),
      pageLength = 20,
      order = list(list(1,'desc'),list(2,'desc'))
    )
  )
  # options = list(
  #   #   columnDefs = list(
  #   #     list(
  #   #       className = 'dt-right',
  #   #       targets = c(1,2)
  #   #     ),
  #   #     list(
  #   #       visible = FALSE,
  #   #       targets = c(0)
  #   #     )
  #   #   ),
  #   #   pageLength = 20,
  #   #   order = list(list(1,'desc'),list(2,'desc'))
  #   # )
  
  # output$selectionsTable <- renderDataTable(
  #   expr = downloadGroupSelections()#, #%>% formatRound(
  #   # columns = c(
  #   #   5,6,7
  #   # ),
  #   # digits = 0
  #   # ),
  #   # options = list(
  #   #   columnDefs = list(
  #   #     list(
  #   #       className = 'dt-right',
  #   #       targets = c(1,2)
  #   #     ),
  #   #     list(
  #   #       visible = FALSE,
  #   #       targets = c(0)
  #   #     )
  #   #   ),
  #   #   pageLength = 20,
  #   #   order = list(list(1,'desc'),list(2,'desc'))
  #   # )
  # )
  
  # create a reactive object called downloadGroupSelections 
  
  # This section DEFINITELY falls into the fw trap of doing wayyy too much in one reactive object
  
  downloadGroupSelections <- eventReactive(
    
    # I tried to make this update whenever we changed tabs to input$formTabSelections.  Probably doing it wrong but it'd be clean
    
    eventExpr = {
      input$stageSubmit
      input$metricDist
      ct_selected()
      # input$formTabSelections
      
    },
    valueExpr = {
      
      # select all submitted entries for the current group based on group name
      
      currentGroup_allStageIter <- allGroupInfo_db$find(
        query = glue(
          '{{
            "groupName" : "{gN}"
          }}',
          gN = input$groupName
        ),
        fields = '{
          "_id" : false,
          "groupStageIterInfo" : true,
          "stageNum" : true,
          "iterNum" : true,
          "ct_selected" : true
        }'
      ) %>% 
        
        # transpose it so that it is a list of lists
        # This way we can iterate through using map()
        
        transpose()
      
      # Do the same thing with currently selected, yet unsubmitted stations
      
      # unsubmitted_df <- ct_selected() %>%
      # mutate(
      #   stage = stageNum(),
      #   iteration = i
      # )
      
      # make an empty dataframe for collating all selected station information
      
      # currentGroup_allSelections <- data.frame()
      currentGroup_allSelections <- ct_selected() %>%
        mutate(
          stage = input$stageNum,
          iteration = iterNum()
        ) %>% as.data.frame()
      
      # get the metric column names that correspond with the selected metric buffer distance
      
      tFM <- totFlowMetric()
      tPM <- totPopMetric()
      tTM <- totTripMetric()
      
      currentGroup_allStageIter %>% map(
        .f = function(x) {
          
          # make an empty dataframe for collating current stage_iter selected information
          
          stageIter_selectedInfo <- data.frame()
          
          # for each entry in ct_selected:
          
          x$ct_selected %>% map(
            .f = function(y) {
              
              # get the info that applies to the station we're looking at
              
              stationInfo <- ct_candidates$sf %>% filter(
                uniqueId == y
              ) %>% 
                
                # add stage and iteration info
                
                mutate(
                  stage = as.integer(x$stageNum),
                  iteration = as.integer(x$iterNum)
                ) %>%
                
                # convert it to data frame to merge without dealing with crs info
                # We might convert this back to sf later
                
                as.data.frame()
              
              # create a data frame entry containing that information, along with 
              
              # selectedInfo <- data.frame(
              #   name = stationInfo$Name,
              #   stage = x$stageNum,
              #   iteration = x$iterNum,
              #   uniqueId = y
              # )
              
              
              # add it to the collating data frame
              
              stageIter_selectedInfo <<- rbind(
                stageIter_selectedInfo,
                stationInfo
                # selectedInfo
              )
            }
          )
          
          # add the collated rows to the collating data frame
          
          currentGroup_allSelections <<- rbind(
            currentGroup_allSelections,
            stageIter_selectedInfo
          )
        }
      )
      
      currentGroup_allSelections <- currentGroup_allSelections %>%
        mutate(
          name = currentGroup_allSelections$Name,
          town = currentGroup_allSelections$municipality,
          `VMT served` = currentGroup_allSelections[[tFM]],
          `population served` = currentGroup_allSelections[[tPM]],
          `trips served` = currentGroup_allSelections[[tTM]]
        ) %>% select(
          stage,
          iteration,
          name,
          town,
          `VMT served`,
          `population served`,
          `trips served`
        )
      
      
      return(
        
        currentGroup_allSelections
      )
    }
  )
  
  # download a .csv containing the table info whenever the download button is clicked
  
  output$downloadGroupSelectionData <- downloadHandler(
    filename = function() {
      glue(
        "{gN}_selections.csv",
        gN = input$groupName
      )
    },
    content = function(file) {
      write.csv(
        downloadGroupSelections(),
        file
      )
    }
  )
  
  
  ##### Iteration Performance Metrics #####
  
  # I'm falling into the Fangwu trap.  Need to keep it limited to one distinct thing per
  # reactive event
  
  output$measuresTable <- DT::renderDataTable(
    expr = downloadGroupMeasures(),
    options = list(
      columnDefs = list(
        list(visible = FALSE, targets = c(0))                
      ),
      pageLength = 20,
      order = list(list(1,'desc'))
    )
  )
  
  downloadGroupMeasures <- eventReactive(
    eventExpr = {
      input$stageSubmit
      input$metricDist
      ct_selected()
    },
    valueExpr = {
      
      # we need to create a data frame that has rows for each iteration in current group
      # each row has info for:
      # stage
      # iteration
      # number of stations in that iteration
      # some summarized stats about the iteration
      
      # get the metric column names that correspond with the selected metric buffer distance
      
      tFM <- totFlowMetric()
      tPM <- totPopMetric()
      tTM <- totTripMetric()
      
      # create entries for the H2 stations in Connecticut
      ct_stationsH2_metrics <- ct_stationsH2Public_4326_sf %>%
        filter(
          State == "CT"
        ) %>%
        mutate(
          VMT = .[[tFM]],
          population = .[[tPM]],
          trips = .[[tTM]]
        )
      
      # create an entry for the current selection:
      currentGroup_currentSelections <- ct_selected() %>% 
        as.data.frame(
        ) %>%
        mutate(
          groupStageIterInfo = groupStageIterInfo(),
          iteration = as.integer(iterNum()),
          VMT = .[[tFM]],
          population = .[[tPM]],
          trips = .[[tTM]]
        ) %>% 
        group_by(
          groupStageIterInfo
        ) %>%
        summarise(
          iteration = mean(iteration),
          `total VMT served` = trunc(sum(VMT)+sum(ct_stationsH2_metrics$VMT)),
          # We need a custom expression to add the two existing stations
          `avg. VMT served` = trunc((sum(VMT)+sum(ct_stationsH2_metrics$VMT)) / (n() + nrow(ct_stationsH2_metrics))),
          `total pop. served` = trunc(sum(population)+sum(ct_stationsH2_metrics$population)),
          `avg. pop. served` = trunc((sum(population)+sum(ct_stationsH2_metrics$population)) / (n() + nrow(ct_stationsH2_metrics))),
          `total trips served` = trunc(sum(trips)+sum(ct_stationsH2_metrics$trips)),
          `avg. trips served` = trunc((sum(trips)+sum(ct_stationsH2_metrics$trips)) / (n() + nrow(ct_stationsH2_metrics))),
          `number of stations` = n() + nrow(ct_stationsH2_metrics)
        ) 
      
      
      
      # select all submitted entries for the current group based on group name
      
      currentGroup_allStageIter <- allGroupInfo_db$find(
        query = glue(
          '{{
            "groupName" : "{gN}",
            "stageNum" : "{sN}"
          }}',
          gN = input$groupName,
          sN = input$stageNum
        ),
        fields = '{
          "_id" : false,
          "groupStageIterInfo" : true,
          "stageNum" : true,
          "iterNum" : true,
          "ct_selected" : true
        }'
      ) 
      
      # create a query for selecting all station uniqueIds where they have been selected
      # in any iteration by the current group
      if(
        nrow(currentGroup_allStageIter) > 0
      ){
        currentGroup_allSelec_query <- mongo_buildInListQuery(
          listToQuery = currentGroup_allStageIter$groupStageIterInfo,
          field = "groupStageIterInfo"
        )
        
        
        
        # select all station uniqueIds where they've been selected
        # in any iteration by the current group & stage
        
        measures_currentGroupAllStage <- groupSelected_joinId_db$find(
          query = currentGroup_allSelec_query,
          fields = '{"_id" : false}'
        ) %>% rename(
          "uniqueId" = "ct_selected_uniqueId"
        ) %>% inner_join(
          ct_candidates$sf
        ) %>% inner_join(
          currentGroup_allStageIter,
          "groupStageIterInfo"
        ) %>% mutate(
          VMT = .[[tFM]],
          population = .[[tPM]],
          trips = .[[tTM]],
          iteration = as.integer(iterNum)
        ) %>% group_by(
          groupStageIterInfo
        )  %>% summarise(
          # This is mean because summarise needs a single value, not a vector of the same value
          iteration = as.integer(mean(iteration)),
          `total VMT served` = trunc(sum(VMT)+sum(ct_stationsH2_metrics$VMT)),
          # We need a custom expression to add the two existing stations
          `avg. VMT served` = trunc((sum(VMT)+sum(ct_stationsH2_metrics$VMT)) / (n() + nrow(ct_stationsH2_metrics))),
          `total pop. served` = trunc(sum(population)+sum(ct_stationsH2_metrics$population)),
          `avg. pop. served` = trunc((sum(population)+sum(ct_stationsH2_metrics$population)) / (n() + nrow(ct_stationsH2_metrics))),
          `total trips served` = trunc(sum(trips)+sum(ct_stationsH2_metrics$trips)),
          `avg. trips served` = trunc((sum(trips)+sum(ct_stationsH2_metrics$trips)) / (n() + nrow(ct_stationsH2_metrics))),
          `number of stations` = n() + nrow(ct_stationsH2_metrics)
        ) %>% rbind(
          currentGroup_currentSelections
        ) %>% select(
          -"groupStageIterInfo"
        )
      } else {
        measures_currentGroupAllStage <- currentGroup_currentSelections %>% select(
          -"groupStageIterInfo"
        )
      }
      return(
        measures_currentGroupAllStage
      )
    }
  )
  
  output$downloadGroupMeasureData <- downloadHandler(
    filename = function() {
      glue(
        "{gN}_stage{sN}.png",
        gN = input$groupName,
        sN = input$stageNum
      )
    },
    content = function(file) {
      write.csv(
        downloadGroupMeasures(),
        file
      )
    }
  )
  
  ##### comparing to other groups #####
  
  
  
  ###### This is all the code needed to create a nested list of groups to render in a shinyTree output
  # and return the selections as a list
  
  # TODO https://stackoverflow.com/questions/55258868/shinytree-with-default-selected-value
  
  nestedListOfPrevGroupsToRender_byGroup <- reactive({
    
    # just to make it run on input$stageSubmit
    
    input$stageSubmit
    
    # So what we want to do is assign to allGroupInfo_byStage_List
    # lists named "Stage #" where # is the stageNum that block of 
    # elements in the list will contain
    
    # We want each list named "Stage #" to have the name "{groupName}"
    # and be composed of a list of groupStageIterInfo for each iteration,
    # with each element named "{groupName} {stageNum}_{paddedIter}
    
    allGroupInfo_byGroup <- allGroupInfo() %>% split(
      
      # Split each groupName by stageNum
      
      .$groupName,
      .$stageNum
    ) %>% set_names(
      
      # set the names of the top levels to "Stage #"
      
      nm = names(.)
      
    ) %>% map(
      
      # in each group that we already split by stageNum and groupName, 
      # split it again by groupName
      # I don't quite know how this works but it does
      
      .f = function(x) split(
        x,
        x$stageNum
      )
    ) %>% map_depth(
      
      # at depth level 2, split by "{groupName} {stageNum}.{paddedIterNum}"
      # hold groupStageIterInfo as factor
      
      .depth = 2,
      .f = function(x) {
        split( 
          x$groupStageIterInfo,
          glue(
            "{gN} {sN}.{paddedIterNum}",
            gN = x$groupName,
            sN = x$stageNum,
            paddedIterNum = str_pad(
              string = as.vector(x$iterNum),
              
              # NOTE: the padded 2 is hard-coded, we assume that we will never get to 100 iterations
              # in one stage.
              
              width = 2,
              pad = "0"
            )
          )
        )
      }
    ) 
    
    return(
      allGroupInfo_byGroup
    )
  })
  
  
  # this is a horrible variable name for this tree and input / output object
  # TODO: change it
  
  output$groupstagePlot <- renderTree({
    
    # using the same variable name as in the last environment don't @ me
    
    allGroupInfo_byGroup <- nestedListOfPrevGroupsToRender_byGroup()
    
    ifelse(
      test = {
        length(allGroupInfo_byGroup) == 0
      },
      yes = {
        
        # if no previous groups exist, return an empty list
        return(list())
      },
      no = {
        
        return(
          allGroupInfo_byGroup
        )
      }
    )
  })
  
  observeEvent(
    eventExpr = nestedListOfPrevGroupsToRender_byGroup(),
    handlerExpr = {
      
      # we're using iterNum_activeSesh as a marker to prevent the updateTree from running at initialization
      
      if(!is.null(iterNum_activeSesh())) {
        updateTree(
          session = session,
          treeId = "groupstagePlot",
          data = nestedListOfPrevGroupsToRender_byGroup()
        )
      }
    }
  )
  
  # observeEvent(
  #   eventExpr = input$stageSubmit,
  #   handlerExpr = {
  #     updateTree(
  #       session = ,
  #       treeId = "groupstagePlot",
  #       data = {
  #         
  #       }
  #     )
  #   }
  # )
  
  # This fetches the selected groups in classid format
  groupstagePlot_selected <- reactive({
    selectedGroups <- unlist(
      get_selected(
        tree = input$groupstagePlot,
        format = "classid"
      )
    )
    
    # this selects the groupStageIterInfo by name, where that name is in the list of selected groups
    # NOTE: we're reusing the prevGroups_all_listNamesGSII reactive object here
    
    groupstagePlot_selectedMatched <- prevGroups_all_listNamesGSII()[
      selectedGroups
      ] %>% unname() 
    
    # This returns the unnamed list as a list of groupStageIterInfo selected
    # Though for some reason it's also giving me two "NULL" entries so let's get rid of those...
    
    return(
      Filter(
        Negate(
          is.null
        ),
        groupstagePlot_selectedMatched
      )
    )
    
  })
  
  ################# End of things that manage shiny tree rendering and output 
  
  measures_allGroupAllStage_table <- eventReactive(
    eventExpr = {
      input$groupstagePlot
      totFlowMetric()
      totPopMetric()
      totTripMetric()
    },
    valueExpr = {
      # this needs to return a data table with stats for each group and stage
      
      # if any groups are selected
      
      if(!{groupstagePlot_selected() %>% is_empty()}){
        
        # Get the column names that correspond to the user-selected
        # buffer distances
        
        tFM <- totFlowMetric()
        tPM <- totPopMetric()
        tTM <- totTripMetric()
        
        # create entries for the H2 stations in Connecticut
        ct_stationsH2_metrics <- ct_stationsH2Public_4326_sf %>%
          filter(
            State == "CT"
          ) %>%
          mutate(
            VMT = .[[tFM]],
            population = .[[tPM]],
            trips = .[[tTM]]
          )
        
        # create a query for selecting all station uniqueIds where they have been selected
        # in any iteration by the current group
        
        allGroup_allSelec_query <- mongo_buildInListQuery(
          listToQuery = groupstagePlot_selected(),
          field = "groupStageIterInfo"
        )
        
        # get groupName, stageNum, iterNum for the selected groups
        
        selectedGroups_gSI <- allGroupInfo_db$find(
          query = allGroup_allSelec_query,
          fields = '{"_id" : false}'
        )
        
        # select all station uniqueIds where they've been selected
        # in any iteration by the selected groups
        
        # it would be good to give them the formatted name as well
        
        measures_allGroupAllStage <- groupSelected_joinId_db$find(
          query = allGroup_allSelec_query,
          fields = '{"_id" : false}'
        ) %>% rename(
          "uniqueId" = "ct_selected_uniqueId"
        ) %>% inner_join(
          
          # add station data
          
          ct_candidates$sf
        ) %>% inner_join(
          # add group, stage, iteration data
          selectedGroups_gSI
        ) %>% mutate(
          # groupStageIterInfo = glue(
          #   "{gN} {sN}.{paddedIterNum}",
          #   gN = groupName,
          #   sN = stageNum,
          #   paddedIterNum = str_pad(
          #     string = iterNum,
          # 
          #     # NOTE: the padded 2 is hard-coded, we assume that we will never get to 100 iterations
          #     # in one stage.
          # 
          #     width = 2,
          #     pad = "0"
          #   )
          # ),
          totFlowXXmi = .[[tFM]],
          totPopXXmi = .[[tPM]],
          totTripXXmi = .[[tTM]]
        ) %>% group_by(
          
          # group em by groupStageIterInfo
          groupStageIterInfo
        )  %>% summarise(
          
          # add summary stats
          group = first(groupName),
          stage = mean(as.integer(stageNum)),
          iteration = mean(as.integer(iterNum)),
          # totFlow = sum(totFlowXXmi)+sum(ct_stationsH2_metrics$VMT),
          # avgFlow = (sum(totFlowXXmi)+sum(ct_stationsH2_metrics$population))/(n()+nrow(ct_stationsH2_metrics)),
          # totPop = sum(totPopXXmi)+sum(ct_stationsH2_metrics$population),
          # avgPop = (sum(totPopXXmi)+sum(ct_stationsH2_metrics$population))/(n()+nrow(ct_stationsH2_metrics)),
          # totTrip = sum(totTripXXmi)+sum(ct_stationsH2_metrics$trips),
          # avgTrip = (sum(totTripXXmi)+sum(ct_stationsH2_metrics$population))/(n()+nrow(ct_stationsH2_metrics)),
          # stationCount = n()+nrow(ct_stationsH2_metrics)
          `total VMT served` = trunc(sum(totFlowXXmi)+sum(ct_stationsH2_metrics$VMT)),
          # We need a custom expression to add the two existing stations
          `avg. VMT served` = trunc((sum(totFlowXXmi)+sum(ct_stationsH2_metrics$VMT)) / (n() + nrow(ct_stationsH2_metrics))),
          `total pop. served` = trunc(sum(totPopXXmi)+sum(ct_stationsH2_metrics$population)),
          `avg. pop. served` = trunc((sum(totPopXXmi)+sum(ct_stationsH2_metrics$population)) / (n() + nrow(ct_stationsH2_metrics))),
          `total trips served` = trunc(sum(totTripXXmi)+sum(ct_stationsH2_metrics$trips)),
          `avg. trips served` = trunc((sum(totTripXXmi)+sum(ct_stationsH2_metrics$trips)) / (n() + nrow(ct_stationsH2_metrics))),
          `number of stations` = n() + nrow(ct_stationsH2_metrics)
        )
        
        return(
          measures_allGroupAllStage
        )
      }
    }
  )
  
  # create the data table
  
  # TODO https://rstudio.github.io/DT/options.html
  
  output$allMeasuresTable <- DT::renderDataTable(
    
    expr = measures_allGroupAllStage_table(),
    options = list(
      columnDefs = list(
        list(visible = FALSE, targets = c(0,1))                
      ),
      pageLength = 20,
      order = list(list(3,'desc'),list(4,'desc'),list(2,'asc'))
    )
    #   data = measures_allGroupAllStage_table() # %>%
    #     # formatSignif(
    #     #   columns = c(
    #     #     "totFlow",
    #     #     "avgFlow",
    #     #     "totPop",
    #     #     "avgPop",
    #     #     "totTrip",
    #     #     "avgTrip"
    #     #   ),
    #     #   digits = 3
    #     # )
    # )
  )
  
  output$downloadAllGroupMeasureData <- downloadHandler(
    filename = function() {
      paste(
        "Group_Measures.csv",
        sep=""
      )
    },
    content = function(file) {
      write.csv(
        measures_allGroupAllStage_table(),
        file
      )
    }
  )
  
  # observe({
  #   print("debug01")
  #   print(head(measures_allGroupAllStage_table()))
  #   print(names(measures_allGroupAllStage_table()))
  #   print(make.names(names(measures_allGroupAllStage_table())))
  #   print("debug02")
  #   print(xAxis_vars)
  #   print(names(xAxis_vars))
  #   print("debug03")
  #   print(yAxis_vars)
  #   print(names(yAxis_vars))
  # })
  
  measuresComparisonPlot <- eventReactive(
    eventExpr = {
      input$xVarPlot
      input$yVarPlot
      measures_allGroupAllStage_table()
    },
    valueExpr = {
      
      m_aGaS <- measures_allGroupAllStage_table()
      
      # this is a pretty hacky way to get around ggplot not being
      # able to take column names with spaces enclosed in facets
      # example: `example column name`
      # make.names changes spaces to periods
      # The results must match the values of xAxis_vars and yAxis_vars
      
      names(m_aGaS) <- make.names(names(m_aGaS))
      
      # if any groups are selected:
      
      if(!{groupstagePlot_selected() %>% is_empty()}){
        
        # setting axes labels
        xvar_name <- names(xAxis_vars)[xAxis_vars == input$xVarPlot]
        yvar_name <- names(yAxis_vars)[yAxis_vars == input$yVarPlot]
        
        # but since the inputs are strings, we need to do a little more work.
        xvar <- as.symbol(input$xVarPlot)
        yvar <- as.symbol(input$yVarPlot)
        
        p <- ifelse(
          test = {
            input$xVarPlot == "groupStageIterInfo"
          },
          
          # if users select "group_stage" as the x-axis, render a bar plot
          
          yes = {
            return(
              ggplot(
                data = m_aGaS,
                mapping = aes_string(
                  x = input$xVarPlot,
                  y = input$yVarPlot
                )
              ) + geom_bar(
                stat = "identity",
                width = 0.1,
                aes(
                  fill = groupStageIterInfo
                )
              ) + labs(
                x = "Group and stage",
                y = yvar_name
              ) + ggtitle(
                glue("Group comparison of {yvar_name}"),
                subtitle = glue(
                  "Buffer distance = {dist} mi",
                  dist = input$metricDist
                )
              ) + scale_fill_hue(
                "Groups and Stages"
              ) + theme(
                legend.text = element_text(
                  size = 14
                ),
                legend.title = element_text(
                  size = 18,
                  margin = margin(
                    0,
                    0,
                    20,
                    0
                  )
                ),
                axis.text = element_text(
                  size = 14
                ),
                axis.title = element_text(
                  size = 18
                ),
                plot.title = element_text(
                  size = 22,
                  face = "bold",
                  margin = margin(
                    0,
                    0,
                    20,
                    0
                  )
                ),
                axis.title.x = element_text(
                  margin = margin(
                    20,
                    0,
                    0,
                    0
                  )
                ),
                axis.title.y = element_text(
                  margin = margin(
                    20,
                    0,
                    0,
                    0
                  )
                )
              )
            )},
          no = {
            return(
              # if users select anything else as the x-axis, render a scatterplot
              ggplot(
                data = m_aGaS,
                mapping = aes_string(
                  x = input$xVarPlot,
                  y = input$yVarPlot
                )
              ) + geom_point(
                size = 8,
                shape = 19,
                aes(
                  color = groupStageIterInfo
                )
              ) + labs(
                x = xvar_name,
                y = yvar_name
              ) + ggtitle(
                glue(
                  "{xAxis} vs {yAxis}",
                  xAxis = xvar_name,
                  yAxis = yvar_name
                ),
                subtitle = glue(
                  "Buffer distance = {dist} mi",
                  dist = input$metricDist
                )
              ) + scale_color_hue(
                "Groups and Stages"
              ) + geom_text(
                aes(
                  label = groupStageIterInfo
                ),
                check_overlap = FALSE,
                hjust = 0.5,
                vjust = -1.25,
                size = 5,
                color = "black"
              ) + xlim(
                0.95 * min(
                  m_aGaS[,input$xVarPlot]
                ),
                0.05 * min(
                  m_aGaS[,input$xVarPlot]
                ) + max(
                  m_aGaS[,input$xVarPlot]
                )
              ) + ylim(
                min(
                  m_aGaS[,input$yVarPlot]
                ),
                0.05 * min(
                  m_aGaS[,input$yVarPlot]
                ) + max(
                  m_aGaS[,input$yVarPlot]
                )
              ) + theme(
                legend.text = element_text(
                  size = 14
                ),
                legend.title = element_text(
                  size = 18, 
                  margin = margin(
                    0,
                    0,
                    20,
                    0
                  )
                ),
                axis.text = element_text(
                  size = 14
                ),
                axis.title = element_text(
                  size = 18
                ),
                plot.title = element_text(
                  size = 22,
                  face = "bold",
                  margin = margin(
                    0,
                    0,
                    20,
                    0
                  )
                ),
                axis.title.x = element_text(
                  margin = margin(
                    20,
                    0,
                    0,
                    0
                  )
                ),
                axis.title.y = element_text(
                  margin = margin(
                    0,
                    20,
                    0,
                    0
                  )
                )
              )
            )}
        )
        return(p)
      }
    }
  )
  
  output$groupComparison_plot <- renderPlot({
    # this has to be print() for some reason
    print(measuresComparisonPlot())
  })
  
  # download the existing plot as a .png
  
  output$downloadPlot <- downloadHandler(
    filename = "comparisonPlot.png",
    content = function(file) {
      png(file,width = 1024, height = 850, units = "px")
      print(measuresComparisonPlot())
      dev.off()
    }
  )
  
  ##### Spatial Group Comparison #####
  
  output$map_compare <- renderLeaflet({
    leaflet() %>%
      
      # set initial viewpoint
      
      setView(
        lng = -72.674167,
        lat = 41.7625,
        zoom = 13
      ) %>%
      
      # add Scale bar
      
      addScaleBar(
        position = "bottomright",
        options = scaleBarOptions(
          maxWidth = 100,
          metric = FALSE,
          imperial = TRUE,
          updateWhenIdle = TRUE
        )
      ) %>%
      
      # add basemap
      
      addTiles(
        group = "OpenStreetMap"
      ) %>%
      
      # add theme tiles
      
      addProviderTiles(
        "Stamen.Toner",
        group = "High Contrast"
      ) %>%
      addProviderTiles(
        "CartoDB.DarkMatter",
        group = "Dark Theme"
      ) %>%
      addProviderTiles(
        "Esri.WorldImagery",
        group = "Satellite View"
      ) %>%
      
      # Add Map panes that will control the rendering of candidates in R
      
      
      addMapPane(
        name = "lines_spC",
        zIndex = 410
      ) %>%
      addMapPane(
        name = "popTrip_spC",
        zIndex = 420
      ) %>%
      addMapPane(
        name = "oppZones_spC",
        zIndex = 430
      ) %>%
      addMapPane(
        name = "h2Industry_spC",
        zIndex = 445
      ) %>%
      addMapPane(
        name = "candidates_spC",
        zIndex = 440
      ) %>% 
      # addMapPane(
      #   name = "candidateData_spC",
      #   zIndex = 421
      # ) %>%
      addMapPane(
        name = "h2Stations_spC",
        zIndex = 450
      ) %>%
      addMapPane(
        name = "optiModelPane_10_spC",
        zIndex = 460
      ) %>%
      addMapPane(
        name = "optiModelPane_05_spC",
        zIndex = 470
      ) %>%
      addMapPane(
        name = "selectedPane_spC",
        zIndex = 480
      ) %>%
      
      # add layer control
      
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap",
          "High Contrast",
          "Dark Theme",
          "Satellite View"
        ),
        
        options = layersControlOptions(
          collapsed = TRUE,
          autoZIndex = TRUE
        )
      ) %>%
      addPolygons(
        data = ct_oppZones_ctcc_4326_sf,
        group = "ct_oppZones_spC",
        fillColor = "orange",
        fillOpacity = .5,
        color = "orange",
        opacity = 1,
        options = pathOptions(
          pane = "oppZones_spC"
        )
      ) %>%
      addPolylines(
        data = ct_aadtInterstate_4326_sf,
        group = "ct_aadt_1_spC",
        color = aadt_line_color,
        opacity = 1,
        weight = .0001 * ct_aadtInterstate_4326_sf$AADT_AADT,
        popup = ~ct_aadtInterstate_4326_sf$AADT_AADT,
        options = pathOptions(
          pane = "lines_spC"
        )
      ) %>%
      addPolylines(
        data = ct_aadtOtherFreeway_4326_sf,
        group = "ct_aadt_2_spC",
        color = aadt_line_color,
        opacity = 1,
        weight = .0001 * ct_aadtOtherFreeway_4326_sf$AADT_AADT,
        popup = ~ct_aadtOtherFreeway_4326_sf$AADT_AADT,
        options = pathOptions(
          pane = "lines_spC"
        )
      ) %>%
      
      addPolylines(
        data = ct_aadtMajorArterial_4326_sf,
        group = "ct_aadt_3_spC",
        color = aadt_line_color,
        opacity = 1,
        weight = .0001 * ct_aadtMajorArterial_4326_sf$AADT_AADT,
        popup = ~ct_aadtMajorArterial_4326_sf$AADT_AADT,
        options = pathOptions(
          pane = "lines_spC"
        )
      ) %>%
      
      addPolylines(
        data = ct_aadtMinorArterial_4326_sf,
        group = "ct_aadt_4_spC",
        color = aadt_line_color,
        opacity = 1,
        weight = .0001 * ct_aadtMinorArterial_4326_sf$AADT_AADT,
        popup = ct_aadtMinorArterial_4326_sf$AADT_AADT,
        options = pathOptions(
          pane = "lines_spC"
        )
      ) %>%
      
      addPolylines(
        data = ct_aadtMajorCollector_4326_sf,
        group = "ct_aadt_5_spC",
        color = aadt_line_color,
        opacity = 1,
        weight = .0001 * ct_aadtMajorCollector_4326_sf$AADT_AADT,
        options = pathOptions(
          pane = "lines_spC"
        )
        
      ) %>% addPolylines(
        data = ct_natGasPipes_4326_sf,
        group = "ct_natGasPipes_spC",
        color = "purple",
        opacity = 1,
        weight = 1.5,
        options = pathOptions(
          pane = "lines_spC"
        )
      ) %>%
      
      addMarkers(
        data = ct_candidates_4326_sf,
        group = "ct_candidates_spC",
        layerId = ct_candidates_4326_sf$layerId,
        icon = ~markerIconList_byCategory["candidate"],
        # make this correspond to starting slider distance value
        label = lapply(stationLabels_02, htmltools::HTML),
        options = pathOptions(
          pane = "candidates_spC"
        )
      ) %>%
      
      addMarkers(
        data = ct_optimalModelResults_4326_sf %>% filter(
          model == "frlm05"
        ),
        group = "ct_frlm05_spC",
        layerId = ct_optimalModelResults_4326_sf,
        icon = ~markerIconList_byCategory["frlm05"],
        options = pathOptions(
          pane = "optiModelPane_05_spC"
        )
      ) %>%
      
      addMarkers(
        data = ct_optimalModelResults_4326_sf %>% filter(
          model == "frlm10"
        ),
        group = "ct_frlm10_spC",
        layerId = ct_optimalModelResults_4326_sf,
        icon = ~markerIconList_byCategory["frlm10"],
        options = pathOptions(
          pane = "optiModelPane_10_spC"
        )
      ) %>%
      
      addMarkers(
        data = ct_optimalModelResults_4326_sf %>% filter(
          model == "ftb05"
        ),
        group = "ct_ftb05_spC",
        layerId = ct_optimalModelResults_4326_sf,
        icon = ~markerIconList_byCategory["ftb05"],
        options = pathOptions(
          pane = "optiModelPane_05_spC"
        )
      ) %>%
      
      addMarkers(
        data = ct_optimalModelResults_4326_sf %>% filter(
          model == "ftb10"
        ),
        group = "ct_ftb10_spC",
        layerId = ct_optimalModelResults_4326_sf,
        icon = ~markerIconList_byCategory["ftb10"],
        options = pathOptions(
          pane = "optiModelPane_10_spC"
        )
      ) %>%
      
      addMarkers(
        data = ct_optimalModelResults_4326_sf %>% filter(
          model == "pMed05"
        ),
        group = "ct_pMed05_spC",
        layerId = ct_optimalModelResults_4326_sf,
        icon = ~markerIconList_byCategory["pMed05"],
        options = pathOptions(
          pane = "optiModelPane_05_spC"
        )
      ) %>%
      
      addMarkers(
        data = ct_optimalModelResults_4326_sf %>% filter(
          model == "pMed10"
        ),
        group = "ct_pMed10_spC",
        layerId = ct_optimalModelResults_4326_sf,
        icon = ~markerIconList_byCategory["pMed10"],
        options = pathOptions(
          pane = "optiModelPane_10_spC"
        )
      ) %>%
      addCircleMarkers(
        data = hartford_demandNodes_mpo_utm18N_sf,
        group = "mpo_pop_spC",
        fillColor = "#0000CD",
        fillOpacity = 1,
        radius = {
          sqrt(
            hartford_demandNodes_mpo_utm18N_sf$Population / max(hartford_demandNodes_mpo_utm18N_sf$Population)
          ) * 30
        },
        opacity = 0,
        options = pathOptions(
          pane = "popTrip_spC"
        )
      ) %>%
      addCircleMarkers(
        data = hartford_demandNodes_mpo_utm18N_sf,
        group = "mpo_pop_spC",
        fillColor = "cyan",
        fillOpacity = 1,
        radius = 1,
        opacity = 0,
        options = pathOptions(
          pane = "popTrip_spC"
        )
      ) %>%
      addCircleMarkers(
        data = hartford_demandNodes_mpo_utm18N_sf,
        group = "mpo_trip_spC",
        fillColor = "#00FF00",
        fillOpacity = 1,
        radius = {
          sqrt(
            hartford_demandNodes_mpo_utm18N_sf$Flow / max(hartford_demandNodes_mpo_utm18N_sf$Flow)
          ) * 30
        },
        opacity = 0,
        options = pathOptions(
          pane = "popTrip_spC"
        )
      ) %>%
      addCircleMarkers(
        data = hartford_demandNodes_mpo_utm18N_sf,
        group = "mpo_trip_spC",
        fillColor = "#008000",
        fillOpacity = 1,
        radius = 1,
        opacity = 0,
        options = pathOptions(
          pane = "popTrip_spC"
        )
      ) %>%
      
      addMarkers(
        data = ct_stationsH2Public_4326_sf,
        layerId = ct_stationsH2Public_4326_sf$layerId,
        group = "ct_stationsH2_spC",
        icon = ~markerIconList_byCategory[{
          ifelse(
            test = ct_stationsH2Public_4326_sf$`Status Code` == 'P',
            yes = "stationH2_planned",
            no = "stationH2_existing"
          )
        }],
        options = pathOptions(
          pane = "h2Stations_spC"
        )
      ) %>%
      ##### misc layers ####
    addMarkers(
      data = ct_autoLots_4326_sf,
      group = "ct_autoLots_spC",
      icon = ~markerIconList_byCategory["autoLots"],
      options = pathOptions(
        pane = "h2Industry_spC"
      )
    ) %>%
      
      addMarkers(
        data = ct_h2Users_wholesale_4326_sf,
        group = "ct_wholesale_spC",
        icon = ~markerIconList_byCategory["wholesale"],
        options = pathOptions(
          pane = "h2Industry_spC"
        )
      ) %>%
      addMarkers(
        data = ct_h2Users_coldStorage_4326_sf,
        group = "ct_coldStorage_spC",
        icon = ~markerIconList_byCategory["coldStorage"],
        options = pathOptions(
          pane = "h2Industry_spC"
        )
      ) %>%
      addMarkers(
        data = ct_truckStops,
        group = "ct_truckStops_spC",
        icon = ~markerIconList_byCategory["truckPark"],
        options = pathOptions(
          pane = "h2Industry_spC"
        )
      ) %>%
      addMarkers(
        data = ct_h2Users_transit_4326_sf,
        group = "ct_transit_spC",
        icon = ~markerIconList_byCategory["transit"],
        options = pathOptions(
          pane = "h2Industry_spC"
        )
      ) %>%
      addMarkers(
        data = ct_h2Users_metalFinishing_4326_sf,
        group = "ct_metalF_spC",
        icon = ~markerIconList_byCategory["metalF"],
        options = pathOptions(
          pane = "h2Industry_spC"
        )
      ) %>%
      addMarkers(
        data = ct_h2Users_foodProcessing_4326_sf,
        group = "ct_foodP_spC",
        icon = ~markerIconList_byCategory["foodP"],
        options = pathOptions(
          pane = "h2Industry_spC"
        )
      ) %>%
      addMarkers(
        data = ct_warehouseLocations,
        group = "ct_warehouse_spC",
        icon = ~markerIconList_byCategory["warehouse"],
        options = pathOptions(
          pane = "h2Industry_spC"
        )
      ) %>% hideGroup(
        c(
          "ct_aadt_1_spC",
          "ct_aadt_2_spC",
          "ct_aadt_3_spC",
          "ct_aadt_4_spC",
          "ct_aadt_5_spC",
          "ct_aadt_N_spC",
          "ct_natGasPipes_spC",
          "ct_candidates_spC",
          "ct_frlm05_spC",
          "ct_frlm10_spC",
          "ct_ftb05_spC",
          "ct_ftb10_spC",
          "ct_pMed05_spC",
          "ct_pMed10_spC",
          "ct_stationsH2_spC",
          "ct_foodP_spC",
          "ct_warehouse_spC",
          "ct_transit_spC",
          "ct_metalF_spC",
          "ct_truckStops_spC",
          "ct_coldStorage_spC",
          "ct_wholesale_spC",
          "ct_autoLots_spC",
          "mpo_trip_spC",
          "mpo_pop_spC",
          "ct_oppZones_spC"
        )
      )
  })
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$oppZone_button_spC,
      group = "ct_oppZones_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_transit_button_spC,
      group = "ct_transit_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_warehouse_button_spC,
      group = "ct_warehouse_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_metalF_button_spC,
      group = "ct_metalF_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_foodP_button_spC,
      group = "ct_foodP_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_wholesale_button_spC,
      group = "ct_wholesale_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_export_button_spC,
      group = "ct_export_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_truckP_button_spC,
      group = "ct_truckStops_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_coldStorage_button_spC,
      group = "ct_coldStorage_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_stationsH2_button_spC,
      group = "ct_stationsH2_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_candidates_button_spC,
      group = "ct_candidates_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_aadt_1_button_spC,
      group = "ct_aadt_1_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_aadt_2_button_spC,
      group = "ct_aadt_2_spC"
    )
  )
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_aadt_3_button_spC,
      group = "ct_aadt_3_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_aadt_4_button_spC,
      group = "ct_aadt_4_spC"
    )
  )
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_aadt_5_button_spC,
      group = "ct_aadt_5_spC"
    )
  )
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_05dflrm_button_spC,
      group = "ct_frlm05_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_10dflrm_button_spC,
      group = "ct_frlm10_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_05ftBack_button_spC,
      group = "ct_ftb05_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_10ftBack_button_spC,
      group = "ct_ftb10_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_05pMed_button_spC,
      group = "ct_pMed05_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_10pMed_button_spC,
      group = "ct_pMed10_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_natGasPipes_button_spC,
      group = "ct_natGasPipes_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$ct_autoLots_button_spC,
      group = "ct_autoLots_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$mpo_pop_button_spC,
      group = "mpo_pop_spC"
    )
  )
  
  observe(
    layerToggle(
      mapId = "map_compare",
      button = input$mpo_trip_button_spC,
      group = "mpo_trip_spC"
    )
  )
  
  output$spatialGroupSelectionTree <- renderTree({
    
    # This code lifted directly from the comparison tab
    
    allGroupInfo_byGroup <- nestedListOfPrevGroupsToRender_byGroup()
    
    ifelse(
      test = {
        length(allGroupInfo_byGroup) == 0
      },
      yes = {
        
        # if no previous groups exist, return an empty list
        return(list())
      },
      no = {
        
        return(
          allGroupInfo_byGroup
        )
      }
    )
  })
  
  observeEvent(
    eventExpr = nestedListOfPrevGroupsToRender_byGroup(),
    handlerExpr = {
      
      if(!is.null(iterNum_activeSesh())) {
        
        updateTree(
          session = session,
          treeId = "spatialGroupSelectionTree",
          data = nestedListOfPrevGroupsToRender_byGroup()
        )
      }
    }
  )
  
  
  # TODO Make this a function so we don't have to keep doing it
  
  spatialGroupsToCompare_selected <- reactive({
    selectedGroups <- unlist(
      get_selected(
        tree = input$spatialGroupSelectionTree,
        format = "classid"
      )
    )
    
    # this selects the groupStageIterInfo by name, where that name is in the list of selected groups
    # NOTE: we're reusing the prevGroups_all_listNamesGSII reactive object here
    
    spatialComparison_selectedMatched <- prevGroups_all_listNamesGSII()[
      selectedGroups
      ] %>% unname() 
    
    # This returns the unnamed list as a list of groupStageIterInfo selected
    # Though for some reason it's also giving me two "NULL" entries so let's get rid of those...
    
    return(
      Filter(
        Negate(
          is.null
        ),
        spatialComparison_selectedMatched
      )
    )
    
  })
  
  # This generates the list of pairs to add
  # TODO make this a function as well
  
  spC_pairsToAdd <- reactive({
    prevGroups_selected <- spatialGroupsToCompare_selected()
    
    # if the list of selected groups is not null:
    
    if(
      length(prevGroups_selected) > 0
    ) {
      
      # first we need to create a list of previous groups to add
      # that is a string of groupStageIteration unique identifiers
      # We'll use this to build the mongolite query as a JSON object
      
      previousGroupsToAdd_charList <- glue_collapse(
        double_quote(
          prevGroups_selected
        ),
        sep = ", "
      )
      
      # Then we construct the query for mongolite
      
      previousGroupsToAdd_query <- glue(
        
        # remember that glue() needs double brackets to represent single
        # bracket characters
        
        '{{ 
            "groupStageIterInfo" : {{ 
              "$in": [
                {selected_gSII}
              ] 
            }} 
          }}',
        
        selected_gSII = previousGroupsToAdd_charList
        
      )
      
      # Then we're using the query to call from the joining database
      # This gets us the uniqueIds for stations that were in
      # previous groups
      
      groupStationUniqueIDPairs <- groupSelected_joinId_db$find(
        query = previousGroupsToAdd_query,
        fields = '{"_id" : false}'
      ) %>% rename(
        "uniqueId" = "ct_selected_uniqueId"
      )
      
      return(
        groupStationUniqueIDPairs
      )
    }
  })
  
  
  
  # Now we need to construct a dataframe containing those stations
  # Along with a counter for the number of times they appear
  # and a list of the groupStageIterations that selected them
  
  spC_stationsToAdd <- reactive({
    
    prevGroups_selected <- spatialGroupsToCompare_selected()
    
    # if the list of selected groups is not null:
    
    if(
      length(prevGroups_selected) > 0
    ) {
      
      spC_pairsToAdd_df <- spC_pairsToAdd() 
      
      spC_pairsToAdd_df <- spC_pairsToAdd_df %>% group_by(
        uniqueId
      ) %>% summarise(
        selecCount = n(),
        groupsSelected = toString(
          (unique(groupStageIterInfo))
        )
      ) %>% inner_join(
        ct_candidates$sf
      )  %>% mutate(
        "category" = "spCompare",
        "layerId" = str_replace(
          string = layerId,
          pattern = "candidate|selected",
          replacement = "spCompare"
        )
      )
      return(
        st_as_sf(spC_pairsToAdd_df)
      )
    }
  })
  
  observeEvent(
    eventExpr = input$spatialGroupSelectionTree,
    handlerExpr = {
      
      prevGroups_selected <- spatialGroupsToCompare_selected()
      stationsToAddOnClick <- spC_stationsToAdd()
      
      colorpal_comparison <- colorFactor(
        rainbow(
          NROW(
            unique(
              stationsToAddOnClick[stationsToAddOnClick$selecCount == 1,]$groupsSelected
            )
          )
        ),
        stationsToAddOnClick[stationsToAddOnClick$selecCount == 1,]$groupsSelected
      )
      
      colorList <- list(
        selectedByOne = ~ function() {colorpal_comparison(stationsToAddOnClick$groupSelected)},
        selectedByMany = "gray"
      )
      
      ifelse(
        test = {
          length(prevGroups_selected) > 0
        },
        yes = {
          
          # if any previous groups are selected, clear the existing displayed
          # groups and redisplay the new selection
          
          leafletProxy(
            mapId = "map_compare"
          ) %>% clearGroup(
            group = "spCompare"
          ) %>% addCircleMarkers(
            data = stationsToAddOnClick,
            group = "spCompare",
            layerId = stationsToAddOnClick$layerId,
            radius = 5,
            fill = TRUE,
            color = ~ ifelse(
              test = stationsToAddOnClick$selecCount > 1,
              yes = "gray",#colorList$many,
              no = colorpal_comparison(stationsToAddOnClick[stationsToAddOnClick$selecCount == 1,]$groupsSelected)#~colorList$one
            ),
            fillColor = ~ ifelse(
              test = stationsToAddOnClick$selecCount > 1,
              yes = "gray",# colorList$many,
              no = colorpal_comparison(stationsToAddOnClick[stationsToAddOnClick$selecCount == 1,]$groupsSelected)# ~colorList$one
            ),
            opacity = 0.7,
            fillOpacity = 0.7
          ) %>% addLegend(
            "bottomleft",
            pal = colorpal_comparison,
            values = stationsToAddOnClick[stationsToAddOnClick$selecCount == 1,]$groupsSelected,
            opacity = 3,
            title = "selected groups & stages",
            layerId = "spC_legend"
          )
        },
        no = {
          leafletProxy(
            mapId = "map_compare"
          ) %>% clearGroup(
            group = "spCompare"
          ) %>% removeControl(
            layerId = "spC_legend"
          )
        }
      )
    }
  )
  
  output$downloadSelectedData_spatial <- downloadHandler(
    filename = function() {
      paste("spatial_comparison.csv", sep = "")
    },
    content = function(file) {
      write.csv(
        spC_stationsToAdd_df,
        file
      )
    }
  )
  
  output$quickStartGuide <- renderUI(
    return(
      tags$iframe(
        style = "
          height:  calc(100vh - 122px) !important;
          width: 100%;
          scrolling=yes
        ",
        src = "collablocation_ctH2_quickStartGuide.pdf"
      )
    )
  )
}

shinyApp(ui,server)