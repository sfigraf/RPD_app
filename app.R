#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#to do: 
#decided it was fine to continue filtering data in funciton; if there's a pool that oesn't show completely up (such as pool 18), go back and manually adjust the limits in depths_and_limits
#maybe make shades of each pool along entire reaches given x limits
#cntrl+shift+R will create a new section of the outline
#note about updates/publishing March 10: when currently running and reading in the .csv files, here() starts at where the project is located
#however, files are published as of now from the /rpd_app/ folder, and that is where here() starts when publishing. So to make updates, you have to delete the "/rpd_app/" portion of the .csv readins
#so it's impossible then to have the app running then publish

#shiny::runApp(display.mode="showcase") #this line is helpful for debugging
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(plotly) #helpful in making plots interactive
library(here) #helpful package for setting working directory to the top level of whatever environment this file is in
library(grDevices) #colors
library(leaflet)
library(geojsonio)
library(rgdal)
library(raster)
library(htmlwidgets) #for using javascript to put title on layer control
library(shinycssloaders) #graphics for loading map
library(DT) #for more complex data table interaction

#this file contains stationing data and RPDs for each pool 
stats_data2 <- read.csv(here("UAR_ResidualPoolDepthAnalysis_2020_ver4.csv"))
#this file is used for getting y limits for individual pools
long_pro_data <- read.csv(here("longpro_master1.csv"))


#Choices for selecting a reach
reach_choices <- unique(as.character(long_pro_data$Reach))
#choices for Year
year_choices <- unique(as.character(stats_data2$Year))

###function for making single pool plot; 
#function is called get_normalized_pool_data
source("normalized_pool_data_function.R")

###mapping
states1 <- geojson_json(states, geometry = "polygon", group = "group")

source("shapefile_readins.R")

#graphics options, mostly for mapping
source("graphics_options.R")


# UI ----------------------------------------------------------------------



# Define UI for application that draws a histogram
ui <- fluidPage(

    navbarPage(title = div(img(src="CPWLogoLarge.png", height = "60px"), " UAR Residual Pool Depths"), 
               selected = c("Map"),
               windowTitle = HTML("<title>Residual Pool Depth</title> <link rel='icon' type='image/gif/png' href='CPWLogoLarge.png'>"),
               #this part changes the navbar options
               tags$head(
                   tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:9px !important; 
                            padding-bottom:0 !important;
                            height: 80px;
                            }
                           .navbar {min-height:25px !important;}'))
               ),
               
               theme = shinytheme("lumen"), #end of navbar page arguments; what follow is all inside it
                tabPanel(tags$div("Plots",style = title_style), 
                    sidebarLayout(
                    sidebarPanel(
                        selectInput("select1", label = tags$div(tags$span("Select a Reach")),                                                    
                                    choices = reach_choices),
                        
                        sliderInput("slider1", 
                                    label = tags$div("Select a Pool ",
                                                     br(),
                                                     
                                                     tags$span("Selected number without a corresponding pool or Year will show up as xlim NA", 
                                                               style = "color: red;
                                                              font-size: 11px;
                                                              font-style: italic")), 
                                    min = 1, 
                                    max = 32, 
                                    value = 1,
                                    step = 1), #end of slider1 
                        
                        helpText("You can also use arrow keys to navigate"),
                        br(),
                        
                        selectInput("select2",
                                    label = "Select a Year ",
                                    choices = year_choices),
                        
                        checkboxInput(inputId = "check1", label = strong("Station Correctors"), value = FALSE),
                        conditionalPanel(condition = "input.check1 == true",
                                         #these buttons are for manually changing y zoomed plot area for individual pools if needed
                                         
                                         tags$div(tags$strong("Y Limits")),
                                         
                                         tags$div(tags$span("These buttons are for manually changing zoomed plot area for individual pools if needed")),
                                         actionButton("minus_ymin", "y min - 1"),
                                         actionButton("plus_ymin", "y min + 1"),
                                         br(),
                                         actionButton("minus_ymax", "y max - 1"),
                                         actionButton("plus_ymax", "y max + 1"),
                                         br(),
                                         
                                         #these buttons are for manually changing x zoomed plot area for individual pools if needed
                                         tags$div(tags$strong("x Limits")),
                                         
                                         tags$div(tags$span("These buttons are for manually changing zoomed plot area for individual pools if needed")),
                                         actionButton("minus_xmin", "x min - 10"),
                                         actionButton("plus_xmin", "x min + 10"),
                                         br(),
                                         actionButton("minus_xmax", "x max - 10"),
                                         actionButton("plus_xmax", "x max + 10"),
                                         br(),
                        ) #end of conditional Panel
                        
                    ),#end of sidebar
                    
                    
                    mainPanel(tabsetPanel(
                        tabPanel("Long Profile",
                            plotlyOutput("WholeReachPlot")),
                        tabPanel("Normalized Single Pool Plot",
                                 plotlyOutput("PoolPlot"))
                        
                    ),#end of tabset panel
                    #tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
                    
                    DT::dataTableOutput("plotdata1"),
                    )#end of mainPanel
                        
                    ), #end of "plots" tab
                ),
               # end of first tab
               tabPanel(title = tags$div("Map", style = title_style),
                        shinycssloaders::withSpinner(leaflet::leafletOutput("map1", height=1000),size=2, color="#0080b7")), #loader widget
                        #leafletOutput("map1", height = 1000)),#end of tabpanel
              
               tabPanel(title = tags$div("About", style = title_style),
                        includeHTML(paste0("www/", "UAR_NRD_RPD_about.html")))
                         
            )) #end of navbar and fluidpage


# Server ------------------------------------------------------------------


# Define server logic 
server <- function(input, output, session) {

# Slider Input Adjustments -----------------------------------------------

    
    
    ###CHanging slider selection choices based on reach
    observeEvent(input$select1,{
        
        range = c(1,100)
        year_choices_2 <- year_choices
        
        if(input$select1 == "Reddy") {
            Reddy_pools <- stats_data2 %>%
                filter(Reach == "Reddy") %>%
                distinct(Pool_Num)
            
            
            range = c(min(Reddy_pools),max(Reddy_pools))
            
            Reddy_years <- stats_data2 %>%
                filter(Reach == "Reddy") %>%
                distinct(Year)
            year_choices_2 <- Reddy_years
            
        } 
        else if(input$select1 == "Hayden") {
            Hayden_pools <- stats_data2 %>%
                filter(Reach == "Hayden") %>%
                distinct(Pool_Num)
            
            range = c(min(Hayden_pools),max(Hayden_pools))
            
            Hayden_years <- stats_data2 %>%
                filter(Reach == "Hayden") %>%
                distinct(Year)
            
            year_choices_2 <- Hayden_years
        }
        else {
            input$select1
        }
        #Error in updateSliderInput: object 'session' not found 
        #solved bc there wasn't a session argument in server <- function(input, output)
        updateSliderInput(session, "slider1", 
                          
                          min = min(range),
                          max = max(range),
                          value = min(range),
                          step = 1)
        
        updateSelectInput(session, "select2",
                          choices = year_choices_2)
    }) #end of ObserveEvent for slider and select updating
    

# Reactive Data Prep ------------------------------------------------------

  
    #filtering data for full long-pro
    
    long_pro_data1 <- reactive(long_pro_data %>%
                                   filter(Reach == input$select1))

## X Limits and Corrective Values ------------------------------------------------------------------

    

    ###X LIMITS based off Pool Selection and Year
    single_pool <- reactive(stats_data2 %>%
                                filter(Pool_Num == input$slider1))
    xlimits <- reactive(stats_data2 %>%
                            filter(Pool_Num == input$slider1,
                                   Year == input$select2))
    
    


    ###manually changing x limits to view more data based on actionbuttons
    #create 2 corrective values
    xmincorrector <- reactiveVal(0)
    xmaxcorrector <- reactiveVal(0)
    
    #xmin corrector button logic; once an event is observed, the other logic is executed
    
    observeEvent(input$minus_xmin, {
        newValue <- xmincorrector() - 10     # newValue <- rv$value - 5
        xmincorrector(newValue)             # rv$value <- newValue
    })
    
    observeEvent(input$plus_xmin, {
        newValue <- xmincorrector() + 10     # newValue <- rv$value + 5
        xmincorrector(newValue)             # rv$value <- newValue
    })
    #xmax correcter
    observeEvent(input$minus_xmax, {
        newValue <- xmaxcorrector() - 10     # newValue <- rv$value - 5
        xmaxcorrector(newValue)             # rv$value <- newValue
    })
    
    observeEvent(input$plus_xmax, {
        newValue <- xmaxcorrector() + 10     # newValue <- rv$value + 5
        xmaxcorrector(newValue)             # rv$value <- newValue
    })
    
    #x values based on filtered data and station correctors
    min_x <- reactive(as.numeric(xlimits()$Station_Upstream[1]) + xmincorrector())
    
    max_x <- reactive(as.numeric(xlimits()$Station_Downstream[1]) + xmaxcorrector())


# Y Limits and Corrective Values ------------------------------------------

    #creating a model for ylimits because they're not available otherwise
    #it's definitely no tperfect since each year has a different x value, but it gets in the ballpark
    model1 <- reactive(lm(Elevation ~ Station, 
                          data = long_pro_data1()))
    ###manually changing Y limits to view more data based on actionbuttons
    #create 2 corrective values
    ymincorrector <- reactiveVal(0)
    ymaxcorrector <- reactiveVal(0)
    
    #ymin corrector button logic; once an event (button input) is observed, the other logic is executed
    
    observeEvent(input$minus_ymin, {
        newValue <- ymincorrector() - 1     # newValue <- rv$value - 1
        ymincorrector(newValue)             # rv$value <- newValue
    })
    
    observeEvent(input$plus_ymin, {
        newValue <- ymincorrector() + 1     # newValue <- rv$value + 1
        ymincorrector(newValue)             # rv$value <- newValue
    })
    #ymax correcter
    observeEvent(input$minus_ymax, {
        newValue <- ymaxcorrector() - 1     # newValue <- rv$value - 1
        ymaxcorrector(newValue)             # rv$value <- newValue
    })
    
    observeEvent(input$plus_ymax, {
        newValue <- ymaxcorrector() + 1     # newValue <- rv$value + 1
        ymaxcorrector(newValue)             # rv$value <- newValue
    })
    
    #resets limit correctors if pool number changes
    observeEvent(input$slider1, {
        newValue <- 0
        xmincorrector(newValue)
        xmaxcorrector(newValue)
        ymincorrector(newValue)
        ymaxcorrector(newValue)
    })
    
    #y values 
    #-4 and + 2 are arbitrary, just helps to make a larger box
    min_y = reactive(
        predict.lm(model1(),data.frame(Station= as.numeric(min_x()))) - 4 + ymincorrector()
    )
    max_y = reactive(
        predict.lm(model1(), data.frame(Station= as.numeric(max_x()))) + 2 + ymaxcorrector()
    )

# Limits List -------------------------------------------------------------

    
    #convenient list of all limits
    limits_list <- reactive(list(
        "xmin" = min_x(),
        "xmax" = max_x(),
        "ymin" = as.numeric(min_y()),
        "ymax" = as.numeric(max_y())
    ))
    
    

# Whole Reach Plot --------------------------------------------------------

    
    
    output$WholeReachPlot <- renderPlotly({
        
        p <- long_pro_data1() %>%
            ggplot(aes(x = Station, y = Elevation, group = Year)) +
            # geom_rect(aes(xmin = min_x(), xmax = max_x(),
            #               ymin = min_y(), ymax = max_y()), fill = NA, color = "red") +
            geom_line(aes(color = Year)) +
            theme_classic() +
            labs(title = paste( "Entire", as.character(input$select1),"Reach"),
                 x = "Station (ft)", y = "Elevation (ft)")
        
        #p needs to be a plotly object to be modified later with relayout
        ggplotly(p, source = "longpro_plot")
        
        
        
    }) #end of whole reach plot
    
    
    
    
    ###ERROR SOLVED: Error in |: operations are possible only for numeric, logical or complex types
    #solved by instead of listing each input with | in observeEvent, make each input a list then pass it to observeEvent. It was just a syntax thing
    plot_proxy_events <- reactive({
        list(input$slider1, input$select2, input$minus_xmin, input$plus_xmin, input$minus_xmax, input$plus_xmax, input$minus_ymin, input$plus_ymin, input$minus_ymax, input$plus_ymax)
    })
    
    #plot proxies for large plot
    #this way the rectangle outlining pool number won't trigger a redraw of the whole plot when the slider input/pool number is changed
    #input (event) is observed, then a proxy is created and "relayout" method is applied
    #this enables the layout to be modified, in this case, a rectangle overlay
    #basically: inside expression will change when the following inputs are observed, but the full map won't redraw
    observeEvent(plot_proxy_events(), {
        #print("relayout")
        plotlyProxy("WholeReachPlot", session) %>%
            plotlyProxyInvoke("relayout", list(shapes = list(
                list(type = "rect",
                     line = list(color = "red"), opacity = 0.6,
                     x0 = limits_list()$xmin[1], x1 = limits_list()$xmax[1], xref = "x",
                     y0 = limits_list()$ymin[1], y1 = limits_list()$ymax[1], yref = "y")),
                title = paste("Pool:", as.character(input$slider1), "\nStations (ft): ", as.character(limits_list()$xmin[1]), "-", as.character(limits_list()$xmax[1])
                ))) #end of proxy invoke
        
    })

# Single Pool Plot --------------------------------------------------------

    ##getting data ready
    #takes station data (that will be filtered by pool number with slider 1), long_pro_data that has already been filtered based on reach, slider input for filtering stationdata, x correctors for changing xlims and viewing more upstream/dowsntream
    #it returns a dataframe of only the selected pool, normalized to start at the same place (because initially, all their  station_upstream and station_downstream are frequently differnt)
    #this is helpful in viewing an overlay of the pool change in elevation over time to see how it fills in or scours
    returned_list <- reactive({ 
        get_normalized_pool_data(stats_data2, long_pro_data1(), input$slider1, xmincorrector(), xmaxcorrector())
                    })
    
    output$PoolPlot <- renderPlotly({
        p <- returned_list()[["long_pro_data_filtered2"]] %>%
            ggplot(aes(x = new_station, y = Elevation, group = Year)) +
            geom_line(aes(color = Year 
                          #frame = Year #this arg makes the plot into an animation
            )) +
            ###Error in : `lims` must be a two-element vector
            ##solved by selecting more specific values for limits
            #xlim(c(returned_list()[["starting_xlim"]], returned_list()[["ending_xlim"]])) +
            labs(title = paste("Pool",as.character(input$slider1)),
                 x = "Normalized Station (ft)", y = "Elevation (ft)") +
            theme_classic()
        
        # p <- long_pro_data1() %>%
        #     group_by(Year) %>%
        #      plot_ly(
        #          x = ~Station, y = ~Elevation, color = ~Year, type = "scatter", mode = "lines"
        #      )
        ###Animation options
        # p <- p %>%
        #     animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom") %>%
        #     animation_opts(frame = 1000000, transition =  1000, easing = "elastic")
        p
    })

# Data Table Output -------------------------------------------------------

    
    output$plotdata1 <- renderDataTable(single_pool(),
                                        # callback = JS(js),
                                        selection = "single"
                                        # extensions = "KeyTable",
                                        # options = list(
                                        #     keys = TRUE
                                        #     )
                                        )

# Map Output --------------------------------------------------------------

    
    output$map1 <- renderLeaflet({
        
        states %>%
            leaflet() %>%
            addProviderTiles(providers$Esri.WorldImagery,options = providerTileOptions(maxZoom = 26)) %>%
            #addTiles(options = providerTileOptions(maxZoom = 26)) %>%
            setView(-106.319856,39.165100, 15) %>%
            ###POOLS
            addPolygons(data = simple_pools,
                        fillColor = pool_color,
                        opacity = opacity_number,
                        weight = pool_border_weight,
                        color = pool_border_color,
                        fillOpacity = fillOpacity_number,
                        label = paste(simple_pools@data$RPD_Num_20),
                        labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                        popup = paste("Pool",simple_pools@data$RPD_Num_20),
                        group = "Pools") %>%
            
            ###Thalwegs
            #Reddy
            addPolylines(data = simplereddy2010to2013_thalweg,
                         opacity = opacity_number,
                         color = color_2010_2013,
                         weight = Thalweg_line_weight,
                         group = "2010 - 2013") %>%
            addPolylines(data = simplereddy2013_thalweg,
                         opacity = opacity_number,
                         color = color_2013,
                         weight = Thalweg_line_weight,
                         group = "2013") %>%
            addPolylines(data = simplereddy2014_thalweg,
                         opacity = opacity_number,
                         color = color_2014,
                         weight = Thalweg_line_weight,
                         group = "2014") %>%
            addPolylines(data = simplereddy2015_thalweg,
                         opacity = opacity_number,
                         color = color_2015,
                         weight = Thalweg_line_weight,
                         group = "2015") %>%
            addPolylines(data = simplereddy2016_thalweg,
                         opacity = opacity_number,
                         color = color_2016,
                         weight = Thalweg_line_weight,
                         group = "2016") %>%
            addPolylines(data = simplereddy2017_thalweg,
                         opacity = opacity_number,
                         color = color_2017,
                         weight = Thalweg_line_weight,
                         group = "2017") %>%
            addPolylines(data = simplereddy2018_thalweg,
                         opacity = opacity_number,
                         color = color_2018,
                         weight = Thalweg_line_weight,
                         group = "2018") %>%
            #Hayden
            addPolylines(data = simplehayden2010to2013_thalweg,
                         opacity = opacity_number,
                         color = color_2010_2013,
                         weight = Thalweg_line_weight,
                         group = "2010 - 2013") %>%
            addPolylines(data = simplehayden2014_thalweg,
                         opacity = opacity_number,
                         color = color_2014,
                         weight = Thalweg_line_weight,
                         group = "2014") %>%
            addPolylines(data = simplehayden2015_thalweg,
                         opacity = opacity_number,
                         color = color_2015,
                         weight = Thalweg_line_weight,
                         group = "2015") %>%
            addPolylines(data = simplehayden2016_thalweg,
                         opacity = opacity_number,
                         color = color_2016,
                         weight = Thalweg_line_weight,
                         group = "2016") %>%
            addPolylines(data = simplehayden2017_thalweg,
                         opacity = opacity_number,
                         color = color_2017,
                         weight = Thalweg_line_weight,
                         group = "2017") %>%
            addPolylines(data = simplehayden2018_thalweg,
                         opacity = opacity_number,
                         color = color_2018,
                         weight = Thalweg_line_weight,
                         group = "2018") %>%
            
            ###Stations
            #Reddy
            addPolylines(data = simplereddy2010to2013_stations,
                         opacity = opacity_number,
                         color = color_2010_2013,
                         weight = Station_line_weight,
                         label = paste(simplereddy2010to2013_stations@data$ET_STATION),
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                         group = "2010 - 2013") %>%
            addPolylines(data = simplereddy2013_stations,
                         opacity = opacity_number,
                         color = color_2013,
                         weight = Station_line_weight,
                         label = paste(simplereddy2013_stations@data$ET_STATION),
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                         group = "2013") %>%
            addPolylines(data = simplereddy2014_stations,
                         opacity = opacity_number,
                         color = color_2014,
                         weight = Station_line_weight,
                         label = paste(simplereddy2014_stations@data$ET_STATION),
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                         group = "2014") %>%
            addPolylines(data = simplereddy2015_stations,
                         opacity = opacity_number,
                         color = color_2015,
                         weight = Station_line_weight,
                         label = paste(simplereddy2015_stations@data$ET_STATION),
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                         group = "2015") %>%
            addPolylines(data = simplereddy2016_stations,
                         opacity = opacity_number,
                         color = color_2016,
                         weight = Station_line_weight,
                         label = paste(simplereddy2016_stations@data$ET_STATION),
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                         group = "2016") %>%
            addPolylines(data = simplereddy2017_stations,
                         opacity = opacity_number,
                         color = color_2017,
                         weight = Station_line_weight,
                         label = paste(simplereddy2017_stations@data$ET_STATION),
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                         group = "2017") %>%
            addPolylines(data = simplereddy2018_stations,
                         opacity = opacity_number,
                         color = color_2018,
                         weight = Station_line_weight,
                         label = paste(simplereddy2018_stations@data$ET_STATION),
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                         group = "2018") %>%
            #Hayden Layers
            addPolylines(data = simplehayden2010to2013_stations,
                         opacity = opacity_number,
                         color = color_2010_2013,
                         weight = Station_line_weight,
                         label = paste(simplehayden2010to2013_stations@data$ET_STATION),
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                         group = "2010 - 2013") %>%
            addPolylines(data = simplehayden2014_stations,
                         opacity = opacity_number,
                         color = color_2014,
                         weight = Station_line_weight,
                         label = paste(simplehayden2014_stations@data$ET_STATION),
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                         group = "2014") %>%
            addPolylines(data = simplehayden2015_stations,
                         opacity = opacity_number,
                         color = color_2015,
                         weight = Station_line_weight,
                         label = paste(simplehayden2015_stations@data$ET_STATION),
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                         group = "2015") %>%
            addPolylines(data = simplehayden2016_stations,
                         opacity = opacity_number,
                         color = color_2016,
                         weight = Station_line_weight,
                         label = paste(simplehayden2016_stations@data$ET_STATION),
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                         group = "2016") %>%
            addPolylines(data = simplehayden2017_stations,
                         opacity = opacity_number,
                         color = color_2017,
                         weight = Station_line_weight,
                         label = paste(simplehayden2017_stations@data$ET_STATION),
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                         group = "2017") %>%
            addPolylines(data = simplehayden2018_stations,
                         opacity = opacity_number,
                         color = color_2018,
                         weight = Station_line_weight,
                         label = paste(simplehayden2018_stations@data$ET_STATION),
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE, style = label_style),
                         group = "2018") %>%
    
            addLayersControl(overlayGroups = c("Pools", "2010 - 2013","2013","2014","2015","2016","2017","2018"),
                             options = layersControlOptions(collapsed = FALSE, title = "Thalweg Lines")) %>%
            #start with all groups off except 2010-2013 and Pools
            hideGroup(c("2013","2014","2015","2016","2017","2018")) %>%
            htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\"><b>Thalweg Survey Lines</b></label>');
        }
    ")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


