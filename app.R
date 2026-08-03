cat("Running:", normalizePath("."), "\n")
cat("This file is being executed.\n")

#SETTING UP SHINY DASHBOARD
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(readxl)
library(DT)
#library(terra)

cat("Loading crashdata...\n")

crashdata <- read.csv(
  "export_213299_0.csv",
  check.names = FALSE
)

cat("Crashdata loaded:", nrow(crashdata), "rows\n")

cat("Loading individualdata...\n")

individualdata <- read.csv(
  "export_213299_2_clean.csv",
  check.names = FALSE
)

cat("Individualdata loaded:", nrow(individualdata), "rows\n")

cat("Making column names unique...\n")

names(crashdata) <- make.unique(
  names(crashdata),
  sep = "_duplicate"
)

names(individualdata) <- make.unique(
  names(individualdata),
  sep = "_duplicate"
)

cat("Joining datasets...\n")

individualdata <- individualdata %>%
  left_join(
    crashdata %>%
      select(
        CrashId,
        Year,
        `Weather Condition Text Format`,
        `Light Condition Text Format`,
        `Road Surface Condition Text Format`,
        `Day of the Week Text Format`,
        `Hour of the Day`,
        `Time of Crash`,
        `Most harmful event`
      ),
    by = "CrashId"
  )

cat("Finished joining datasets.\n")

cat("Finished joining datasets.\n")





#new haven boundary line
newhaven <- st_read(
  "cityboundary.shp",
  quiet = TRUE
) %>%
  st_transform(crs = 4326)

library(readxl)

neighborhood <- read_excel("neighborhood2.xlsx")

severitydata <- st_read(
  "SeverityIndex.geojson",
  quiet = TRUE
) %>%
  st_transform(crs = 4326)

fatalitiesdata <- st_read(
  "FatalitiesFile.geojson",
  quiet = TRUE
) %>%
  st_transform(crs = 4326)

#crashes by neighborhood
neighborhood_plot_data <- neighborhood %>%
  transmute(
    Neighborhood = as.character(Neighborhood),
    MVA = round(as.numeric(MVA),1)
  ) %>%
  filter(
    !is.na(Neighborhood),
    !is.na(MVA)
  ) %>%
  arrange(desc(MVA)) %>%
  mutate(
    Neighborhood = factor(
      Neighborhood,
      levels = unique(Neighborhood)
    )
  )

#MVA per mile
neighborhood_plot_data2 <- neighborhood %>%
  transmute(
    Neighborhood = as.character(Neighborhood),
    `MVA per Mile` = round(as.numeric(`MVA per Mile`),1)
  ) %>%
  filter(
    !is.na(Neighborhood),
    !is.na(`MVA per Mile`)
  ) %>%
  arrange(desc(`MVA per Mile`)) %>%
  mutate(
    Neighborhood = factor(
      Neighborhood,
      levels = unique(Neighborhood)
    )
  )

#fatal per mile 
neighborhood_plot_data3 <- neighborhood %>%
  transmute(
    Neighborhood = as.character(Neighborhood),
    `Fatal per mile` = round(as.numeric(`Fatal per mile`),4)
  ) %>%
  filter(
    !is.na(Neighborhood),
    !is.na(`Fatal per mile`)
  ) %>%
  arrange(desc(`Fatal per mile`)) %>%
  mutate(
    Neighborhood = factor(
      Neighborhood,
      levels = unique(Neighborhood)
    )
  )

#raw count fatalities
neighborhood_plot_data4 <- neighborhood %>%
  transmute(
    Neighborhood = as.character(Neighborhood),
    `Number of Fatals` = as.numeric(`Number of Fatals`)
  ) %>%
  filter(
    !is.na(Neighborhood),
    !is.na(`Number of Fatals`)
  ) %>%
  arrange(desc(`Number of Fatals`)) %>%
  mutate(
    Neighborhood = factor(
      Neighborhood,
      levels = unique(Neighborhood)
    )
  )



#rename latitude and longitude columns
# Columns 6 and 7 contain the crash latitude and longitude
names(crashdata)[6:7] <- c("latitude", "longitude")

crashdata$latitude <- as.numeric(crashdata$latitude)
crashdata$longitude <- as.numeric(crashdata$longitude)


#SOURCE CODE via Source_Info

cat("crashdata loaded:", exists("crashdata"), "\n")
cat("crashdata rows:", nrow(crashdata), "\n")

cat("Starting Source_Info.R\n")
source("Source_Info.R", local = environment())
cat("Finished Source_Info.R\n")

ui <- dashboardPage(
  
  dashboardHeader(
    title = "New Haven Crash Dashboard"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Overview",
        tabName = "overview",
        icon = icon("chart-line")
      ),
      
      menuItem(
        "Overall Trends",
        tabName = "overall_trends",
        icon = icon("map")
      ),
      
      menuItem(
        "Neighborhood Trends",
        tabName = "neighborhood_trends",
        icon = icon("person-walking")
      ),
      
      menuItem(
        "Fatality Trends",
        tabName = "fatalities_trends",
        icon = icon("user-injured")
      ),
      menuItem(
        "Recent Crashes",
        tabName = "recent_crashes",
        icon = icon("triangle-exclamation")
      )
      
    
  )
  ),
  
  dashboardBody(
    tabItems(
      
      #overview tab
      tabItem(
        tabName = "overview",
        
        h2(
          "Welcome to the New Haven Crash Dashboard",
          style = "font-weight:bold; font-size:40px"
        ),
        
        
        
        h4("Overview Statement", style="text-decoration:underline; font-size: 32px;"),  
        
        h3("The New Haven Crash Dashboard is designed to visualize and explore motor vehicle crashes. Users can interact with the maps, charts, and filters to examine crash patterns, contributing actions, and
         spatiotemporal trends that can support data-driven traffic safety analysis, decision-making, and safety improvements for 
         all users. Some maps only use crashes with at least 1 possible injury and some maps include crahses with all injury types 
           (including no injuries). Please see text boxes with more information. ", style="font-size:20px;")
        
        ,
        
         h4("How to Use:", style="text-decoration:underline; font-size:32px;"),
         h3("Click on the tabs in the left-hand navigation menu. Each tab showcases a unique topic, including information
         on the city-wide crash trends, neighborhood-level trends, and city-wide fatality statistics. Users can hover over each crash
         point, bar chart, and pie chart to see additional information. The Recent Crashes tab displays information 
         from the UConn Crash Repository, which is updated daily and includes crashes involving at least one possible injury as well 
         as fatal crashes. Both injury and fatality data can be viewed for the last 30 or 90 days and 6 or 12 months. Interactive tables are 
         also provided, allowing users to filter, sort, and search for crashes based on specific attributes.", style="font-size:20px")
        , 
        
        h4("Additional Information", style="text-decoration:underline; font-size:32px"),
        h3("The first four tabs are data from 1/1/21-6/11/26 (5.5 year analysis span), while the last tab
           contains real-time information from the last 30 or 90 days and 6 or 12 months that is updated daily from the UConn crash servers. 
           NOTE: it may take up to 60 seconds for all features and maps to load. ", style="font-size:20px;")
        ,
        
        
        h4(
          "Quick Facts: 1/1/21-6/11/26",
          style = "font-size:35px;"
        ),
        
        fluidRow(
          
          #box 1
          valueBox(
            value = "7,498",
            subtitle = "Unique Crashes with at least 1 possible injury",
            icon = icon("car"),
            color = "blue"
          ),
          
          #box 2
          valueBox(
            value = "10,628",
            subtitle = "Individuals involved in a crash with at least 1 possible injury",
            icon = icon("users"),
            color = "yellow"
          ),
          
          #box 3
          valueBox(
            value = "20,744",
            subtitle = "Individuals involved in crashes of any severity, including no injury",
            icon = icon("road"),
            color = "aqua"
          )
        ),
      
        
        h4("Crash Maps (1/1/21-6/11/2026)",
          style="font-size:35px"
        ),
        h4("The following maps are interative. For all crashes, fatalities, and pedestrian injuries zoom in and click on a dot to see additional features. 
           For the Severity Index map, click on a road to see its SI score and number of crashes on the 500 foot segment. "),
        selectInput(
          inputId = "mapType",
          label = "Select Map",
          choices = c(
            "All Crashes Map" = "crashes",
            "Crash Severity Index Map" = "severity",
            "All Fatalities Map" = "fatalities",
            "Pedestrian Injury and Fatalites" = "pedestrian"
          ),
          selected = "crashes"
        ),
        
        tags$details(
          open = TRUE,
          
          tags$summary(
            style = "font-weight: bold; font-size: 18px; cursor: pointer;",
            "About this map"
          ),
          
          div(
            style = "
      padding: 12px;
      margin-top: 8px;
      background-color: white;
      border-radius: 6px;
    ",
            
            uiOutput("map_description")
          )
        ),
        
        br(),
        
        
        leafletOutput(
          "overviewMap",
          height = "600px"
        )
        
        #end of overview tab
      ),
      
      
      #overall trends tab
      tabItem(
        tabName = "overall_trends",
        
        h2(
          "Crash Characteristics",
          style = "font-size:40px; font-weight:bold; text-decoration:underline;"
        ),
        
        
        #crashes by year
        h4(
          "Crashes by Year",
          style = "font-size:35px;"
        ),
        
        h5(
          "*Note: 2026 is still incomplete",
          style = "font-size:20px;"
        ),
        
        sliderInput(
          inputId = "yearRange",
          label = "Select by Year",
          min = 2021,
          max = 2026,
          value = c(2021, 2026),
          ticks = TRUE,
          step = 1,
          sep = ""
        ),
        
        plotlyOutput(
          "yearPlot",
          height = "500px"
        ),
        
        
        #weather, road surface, and light conditions
        h4(
          "Weather, road surface, and light conditions",
          style = "font-size:35px;"
        ),
        
        plotlyOutput(
          "weatherPlot",
          height = "500px"
        ),
        
        plotlyOutput(
          "lightPlot",
          height = "500px"
        ),
        
        plotlyOutput(
          "roadPlot",
          height = "500px"
        ),
        
        
        #time of day and day of the week
        h4(
          "Day of the Week and Time of Day",
          style = "font-size:35px;"
        ),
        
        plotlyOutput(
          "dayPlot",
          height = "500px"
        ),
        
        plotlyOutput(
          "timePlot",
          height = "500px"
        ),
        
        
        #contributing action by driver
        h4(
          "Contributing Action by Driver",
          style = "font-size:35px;"
        ),
        
        h5(
          "Definition: action committed by the driver prior to crash",
          style = "font-size:20px;"
        ),
        
        #contributing action by driver
        plotlyOutput(
          "contributingPie",
          height = "500px"
        ),
        
        
        #most harmful event
        h4(
          "Most Harmful Event",
          style = "font-size:35px;"
        ),
        
        h5(
          "Definition: event that results in the most severe injury or property damage",
          style = "font-size:20px;"
        ),
        
        plotlyOutput(
          "harmfulPie",
          height = "500px"
        )
        
        #end of overall trends tab
      ),
      
      
      #neighborhood trends tab
      tabItem(
        tabName = "neighborhood_trends",
        
        h2(
          "Neighborhood Trends",
          style = "font-size:40px; font-weight:bold; text-decoration:underline;"
        ),
        
        h4(
          "Motor Vehicle Accident Count",
          style = "font-size:30px"
        ),
        
        plotlyOutput(
          "neighborhoodPlot",
          height = "500px"
        ),
        
        plotlyOutput(
          "mvaMilePlot",
          height = "500px"
        ),
        
        plotlyOutput(
          "fatalPlot",
          height="500px"
        ),
        
        plotlyOutput(
          "fatalMilePlot",
          height = "500px"
      
        )
        
      ), #closing neighborhood tab
      
      #fatalities tab
      tabItem(
        tabName = "fatalities_trends",
        
        h2(
          "Fatality Trends",
          style = "font-size:40px; font-weight:bold; text-decoration:underline;"
        ),
        
        h4(
          "Fatalities by Year",
          style="font-size:25px"
        ),
        h5(
          "*Note: 2026 is still incomplete",
          style = "font-size:20px;"
          ), 
        
        sliderInput(
          inputId = "fatalyearRange",
          label = "Select by Year",
          min = 2021,
          max = 2026,
          value = c(2021, 2026),
          ticks = TRUE,
          step = 1,
          sep = ""
        ),
        
        plotlyOutput(
          "yearlyfatalPlot",
          height = "500px"
        ), 
        
        h4(
          "Weather, light, and road surface conditions"
        ),
        
        plotlyOutput(
          "weatherfatalPlot",
          height = "500px"
        ),
        
        plotlyOutput(
          "lightfatalPlot",
          height = "500px"
        ), 
        
        plotlyOutput(
          "roadfatalPlot",
          height = "500px"
          ),
          
        h4(
        "Day of the Week and Time of Day"
        ),
        
        plotlyOutput(
          "dayfatalPlot",
          height = "500px"
        ), 
        
        plotlyOutput(
          "timefatalPlot",
          height = "500px"
        ), 
        
        h4("Contributing Action by Driver Prior to Crash"
          
        ),
        
        plotlyOutput(
          "contributingfatalPie",
          height = "500px"
        ), 
        
        h4(
        "Most Harmful Event")
        ,
        
        plotlyOutput(
          "harmfulfatalPie",
          height = "500px"
        )
      ),
      
      tabItem(
        tabName = "recent_crashes",
        
        h2(
          "Recent Crashes",
          style = "font-size:40px; font-weight:bold; text-decoration:underline;"
        ),
        
        fluidRow(
          column(
            width = 6,
            
            selectInput(
              inputId = "crashType",
              label = "Crash type:",
              choices = c(
                "Fatal Crashes",
                "All Injury Crashes"
              ),
              selected = "Fatal Crashes"
            )
          ),
          
          column(
            width = 6,
            
            selectInput(
              inputId = "dayRange",
              label = "Time period:",
              choices = c(
                "Last 30 Days" = 30,
                "Last 90 Days" = 90,
                "Last 6 months" = 182,
                "Last 12 months" = 365,
                "Last 24 months" = 730
              ),
              selected = 30
            )
          )
        ),
        
        h3(textOutput(
          "recentCrashTitle",
          container = h3
        )),
        
        leafletOutput(
          "recentCrashMap",
          height = "600px"
        ),
        
        h3("Crash Details"),
        
      DTOutput("recentCrashTable")
      )
    
     )#end of tab items 
  
 
    ) #end of dashboard body  

) #end of dashboard page 

recentSeverityPal <- colorFactor(
  palette = c(
    "red",
    "darkorange",
    "gray50",
    "steelblue"
  ),
  
  domain = c(
    "Fatal Injury (K)",
    "Suspected Serious Injury (A)",
    "Suspected Minor Injury (B)",
    "Possible Injury (C)"
  )
)

#severity index colors
severityPal <- colorFactor(
  palette = c(
    "gray",
    "white",
    "skyblue",
    "royalblue",
    "blue4"
  ),
  domain = 1:5,
  levels = 1:5,
  ordered = TRUE
)

#fatality type colors
fatalPal <- colorFactor(
                        palette=c("red", "orange", "purple", "forestgreen", "dodgerblue"),
                        domain=fatalitiesdata$`Person_Type_Text_Format`
                          )

#pedestrian injury colors 
pedestrianSeverityLevels <- c(
  "No Apparent Injury (O)",
  "Possible Injury (C)",
  "Suspected Minor Injury (B)",
  "Suspected Serious Injury (A)",
  "Fatal Injury (K)"
)

pedestrianSeverityPal <- colorFactor(
 palette = c(
    "gray",
    "yellow",
    "orange",
    "red",
    "darkred"
  ),
  domain = pedestrianSeverityLevels,
  levels = pedestrianSeverityLevels,
  ordered = TRUE
)


# UConn crash layer query URL LAYER 0 CRASH
uconn_all_crashes_url <- paste0(
  "https://gis.cti.uconn.edu/arcgis/rest/services/",
  "Crash_Dashboards/ConnecticutCrash/FeatureServer/0/query"
)

# UConn crash layer query URL LAYER 1 PERSON 
uconn_person_url <- paste0(
  "https://gis.cti.uconn.edu/arcgis/rest/services/",
  "Crash_Dashboards/ConnecticutCrash/FeatureServer/1/query"
)

get_nonparking_crash_ids <- function() {
  
  response <- httr2::request(uconn_all_crashes_url) |>
    httr2::req_url_query(
      where = paste(
        "CrashTownName = 'New Haven'",
        "AND LawEnforcementAgencyName = 'New Haven PD'",
        "AND TrafficwayClassType <> 'Parking Lot'",
        "AND CrashDateYear >= 2021"
      ),
      outFields = "CrashID",
      returnGeometry = "false",
      f = "json"
    ) |>
    httr2::req_timeout(60) %>%
    httr2::req_perform()
  
  jsonlite::fromJSON(
    httr2::resp_body_string(response)
  )$features$attributes %>%
    dplyr::distinct(CrashID)
}

get_live_person_types <- function() {
  
  response <- httr2::request(uconn_person_url) |>
    httr2::req_url_query(
      where = paste(
        "CrashTownName = 'New Haven'",
        "AND CrashDateYear >= 2021",
        "AND LawEnforcementAgencyName = 'New Haven PD'"
      ),
      outFields = "CrashID,PersonType",
      returnGeometry = "false",
      f = "json"
    ) |>
    httr2::req_timeout(60) |>
    httr2::req_perform()
  
  person_json <- jsonlite::fromJSON(
    httr2::resp_body_string(response),
    simplifyVector = FALSE
  )
  
  person_data <- dplyr::bind_rows(
    lapply(
      person_json$features,
      function(feature) {
        as.data.frame(
          feature$attributes,
          stringsAsFactors = FALSE
        )
      }
    )
  )
  
  person_data |>
    dplyr::transmute(
      CrashID = as.character(CrashID),
      PersonType = as.character(PersonType)
    ) |>
    dplyr::filter(
      !is.na(CrashID),
      !is.na(PersonType),
      PersonType != ""
    ) |>
    dplyr::group_by(CrashID) |>
    dplyr::summarise(
      PersonType = paste(
        sort(unique(PersonType)),
        collapse = ", "
      ),
      .groups = "drop"
    )
}



# Download all New Haven crashes from LAYER 0 CRASH
get_live_newhaven_crashes <- function() {
  
  batch_size <- 500
  offset <- 0
  crash_batches <- list()
  
  repeat {
    
    crash_response <- httr2::request(uconn_all_crashes_url) %>%
      httr2::req_url_query(
        where = paste(
          "CrashTownName = 'New Haven'",
          "AND CrashDateYear >= 2021",
          "AND LawEnforcementAgencyName = 'New Haven PD'",
          "AND TrafficwayClassType <> 'Parking Lot'"
        ),
        
        outFields = paste(
          "CrashID",
          "CrashDate",
          "CrashTimeHour",
          "CrashTownName",
          "NameOfRoadway",
          "NameOfIntersectingRoadway",
          "CrashSpecificLocation",
          "MostSevereInjury",
          "CrashSeverity",
          "WeatherCondition",
          "LightCondition",
          "TrafficSurfaceCondition",
          "FirstHarmfulEvent",
          "LawEnforcementAgencyName",
          "TrafficwayClassType",
          sep = ","
        ),
        
        returnGeometry = "true",
        outSR = 4326,
        
        # Keeps pagination in a consistent order
        orderByFields = "CrashID",
        
        # Inside get_live_newhaven_crashes()
        resultOffset = offset,
        resultRecordCount = batch_size,
        
        f = "geojson"
      ) %>%
      httr2::req_timeout(60) %>%
      httr2::req_perform()
    
    crash_batch <- sf::st_read(
      httr2::resp_body_string(crash_response),
      quiet = TRUE
    )
    
    # Stop when the server returns no records
    if (nrow(crash_batch) == 0) {
      break
    }
    
    crash_batches[[length(crash_batches) + 1]] <- crash_batch
    
    message(
      "Downloaded ",
      offset + nrow(crash_batch),
      " New Haven crash records"
    )
    
    # Stop when the final batch contains fewer than 500 records
    if (nrow(crash_batch) < batch_size) {
      break
    }
    
    offset <- offset + batch_size
  }
  
  live_crashes <- dplyr::bind_rows(crash_batches) %>%
    dplyr::mutate(
      CrashDate = as.Date(
        as.POSIXct(
          CrashDate / 1000,
          origin = "1970-01-01",
          tz = "America/New_York"
        ),
        tz = "America/New_York"
      )
    ) %>%
    dplyr::filter(
      MostSevereInjury %in% c(
        "Fatal Injury (K)",
        "Suspected Serious Injury (A)",
        "Possible Injury (C)",
        "Suspected Minor Injury (B)"
      )
    )
#  person_types <- get_live_person_types()
  
 # live_crashes <- live_crashes |>
  #          dplyr::mutate(
   #           CrashID = as.character(CrashID)
  #  ) |>
  #  dplyr::left_join(
   #   person_types,
  #    by = "CrashID"
    #) |>
  #  dplyr::mutate(
   #   PersonType = dplyr::if_else(
    #    is.na(PersonType) | PersonType == "",
    #    "Not reported",
    #    PersonType
  #  )
  #  )
  return(live_crashes)
}

#server set up
server <- function(input, output, session) {
  #live feed
  
  #fatalities by year
  output$yearlyfatalPlot <- renderPlotly({
    
    year_count <- individualdata %>%
      filter(
        Year >= input$fatalyearRange[1],
        Year <= input$fatalyearRange[2],
        `Injury Status` == "K"
      ) %>%
      count(Year)
    
    FY <- ggplot(
      year_count,
      aes(
        x = Year,
        y = n,
        text = paste0(
          "Year: ", Year,
          "<br>Fatalities: ", n
        )
      )
    ) +
      geom_col(fill = "seagreen") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5)
      ) +
      scale_x_continuous(
        breaks = 2021:2026
      ) +
      labs(
        title = "Fatalities by Year",
        x = "Year",
        y = "Count"
      )
    
    ggplotly(
      FY,
      tooltip = "text"
    )
    
    #crashes by year end brackets
  })
  
  
  #crashes by year
  output$yearPlot <- renderPlotly({
    
    year_count <- crashdata %>%
      filter(
        Year >= input$yearRange[1],
        Year <= input$yearRange[2]
      ) %>%
      count(Year)
    
    Y <- ggplot(
      year_count,
      aes(
        x = Year,
        y = n,
        text = paste0(
          "Year: ", Year,
          "<br>Crashes: ", n
        )
      )
    ) +
      geom_col(fill = "seagreen") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5)
      ) +
      scale_x_continuous(
        breaks = 2021:2026
      ) +
      labs(
        title = "Crashes by Year",
        x = "Year",
        y = "Number of Crashes"
      )
    
    ggplotly(
      Y,
      tooltip = "text"
    )
    
    #crashes by year end brackets
  })
  
  #weather
  output$weatherPlot <- renderPlotly({
    
    W <- ggplot(
      weather_count,
      mapping = aes(
        x = reorder(`Weather Condition Text Format`, -n),
        y = n,
        text = paste0("Weather: ",`Weather Condition Text Format`,
          "<br>Count: ",n, "<br>Percentage: ",
          round(n / sum(n) * 100, 1),"%"
        )
      )
    ) +
      geom_col(fill = "steelblue") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        plot.title = element_text(
          hjust = 0.5
        )
      ) +
      labs(
        title = "Weather Conditions During Crash",
        x = "Weather Condition",
        y = "Count"
      )
    
    ggplotly(
      W,
      tooltip = "text"
    )
    
    #weather end brackets
  })
  # fatal weather
  output$weatherfatalPlot <- renderPlotly({
    
    fatalweather_count <- individualdata %>%
                  filter(`Injury Status` == "K") %>%
                  count(`Weather Condition Text Format`, name = "n")
    
    FW <- ggplot(
      fatalweather_count,
      mapping = aes(
        x = reorder(`Weather Condition Text Format`, -n),
        y = n,
        text = paste0("Weather: ",`Weather Condition Text Format`,
                      "<br>Count: ",n, "<br>Percentage: ",
                      round(n / sum(n) * 100, 1),"%"
        )
      )
    ) +
      geom_col(fill = "steelblue") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        plot.title = element_text(
          hjust = 0.5
        )
      ) +
      labs(
        title = "Weather Conditions During Fatal Crash",
        x = "Weather Condition",
        y = "Count"
      )
    
    ggplotly(FW,
      tooltip = "text"
    )
    
    #weather end brackets
  })
  
  #light
  output$lightPlot <- renderPlotly({
    
    L <- ggplot(
      light_count,
      mapping = aes(
        x = reorder(`Light Condition Text Format`, -n),
        y = n,
        text = paste0(
          "Light: ",
          `Light Condition Text Format`,
          "<br>Count: ",
          n,
          "<br>Percentage: ",
          round(n / sum(n) * 100, 1),
          "%"
        )
      )
    ) +
      geom_col(fill = "orange") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        plot.title = element_text(
          hjust = 0.5
        )
      ) +
      labs(
        title = "Light Conditions During Crash",
        x = "Light Condition",
        y = "Count"
      )
    
    ggplotly(
      L,
      tooltip = "text"
    )
    
    #light end brackets
  })
  
  #fatal light
  output$lightfatalPlot <- renderPlotly({
    
    fatallight_count <- individualdata %>%
      filter(`Injury Status` == "K") %>%
      count(`Light Condition Text Format`, name = "n")
    
    FL <- ggplot(
      fatallight_count,
      mapping = aes(
        x = reorder(`Light Condition Text Format`, -n),
        y = n,
        text = paste0(
          "Light: ",
          `Light Condition Text Format`,
          "<br>Count: ",
          n,
          "<br>Percentage: ",
          round(n / sum(n) * 100, 1),
          "%"
        )
      )
    ) +
      geom_col(fill = "orange") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        plot.title = element_text(
          hjust = 0.5
        )
      ) +
      labs(
        title = "Light Conditions During Fatal Crash",
        x = "Light Condition",
        y = "Count"
      )
    
    ggplotly(
      FL,
      tooltip = "text"
    )
    
    #fatal light end brackets
  })
  
  #road surface
  output$roadPlot <- renderPlotly({
    
    RS <- ggplot(
      roadsurface_count,
      mapping = aes(
        x = reorder(`Road Surface Condition Text Format`, -n),
        y = n,
        text = paste0(
          "Road Surface: ",
          `Road Surface Condition Text Format`,
          "<br>Count: ",
          n,
          "<br>Percentage: ",
          round(n / sum(n) * 100, 1),
          "%"
        )
      )
    ) +
      geom_col(fill = "navy") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        plot.title = element_text(
          hjust = 0.5
        )
      ) +
      labs(
        title = "Road Surface Conditions During Crash",
        x = "Road Surface Condition",
        y = "Count"
      )
    
    ggplotly(
      RS,
      tooltip = "text"
    )
    
    #road surface end brackets
  })
  
  #fatal road surface
  output$roadfatalPlot <- renderPlotly({
    
    fatalroad_count <- individualdata %>%
      filter(`Injury Status` == "K") %>%
      count(`Road Surface Condition Text Format`, name = "n")
    
    FRS <- ggplot(
      fatalroad_count,
      mapping = aes(
        x = reorder(`Road Surface Condition Text Format`, -n),
        y = n,
        text = paste0(
          "Road Surface: ",
          `Road Surface Condition Text Format`,
          "<br>Count: ",
          n,
          "<br>Percentage: ",
          round(n / sum(n) * 100, 1),
          "%"
        )
      )
    ) +
      geom_col(fill = "navy") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        plot.title = element_text(
          hjust = 0.5
        )
      ) +
      labs(
        title = "Road Surface Conditions During Fatal Crash",
        x = "Road Surface Condition",
        y = "Count"
      )
    
    ggplotly(
      FRS,
      tooltip = "text"
    )
    
    #fatal road surface end brackets
  })
  
  
  #day of the week
  output$dayPlot <- renderPlotly({
    
    DW <- ggplot(
      day_count,
      mapping = aes(
        x = `Day of the Week Text Format`,
        y = n,
        text = paste0(
          "Day: ",
          `Day of the Week Text Format`,
          "<br>Count: ",
          n,
          "<br>Percentage: ",
          round(n / sum(n) * 100, 1),
          "%"
        )
      )
    ) +
      geom_col(fill = "gray") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        plot.title = element_text(
          hjust = 0.5
        )
      ) +
      labs(
        title = "Day of the Week",
        y = "Count",
        x = " "
      )
    
    ggplotly(
      DW,
      tooltip = "text"
    )
    
    #day of the week end brackets
  })
  
  
  #fatal day of the week
  output$dayfatalPlot <- renderPlotly({
    
    fatalday_count <- individualdata %>%
      filter(`Injury Status` == "K") %>%
      count(`Day of the Week Text Format`, name = "n") %>%
      mutate(`Day of the Week Text Format`= factor(`Day of the Week Text Format`, levels=c(
        "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
      )
      )
      )
    
    FDW <- ggplot(
      fatalday_count,
      mapping = aes(
        x = `Day of the Week Text Format`,
        y = n,
        text = paste0(
          "Day: ",
          `Day of the Week Text Format`,
          "<br>Count: ",
          n,
          "<br>Percentage: ",
          round(n / sum(n) * 100, 1),
          "%"
        )
      )
    ) +
      geom_col(fill = "gray") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        plot.title = element_text(
          hjust = 0.5
        )
      ) +
      labs(
        title = "Day of the Week",
        y = "Count",
        x = " "
      )
    
    ggplotly(
      FDW,
      tooltip = "text"
    )
    
    #day of the week end brackets
  })
  
  #time of day
  output$timePlot <- renderPlotly({
    
    TD <- ggplot(
      time_count,
      aes(
        x = timebins,
        y = n,
        text = paste0(
          "Hour: ",
          timebins,
          "<br>Count: ",
          n,
          "<br>Percentage: ",
          round(n / sum(n) * 100, 1),
          "%"
        )
      )
    ) +
      geom_col(fill = "darkolivegreen") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        plot.title = element_text(
          hjust = 0.5
        )
      ) +
      labs(
        title = "Time of Day",
        y = "Count",
        x = " "
      )
    ggplotly(
      TD,
      tooltip = "text"
    )
    #time of day end brackets
  })
  
  #fatal time of day
  output$timefatalPlot <- renderPlotly({
    
    fataltime_count <- individualdata %>%
      filter(`Injury Status` == "K") %>%
      mutate(
        fataltimebins = case_when(
          `Hour of the Day` >= 0 & `Hour of the Day` < 3  ~ "12-2:59 am",
          `Hour of the Day` >= 3 & `Hour of the Day` < 6  ~ "3-5:59 am",
          `Hour of the Day` >= 6 & `Hour of the Day` < 9  ~ "6-8:59 am",
          `Hour of the Day` >= 9 & `Hour of the Day` < 12 ~ "9-11:59 am",
          `Hour of the Day` >= 12 & `Hour of the Day` < 15 ~ "12-2:59 pm",
          `Hour of the Day` >= 15 & `Hour of the Day` < 18 ~ "3-5:59 pm",
          `Hour of the Day` >= 18 & `Hour of the Day` < 21 ~ "6-8:59 pm",
          `Hour of the Day` >= 21 & `Hour of the Day` < 24 ~ "9-11:59 pm",
          TRUE ~ NA_character_
        )
      ) %>%
      count(fataltimebins, name = "n") %>%
      mutate(
        fataltimebins = factor(
          fataltimebins,
          levels = c(
            "12-2:59 am",
            "3-5:59 am",
            "6-8:59 am",
            "9-11:59 am",
            "12-2:59 pm",
            "3-5:59 pm",
            "6-8:59 pm",
            "9-11:59 pm"
          ),
          ordered = TRUE
        )
      ) %>%
      arrange(fataltimebins)
    
    FTD <- ggplot(
      fataltime_count,
      aes(
        x = fataltimebins,
        y = n,
        text = paste0(
          "Hour: ",
          fataltimebins,
          "<br>Count: ",
          n,
          "<br>Percentage: ",
          round(n / sum(n) * 100, 1),
          "%"
        )
      )
    ) +
      geom_col(fill = "darkolivegreen") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        plot.title = element_text(
          hjust = 0.5
        )
      ) +
      labs(
        title = "Time of Day",
        y = "Count",
        x = " "
      )
    ggplotly(
      FTD,
      tooltip = "text"
    )
    #time of day end brackets
  })
  
  #contributing action by driver
  output$contributingPie <- renderPlotly({
    
    plot_ly(
      data = contributing_action_count,
      labels = ~contributing_group,
      values = ~n,
      type = "pie",
      textinfo = "percent",
      textposition = "inside",
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Count: %{value}<br>",
        "Percent: %{percent}<extra></extra>"
      )
    ) |>
      layout(
        title = list(
          x = 0.5
        ),
        showlegend = TRUE
      )
    #end contributing action brackets
  })
  
  #fatal contributing action by driver
  output$contributingfatalPie <- renderPlotly({
    
    fatal_contributing_count <- individualdata %>%
      filter(`Injury Status` == "K") %>%
      mutate(
        `Driver Actions 1 Text Format` =
          na_if(trimws(`Driver Actions 1 Text Format`), ""),
        
        contributing_group = case_when(
          `Driver Actions 1 Text Format` %in% c(
            "Wrong Side or Wrong Way",
            "Operated Motor Vehicle in Reckless or Aggressive Manner",
            "Improper Backing",
            "Operated Motor Vehicle in Inattentive, Careless, Negligent, or Erratic Manner",
            "Swerved or Avoided Due to Wind, Slippery Surface, Motor Vehicle, Object, Non-Motorist in Roadway, etc.",
            "Disregarded Other Traffic Sign",
            "Disregarded Other Road Markings",
            "Over-Correcting/Over-Steering",
            "Not Applicable"
          ) ~ "Other Contributing Action",
          TRUE ~ `Driver Actions 1 Text Format`
        )
      ) %>%
      filter(!is.na(contributing_group)) %>%
      count(contributing_group, sort = TRUE)
    
    plot_ly(
      data = fatal_contributing_count,
      labels = ~contributing_group,
      values = ~n,
      type = "pie",
      textinfo = "percent",
      textposition = "inside",
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Count: %{value}<br>",
        "Percent: %{percent}<extra></extra>"
      )
    ) |>
      layout(
        title = list(
          x = 0.5
        ),
        showlegend = TRUE
      )
    #end contributing action brackets
  })
  
  #most harmful event
  output$harmfulPie <- renderPlotly({
    
    plot_ly(
      data = harmful_count,
      labels = ~HarmfulEventGroup,
      values = ~n,
      type = "pie",
      textinfo = "percent",
      textposition = "inside",
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Count: %{value}<br>",
        "Percent: %{percent}<extra></extra>"
      )
    ) |>
      layout(
        title = list(
          x = 0.5
        ),
        showlegend = TRUE
      )
    
    #end most harmful event bracket
  })
  
  #fatal most harmful event
  output$harmfulfatalPie <- renderPlotly({
    
    fatal_harmful_count <- individualdata %>%
      filter(`Injury Status` == "K") %>%
      mutate(
        HarmfulEventGroup = case_when(
          `Most harmful event` %in% c(
            "Ran Off Roadway Right",
            "Other Post, Pole, or Support",
            "Curb",
            "Other Non-motorist",
            "Not Applicable",
            "Separation of Units",
            "Ran Off Roadway Left",
            "Fence",
            "Fell/Jumped From Vehicle",
            "Other Non-Collision",
            "Reentering Roadway",
            "Other Non-Fixed Object",
            "Embankment",
            "Guardrail Face",
            "Traffic Sign Support",
            "Unknown",
            "Other Traffic Barrier",
            "Concrete Traffic Barrier",
            "Traffic Signal Support",
            "Cross Centerline",
            "Struck By Falling, Shifting Cargo or Anything Set in Motion By Motor Vehicle",
            "Light Support",
            "Guardrail End",
            "Jackknife",
            "Work Zone/Maintenance Equipment",
            "Cargo/Equipment Loss or Shift",
            "Immersion, Full or Partial",
            "Fire / Explosion",
            "Animal (live)",
            "Ditch",
            "Bridge Pier or Support",
            "Bridge Rail",
            "Equipment Failure (blown tire, brake failure, etc.)",
            "Cross Median"
          ) ~ "Other Harmful Event",
          #all else 
          TRUE ~ `Most harmful event`
        )
    ) %>%
  filter(!is.na(HarmfulEventGroup)) %>%
  count(`HarmfulEventGroup`, sort=TRUE)
    
    plot_ly(
      data = fatal_harmful_count,
      labels = ~HarmfulEventGroup,
      values = ~n,
      type = "pie",
      textinfo = "percent",
      textposition = "inside",
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Count: %{value}<br>",
        "Percent: %{percent}<extra></extra>"
      )
    ) |>
      layout(
        title = list(
          x = 0.5
        ),
        showlegend = TRUE
      )
    
    #end fatal most harmful event bracket
  })
  
  #crash map
  output$overviewMap <- renderLeaflet({
    
    req(input$mapType)
    
    if (input$mapType == "crashes") {
    mapdata <- crashdata %>%
      filter(
        !is.na(latitude),
        !is.na(longitude)
      )
   
    leaflet(data = mapdata) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      addPolygons(
        data = newhaven,
        color = "black",
        weight = 3,
        fill = FALSE
      ) %>%
      
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 3,
        color = "steelblue",
        fillColor = "steelblue",
        fillOpacity = 0.6,
        stroke = FALSE,
        popup = ~paste0(
          "<b>Crash Id:</b> ", CrashId,
          "<br><b>Date:</b> ", `Date Of Crash`,
          "<br><b>Day of the Week:</b> ", `Day of the Week Text Format`,
          "<br><b>Time of Crash:</b> ", `Time of Crash`,
          "<br><b>Highest Severity:</b> ", `Most Severe Injury Text Format`
        )
      )
    }
    
    else if (input$mapType == "severity") {
      
      leaflet(data = severitydata) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          data = newhaven,
          color = "black",
          weight = 3,
          fill = FALSE
        ) %>%
        addPolylines(
          color = ~severityPal(SeverityIndex_5category),
          weight=3,
          opacity=0.9,
          popup=~paste0("<br><b>Severity Index Score:</b> ", SeverityIndex,
                 "<br><b>Street Name:</b> ", GEO_ADDR,
                 "<br><b>Number of Crashes on Street Segment: </b>", Join_Count
                 )
        ) %>%
        addLegend(
          position = "topright",
          colors = c(
            "gray",
            "white",
            "skyblue",
            "royalblue",
            "darkblue"
          ),
          labels = c(
            "Lowest Severity",
            "Lower Severity",
            "Moderate Severity",
            "Higher Severity",
            "Highest Severity"
          )
        )
      
    }
    else if (input$mapType == "fatalities") {
      mapdata <- fatalitiesdata %>%
        filter(
          !is.na(Latitude),
          !is.na(Longitude)
        )
      
      leaflet(data = mapdata) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        addPolygons(
          data = newhaven,
          color = "black",
          weight = 3,
          fill = FALSE
        ) %>%
        
        addCircleMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          radius = 4,
          color = ~fatalPal(Person_Type_Text_Format),
          fillColor = ~fatalPal(Person_Type_Text_Format),
          fillOpacity = 0.6,
          stroke = FALSE,
          popup=~paste0(
                         "<br><b>Person Type:</b> ", Person_Type_Text_Format,
                         "<br><b>Date of Crash:</b> ", Date_Of_Crash,
                         "<br><b>Time of Crash:</b> ", Time_of_Crash,
                         "<br><b>Age: </b> ", Age,
                         "<br><b>Driver Condition at Time of Crash:</b> ", Condition_at_Time_of_Crash_Text_Format,
                         "<br><b>Day of the Week:</b> ", Day_of_the_Week_Text_Format
            
            
          )
        ) %>%
        
        addLegend(position="topright",
                  pal=fatalPal,
                  values=~`Person_Type_Text_Format`,
                  title="Person Type")
    }
    
    else if (input$mapType == "pedestrian") {
      
      pedestrian_data <- individualdata %>%
        dplyr::filter(
          `Person Type Text Format` == "Pedestrian"
        ) %>%
        dplyr::select(
          CrashId,
          `Injury Status Text Format`
        ) %>%
        dplyr::left_join(
          crashdata %>%
            dplyr::select(
              CrashId,
              latitude,
              longitude,
              `Date Of Crash`,
              `Day of the Week Text Format`,
              `Time of Crash`
            ),
          by = "CrashId"
        ) %>%
        dplyr::filter(
          !is.na(latitude),
          !is.na(longitude)
        )
      
      leaflet(data = pedestrian_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        addPolygons(
          data = newhaven,
          color = "black",
          weight = 3,
          fill = FALSE
        ) %>%
        
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = 4,
          color = ~pedestrianSeverityPal(
            `Injury Status Text Format`
          ),
          fillColor = ~pedestrianSeverityPal(
            `Injury Status Text Format`
          ),
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 1,
          
          popup = ~paste0(
            "<b>Crash ID:</b> ", CrashId,
            "<br><b>Date:</b> ", `Date Of Crash`,
            "<br><b>Day of the Week:</b> ",
            `Day of the Week Text Format`,
            "<br><b>Time of Crash:</b> ",
            `Time of Crash`,
            "<br><b>Pedestrian Injury Severity:</b> ",
            `Injury Status Text Format`
          )
        ) %>%
        
        addLegend(
          position = "topright",
          pal = pedestrianSeverityPal,
          values = ~`Injury Status Text Format`,
          title = "Pedestrian Injury Severity",
          opacity = 1
        )
    }
    
    
      })
  #raw crash counts by neighborhood
  output$neighborhoodPlot <- renderPlotly({
    
    N <- ggplot(
      neighborhood_plot_data,
      aes(
        x = Neighborhood,
        y = MVA,
        text = paste0(
          "Neighborhood: ", Neighborhood,
          "<br>Number of Crashes: ", MVA
        )
      )
    ) +
      geom_col(fill = "skyblue") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        title = "Crashes by Neighborhood",
        x = "Neighborhood",
        y = "Count"
      )
    
    ggplotly(N, tooltip = "text")
    
      #raw crash count end 
  })
  
  #MVA per mile
  output$mvaMilePlot <- renderPlotly({
    
    CM <- ggplot(
      neighborhood_plot_data2,
      aes(
        x = Neighborhood,
        y = `MVA per Mile`,
        text = paste0(
          "Neighborhood: ", Neighborhood,
          "<br>Crashes per Mile: ", `MVA per Mile`
        )
      )
    ) +
      geom_col(fill = "skyblue") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        title = "Crashes per Mile of Roadway by Neighborhood",
        x = "Neighborhood",
        y = "Crashes per Mile"
      )
    
    ggplotly(CM, tooltip = "text")
  })
    #raw crash count end 
  #raw fatal counts (fatalPlot)
  output$fatalPlot <- renderPlotly({
    
    F <- ggplot(
      neighborhood_plot_data4,
      aes(
        x = Neighborhood,
        y = `Number of Fatals`,
        text = paste0(
          "Neighborhood: ", Neighborhood,
          "<br>Fatalities count: ", `Number of Fatals`
        )
      )
    ) +
      geom_col(fill = "cornflowerblue") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        title = "Number of Fatalities by Neighborhood",
        x = "Neighborhood",
        y = "Count"
      )
    
    ggplotly(F, tooltip = "text")
  })
  
  #fatal per mile
  output$fatalMilePlot <- renderPlotly({
    
    FM <- ggplot(
      neighborhood_plot_data3,
      aes(
        x = Neighborhood,
        y = `Fatal per mile`,
        text = paste0(
          "Neighborhood: ", Neighborhood,
          "<br>Fatalities per Mile: ", `Fatal per mile`
        )
      )
    ) +
      geom_col(fill = "cornflowerblue") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        title = "Fatalities per Mile of Roadway by Neighborhood",
        x = "Neighborhood",
        y = "Fatalities per Mile"
      )
    
    ggplotly(FM, tooltip = "text")
  })
  
#recent crashes live 
  
  # Refresh general crash data every hour
  recent_crashes_live <- reactivePoll(
    intervalMillis = 3600000,
    session = session,
    
    checkFunc = function() {
      floor(as.numeric(Sys.time()) / 3600)
    },
    
    valueFunc = function() {
      get_live_newhaven_crashes()
    }
  )
  
  
  # Filter the correct dataset based on crash type and time period
  filtered_recent_crashes <- reactive({
    
    req(input$crashType, input$dayRange)
    
    number_of_days <- as.numeric(input$dayRange)
    cutoff_date <- Sys.Date() - number_of_days
  
      
      recent_data <- recent_crashes_live() %>%
        filter(
          CrashDate >= cutoff_date,
          CrashDate <= Sys.Date()
        )
      
      if (input$crashType == "Fatal Crashes") {
        
        recent_data <- recent_data %>%
          filter(
            MostSevereInjury == "Fatal Injury (K)"
          )
      }
    
    recent_data
  })
  
#recent crashes map
  
  output$recentCrashMap <- renderLeaflet({
    
    req(input$crashType)
    
    map_data <- filtered_recent_crashes()
    
    shiny::validate(
      shiny::need(
        nrow(map_data) > 0,
        paste0(
          "No ",
          tolower(input$crashType),
          " were reported during the selected period."
        )
      )
    )
  
      
#fatal and injury
      
      leaflet(data = map_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        addPolygons(
          data = newhaven,
          color = "black",
          weight = 2,
          fill = FALSE
        ) %>%
        
        addCircleMarkers(
          radius = 4,
          color = ~recentSeverityPal(MostSevereInjury),
          fillColor = ~recentSeverityPal(MostSevereInjury),
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 1,
          
          popup = ~paste0(
            "<b>Crash ID:</b> ", CrashID,
            "<br><b>Date:</b> ", CrashDate,
            
            "<br><b>Time:</b> ",
            ifelse(
              is.na(CrashTimeHour),
              "Not reported",
              paste0(sprintf("%02d", CrashTimeHour), ":00")
            ),
            
            "<br><b>Injury Severity:</b> ",
            ifelse(
              is.na(MostSevereInjury) |
                MostSevereInjury == "",
              "Not reported",
              MostSevereInjury
            ),
            
            "<br><b>Road:</b> ",
            ifelse(
              is.na(NameOfRoadway) |
                NameOfRoadway == "",
              "Not reported",
              NameOfRoadway
            ),
            
            "<br><b>Cross Street:</b> ",
            ifelse(
              is.na(NameOfIntersectingRoadway) |
                NameOfIntersectingRoadway == "",
              "Not reported",
              NameOfIntersectingRoadway
            ),
            
            "<br><b>Specific Location:</b> ",
            ifelse(
              is.na(CrashSpecificLocation) |
                CrashSpecificLocation == "",
              "Not reported",
              CrashSpecificLocation
            ) 
          )
        ) %>%
      addLegend(
          position = "topright",
          pal = recentSeverityPal,
          values = ~MostSevereInjury,
          title = "Crash Injury Severity",
          opacity = 1
        )
    })
  
#recent table
  
  output$recentCrashTable <- renderDT({
    
    req(input$crashType)
    
    table_source <- filtered_recent_crashes()
    
    shiny::validate(
      shiny::need(
        nrow(table_source) > 0,
        "No records were reported during the selected period."
      )
    )
    
    if (input$crashType == "Pedestrian Injury and Fatalities") {
      
      table_data <- table_source %>%
        sf::st_drop_geometry() %>%
        
        dplyr::mutate(
          Time = ifelse(
            is.na(CrashTimeHour),
            "Not reported",
            paste0(sprintf("%02d", CrashTimeHour), ":00")
          )
        ) %>%
        
        dplyr::arrange(
          dplyr::desc(CrashDate),
          dplyr::desc(CrashTimeHour)
        ) %>%
        
        dplyr::select(
          `Crash ID` = CrashID,
          Date = CrashDate,
          Time,
          `Pedestrian Injury Severity` = InjuryStatus,
          Age,
          Gender,
          Road = NameOfRoadway,
          `Specific Location` = CrashSpecificLocation
        )
      
    } else {
      
      table_data <- table_source %>%
        sf::st_drop_geometry() %>%
        
        dplyr::mutate(
          Time = ifelse(
            is.na(CrashTimeHour),
            "Not reported",
            paste0(sprintf("%02d", CrashTimeHour), ":00")
          )
        ) %>%
        
        dplyr::arrange(
          dplyr::desc(CrashDate),
          dplyr::desc(CrashTimeHour)
        ) %>%
        
        dplyr::select(
          `Crash ID` = CrashID,
          Date = CrashDate,
          Time,
          `Injury Severity` = MostSevereInjury,
          Road = NameOfRoadway,
          `Cross Street` = NameOfIntersectingRoadway,
          `Specific Location` = CrashSpecificLocation,
          Weather = WeatherCondition,
          Light = LightCondition,
          `Road Surface` = TrafficSurfaceCondition,
          `First Harmful Event` = FirstHarmfulEvent
        )
    }
    
    table_data <- table_data %>%
      dplyr::mutate(
        dplyr::across(
          where(is.character),
          ~dplyr::if_else(
            is.na(.) | . == "",
            "Not reported",
            .
          )
        )
      )
    
    DT::datatable(
      table_data,
      rownames = FALSE,
      filter = "top",
      
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        order = list(
          list(1, "desc")
        )
      )
    )
  }) 
  
  
#title
  
  output$recentCrashTitle <- renderText({
    
    req(input$crashType, input$dayRange)
    
    time_label <- dplyr::case_when(
      input$dayRange == "30"  ~ "Last 30 Days",
      input$dayRange == "90"  ~ "Last 90 Days",
      input$dayRange == "182" ~ "Last 6 Months",
      input$dayRange == "365" ~ "Last 12 Months",
      input$dayRange == "730" ~ "Last 24 Months",
      TRUE ~ paste0("Last ", input$dayRange, " Days")
    )
    
    paste0(
      input$crashType,
      " — ",
      time_label
    )
  })
  

  
  output$map_description <- renderUI({
      
      req(input$mapType)
      
      if (input$mapType == "severity") {
        
        tagList(
          p(
            "The Severity Index Map identifies 500-foot roadway segments with crashes involving greater levels of injury severity. Only crashes with at least 1 possible injury were included in this map"
          ),
          
          tags$strong(
            "Severity Index = (12 × Number of Fatals) + (3 × Number of Suspected Serious Injuries) + (1 × Number of Suspected Minor or Possible Injuries)"
          ),
          
          p(
            "Higher SI values indicate 500-foot roadway segments with a greater concentration of severe crash outcomes."
          )
        )
        
      } else if (input$mapType == "fatalities") {
        
        p("This map displays the locations of all fatalities in New Haven.")
        
      } else if (input$mapType == "pedestrian") {
        
        p(
          "This map displays crashes involving a pedestrian injury or fatality, with any injury type, including no injuries"
        )
        
      } else {
        
        p(
          "This map displays crashes resulting in at least one possible injury in New Haven."
        )
      }
    })
    
} #server end bracket
get_live_person_types <- function() {
  
  batch_size <- 500
  offset <- 0
  person_batches <- list()
  
  repeat {
    
    response <- httr2::request(uconn_person_url) |>
      httr2::req_url_query(
        where = paste(
          "CrashTownName = 'New Haven'",
          "AND CrashDateYear >= 2021",
          "AND LawEnforcementAgencyName = 'New Haven PD'"
        ),
        outFields = "CrashID,PersonType",
        returnGeometry = "false",
        orderByFields = "CrashID",
        resultOffset = offset,
        resultRecordCount = batch_size,
        f = "json"
      ) |>
      httr2::req_timeout(60) |>
      httr2::req_perform()
    
    person_json <- jsonlite::fromJSON(
      httr2::resp_body_string(response),
      simplifyVector = FALSE
    )
    
    if (!is.null(person_json$error)) {
      stop(person_json$error$message)
    }
    
    features <- person_json$features
    
    if (length(features) == 0) {
      break
    }
    
    person_batch <- dplyr::bind_rows(
      lapply(
        features,
        function(feature) {
          as.data.frame(
            feature$attributes,
            stringsAsFactors = FALSE
          )
        }
      )
    )
    
    person_batches[[length(person_batches) + 1]] <- person_batch
    
    if (length(features) < batch_size) {
      break
    }
    
    offset <- offset + batch_size
  }
  
  dplyr::bind_rows(person_batches) |>
    dplyr::transmute(
      CrashID = as.character(CrashID),
      PersonType = as.character(PersonType)
    ) |>
    dplyr::filter(
      !is.na(CrashID),
      !is.na(PersonType),
      PersonType != ""
    ) |>
    dplyr::group_by(CrashID) |>
    dplyr::summarise(
      PersonType = paste(
        sort(unique(PersonType)),
        collapse = ", "
      ),
      .groups = "drop"
    )
}
#launching server
shinyApp(ui, server)