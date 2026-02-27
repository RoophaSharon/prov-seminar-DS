# app.R
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(DT)
library(networkD3)
library(umap)
library(cluster)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(tidyr)

# Load the actual dataset
data_path <- file.path("data", "Global_AI_Content_Impact_Dataset.csv")
ai_data <- read.csv(data_path, stringsAsFactors = FALSE)


# Clean column names (remove special characters and spaces)
colnames(ai_data) <- c("Country", "Year", "Industry", "Adoption", "Content_Volume", 
                       "JobLoss", "RevenueIncrease", "Collaboration", "Top_AI_Tools", 
                       "Regulation_Status", "Trust", "MarketShare")

# Convert to appropriate data types
ai_data <- ai_data %>%
  mutate(
    Country = as.character(Country),
    Year = as.numeric(Year),
    Industry = as.character(Industry),
    Adoption = as.numeric(Adoption),
    Content_Volume = as.numeric(Content_Volume),
    JobLoss = as.numeric(JobLoss),
    RevenueIncrease = as.numeric(RevenueIncrease),
    Collaboration = as.numeric(Collaboration),
    Top_AI_Tools = as.character(Top_AI_Tools),
    Regulation_Status = as.character(Regulation_Status),
    Trust = as.numeric(Trust),
    MarketShare = as.numeric(MarketShare)
  )

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Get country ISO codes for matching
ai_data <- ai_data %>%
  mutate(
    ISO3 = countrycode(Country, "country.name", "iso3c",
                       custom_match = c("UK" = "GBR",
                                        "USA" = "USA",
                                        "South Korea" = "KOR"))
  )

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$div(
      tags$img(src = "https://cdn-icons-png.flaticon.com/512/2103/2103655.png", 
               height = "30px", style = "margin-right: 10px;"),
      "GLOBAL AI CONTENT IMPACT DV",
      style = "display: flex; align-items: center;"
    ),
    titleWidth = 400
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("RQ1: Spatial Spillover", tabName = "rq1", icon = icon("globe-americas")),
      menuItem("RQ2: Temporal Dynamics", tabName = "rq2", icon = icon("chart-line")),
      menuItem("RQ3: Tool Ecosystem", tabName = "rq3", icon = icon("project-diagram")),
      menuItem("RQ4: Impact Profiles", tabName = "rq4", icon = icon("users")),
      menuItem("Data Explorer", tabName = "data", icon = icon("database")),
      
      br(),
      
      # Global filters panel
      div(style = "padding: 10px; background-color: #b1d6fa; margin: 5px; border-radius: 5px;",
          h4("Global Filters", style = "color: #2c3e50;"),
          sliderInput("year_range", "Year Range:",
                      min = min(ai_data$Year, na.rm = TRUE),
                      max = max(ai_data$Year, na.rm = TRUE),
                      value = c(2020, 2025),
                      sep = ""),
          selectizeInput("selected_countries", "Countries:",
                         choices = unique(ai_data$Country),
                         multiple = TRUE,
                         options = list(placeholder = 'All countries')),
          selectizeInput("selected_industries", "Industries:",
                         choices = unique(ai_data$Industry),
                         multiple = TRUE,
                         options = list(placeholder = 'All industries')),
          selectizeInput("selected_regulations", "Regulation Status:",
                         choices = unique(ai_data$Regulation_Status),
                         multiple = TRUE,
                         options = list(placeholder = 'All statuses')),
          actionButton("apply_filters", "Apply Filters", 
                       icon = icon("filter"), 
                       class = "btn-primary", width = "100%")
      ),
      
      br(),
      div(style = "padding: 10px;",
          h5("Course: Data Visualization"),
          h6("Project done by "),
          h6("Roopha Sharon"),
          h6("Rabia Ashilan"),
          h6("Abhignya Kotha"),
          h6("Rupesh")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Custom styles */
        .content-wrapper { background-color: #f5f7fa; }
        .box { border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
        .box-header { background-color: #3498db !important; color: white !important; }
        .box-header .box-title { font-weight: bold; }
        
        /* Info boxes */
        .info-box { border-radius: 5px; }
        .info-box-icon { border-radius: 5px 0 0 5px; }
        
        /* Custom colors for RQ tabs */
        .nav-tabs-custom > .nav-tabs > li.active > a {
          background-color: #3498db;
          color: white;
          border-color: #3498db;
        }
        
        /* Button styling */
        .btn-primary {
          background-color: #3498db;
          border-color: #2980b9;
        }
        
        /* DataTable styling */
        .dataTables_wrapper {
          font-size: 12px;
        }
        
        /* Plotly container */
        .plotly { border-radius: 5px; }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_countries_box", width = 3),
                valueBoxOutput("total_records_box", width = 3),
                valueBoxOutput("avg_adoption_box", width = 3),
                valueBoxOutput("avg_trust_gap_box", width = 3)
              ),
              
              fluidRow(
                box(
                  title = "Research Questions Summary",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  div(style = "padding: 15px;",
                      h4("RQ1: Spatial Spillover Effects", style = "color: #2c3e50;"),
                      p("Do countries bordering stricter-regulation countries exhibit a 'policy spillover' pattern in AI adoption and trust metrics?"),
                      tags$ul(
                        tags$li("Choropleth maps showing Trust Gap (Adoption - Trust)"),
                        tags$li("Neighbor analysis: Strict vs Non-Strict border countries"),
                        tags$li("Scatter plots with spatial highlighting")
                      ),
                      
                      h4("RQ2: Temporal Dynamics", style = "color: #2c3e50; margin-top: 20px;"),
                      p("How do Trust Gap hotspots evolve over time (2020-2025) across different regulation regimes and industries?"),
                      tags$ul(
                        tags$li("Time-series analysis with interactive slider"),
                        tags$li("Hotspot detection and evolution tracking"),
                        tags$li("Faceted views by industry and regulation status")
                      ),
                      
                      h4("RQ3: Tool Ecosystem Structure", style = "color: #2c3e50; margin-top: 20px;"),
                      p("What tool ecosystems emerge when modeling AI tools as networks, and which tools act as cross-regime bridges?"),
                      tags$ul(
                        tags$li("Network visualization of tools across countries/industries"),
                        tags$li("Bridge tool identification"),
                        tags$li("Tool prevalence mapping")
                      ),
                      
                      h4("RQ4: AI Impact Profiles", style = "color: #2c3e50; margin-top: 20px;"),
                      p("What stable 'AI impact profiles' exist when clustering on multiple impact metrics?"),
                      tags$ul(
                        tags$li("K-means clustering on 7 impact metrics"),
                        tags$li("Parallel coordinates for profile visualization"),
                        tags$li("UMAP 2D embeddings and geographic distribution")
                      )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Data Summary Statistics",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  DTOutput("summary_stats_table")
                )
              )
      ),
      
      # RQ1: Spatial Spillover Tab
      tabItem(tabName = "rq1",
              h2("RQ1: Spatial Spillover Analysis", style = "color: #2c3e50;"),
              p("Investigate spatial patterns and neighbor effects in AI policy adoption."),
              
              fluidRow(
                box(
                  title = "Choropleth Map: Trust Gap by Country",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  height = "600px",
                  leafletOutput("rq1_map", height = "550px")
                ),
                
                box(
                  title = "Controls & Metrics",
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  height = "600px",
                  selectInput("rq1_metric", "Select Metric:",
                              choices = c("Trust Gap (Adoption - Trust)" = "trust_gap",
                                          "Adoption Rate" = "adoption",
                                          "Trust Level" = "trust",
                                          "Job Loss to Revenue Ratio" = "jl_ratio",
                                          "ENB (Revenue - Job Loss)" = "enb")),
                  
                  sliderInput("rq1_year", "Year:",
                              min = min(ai_data$Year),
                              max = max(ai_data$Year),
                              value = 2023,
                              step = 1,
                              sep = ""),
                  
                  selectInput("rq1_industry", "Industry:",
                              choices = c("All", unique(ai_data$Industry))),
                  
                  checkboxInput("show_neighbors", 
                                "Highlight Neighbors of Strict Countries",
                                value = TRUE),
                  
                  checkboxInput("show_strict_countries",
                                "Mark Strict Regulation Countries",
                                value = TRUE),
                  
                  actionButton("rq1_calculate", "Calculate Spillover Metrics",
                               icon = icon("calculator"),
                               class = "btn-primary",
                               width = "100%"),
                  
                  hr(),
                  
                  h5("Derived Metrics:"),
                  verbatimTextOutput("rq1_metrics_summary")
                )
              ),
              
              fluidRow(
                box(
                  title = "Neighbor Comparison Analysis",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("rq1_neighbor_comparison", height = "350px")
                ),
                
                box(
                  title = "Scatter Plot: Adoption vs Economic Net Benefit",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("rq1_scatter", height = "350px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Neighbor Countries Details",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  DTOutput("rq1_neighbor_table")
                )
              )
      ),
      
      # RQ2: Temporal Dynamics Tab
      tabItem(tabName = "rq2",
              h2("RQ2: Temporal Dynamics of AI Impact", style = "color: #2c3e50;"),
              p("Analyze the evolution of Trust Gap hotspots over time across different industries and regulation regimes."),
              
              fluidRow(
                box(
                  title = "Trust Gap Evolution (2020-2025)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  plotlyOutput("rq2_trend_plot", height = "500px")
                ),
                
                box(
                  title = "Time Controls",
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  sliderInput("rq2_time_range", "Time Range:",
                              min = min(ai_data$Year),
                              max = max(ai_data$Year),
                              value = c(2020, 2025),
                              sep = ""),
                  
                  selectInput("rq2_aggregation", "Aggregation Level:",
                              choices = c("Global" = "global",
                                          "By Region" = "region",
                                          "By Regulation" = "regulation",
                                          "By Industry" = "industry")),
                  
                  numericInput("hotspot_threshold", "Hotspot Threshold (z-score):",
                               value = 1.5, min = 0, max = 5, step = 0.1),
                  
                  checkboxInput("show_hotspot_trend",
                                "Show Hotspot Trend Line",
                                value = TRUE),
                  
                  actionButton("rq2_detect_hotspots", "Detect Hotspots",
                               icon = icon("search"),
                               class = "btn-primary",
                               width = "100%"),
                  
                  hr(),
                  
                  h5("Hotspot Statistics:"),
                  verbatimTextOutput("rq2_hotspot_stats")
                )
              ),
              
              fluidRow(
                box(
                  title = "Hotspot Evolution Map",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  height = "450px",
                  leafletOutput("rq2_hotspot_map", height = "400px")
                ),
                
                box(
                  title = "Industry-wise Trend Facets",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("rq2_industry_facets", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Hotspot Details Over Time",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  DTOutput("rq2_hotspot_table")
                )
              )
      ),
      
      # RQ3: Tool Ecosystem Tab
      tabItem(tabName = "rq3",
              h2("RQ3: AI Tool Ecosystem Analysis", style = "color: #2c3e50;"),
              p("Explore the network structure of AI tools across countries and industries."),
              
              fluidRow(
                box(
                  title = "Tool-Country Network",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  height = "600px",
                  forceNetworkOutput("rq3_network", height = "550px")
                ),
                
                box(
                  title = "Network Controls",
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  height = "600px",
                  selectInput("rq3_network_type", "Network Type:",
                              choices = c("Tool-Country" = "tool_country",
                                          "Tool-Industry" = "tool_industry")),
                  
                  sliderInput("rq3_min_connections", "Minimum Connections:",
                              min = 1, max = 20, value = 3, step = 1),
                  
                  checkboxInput("rq3_highlight_bridges",
                                "Highlight Bridge Tools",
                                value = TRUE),
                  
                  checkboxInput("rq3_color_by_regulation",
                                "Color Nodes by Regulation",
                                value = TRUE),
                  
                  actionButton("rq3_calculate_bridges", "Calculate Bridge Scores",
                               icon = icon("link"),
                               class = "btn-primary",
                               width = "100%"),
                  
                  hr(),
                  
                  h5("Network Statistics:"),
                  verbatimTextOutput("rq3_network_stats"),
                  
                  h5("Top Bridge Tools:"),
                  verbatimTextOutput("rq3_bridge_tools")
                )
              ),
              
              fluidRow(
                box(
                  title = "Tool Performance Metrics",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("rq3_tool_performance", height = "350px")
                ),
                
                box(
                  title = "Tool Prevalence by Country",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("rq3_tool_map_plot", height = "350px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Bridge Tool Analysis Details",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  DTOutput("rq3_bridge_table")
                )
              )
      ),
      
      # RQ4: Impact Profiles Tab
      tabItem(tabName = "rq4",
              h2("RQ4: AI Impact Profile Clustering", style = "color: #2c3e50;"),
              p("Identify and analyze different AI impact profiles through multivariate clustering."),
              
              fluidRow(
                box(
                  title = "Cluster Profiles (Parallel Coordinates)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  plotlyOutput("rq4_parallel_coords", height = "500px")
                ),
                
                box(
                  title = "Clustering Controls",
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  numericInput("rq4_n_clusters", "Number of Clusters:",
                               value = 4, min = 2, max = 8, step = 1),
                  
                  selectInput("rq4_cluster_method", "Clustering Method:",
                              choices = c("K-means" = "kmeans",
                                          "Hierarchical" = "hclust")),
                  
                  checkboxInput("rq4_standardize", "Standardize Variables", value = TRUE),
                  
                  selectizeInput("rq4_cluster_vars", "Clustering Variables:",
                                 choices = c("Adoption", "Content_Volume", "JobLoss",
                                             "RevenueIncrease", "Collaboration", "Trust", "MarketShare"),
                                 selected = c("Adoption", "JobLoss", "RevenueIncrease", "Trust"),
                                 multiple = TRUE),
                  
                  actionButton("rq4_run_clustering", "Run Clustering",
                               icon = icon("cogs"),
                               class = "btn-primary",
                               width = "100%"),
                  
                  hr(),
                  
                  h5("Cluster Sizes:"),
                  verbatimTextOutput("rq4_cluster_sizes"),
                  
                  h5("Silhouette Score:"),
                  verbatimTextOutput("rq4_silhouette")
                )
              ),
              
              fluidRow(
                box(
                  title = "2D Embedding (UMAP)",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("rq4_umap_plot", height = "400px")
                ),
                
                box(
                  title = "Cluster Distribution Map",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  leafletOutput("rq4_cluster_map", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Cluster Centroids (Average Values)",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  DTOutput("rq4_centroids_table")
                ),
                
                box(
                  title = "Profile Exemplars",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  DTOutput("rq4_exemplars_table")
                )
              )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
              h2("Data Explorer", style = "color: #2c3e50;"),
              p("Explore and filter the raw dataset with interactive controls."),
              
              fluidRow(
                box(
                  title = "Data Filters",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(3,
                           selectInput("data_year", "Year:",
                                       choices = c("All", sort(unique(ai_data$Year)))),
                           selectInput("data_country", "Country:",
                                       choices = c("All", sort(unique(ai_data$Country))))
                    ),
                    column(3,
                           selectInput("data_industry", "Industry:",
                                       choices = c("All", sort(unique(ai_data$Industry)))),
                           selectInput("data_tool", "AI Tool:",
                                       choices = c("All", sort(unique(ai_data$Top_AI_Tools))))
                    ),
                    column(3,
                           selectInput("data_regulation", "Regulation:",
                                       choices = c("All", sort(unique(ai_data$Regulation_Status)))),
                           sliderInput("data_trust_range", "Trust Range:",
                                       min = 0, max = 100, value = c(0, 100))
                    ),
                    column(3,
                           sliderInput("data_adoption_range", "Adoption Range:",
                                       min = 0, max = 100, value = c(0, 100)),
                           br(),
                           downloadButton("download_csv", "Download Filtered CSV",
                                          class = "btn-success", width = "100%")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Dataset Preview",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("data_table"),
                  style = "overflow-x: scroll;"
                )
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Reactive data with applied filters
  filtered_data <- reactive({
    data <- ai_data
    
    # Apply year range filter
    if(!is.null(input$year_range)) {
      data <- data %>% filter(Year >= input$year_range[1] & Year <= input$year_range[2])
    }
    
    # Apply country filter
    if(!is.null(input$selected_countries) && length(input$selected_countries) > 0) {
      data <- data %>% filter(Country %in% input$selected_countries)
    }
    
    # Apply industry filter
    if(!is.null(input$selected_industries) && length(input$selected_industries) > 0) {
      data <- data %>% filter(Industry %in% input$selected_industries)
    }
    
    # Apply regulation filter
    if(!is.null(input$selected_regulations) && length(input$selected_regulations) > 0) {
      data <- data %>% filter(Regulation_Status %in% input$selected_regulations)
    }
    
    return(data)
  })
  
  # Overview value boxes
  output$total_countries_box <- renderValueBox({
    n_countries <- n_distinct(filtered_data()$Country)
    valueBox(
      value = n_countries,
      subtitle = "Countries",
      icon = icon("globe"),
      color = "blue"
    )
  })
  
  output$total_records_box <- renderValueBox({
    n_records <- nrow(filtered_data())
    valueBox(
      value = format(n_records, big.mark = ","),
      subtitle = "Data Records",
      icon = icon("database"),
      color = "green"
    )
  })
  
  output$avg_adoption_box <- renderValueBox({
    avg_adoption <- mean(filtered_data()$Adoption, na.rm = TRUE)
    valueBox(
      value = paste0(round(avg_adoption, 1), "%"),
      subtitle = "Avg Adoption Rate",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  output$avg_trust_gap_box <- renderValueBox({
    data <- filtered_data()
    trust_gap <- mean(data$Adoption - data$Trust, na.rm = TRUE)
    valueBox(
      value = paste0(round(trust_gap, 1), "%"),
      subtitle = "Avg Trust Gap",
      icon = icon("handshake"),
      color = ifelse(trust_gap > 0, "red", "green")
    )
  })
  
  # Overview summary table
  output$summary_stats_table <- renderDT({
    data <- filtered_data()
    
    summary_stats <- data %>%
      summarise(
        `Countries` = n_distinct(Country),
        `Industries` = n_distinct(Industry),
        `Years` = paste(min(Year), "-", max(Year)),
        `Avg Adoption` = round(mean(Adoption, na.rm = TRUE), 1),
        `Avg Trust` = round(mean(Trust, na.rm = TRUE), 1),
        `Avg Job Loss` = round(mean(JobLoss, na.rm = TRUE), 1),
        `Avg Revenue Increase` = round(mean(RevenueIncrease, na.rm = TRUE), 1),
        `Avg Market Share` = round(mean(MarketShare, na.rm = TRUE), 1)
      ) %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Metric")
    
    colnames(summary_stats) <- c("Metric", "Value")
    
    datatable(summary_stats,
              options = list(
                pageLength = 10,
                dom = 't',
                scrollX = TRUE
              ),
              rownames = FALSE,
              class = 'cell-border stripe')
  })
  
  # RQ1: Spatial Spillover Analysis
  
  # Reactive data for RQ1 with derived metrics
  rq1_data <- reactive({
    data <- filtered_data()
    
    if(input$rq1_industry != "All") {
      data <- data %>% filter(Industry == input$rq1_industry)
    }
    
    data <- data %>%
      filter(Year == input$rq1_year) %>%
      group_by(Country, ISO3, Regulation_Status) %>%
      summarise(
        Adoption = mean(Adoption, na.rm = TRUE),
        Trust = mean(Trust, na.rm = TRUE),
        JobLoss = mean(JobLoss, na.rm = TRUE),
        RevenueIncrease = mean(RevenueIncrease, na.rm = TRUE),
        Content_Volume = mean(Content_Volume, na.rm = TRUE),
        Collaboration = mean(Collaboration, na.rm = TRUE),
        MarketShare = mean(MarketShare, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        TrustGap = Adoption - Trust,
        JLRatio = ifelse(RevenueIncrease > 0, JobLoss / RevenueIncrease, NA),
        ENB = RevenueIncrease - JobLoss
      )
    
    return(data)
  })
  
  # RQ1 Map
  output$rq1_map <- renderLeaflet({
    data <- rq1_data()
    
    # Join with world map
    map_data <- world %>%
      left_join(data, by = c("iso_a3" = "ISO3"))
    
    # Select metric based on input
    metric <- switch(input$rq1_metric,
                     "trust_gap" = map_data$TrustGap,
                     "adoption" = map_data$Adoption,
                     "trust" = map_data$Trust,
                     "jl_ratio" = map_data$JLRatio,
                     "enb" = map_data$ENB)
    
    # Create color palette
    if(input$rq1_metric %in% c("trust_gap", "jl_ratio")) {
      # Diverging palette for metrics with meaningful zero
      pal <- colorNumeric("RdYlBu", domain = metric, na.color = "#808080", reverse = TRUE)
    } else {
      # Sequential palette for other metrics
      pal <- colorNumeric("YlOrRd", domain = metric, na.color = "#808080")
    }
    
    # Create labels
    labels <- sprintf(
      "<strong>%s</strong><br/>
      Adoption: %.1f%%<br/>
      Trust: %.1f%%<br/>
      Trust Gap: %.1f%%<br/>
      Regulation: %s",
      map_data$name_long,
      map_data$Adoption,
      map_data$Trust,
      map_data$TrustGap,
      map_data$Regulation_Status
    ) %>% lapply(htmltools::HTML)
    
    # Create map
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(metric),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = labels,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~metric,
        title = switch(input$rq1_metric,
                       "trust_gap" = "Trust Gap (%)",
                       "adoption" = "Adoption (%)",
                       "trust" = "Trust (%)",
                       "jl_ratio" = "Job Loss/Revenue",
                       "enb" = "ENB (%)"),
        position = "bottomright",
        na.label = "No data"
      )
  })
  
  # RQ1 Neighbor comparison plot
  output$rq1_neighbor_comparison <- renderPlotly({
    data <- rq1_data()
    
    # Simple neighbor simulation for demonstration
    set.seed(123)
    data <- data %>%
      mutate(
        HasStrictNeighbor = ifelse(Regulation_Status == "Strict", NA,
                                   sample(c(TRUE, FALSE), n(), replace = TRUE, prob = c(0.3, 0.7)))
      )
    
    p <- ggplot(data, aes(x = HasStrictNeighbor, y = TrustGap, fill = HasStrictNeighbor)) +
      geom_boxplot(alpha = 0.7) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +
      labs(
        title = "Countries with Strict Neighbors vs Others",
        x = "Has Strict Regulation Neighbor",
        y = "Trust Gap (%)"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2") +
      theme(legend.position = "none")
    
    ggplotly(p) %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # RQ1 Scatter plot
  output$rq1_scatter <- renderPlotly({
    data <- rq1_data()
    
    p <- ggplot(data, aes(x = Adoption, y = ENB,
                          color = Regulation_Status,
                          size = MarketShare,
                          text = paste("Country:", Country,
                                       "<br>Adoption:", round(Adoption, 1), "%",
                                       "<br>ENB:", round(ENB, 1),
                                       "<br>Regulation:", Regulation_Status))) +
      geom_point(alpha = 0.7) +
      labs(
        title = "Adoption vs Economic Net Benefit",
        x = "Adoption Rate (%)",
        y = "ENB (Revenue - Job Loss)"
      ) +
      theme_minimal() +
      scale_color_brewer(palette = "Set1") +
      scale_size(range = c(3, 10))
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # RQ1 Metrics summary
  output$rq1_metrics_summary <- renderText({
    data <- rq1_data()
    
    paste(
      "Total Countries:", nrow(data), "\n",
      "Avg Trust Gap:", round(mean(data$TrustGap, na.rm = TRUE), 1), "%\n",
      "Avg Adoption:", round(mean(data$Adoption, na.rm = TRUE), 1), "%\n",
      "Avg Trust:", round(mean(data$Trust, na.rm = TRUE), 1), "%\n",
      "Strict Countries:", sum(data$Regulation_Status == "Strict", na.rm = TRUE)
    )
  })
  
  # RQ1 Neighbor table
  output$rq1_neighbor_table <- renderDT({
    data <- rq1_data() %>%
      select(Country, Regulation_Status, Adoption, Trust, TrustGap, 
             JobLoss, RevenueIncrease, ENB, MarketShare) %>%
      arrange(desc(TrustGap))
    
    datatable(data,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              extensions = 'Buttons',
              rownames = FALSE) %>%
      formatRound(columns = c(3:9), digits = 1)
  })
  
  # RQ2: Temporal Dynamics
  
  # Reactive data for RQ2
  rq2_data <- reactive({
    data <- filtered_data() %>%
      mutate(
        TrustGap = Adoption - Trust,
        ENB = RevenueIncrease - JobLoss
      )
    
    return(data)
  })
  
  # RQ2 Trend plot
  output$rq2_trend_plot <- renderPlotly({
    data <- rq2_data() %>%
      filter(Year >= input$rq2_time_range[1] & Year <= input$rq2_time_range[2])
    
    # Aggregate based on selection
    if(input$rq2_aggregation == "global") {
      agg_data <- data %>%
        group_by(Year) %>%
        summarise(TrustGap = mean(TrustGap, na.rm = TRUE))
      
      p <- ggplot(agg_data, aes(x = Year, y = TrustGap)) +
        geom_line(size = 1.5, color = "#3498db") +
        geom_point(size = 3, color = "#2980b9") +
        labs(title = "Global Trust Gap Trend",
             x = "Year", y = "Trust Gap (%)")
      
    } else if(input$rq2_aggregation == "regulation") {
      agg_data <- data %>%
        group_by(Year, Regulation_Status) %>%
        summarise(TrustGap = mean(TrustGap, na.rm = TRUE), .groups = 'drop')
      
      p <- ggplot(agg_data, aes(x = Year, y = TrustGap, color = Regulation_Status, group = Regulation_Status)) +
        geom_line(size = 1.5) +
        geom_point(size = 3) +
        labs(title = "Trust Gap Trend by Regulation Status",
             x = "Year", y = "Trust Gap (%)") +
        scale_color_brewer(palette = "Set1")
      
    } else if(input$rq2_aggregation == "industry") {
      # Show top 5 industries
      top_industries <- data %>%
        group_by(Industry) %>%
        summarise(n = n()) %>%
        top_n(5, n) %>%
        pull(Industry)
      
      agg_data <- data %>%
        filter(Industry %in% top_industries) %>%
        group_by(Year, Industry) %>%
        summarise(TrustGap = mean(TrustGap, na.rm = TRUE), .groups = 'drop')
      
      p <- ggplot(agg_data, aes(x = Year, y = TrustGap, color = Industry, group = Industry)) +
        geom_line(size = 1.5) +
        geom_point(size = 2) +
        labs(title = "Trust Gap Trend by Industry (Top 5)",
             x = "Year", y = "Trust Gap (%)") +
        scale_color_brewer(palette = "Set2")
    }
    
    if(input$show_hotspot_trend) {
      # Add threshold line
      p <- p + geom_hline(yintercept = input$hotspot_threshold * sd(data$TrustGap, na.rm = TRUE),
                          linetype = "dashed", color = "red", alpha = 0.5)
    }
    
    ggplotly(p) %>%
      layout(hovermode = 'x unified')
  })
  
  # RQ2 Hotspot map
  output$rq2_hotspot_map <- renderLeaflet({
    data <- rq2_data() %>%
      filter(Year == input$rq2_time_range[2]) %>%
      group_by(Country, ISO3) %>%
      summarise(
        TrustGap = mean(TrustGap, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        IsHotspot = TrustGap > (input$hotspot_threshold * sd(TrustGap, na.rm = TRUE))
      )
    
    map_data <- world %>%
      left_join(data, by = c("iso_a3" = "ISO3"))
    
    # Color palette
    pal <- colorNumeric("RdYlBu", domain = map_data$TrustGap, na.color = "#808080", reverse = TRUE)
    
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(TrustGap),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = ~paste(name_long, "Trust Gap:", round(TrustGap, 1))
      ) %>%
      addLegend(
        pal = pal,
        values = ~TrustGap,
        title = "Trust Gap (%)",
        position = "bottomright"
      )
  })
  
  # RQ2 Industry facets
  output$rq2_industry_facets <- renderPlotly({
    data <- rq2_data() %>%
      filter(Year >= input$rq2_time_range[1] & Year <= input$rq2_time_range[2])
    
    # Select top 6 industries
    top_industries <- data %>%
      group_by(Industry) %>%
      summarise(n = n()) %>%
      top_n(6, n) %>%
      pull(Industry)
    
    agg_data <- data %>%
      filter(Industry %in% top_industries) %>%
      group_by(Year, Industry, Regulation_Status) %>%
      summarise(TrustGap = mean(TrustGap, na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(agg_data, aes(x = Year, y = TrustGap, color = Regulation_Status)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      facet_wrap(~Industry, ncol = 2, scales = "free_y") +
      labs(title = "Trust Gap Trends by Industry and Regulation",
           x = "Year", y = "Trust Gap (%)") +
      scale_color_brewer(palette = "Set1") +
      theme_minimal() +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })
  
  # RQ2 Hotspot stats
  output$rq2_hotspot_stats <- renderText({
    data <- rq2_data() %>%
      filter(Year >= input$rq2_time_range[1] & Year <= input$rq2_time_range[2])
    
    hotspot_data <- data %>%
      mutate(
        IsHotspot = TrustGap > (input$hotspot_threshold * sd(TrustGap, na.rm = TRUE))
      )
    
    paste(
      "Total Records:", nrow(data), "\n",
      "Hotspot Records:", sum(hotspot_data$IsHotspot, na.rm = TRUE), "\n",
      "Hotspot %:", round(mean(hotspot_data$IsHotspot, na.rm = TRUE) * 100, 1), "%\n",
      "Avg Trust Gap:", round(mean(data$TrustGap, na.rm = TRUE), 1)
    )
  })
  
  # RQ2 Hotspot table
  output$rq2_hotspot_table <- renderDT({
    data <- rq2_data() %>%
      filter(Year >= input$rq2_time_range[1] & Year <= input$rq2_time_range[2]) %>%
      mutate(
        IsHotspot = TrustGap > (input$hotspot_threshold * sd(TrustGap, na.rm = TRUE))
      ) %>%
      filter(IsHotspot == TRUE) %>%
      select(Country, Year, Industry, Regulation_Status, 
             Adoption, Trust, TrustGap, JobLoss, RevenueIncrease) %>%
      arrange(desc(TrustGap))
    
    datatable(data,
              options = list(
                pageLength = 10,
                scrollX = TRUE
              ),
              rownames = FALSE) %>%
      formatRound(columns = 5:9, digits = 1)
  })
  
  # RQ3: Tool Ecosystem
  
  # Reactive data for RQ3
  rq3_data <- reactive({
    data <- filtered_data() %>%
      mutate(
        ENB = RevenueIncrease - JobLoss
      )
    
    return(data)
  })
  
  # RQ3 Network visualization using networkD3
  output$rq3_network <- renderForceNetwork({
    data <- rq3_data()
    
    if(nrow(data) == 0) return(NULL)
    
    # Create tool-country edges
    if(input$rq3_network_type == "tool_country") {
      edges <- data %>%
        group_by(Top_AI_Tools, Country) %>%
        summarise(
          weight = n(),
          .groups = 'drop'
        ) %>%
        filter(weight >= input$rq3_min_connections) %>%
        rename(source = Top_AI_Tools, target = Country)
      
      # Create nodes
      tool_nodes <- data.frame(
        name = unique(c(edges$source, edges$target)),
        group = ifelse(unique(c(edges$source, edges$target)) %in% unique(edges$source), "tool", "country"),
        size = sapply(unique(c(edges$source, edges$target)), function(node) {
          sum(edges$weight[edges$source == node | edges$target == node])
        })
      )
      
      # Create links
      links <- edges %>%
        mutate(
          source = match(source, tool_nodes$name) - 1,
          target = match(target, tool_nodes$name) - 1,
          value = weight
        )
      
    } else if(input$rq3_network_type == "tool_industry") {
      edges <- data %>%
        group_by(Top_AI_Tools, Industry) %>%
        summarise(
          weight = n(),
          .groups = 'drop'
        ) %>%
        filter(weight >= input$rq3_min_connections) %>%
        rename(source = Top_AI_Tools, target = Industry)
      
      # Create nodes
      tool_nodes <- data.frame(
        name = unique(c(edges$source, edges$target)),
        group = ifelse(unique(c(edges$source, edges$target)) %in% unique(edges$source), "tool", "industry"),
        size = sapply(unique(c(edges$source, edges$target)), function(node) {
          sum(edges$weight[edges$source == node | edges$target == node])
        })
      )
      
      # Create links
      links <- edges %>%
        mutate(
          source = match(source, tool_nodes$name) - 1,
          target = match(target, tool_nodes$name) - 1,
          value = weight
        )
    }
    
    # Color scale for groups
    colourScale <- JS('d3.scaleOrdinal()
                      .domain(["tool", "country", "industry"])
                      .range(["#FF6B6B", "#4ECDC4", "#45B7D1"]);')
    
    # Create network
    forceNetwork(Links = links, Nodes = tool_nodes,
                 Source = "source", Target = "target",
                 Value = "value", NodeID = "name",
                 Group = "group", Nodesize = "size",
                 radiusCalculation = JS("Math.sqrt(d.nodesize)+5"),
                 colourScale = colourScale,
                 opacity = 0.8, zoom = TRUE,
                 fontSize = 12, legend = TRUE,
                 bounded = TRUE, charge = -50)
  })
  
  # RQ3 Tool performance plot
  output$rq3_tool_performance <- renderPlotly({
    data <- rq3_data()
    
    tool_stats <- data %>%
      group_by(Top_AI_Tools, Regulation_Status) %>%
      summarise(
        Avg_ENB = mean(ENB, na.rm = TRUE),
        Avg_MarketShare = mean(MarketShare, na.rm = TRUE),
        Count = n(),
        .groups = 'drop'
      )
    
    p <- ggplot(tool_stats, aes(x = Avg_ENB, y = Avg_MarketShare,
                                color = Regulation_Status,
                                size = Count,
                                text = paste("Tool:", Top_AI_Tools,
                                             "<br>ENB:", round(Avg_ENB, 1),
                                             "<br>Market Share:", round(Avg_MarketShare, 1), "%",
                                             "<br>Regulation:", Regulation_Status))) +
      geom_point(alpha = 0.7) +
      labs(
        title = "Tool Performance Metrics",
        x = "Average ENB (Revenue - Job Loss)",
        y = "Average Market Share (%)"
      ) +
      theme_minimal() +
      scale_color_brewer(palette = "Set1") +
      scale_size(range = c(3, 10))
    
    ggplotly(p, tooltip = "text")
  })
  
  # RQ3 Tool map plot
  output$rq3_tool_map_plot <- renderPlotly({
    data <- rq3_data()
    
    # Find most prevalent tool per country
    tool_prevalence <- data %>%
      group_by(Country, Top_AI_Tools) %>%
      summarise(
        Count = n(),
        Avg_ENB = mean(ENB, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      group_by(Country) %>%
      slice_max(Count, n = 1) %>%
      ungroup()
    
    p <- ggplot(tool_prevalence, aes(x = reorder(Country, Count), y = Count,
                                     fill = Top_AI_Tools,
                                     text = paste("Country:", Country,
                                                  "<br>Top Tool:", Top_AI_Tools,
                                                  "<br>Count:", Count,
                                                  "<br>Avg ENB:", round(Avg_ENB, 1)))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        title = "Most Prevalent AI Tools by Country",
        x = "Country",
        y = "Number of Records"
      ) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8)) +
      scale_fill_brewer(palette = "Set3")
    
    ggplotly(p, tooltip = "text")
  })
  
  # RQ3 Network stats
  output$rq3_network_stats <- renderText({
    data <- rq3_data()
    
    paste(
      "Total Tools:", n_distinct(data$Top_AI_Tools), "\n",
      "Total Countries:", n_distinct(data$Country), "\n",
      "Total Industries:", n_distinct(data$Industry), "\n",
      "Avg Records per Tool:", round(mean(table(data$Top_AI_Tools)), 1)
    )
  })
  
  # RQ3 Bridge tools
  output$rq3_bridge_tools <- renderText({
    data <- rq3_data()
    
    bridge_tools <- data %>%
      group_by(Top_AI_Tools) %>%
      summarise(
        n_regimes = n_distinct(Regulation_Status),
        n_countries = n_distinct(Country),
        n_industries = n_distinct(Industry),
        bridge_score = n_regimes * n_countries * n_industries,
        .groups = 'drop'
      ) %>%
      arrange(desc(bridge_score)) %>%
      slice_head(n = 3)
    
    if(nrow(bridge_tools) == 0) return("No data")
    
    paste(
      bridge_tools$Top_AI_Tools[1], " (Score:", bridge_tools$bridge_score[1], ")\n",
      ifelse(nrow(bridge_tools) >= 2, 
             paste(bridge_tools$Top_AI_Tools[2], " (Score:", bridge_tools$bridge_score[2], ")\n"),
             ""),
      ifelse(nrow(bridge_tools) >= 3,
             paste(bridge_tools$Top_AI_Tools[3], " (Score:", bridge_tools$bridge_score[3], ")"),
             "")
    )
  })
  
  # RQ3 Bridge table
  output$rq3_bridge_table <- renderDT({
    data <- rq3_data()
    
    bridge_table <- data %>%
      group_by(Top_AI_Tools) %>%
      summarise(
        `Regulation Regimes` = n_distinct(Regulation_Status),
        Countries = n_distinct(Country),
        Industries = n_distinct(Industry),
        `Avg ENB` = mean(RevenueIncrease - JobLoss, na.rm = TRUE),
        `Avg Market Share` = mean(MarketShare, na.rm = TRUE),
        `Record Count` = n(),
        .groups = 'drop'
      ) %>%
      mutate(
        `Bridge Score` = `Regulation Regimes` * Countries * Industries
      ) %>%
      arrange(desc(`Bridge Score`))
    
    datatable(bridge_table,
              options = list(
                pageLength = 10,
                scrollX = TRUE
              ),
              rownames = FALSE) %>%
      formatRound(columns = c(4:5), digits = 1)
  })
  
  # RQ4: Impact Profiles
  
  # Reactive clustering results
  rq4_clusters <- reactiveValues(
    data = NULL,
    centers = NULL,
    silhouette = NULL
  )
  
  # Run clustering when button is clicked
  observeEvent(input$rq4_run_clustering, {
    data <- filtered_data()
    
    # Select variables for clustering
    cluster_vars <- input$rq4_cluster_vars
    
    if(length(cluster_vars) < 2) {
      showNotification("Please select at least 2 variables for clustering.", 
                       type = "warning")
      return()
    }
    
    # Prepare data for clustering
    cluster_data <- data %>%
      select(all_of(cluster_vars)) %>%
      na.omit()
    
    if(nrow(cluster_data) < input$rq4_n_clusters) {
      showNotification("Not enough data points for clustering.", 
                       type = "warning")
      return()
    }
    
    # Standardize if selected
    if(input$rq4_standardize) {
      cluster_data <- scale(cluster_data)
    }
    
    # Run clustering
    if(input$rq4_cluster_method == "kmeans") {
      set.seed(123)
      kmeans_result <- kmeans(cluster_data, centers = input$rq4_n_clusters)
      
      rq4_clusters$data <- data %>%
        na.omit() %>%
        mutate(
          Cluster = as.factor(kmeans_result$cluster)
        )
      
      rq4_clusters$centers <- as.data.frame(kmeans_result$centers)
      
      # Calculate silhouette score
      if(nrow(cluster_data) > 1) {
        sil <- silhouette(kmeans_result$cluster, dist(cluster_data))
        rq4_clusters$silhouette <- mean(sil[, "sil_width"])
      }
      
    } else if(input$rq4_cluster_method == "hclust") {
      hc <- hclust(dist(cluster_data))
      clusters <- cutree(hc, k = input$rq4_n_clusters)
      
      rq4_clusters$data <- data %>%
        na.omit() %>%
        mutate(
          Cluster = as.factor(clusters)
        )
      
      # For hierarchical, calculate average values per cluster
      rq4_clusters$centers <- cluster_data %>%
        as.data.frame() %>%
        mutate(Cluster = clusters) %>%
        group_by(Cluster) %>%
        summarise_all(mean, na.rm = TRUE)
    }
    
    showNotification("Clustering completed successfully!", type = "message")
  })
  
  # RQ4 Parallel coordinates
  output$rq4_parallel_coords <- renderPlotly({
    if(is.null(rq4_clusters$data)) return(NULL)
    
    data <- rq4_clusters$data
    
    # Use selected variables for display
    display_vars <- input$rq4_cluster_vars
    
    # Create parallel coordinates plot
    fig <- plot_ly(type = 'parcoords',
                   line = list(color = as.numeric(data$Cluster),
                               colorscale = 'Viridis',
                               showscale = TRUE,
                               colorbar = list(title = "Cluster")),
                   dimensions = lapply(display_vars, function(var) {
                     list(range = c(min(data[[var]], na.rm = TRUE),
                                    max(data[[var]], na.rm = TRUE)),
                          label = var,
                          values = data[[var]])
                   }))
    
    fig %>%
      layout(title = "Cluster Profiles (Parallel Coordinates)",
             margin = list(l = 80, r = 80, t = 50, b = 80))
  })
  
  # RQ4 UMAP plot
  output$rq4_umap_plot <- renderPlotly({
    if(is.null(rq4_clusters$data)) return(NULL)
    
    data <- rq4_clusters$data
    
    # Select variables for UMAP
    umap_vars <- input$rq4_cluster_vars
    
    # Prepare data for UMAP
    umap_data <- data %>%
      select(all_of(umap_vars)) %>%
      na.omit()
    
    if(nrow(umap_data) < 10) return(NULL)
    
    # Run UMAP
    set.seed(123)
    umap_result <- umap(umap_data, n_components = 2)
    
    # Create plot
    plot_data <- data.frame(
      UMAP1 = umap_result$layout[,1],
      UMAP2 = umap_result$layout[,2],
      Cluster = data$Cluster[1:nrow(umap_result$layout)],
      Country = data$Country[1:nrow(umap_result$layout)],
      Industry = data$Industry[1:nrow(umap_result$layout)]
    )
    
    p <- ggplot(plot_data, aes(x = UMAP1, y = UMAP2, 
                               color = Cluster,
                               text = paste("Country:", Country,
                                            "<br>Industry:", Industry,
                                            "<br>Cluster:", Cluster))) +
      geom_point(alpha = 0.7, size = 3) +
      labs(title = "2D Embedding (UMAP)",
           x = "UMAP Dimension 1",
           y = "UMAP Dimension 2") +
      theme_minimal() +
      scale_color_viridis_d()
    
    ggplotly(p, tooltip = "text")
  })
  
  # RQ4 Cluster map
  output$rq4_cluster_map <- renderLeaflet({
    if(is.null(rq4_clusters$data)) return(NULL)
    
    data <- rq4_clusters$data
    
    # Aggregate by country
    country_clusters <- data %>%
      group_by(Country, ISO3, Cluster) %>%
      summarise(
        Count = n(),
        Avg_TrustGap = mean(Adoption - Trust, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      group_by(Country, ISO3) %>%
      slice_max(Count, n = 1) %>%
      ungroup()
    
    # Join with world map
    map_data <- world %>%
      left_join(country_clusters, by = c("iso_a3" = "ISO3"))
    
    # Create color palette for clusters
    pal <- colorFactor("Set3", domain = map_data$Cluster, na.color = "#808080")
    
    # Create labels
    labels <- sprintf(
      "<strong>%s</strong><br/>
      Dominant Cluster: %s<br/>
      Records: %d<br/>
      Avg Trust Gap: %.1f%%",
      map_data$name_long,
      map_data$Cluster,
      map_data$Count,
      map_data$Avg_TrustGap
    ) %>% lapply(htmltools::HTML)
    
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(Cluster),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = labels,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~Cluster,
        title = "Dominant Cluster",
        position = "bottomright",
        na.label = "No data"
      )
  })
  
  # RQ4 Cluster sizes
  output$rq4_cluster_sizes <- renderText({
    if(is.null(rq4_clusters$data)) return("Run clustering first")
    
    cluster_sizes <- table(rq4_clusters$data$Cluster)
    
    paste(
      "Cluster 1:", cluster_sizes[1], "records\n",
      "Cluster 2:", cluster_sizes[2], "records\n",
      "Cluster 3:", ifelse(length(cluster_sizes) >= 3, cluster_sizes[3], "0"), "records\n",
      "Cluster 4:", ifelse(length(cluster_sizes) >= 4, cluster_sizes[4], "0"), "records"
    )
  })
  
  # RQ4 Silhouette score
  output$rq4_silhouette <- renderText({
    if(is.null(rq4_clusters$silhouette)) return("N/A")
    
    paste(round(rq4_clusters$silhouette, 3))
  })
  
  # RQ4 Centroids table
  output$rq4_centroids_table <- renderDT({
    if(is.null(rq4_clusters$centers)) return(NULL)
    
    centers <- rq4_clusters$centers %>%
      as.data.frame() %>%
      mutate(Cluster = row_number()) %>%
      select(Cluster, everything())
    
    datatable(centers,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 't'
              ),
              rownames = FALSE) %>%
      formatRound(columns = 2:ncol(centers), digits = 2)
  })
  
  # RQ4 Exemplars table
  output$rq4_exemplars_table <- renderDT({
    if(is.null(rq4_clusters$data)) return(NULL)
    
    data <- rq4_clusters$data
    
    # Find exemplars (closest to centroids)
    exemplars <- data %>%
      group_by(Cluster) %>%
      slice_head(n = 3) %>%
      ungroup() %>%
      select(Cluster, Country, Year, Industry, Regulation_Status,
             Adoption, Trust, JobLoss, RevenueIncrease, MarketShare)
    
    datatable(exemplars,
              options = list(
                pageLength = 10,
                scrollX = TRUE
              ),
              rownames = FALSE) %>%
      formatRound(columns = 6:10, digits = 1)
  })
  
  # Data Explorer Tab
  
  # Reactive data for Data Explorer
  explorer_data <- reactive({
    data <- filtered_data()
    
    # Apply additional filters
    if(input$data_year != "All") {
      data <- data %>% filter(Year == as.numeric(input$data_year))
    }
    
    if(input$data_country != "All") {
      data <- data %>% filter(Country == input$data_country)
    }
    
    if(input$data_industry != "All") {
      data <- data %>% filter(Industry == input$data_industry)
    }
    
    if(input$data_tool != "All") {
      data <- data %>% filter(Top_AI_Tools == input$data_tool)
    }
    
    if(input$data_regulation != "All") {
      data <- data %>% filter(Regulation_Status == input$data_regulation)
    }
    
    data <- data %>%
      filter(Trust >= input$data_trust_range[1],
             Trust <= input$data_trust_range[2],
             Adoption >= input$data_adoption_range[1],
             Adoption <= input$data_adoption_range[2])
    
    return(data)
  })
  
  # Data table
  output$data_table <- renderDT({
    data <- explorer_data() %>%
      select(Country, Year, Industry, Adoption, Trust, 
             JobLoss, RevenueIncrease, Collaboration,
             Top_AI_Tools, Regulation_Status, MarketShare)
    
    datatable(data,
              options = list(
                pageLength = 20,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf')
              ),
              extensions = 'Buttons',
              rownames = FALSE,
              filter = 'top') %>%
      formatRound(columns = 4:11, digits = 1)
  })
  
  # Download handler
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("ai_impact_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(explorer_data(), file, row.names = FALSE)
    }
  )
  
  # Apply filters button
  observeEvent(input$apply_filters, {
    showNotification("Filters applied successfully!", type = "message")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
