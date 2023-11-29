#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Steps to adding a feature:
# 1. Specify the feature.
#   - What does it do?
#   - What will be shown?
# 2. Specify the interface
#   - Where should it go?
#   - Does it enhance the user experience?
# 3. Implement the feature without the UI
# 4. Integrate the feature.


# sourcing all the functions and variables from ct file
source("ct-util_project_final.R")

# adding maximum number of observations to collect for the query
max_num_studies = 1000

# Define UI for the application 
ui <- fluidPage(

  # Application title
  titlePanel("Clinical Trials Query"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("brief_title_kw", "Brief title keywords"), 
      #### PROBLEM 3: CHANGING THE CHECKBOX ONTO DROPDOWN ####
        # source: https://shiny.posit.co/r/gallery/widgets/widget-gallery/ 
      selectInput("source_class", 
                         label = h3('Sponsor Type'), 
                      # PROBLEM 3 UPDATE: in dropdown there is always something selected
                        # so we decided to put an 'all' choice which will show the data for all 
                        # sponsor types, and it's convenient for the user
                         choices = list("All" = "All", 
                                       "Federal" = "FED", 
                                        "Individual" = "INDIV", 
                                        "Industry" = "INDUSTRY", 
                                       "Network" = "NETWORK",
                                        "NIH" = "NIH", 
                                        "Other" = "OTHER", 
                                       "Other_gov" = "OTHER_GOV", 
                                       "Unknown" = "UNKNOWN"), 
                         # PROBLEM 3 UPDATE: select the first dropdown option ('All') by default
                         selected = 'All'),
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
    ),

    
    # adding panels for our app
    mainPanel(
      tabsetPanel(
         type = "tabs",
         tabPanel("Phase", plotOutput("phase_plot")),
         tabPanel("Concurrent", plotOutput("concurrent_plot")), 
         # PROBLEM 2: ADDING ANOTHER PANEL WITH CONDITIONS
         tabPanel("Conditions", plotOutput("conditions_plot")),
         # ADVERSE EVENTS FEATURE
         tabPanel("Adverse events", plotOutput("adverse_events_plot")), 
         # WORLD MAP FEATURE
         tabPanel('World Map', plotOutput("world_map_plot")), 
         # DETAILEDNESS FEATURE
         tabPanel('Detailedness', plotOutput("detailedness_plot"))
       ),
      # showing selected data under the tabs
      dataTableOutput("trial_table")
    )
  )
)


# server functon which generates the output
server <- function(input, output) {
  
  # using reactive function to update the selected data
    # every time the user changes its input
  get_studies = reactive({
    # filter data by keyword specified by user
    if (input$brief_title_kw != "") {
      ret = studies |> filter(grepl(input$brief_title_kw, brief_title))
      
    } else {
      ret = studies
    }
    
    # PROBLEM 3: filtering the data by sponsor type selected by user in dropdown
    if (input$source_class != 'All'){
      ret = ret|> filter(source_class %in% !!input$source_class)
    }
    
    # selecting at most max_num_studies observations to speed up the processing of the app
    ret |>
      head(max_num_studies)
    
  })
  # end or reactive function, data is generated for the query
  

  # concurrent plot created in class
  output$concurrent_plot = renderPlot({
    get_studies() |>
      select(start_date, completion_date) |>
      get_concurrent_trials() |>
      ggplot(aes(x = date, y = count)) +
        geom_line() +
        xlab("Date") +
        ylab("Number of trials") +
        theme_bw() + 
        labs(title = 'Number of active trials for each date') + 
        theme(axis.title.x = element_text(size=16),
              axis.text.x  = element_text(size=13), 
              axis.title.y = element_text(size=16),
              axis.text.y  = element_text(size=13), 
              plot.title = element_text(size = 20))
  })
  
  
  #### PROBLEM 1: FIX PHASE HISTOGRAM ####
  # getting the list of all posible phases in the database
  phases_all <- get_all_phases(studies)
  
  # phase plot using new plot_phase_histogram_new function
  # detailed info in ct and description files
  output$phase_plot = renderPlot({
    get_studies() |>
      plot_phase_histogram_new(phases_all = phases_all)
  })
  

  #### PROBLEM 2 : CONDITIONS PLOT ####
  output$conditions_plot = renderPlot({
    # detailed info in ct and description files
    plot_conditions(get_studies(), conditions)
  })
  
  
  # rendering data table under the tabs, did in class
  output$trial_table = renderDataTable({
    get_studies() |> 
      select(nct_id, brief_title, start_date, completion_date) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date)
  })
  
  
  #### PROBLEM 4: ADDING EXTRA FEATURES ####
  
  #### ADVERSE EVENTS FEATURE ####
  output$adverse_events_plot = renderPlot({
    plot_adverse_events(get_studies(), reported_events)
  })
  
  
  #### WORLD MAP FEATURE ####
  output$world_map_plot = renderPlot({
    plot_world_map(get_studies(), countries, world_map)
  })
  
  
  #### DETAILEDNESS FEATURE ####
  output$detailedness_plot = renderPlot({
    plot_detailedness(get_studies(), calculated_values)
  })
  
  # note the 4th feature is the GENDER FEATURE defined in the UI
    # and used in the reactive function above
    
}


# Run the application 
shinyApp(ui = ui, server = server)



