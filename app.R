#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

#framing parameters for population for sidebar sliders
people_min <- 100
people_max <- 2000000
people_value <- people_max/2

# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  
  #sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Disease simulation", tabName = "simulation", icon = icon("dashboard")),
      
      #Dropdown for disease selection
      selectInput(
        "disease", "Choose a disease:", 
          c("Covid-19" = "covid-19", 
            "Malaria" = "malaria",
            "Tubercolosis" = "tubercolosis"),
      ),
        
        #model duration
        sliderInput("duration", "model duration in months", value = 6, min = 6, max = 24),
        
      
      
     
      
      
      #interventions
      h5(strong("Interventions"), align = "center", style = "color:orange"),
      
      #checkbox for intervention
       checkboxGroupInput("interventions", "choose intervention:", c("mask", "social distancing", "lockdown", 
                                                                "quarantine for symptomatic cases"
                                                                )),
       #conditional cost slider if vaccination is selected
      checkboxInput("vaccination", "Vaccination"),
      conditionalPanel(
        condition = "input.vaccination == true",
        sliderInput("vac_cost", "cost of vaccine per person in $", value = 1, min = 1, max = 20),
      ),
      
      #costs
      h5(strong("Costs"), align = "center", style = "color:orange"),
      
      strong("costs per person in $ for", align = "center"),
      
      #slider for costs
      sliderInput("cost_hospitalization", "hospitalization", value = 1000, min = 1, max = 10000),
      sliderInput("dur_asym", "outpatient visit", value = 500, min = 1, max = 5000),
      
      #button for run simulation
      actionButton(
        inputId = 'run_simulation',
        label = "Run simulation"
      ),
      
      #another tab for a report if necessary 
      menuItem("Report", tabName = "widgets", icon = icon("th"))
    )
  ),
  
  #add error message if infected_people > than population size, make population reactive to size
  
  
  #bodey on the right
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "simulation",
              fluidRow(
                
                #box for population parameters
                column(8, 
                box(
                       #population parameters - change people_min, people_max in header
                       h5(strong("Population parameters"), align = "center", style = "color:orange"),
                       
                       #population size
                       sliderInput("population_size", "size of the population", value = people_value, min = people_min , max = people_max ),
                       
                       strong("% of people who"),
                       
                       #infected people
                       sliderInput("infected_people", "got infected", value = 0, min = 0, max = 100),
                       
                       #1st dose vaccine
                       sliderInput("firstdose_people", "got 1st dose vaccine", value = 0, min = 0, max = 100),
                       
                       #second dose vaccine
                       sliderInput("seconddose_people", "2nd dose vaccine", value = 0, min = 0, max = 100),
                
                ), 
                
                box(
                  #disease parameters 
                  h5(strong("Disease parameters"), align = "center", style = "color:orange"),
                  
                  #incubation period
                  sliderInput("incubation_period", "incubation period", value = 5, min = 1, max = 14),
                  
                  strong("duration in days of symptoms for"),
                  
                  #duration of symptoms of  cases
                  sliderInput("dur_mild", "mild cases", value = 5, min = 1, max = 14),
                  sliderInput("dur_sever", "severe cases", value = 5, min = 1, max = 14),
                  sliderInput("dur_asym", "asymptomatic infection", value = 1, min = 0, max = 14),
                  
                )
                      ),
                
                
                #test histogram
                box(plotOutput("plot1", height = 250)),
                
               
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$population_size/100000)]
    hist(data)
  })
}


# test server logic required to draw a histogram
server <- function(input, output) {

    set.seed(122)
    histdata <- rnorm(500)

    output$plot1 <- renderPlot({
      data <- histdata[seq_len(input$population_size/100000)]
      hist(data)
    })
}

############### MODEL #######################





# Run the application 
shinyApp(ui = ui, server = server)
