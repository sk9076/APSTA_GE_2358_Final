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
  dashboardHeader(title = "APSTA-GE-2358 Final Project"),
  
  #sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("READ ME", tabName = "readme", icon = icon("info")),
      menuItem("Disease simulation", tabName = "simulation", icon = icon("dashboard"), selected=T),
      
      #Dropdown for disease selection
      selectInput(
        "disease", "Choose a disease:", 
          c("COVID-19" = "covid-19", 
            "Malaria (Currently disabled)" = "malaria"
            ),
      ),
        
        #model duration
        sliderInput("duration", "Duration of the simulation (in months)", 
                    value = 6, min = 6, max = 24),
      
      br(),
      
      #interventions
      h5(strong("Choose at least ONE intervention(s)"), align = "center", style = "color:orange"),
      
      # Conditional checkbox for intervention
       checkboxGroupInput(inputId = "interventions", 
                          label = NULL, 
                          choices = NA),
      
      br(), 
      
      #costs
      h5(strong("Enter costs in USD"), align = "center", style = "color:orange"),
    
      #slider for costs
      numericInput("c_hosp", "Hospitalization per person", 
                   value = 72, min = 0, max = 100000, 
                   step = 0.1, width = "96%"),
      numericInput("c_outp", "Outpatient cost per visit", 
                   value = 27562, min = 0, max = 100000, 
                   step = 0.1, width = "96%"),
      
      #conditional cost slider if vaccination is selected
      uiOutput("other_cost"),
      
      #button for run simulation
      actionButton(
        inputId = 'update_parm_tab',
        label = "Update Parameters"
      ),
      
      #another tab for a report if necessary 
      menuItem("Report", tabName = "widgets", icon = icon("th"))
    )
  ),
  
  #add error message if infected_people > than population size, make population reactive to size
  
  
  #bodey on the right
  dashboardBody(
    tabItems(
      # READ ME tab
      tabItem(tabName = "readme",
              includeMarkdown(here::here("README.md"))
              ),
      
      # First tab content
      tabItem(tabName = "simulation",
              fluidRow(
                
                #box for population parameters
                column(12, 
                box(
                       #population parameters - change people_min, people_max in header
                       h5(strong("Population parameters"), align = "center", style = "color:orange"),
                       
                       uiOutput("pop_parm_tab"),
                
                ), 
                
                box(
                  #disease parameters 
                  h5(strong("Disease parameters"), align = "center", style = "color:orange"),
                  uiOutput("disease_parm_tab")
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

  # Set reactive values
  rv <- reactiveValues(
    intervention = list(
      covid19 = c("Mask mandate", "Social distancing", "Lock-down", 
                  "Quarantine for symptomatic cases", "Vaccination"),
      malaria = c("Indoor residual spray (IRS)", 
                  "Antimalarial treatment for symptomatic cases",
                  "Bednet")
    )
  )
  
  # Update the intervention menu based on the disease choice
  observeEvent(input$disease, {
    require(input$disease)
    if(input$disease=="malaria"){
      updateCheckboxGroupInput(
      inputId = "interventions",
      choices = rv$intervention[["malaria"]]
     )
    }else if(input$disease == "covid-19"){
      updateCheckboxGroupInput(
        inputId = "interventions",
        choices = rv$intervention[["covid19"]]
      )
    }
  })
  
  output$other_cost <- renderUI({
    require(input$interventions)
    output <- tagList()
    if(input$disease=="covid-19"){
      if(sum("Vaccination" %in% input$interventions, na.rm=T)!=0){
        output[[1]] <- numericInput("c_vac", label = "Cost of vaccine per dose",
                    min = 1, max = 500, value = 12, step = 0.1, width = "96%")
      }else{output[[1]] <- NULL}
    }else if(input$disease=="malaria"){
      require(input$interventions)
      output[[1]] <- numericInput("c_irs", label = "Total cost of IRS activities",
                                  min = 1, max = 10000, value = 2500, step = 0.1, width = "96%")
      output[[2]] <- numericInput("c_antimal", label = "Cost of antimalarial per person",
                                  min = 1, max = 500, value = 20, step = 0.1, width = "96%")
      output[[3]] <- numericInput("c_bednet", label = "Total cost of bednet distribution",
                                  min = 1, max = 10000, value = 2500, step = 0.1, width = "96%")
      output <- output[which(rv$intervention[["malaria"]] %in% input$interventions)]
    }
    return(output)
  })
  
  # Update the population parameter tab
  output$pop_parm_tab <- renderUI({
    input$update_parm_tab
    require(input$disease)
    isolate({
      output <- tagList()
      # population parameters for covid
      if(input$disease =="covid-19"){
      #population size
      output[[1]] <-sliderInput("n_pop", "size of the target population", value = people_value, min = people_min , max = people_max)
      
      #infected people
      output[[2]] <-sliderInput("n_recovered", "% of population who got malaria in the past 12 months (%)", value = 0, min = 0, max = 100)
      
      #1st dose vaccine
      output[[3]] <-sliderInput("n_vacc_1", "% of population who only received the 1st dose of vaccine", value = 0, min = 0, max = 100)
      
      #second dose vaccine
      output[[4]] <-sliderInput("n_vacc_2", "% of population who received the 2nd dose vaccine", value = 0, min = 0, max = 100)
      
      # population parameters for covid
      }else if(input$disease == "malaria"){
        # population size
        output[[1]] <-sliderInput("n_pop", "Size of the target population", value = people_value, min = people_min , max = people_max)
        
        #infected people
        output[[2]] <-sliderInput("n_recovered", "% of population who got malaria in the past 12 months", value = 0, min = 0, max = 100)
        
        #percentage of asymptomatic
        output[[3]] <-sliderInput("p_asymp", "% of asymptomatic infection", value = 15 , min = 0, max = 100)
        #percentage of severe malaria cases
        output[[4]] <-sliderInput("p_asymp", "% of cerebral malaria", value = 1 , min = 0, max = 100, step = 0.5)
      }
      
      return(output)
    })
  })
  
  # Update the disease parameter tab accordingly
  # Update the population parameter tab
  output$disease_parm_tab <- renderUI({
    input$update_parm_tab
    require(input$disease)
    isolate({
      output <- tagList()
      # disease parameters for COVID-19
      if(input$disease =="covid-19"){
        #incubation period
        output[[1]]<-sliderInput("dur_incub", "Incubation period (in days)", value = 5, min = 1, max = 21)
        #duration of symptoms of  cases
        output[[2]]<-sliderInput("dur_mild", "Duration of symptom(s) for mild cases", value = 5, min = 1, max = 21)
        output[[3]]<-sliderInput("dur_sever", "Duration of symptom(s) for severe cases", value = 5, min = 1, max = 21)
        output[[4]]<-sliderInput("dur_asym", "Duration of Asymptomatic infection", value = 1, min = 0, max = 21)
      # disease parameters for malaria
      }else if(input$disease == "malaria"){
        #incubation period
        output[[1]]<-sliderInput("dur_incub", "Incubation period (in days)", value = 5, min = 1, max = 21)
        #duration of symptoms of  cases
        output[[2]]<-sliderInput("dur_mild", "Duration of symptom(s) for mild cases", value = 5, min = 1, max = 21)
        output[[3]]<-sliderInput("dur_sever", "Duration of symptom(s) for severe cases", value = 5, min = 1, max = 21)
        output[[4]]<-sliderInput("dur_asym", "Duration of Asymptomatic infection", value = 1, min = 0, max = 21)
        output[[5]]<-sliderInput("eir", "Effective innoculation rate (infectious bites/person/6 months", value = 3.9, min = 0, max = 20)
      }
      
      return(output)
    })
  })
  
    set.seed(122)
    histdata <- rnorm(500)

    output$plot1 <- renderPlot({
      data <- histdata[seq_len(input$n_pop/100000)]
      hist(data)
    })
}

############### MODEL #######################





# Run the application 
shinyApp(ui = ui, server = server)
