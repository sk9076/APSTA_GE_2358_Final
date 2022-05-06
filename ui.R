library(shiny)
library(shinydashboard)

#framing parameters for population for sidebar sliders


shinyUI(dashboardPage(
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
            sliderInput("t_max", "Duration of the simulation (in months)", 
                        value = 6, min = 6, max = 24),
            
            br(),
            
            #interventions
            h5(strong("Choose at least ONE intervention(s)"), align = "center", style = "color:orange"),
            
            # Conditional checkbox for intervention
            checkboxGroupInput(inputId = "interventions", 
                               label = NULL, 
                               choices = NA),
            uiOutput("vaccine_dose"),
            
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
            
            #button to move to the next step
            actionButton(
                inputId = 'update_parm_tab',
                label = "Update Parameters"
            ),
            
            #another tab for a report if necessary 
            menuItem("Report", tabName = "report", icon = icon("th"))
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
                                   box(
                                       #population parameters - change people_min, people_max in header
                                       h5(strong("Population parameters"), align = "center", style = "color:orange"),
                                       
                                       uiOutput("pop_parm_tab")
                                       
                                   ), 
                                   
                                   box(
                                       #disease parameters 
                                       h5(strong("Disease parameters"), align = "center", style = "color:orange"),
                                       uiOutput("disease_parm_tab")
                                   ),
                                   width=12,
                                   collapsible=T
                               )
                        )),
                    
                    # button to trigger model run
                    fluidRow(
                        actionButton(
                            inputId = 'run_model',
                            label = "Run Simulation"
                        ),
                        br(),
                        align = "right"
                    ),
                    
                    # Outputs
                    fluidRow(
                        # plots
                        box(plotOutput("daily_cases"))
                    ),
                    fluidRow(
                      # summary table
                      box(tableOutput("table1"))
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "report",
                    h2("Report download function currendly under development")
            )
        )
    )
))

