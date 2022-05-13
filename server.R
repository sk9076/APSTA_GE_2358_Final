library(shiny)
library(shinydashboard)
library(shinyalert)
library(xtable)
# source the function scripts
source(here::here("model_scripts", "0_load_packages.R"))
source(here::here("model_scripts", "2_M1_function.R"))

# source the initial number scripts
source(here::here("model_scripts", "1_param_definition.R"))
source(here::here("model_scripts", "1_initial_num.R"))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # Set reactive values
    rv <- reactiveValues(
        # Intervention list
        intervention = list(
            covid19 = c("Mask mandate", "Social distancing", "Lock-down", 
                        "Quarantine for symptomatic cases", "Vaccination"),
            malaria = c("Indoor residual spray (IRS)", 
                        "Antimalarial treatment for symptomatic cases",
                        "Bednet")
        ),
        
        # Model parameters (now for COVID only)
        init_n = init_12,
        parm_base = parm,
        parm_int = parm,
        
        # data frame to save the results
        res_base = data.frame(),
        res_int = data.frame()
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
    
    output$vaccine_dose <- renderUI({
        require(input$interventions)
        if(input$disease=="covid-19"){
            if(sum("Vaccination" %in% input$interventions, na.rm=T)!=0){
                numericInput("vac_dose", label = "Daily vaccine supply (dose/day)",
                             min = 0, max = 1000000, value = 8000, step = 1, width = "96%")
            }
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
                output[[1]] <-sliderInput("n_pop", "size of the target population", value = 1000000, min = 100 , max = 2000000)
                
                #infected people
                output[[2]] <-sliderInput("p_recovered", "% of population who got COVID-19 in the past 12 months (%)", value = 0, min = 0, max = 100)
                
                if("Vaccination" %in% input$interventions){
                    #1st dose vaccine
                    output[[3]] <-sliderInput("p_vac_1", "% of population who only received the 1st dose of vaccine", value = 0, min = 0, max = 100)
                    
                    #second dose vaccine
                    output[[4]] <-sliderInput("p_vac_2", "% of population who received the 2nd dose vaccine", value = 0, min = 0, max = 100)
                }
                # population parameters for covid
            }else if(input$disease == "malaria"){
                # population size
                output[[1]] <-sliderInput("n_pop", "Size of the target population", value = 1000000, min = 100 , max = 2000000)
                
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
    
    # run the model
    observeEvent(input$run_model, {
        isolate({
            if(input$disease=="covid-19"){
            # update the model parameters
            rv$init_n$R <- input$n_pop*input$p_recovered/100
            rv$init_n$S <- input$n_pop -sum(unlist(rv$init_n)[-1])
            rv$parm_base$a <- 1/input$dur_incub
            rv$parm_base$r <- 1/input$dur_mild
            rv$parm_base$rs <- 1/input$dur_sever
            rv$parm_base_ra <- 1/input$dur_asym
            
            rv$parm_int <- rv$parm_base
            
            # adjust intervention parameters
            ## Vaccination
            if(sum("Vaccination" %in% input$interventions, na.rm=T)>0){
                rv$init_n$V1 <- input$n_pop * input$p_vac_1 / 100
                rv$init_n$V2 <- input$n_pop * input$p_vac_2 / 100
                rv$init_n$S <- rv$init_n$S - rv$init_n$V1 - rv$init_n$V2
                rv$parm_int$vac_daily <- input$vac_dose
            }
            ## Mask
            if(sum("Mask mandate" %in% input$interventions, na.rm=T)>0){
                rv$parm_int$c <- 0.7*rv$parm_int$c # effective contact reduced to 25% capacity (for essential activities)
            }
            ## Social distancing
            if(sum("Social distancing" %in% input$interventions, na.rm=T)>0){
                rv$parm_int$c <- 0.5*rv$parm_int$c # effective contact reduced to 25% capacity (for essential activities)
            }
            ## Lock-down
            if(sum("Lock-down" %in% input$interventions, na.rm=T)>0){
                rv$parm_int$c <- 0.25*rv$parm_int$c # effective contact reduced to 25% capacity (for essential activities)
            }
            # run base model
            rv$res_base <- ode(y=unlist(rv$init_n),
                               times=1:(input$t_max*30),
                               func=M1,
                               parms=unlist(rv$parm_base), 
                               method = "rk4") %>% as.data.frame() %>% 
              mutate(group = "without invervention")
            
            browser()
            # run intervention model
            rv$res_int <- ode(y=unlist(rv$init_n),
                              times=1:(input$t_max*30),
                              ## Run different model when Quarantine is chosen
                              func= ifelse(sum("Quarantine for symptomatic cases" %in% input$interventions, na.rm=T)>0, M1_q,M1),
                              parms=unlist(rv$parm_int), 
                              method = "rk4") %>% as.data.frame() %>%
              mutate(group="with intervention")
            rv$res_total <- rbind(rv$res_int,rv$res_base)
            rv$daily_cases <- if(is.null(input$interventions)){
              if(input$pype == F){
                ggplot(data = rv$res_base, aes(x=time,y=A+I+Q+H+D+AV1+IV1+QV1+HV1+DV1+AV2+IV2+QV2+HV2+DV2,color = "All Cases")) +
                  geom_bar(stat="identity", width=0.01) +
                  theme_classic() +
                  ylab("Number of Cases") + 
                  ggtitle("Daily Cases with No Intervention") + 
                  theme(
                    plot.title = element_text(color="black", size=20, face="bold.italic"))
              }
              else{
                ggplot(data = rv$res_base, aes(x=time,y = D, color = "Death cases")) +
                  geom_bar(stat="identity", width=0.01) +
                  geom_bar(data = rv$res_base, aes(x=time,y = H, color = "Hospitalized cases"),stat="identity", width=0.01) + 
                  geom_bar(data = rv$res_base, aes(x=time,y = A+I+Q+AV1+IV1+QV1+HV1+DV1+AV2+IV2+QV2+HV2+DV2, color = "Other Cases"),stat="identity", width=0.01) + 
                  theme_classic() +
                  ylab("Number of Cases") + 
                  ggtitle("Daily Cases with No Intervention") + 
                  theme(
                    plot.title = element_text(color="black", size=20, face="bold.italic"))
              }
            #browser()
            }
            else{
              if(input$pype == F){
                ggplot(data = rv$res_total, aes(x=time,y = A+I+Q+H+D+AV1+IV1+QV1+HV1+DV1+AV2+IV2+QV2+HV2+DV2, color = "All cases")) +
                  geom_bar(stat="identity", width=0.01) + 
                  theme_classic() +
                  ylab("Number of Cases") + 
                  ggtitle("Daily Cases Without v.s. With Intervention") + 
                  theme(
                    plot.title = element_text(color="black", size=20, face="bold.italic")) +
                  facet_grid(~group)
              }
              else{
                ggplot(data = rv$res_total, aes(x=time,y = D, color = "Death cases")) +
                  geom_bar(stat="identity", width=0.01) +
                  geom_bar(data = rv$res_total, aes(x=time,y = H, color = "Hospitalized cases"),stat="identity", width=0.01) + 
                  geom_bar(data = rv$res_total, aes(x=time,y = A+I+Q+AV1+IV1+QV1+HV1+DV1+AV2+IV2+QV2+HV2+DV2, color = "Other Cases"),stat="identity", width=0.01) + 
                  theme_classic() +
                  ylab("Number of Cases") + 
                  ggtitle("Daily Cases Without v.s. With Intervention") + 
                  theme(
                    plot.title = element_text(color="black", size=20, face="bold.italic")) +
                  facet_grid(~group)
              }
            }
            
            rv$table_base_h <- rv$res_base %>%
              select(H) %>% 
              colSums() %>%
              as.matrix()
            
            rv$table_int_h <- rv$res_int %>%
              select(H) %>% 
              colSums() %>%
              as.matrix()
            
            rv$table_base_total <- rv$res_base %>%
              transmute(total_case = A+I+Q+H+AV1+IV1+QV1+HV1+AV2+IV2+QV2+HV2+R+RV1+RV2+D+DV1+DV2,
                        death = D)
            
            rv$table_int_total <- rv$res_int %>%
              transmute(total_case = A+I+Q+H+AV1+IV1+QV1+HV1+AV2+IV2+QV2+HV2+R+RV1+RV2+D+DV1+DV2,
                        death = D)
            
            rv$cost_base <- input$c_outp*as.numeric(rv$table_base_total[input$t_max*30, 1])
            + input$c_hosp * as.numeric(rv$table_base_h)
            
            rv$cost_vac <- if(sum("Vaccination" %in% input$interventions, na.rm=T)!=0){
              input$c_outp*as.numeric(rv$table_int_total[input$t_max*30, 1])
              + input$c_hosp * as.numeric(rv$table_int_h) + 
                (min(input$t_max*input$vac_dose, input$n_pop*(1-input$p_vac_1/100) + input$n_pop*(1-input$p_vac_2/100)) + input$n_pop*input$p_vac_1/100 + input$n_pop*input$p_vac_2/100)*input$c_vac 
            }
            else{
              input$c_outp*as.numeric(rv$table_int_total[input$t_max*30, 1])
              + input$c_hosp * as.numeric(rv$table_int_h)
              }
            
            rv$table <- if(is.null(input$interventions)){
              data.frame("Columns" = c("Total cases","Total Death","Total Hospitalized Days", "Total Cost"),
                "Without_intervention"=rbind(rv$table_base_total[input$t_max*30, 1],
                                             rv$table_base_total[input$t_max*30, 2],
                                             rv$table_base_h,
                                             rv$cost_base
                                             ))
              
            }
            else{
              data.frame("Columns" = c("Total cases","Total Death","Total Hospitalized Days", "Total Cost"),
                         "Without_intervention"= rbind(rv$table_base_total[input$t_max*30,1],
                                                       rv$table_base_total[input$t_max*30,2],
                                                       rv$table_base_h,
                                                       rv$cost_base), 
                         "With_intervention"= rbind(rv$table_int_total[input$t_max*30,1],
                                                    rv$table_int_total[input$t_max*30,2],
                                                    rv$table_int_h,
                                                    rv$cost_vac)) %>%
                mutate(difference = Without_intervention-With_intervention)
            }
              
            # browser()
            }else{
                shinyalert("Oops!", "Malaria model is currently not available. Try COVID-19 instead!", type = "error")
            }
        })    

    })
    
    output$daily_cases <- renderPlot({
        rv$daily_cases
    })
    
    output$table1 <- renderTable({
      rv$table
    }, caption = "Summary Table",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))

})

