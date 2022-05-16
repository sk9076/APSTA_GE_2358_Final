# APSTA-GE-2353 Capstone Project

Fabin Froelich (ff2093@nyu.edu) 

Sooyoung Kim (sk9076@nyu.edu)

Simon Liao (cl6399@nyu.edu) 

## Overview of the project 

### Background
One of the critical approaches to reduce the burden of infectious diseases is preventive measures informed by early warning signals [1-4]. However, the complex transmission dynamics of many such diseases result in ineffective and inefficient control programs [5-7]. The importance of predictive early warning systems (EWS) that provide longer lead times for strategic planning and implementation of dengue control activities has been propounded, yet few countries have operationalized EWSs for preparedness and response. The main issue from the decision-maker’s point of view is that public health response triggered by an EWS needs to be not only effective but also show economic viability in the face of uncertainty in disease forecasts. If the economic evaluation of potential response activities can be coupled with a disease forecasting model, this framework can simultaneously evaluate the effectiveness and economic value of public health responses triggered at different lead times. When the model tailored to a specific local context, this hybrid framework has the potential to recommend pragmatic vector control targets and interventions and quantify the economic risk associated with acting upon imperfect early warning signals to inform decision-making.

### Objective: 
To build a Shiny application that can inform the infectious disease control program officers on their intervention effectiveness before executing it. 

### Key features of the application:
* 1) Let the users choose the infectious disease of their interest (we can start with 2-3 diseases with simple transmission dynamics, or even 1 if it’s too much burden)
* 2) Tweak the transmission-related parameters (i.e. incubation period, effective contact rate, initial vaccination rate in the population, etc)
* 3) Choose the intervention (i.e. social distancing, vaccination, medical treatment)
* 4) Simulate the averted disease burden (i.e. # of cases and/or healthcare cost averted) in simple deterministic model (point estimate with no 95% CI)

### Project timeline and key tasks:

(You can click on the milestone product names to see the products)

| Tasks                                               | Completed Date/Period          | Milestone Product                                                   |
|-----------------------------------------------------|--------------------------------|---------------------------------------------------------------------|
| Team introduction                                   |   April 11, 2022               |  N/A                                                                |
| Development of user research plan (design plan)     |   April 18, 2022               |  [User research/design plan](https://docs.google.com/document/d/1t9ZgOPcFT7rDFDlWfWsLlgWZNPPQ7JuB8Cz_U5okb6c/edit?usp=sharing)                                          |
| User research (Dr. Yesim Tozan)                     |   April 22, 2022               |  [Findings from design research phase I](https://docs.google.com/document/d/1q7zuqgdYAc4IIdkG1ZQOf-q160EIXHX__tkO1gUecN8/edit?usp=sharing)                              |
| 1st prototype development                           |   April 23 - May 14, 2022      |  [1st Shiny Prototype (core codes only. may not run as is)](https://github.com/sk9076/APSTA_GE_2358_Final/tree/main/1st%20prototype)                                                |
| User test (Dr. Yesim Tozan)                         |  May 15, 2022                  |  [User test report](https://docs.google.com/document/d/1C4yZy6OobXF6VqQ1AdK91Fm5Tt1dWj5q6KE1ZY7A2LI/edit?usp=sharing)                                                   |
| 2nd prototype development for course submission     | May 16-18, 2022                |  2nd Shiny Prototype                                                |

## Summary of the user test feedback and corresponding key improvements
|         Feedback                                    |    Improvement                 |
|-----------------------------------------------------|--------------------------------|
|* __The user can not find the “run simulation” button. He mistakes the report tab for the run simulation button__ | This button has been enlarged with the noticeable color and positioned in the center of the main panel. | 
|* __The user does not use the “update parameters” button which causes the application to crash, when there are no entries and one hits the “run simulation button” for the first time. The order of first hitting the “update parameters” button and then hitting the “run simulation” button appears tedious to him.__  | We removed the dependency of the parameter tabs on this button so that the user only needs to click "Run Simulation" button once to run the model. |
|* __The user did not use the “compare by cases” check-box.__ | We changed the wording of this box to further specify what it does. |
|* __The user wonders why it is possible to put in a higher 2nd dose vaccination rate than the 1st dose (e.g. 30% of people who only received the 1sd does, 60% of population who received 2nd dose vaccine, should trigger an error message.) She could still run the simulation, which triggered the question of what is going on under the hood. He wants to see the formulas/ the math. __ | We changed the wording to avoid confusion and implemented an additional check/warning when the sum of two percentages exceed 100%.|
|* __The user perceived the numbers in the table as hard to read. Especially the total cost. She prefers a separation with . or , to highlight decimal and thousands separators. Or to rond everything up in a written blurb saying: “The interventions saved 3.000 people and cause a total cost of 12 mio."__ | The costs are now presented in the unit of 1 million USD. We also added an interpretation in plain text format on top of the plot and the table output to help users understand the results better. |


## Potential Next Step (in the order of priority)
* Embedding "malaria" model into the application
* Development of "Report" feature where users can save and download the simulation results
* Add different disease models other than COVID-19 and malaria

## References

1.	World Health Organization. Dengue and severe dengue. 2022 January 10, 2022 [cited 2022 February 15, 2022]; Available from: https://www.who.int/news-room/fact-sheets/detail/dengue-and-severe-dengue#:~:text=Dengue%20is%20a%20viral%20infection,called%20dengue%20virus%20(DENV).
2. 	Hussain-Alkhateeb, L., et al., Early warning and response system (EWARS) for dengue outbreaks: Recent advancements towards widespread applications in critical settings. PLoS One, 2018. 13(5): p. e0196811.
3. 	Lee, K.S., et al., Dengue virus surveillance for early warning, Singapore. Emerg Infect Dis, 2010. 16(5): p. 847-9.
4. 	Beatty, M.E., et al., Best practices in dengue surveillance: a report from the Asia-Pacific and Americas Dengue Prevention Boards. PLoS Negl Trop Dis, 2010. 4(11): p. e890.
5. 	Liyanage, P., et al., Evaluation of intensified dengue control measures with interrupted time series analysis in the Panadura Medical Officer of Health division in Sri Lanka: a case study and cost-effectiveness analysis. The Lancet Planetary Health, 2019. 3(5): p. e211-e218.
6. 	Baly, A., et al., Costs of dengue prevention and incremental cost of dengue outbreak control in Guantanamo, Cuba. Trop Med Int Health, 2012. 17(1): p. 123-32.
7. 	Constenla, D., C. Garcia, and N. Lefcourt, Assessing the Economics of Dengue: Results from a Systematic Review of the Literature and Expert Survey. Pharmacoeconomics, 2015. 33(11): p. 1107-35.
