# Cost parameters
prod_q <- 6739*rq # lost productivity COVID (not hospitalized)
prod_h <- 11187*rh # lost productivity for hospitalization
prod_all <- 56 # lost productivity for testing/vaccination/physician visit per person
prod_death <- 30000

c_vac <- 120 # cost of vaccination
c_hos <- 27562*rh # cost of hospitalization
c_scr <- 20.68 # cost of screening from Ava JB(https://www.health.gov.on.ca/en/pro/programs/ohip/sob/lab/lab_mn2020.pdf)
c_mild <- 72 # cost of outpatient visit (for mild cases)

# utility
daly_h <- 0.58/365

# others
life_exp <- 81.95 # life expectancy of Canada (source: World Bank)
mean_age <- 41.12 # mean age of Canada
dis <- 1.015 # annual discount of 1.5%

# Saved productivity ($)
cal_prod(ICER)

# ICER with monetary cost($) from the provider perspective
cal_ICER_mon(ICER)

# DALY averted per cost provider perspective
cal_ICER_DALY(ICER)

# net monetary benefit
cal_nmb(ICER)

