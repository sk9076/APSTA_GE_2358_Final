# Source the required scripts
source(here::here("model_scripts", "0_load_packages.R"))
source(here::here("model_scripts", "2_M1_function.R"))

# These codes should be somehow incorporated with the UI
source(here::here("model_scripts", "1_initial_num.R"))
source(here::here("model_scripts", "1_param_definition.R"))

# create the template to save the results
res_base <- data.frame()
res_int <- data.frame()

# set parm_base and parm_int before running the code below

# base model
  res_base <- ode(y=init_12,
                  times=1:tmax,
                  func=M1,
                  parms=parm_base, # this needs to be fixed
                   method = "rk4") %>% as.data.frame() 


# intervention model
  res_int <- ode(y=init_12,
                 times=1:tmax,
                 func=M1,
                 parms=parm_int, # this needs to be fixed
                 method = "rk4") %>% as.data.frame() 
