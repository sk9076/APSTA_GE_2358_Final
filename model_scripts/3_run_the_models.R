iteration <- 1000
tmax <- 365 # 6 months simulation

# # load the parameter table
parm_tab <- rio::import(paste0(sub("*\\/fieldgroup2", "", here::here()), "/parm_dist.xlsx")) %>%
  filter(!is.na(dist))

# # this was because I have a linux machine, and it don't understand this forwardslash bidness, blasphemy
# parm_tab <- rio::import(paste0("parm_dist.xlsx")) %>% filter(!is.na(dist))


# create the template to save the results
M1_res <- list()
M2_res <- list()
M3_res <- list()
M4_res <- list()

ICER <- data.frame(
  vac_m1= rep(NA, iteration),
  vac_m2= rep(NA, iteration),
  vac_m3= rep(NA, iteration),
  vac_m4 = rep(NA, iteration),
  scr_m3= rep(NA, iteration),
  scr_m4 = rep(NA, iteration),
  q_m1= rep(NA, iteration),
  q_m2= rep(NA, iteration),
  q_m3= rep(NA, iteration),
  q_m4 = rep(NA, iteration),
  h_m1= rep(NA, iteration),
  h_m2= rep(NA, iteration),
  h_m3= rep(NA, iteration),
  h_m4 = rep(NA, iteration),
  d_m1= rep(NA, iteration),
  d_m2= rep(NA, iteration),
  d_m3= rep(NA, iteration),
  d_m4 = rep(NA, iteration)
)

# run the model
for(i in 1:iteration){
  # Set the parameters
  parm_p <- gen_parm_p(parm_tab)

  # model 1
  M1_res[[i]]<- ode(y=init_12,
                  times=1:tmax,
                  func=M1,
                  parms=parm_p,
                  #method = "lsoda") %>% as.data.frame()
                   method = "rk4") %>% as.data.frame() #RungaKutta has been kind to me in the past, try ode45 as well as that is exactly what yang is using

  # model 2
  M2_res[[i]]<- ode(y=init_12,
                    times=1:tmax,
                    func=M2,
                    parms=parm_p,
                    #method = "lsoda") %>% as.data.frame()
                     method = "rk4") %>% as.data.frame() #RungaKutta has been kind to me in the past, try ode45 as well as that is exactly what yang is using

  # model 3
  M3_res[[i]]<- ode(y=init_3,
                    times=1:tmax,
                    func=M3,
                    parms = parm_p,
                    #method = "lsoda") %>% as.data.frame()
                     method = "rk4") %>% as.data.frame() #RungaKutta has been kind to me in the past, try ode45 as well as that is exactly what yang is using

  # model4
  M4_res[[i]]<- ode(y=init_3,
                    times=1:tmax,
                    func=M4,
                    parms = parm_p,
                    #method = "lsoda") %>% as.data.frame()
                    method = "rk4") %>% as.data.frame() #RungaKutta has been kind to me in the past, try ode45 as well as that is exactly what yang is using

  # Fill in the result
  ICER$vac_m1[i] <- M1_res[[i]]$PI1[tmax]+M1_res[[i]]$PI2[tmax]
  ICER$vac_m2[i] <- M2_res[[i]]$PI1[tmax]+M2_res[[i]]$PI2[tmax]
  ICER$vac_m3[i] <- M3_res[[i]]$PI1[tmax]+M3_res[[i]]$PI2[tmax]
  ICER$vac_m4[i] <- M4_res[[i]]$PI1[tmax]+M4_res[[i]]$PI2[tmax]

  ICER$scr_m3[i] <- M3_res[[i]]$SCR[tmax]
  ICER$scr_m4[i] <- M4_res[[i]]$SCR[tmax]


  ICER$q_m1[i] <- sum(M1_res[[i]]$Q, M1_res[[i]]$QV1, M1_res[[i]]$QV2, na.rm=T)
  ICER$q_m2[i] <- sum(M2_res[[i]]$Q, M2_res[[i]]$QV1, M2_res[[i]]$QV2, na.rm=T)
  ICER$q_m3[i] <- sum(M3_res[[i]]$Q, M3_res[[i]]$QV1, M3_res[[i]]$QV2, na.rm=T)
  ICER$q_m4[i] <- sum(M4_res[[i]]$Q, M4_res[[i]]$QV1, M4_res[[i]]$QV2, na.rm=T)


  ICER$h_m1[i] <- sum(M1_res[[i]]$H, M1_res[[i]]$HV1, M1_res[[i]]$HV2, na.rm=T)
  ICER$h_m2[i] <- sum(M2_res[[i]]$H, M2_res[[i]]$HV1, M2_res[[i]]$HV2, na.rm=T)
  ICER$h_m3[i] <- sum(M3_res[[i]]$H, M3_res[[i]]$HV1, M3_res[[i]]$HV2, na.rm=T)
  ICER$h_m4[i] <- sum(M4_res[[i]]$H, M4_res[[i]]$HV1, M4_res[[i]]$HV2, na.rm=T)

  ICER$d_m1[i] <- M1_res[[i]]$D[tmax]+M1_res[[i]]$DV1[tmax] + M1_res[[i]]$DV2[tmax]
  ICER$d_m2[i] <- M2_res[[i]]$D[tmax]+M2_res[[i]]$DV1[tmax] + M2_res[[i]]$DV2[tmax]
  ICER$d_m3[i] <- M3_res[[i]]$D[tmax]+M3_res[[i]]$DV1[tmax] + M3_res[[i]]$DV2[tmax]
  ICER$d_m4[i] <- M4_res[[i]]$D[tmax]+M4_res[[i]]$DV1[tmax] + M4_res[[i]]$DV2[tmax]

}

# save the result in excel file
#rio::export(ICER,
#            paste0(sub("*\\/fieldgroup2", "", here::here()),
#                    "/result_",
#                   format(Sys.time(), "%Y%m%d_%H%M"),
#                   ".xlsx"))

