require(pacman)
pacman::p_load(ggplot2, deSolve, dplyr, magrittr, tidyverse, triangle) #ggthemr

# define function
gen_prob_param <- function(dta){
  temp <- apply(select(dta, dist, val, par1, par2), MARGIN = 1,
                function(x, n=1) {
                  par1 <- as.numeric(x["par1"])
                  par2 <- as.numeric(x["par2"])

                  if(x["dist"]=="beta") return(rbeta(n, par1, par2))
                  if(x["dist"]=="log-normal") return(rlnorm(n, meanlog = as.numeric(x["par1"]), sdlog = as.numeric(x["par2"])))
                  if(x["dist"]=="uniform") return(runif(n, min = min(as.numeric(x["par1"]),as.numeric(x["par2"])),
                                                        max = max(as.numeric(x["par1"]), as.numeric(x["par2"]))))
                  if(x["dist"]=="gamma") return(rgamma(n, shape = as.numeric(x["par1"]), scale = as.numeric(x["par2"])))
                  if(x["dist"]=="triangular") return(rtriangle(n, as.numeric(x["par1"]), as.numeric(x["par2"])))
                  if(x["dist"]=="none") return(rep(as.numeric(x["val"]), n))
                })

  temp %<>% unlist()
  names(temp) <- dta$var_name
  return(temp)
}

gen_parm_p <- function(parm_tab){
  parm_p <- gen_prob_param(parm_tab)
  parm_p[c("a", "gamma", "gamma_2", "ra", "rh", "yi")]%<>%.^-1

  parm_p %<>% c(
    vac_daily = vac_daily,
    # Adjustment following Moghadas et al,
    r = (1/parm_p["a"] + 1/parm_p["yi"] + 1/parm_p["gamma"])^(-1),
    rs = (1/parm_p["a"] + 1/parm_p["yi"] + 1/parm_p["gamma_2"] + 1/parm_p["rh"])^(-1),
    betaa = 0.5*parm_p["betas"],

    # First dose flow
    pv1 = parm_p["p"]*parm_p["ep"],
    av1 = parm_p["a"],
    rav1 = parm_p["ra"],
    yiv1 = parm_p["yi"],
    qv1 = parm_p["ep"]*parm_p["q"],
    rhv1 = parm_p["rh"],
    uv1 = parm_p["u"],

    # Second dose flow
    pv2 = parm_p["epv"]*parm_p["p"],
    av2 = parm_p["a"],
    rav2 = parm_p["ra"],
    yiv2 = parm_p["yi"],
    qv2 = parm_p["epv"]*parm_p["q"],
    uv2 = parm_p["u"],
    rhv2 = parm_p["rh"]
  )
  names(parm_p) <- sub("\\..*", "", names(parm_p))

  parm_p%<>%
    c(
      rq = parm_p["r"]*parm_p["yi"]/(parm_p["yi"]-parm_p["r"]), # duration of quarantine for mild cases
      delt = parm_p["rs"]*parm_p["yi"]*parm_p["rh"]/
        (parm_p["yi"]*parm_p["rh"] - parm_p["rs"]*parm_p["rh"] - parm_p["yi"]*parm_p["rs"]))
  names(parm_p) <- sub("\\..*", "", names(parm_p))

  parm_p%<>%
    c(
      rqv1 = parm_p["rq"],
      rqv2 = parm_p["rq"],
      deltv1 = parm_p["delt"],
      deltv2 = parm_p["delt"]
    )
  names(parm_p) <- sub("\\..*", "", names(parm_p))

  return(parm_p)
}

spit_result <- function(ICER){
  print(
    sprintf("Quarantine days averted (M2 vs. M1) = %.0f days (95%% CrI %.0f - %.0f days)",
          mean(ICER$q_m2 - ICER$q_m1)*-1,
           quantile(ICER$q_m2-ICER$q_m1, 0.975)*-1,
          quantile(ICER$q_m2-ICER$q_m1, 0.025)*-1
    )
  )

  print(
    sprintf("Quarantine days averted (M3 vs. M1) = %.0f days (95%% CrI %.0f - %.0f days)",
          mean(ICER$q_m3 - ICER$q_m1)*-1,
          quantile(ICER$q_m3-ICER$q_m1, 0.975)*-1,
          quantile(ICER$q_m3-ICER$q_m1, 0.025)*-1
    )
  )

  print(
    sprintf("Quarantine days averted (M4 vs. M1) = %.0f days (95%% CrI %.0f - %.0f days)",
            mean(ICER$q_m4 - ICER$q_m1)*-1,
            quantile(ICER$q_m4-ICER$q_m1, 0.975)*-1,
            quantile(ICER$q_m4-ICER$q_m1, 0.025)*-1
    )
  )

  print(
    sprintf("Hospitalized days averted (M2 vs. M1) = %.0f days (95%% CrI %.0f - %.0f days)",
          mean(ICER$h_m2 - ICER$h_m1)*-1,
          quantile(ICER$h_m2-ICER$h_m1, 0.975)*-1,
          quantile(ICER$h_m2-ICER$h_m1, 0.025)*-1
    )
  )

  print(
    sprintf("Hospitalized days averted (M3 vs. M1) = %.0f days (95%% CrI %.0f - %.0f days)",
          mean(ICER$h_m3 - ICER$h_m1)*-1,
          quantile(ICER$h_m3-ICER$h_m1, 0.975)*-1,
          quantile(ICER$h_m3-ICER$h_m1, 0.025)*-1
      )
  )

  print(
    sprintf("Hospitalized days averted (M4 vs. M1) = %.0f days (95%% CrI %.0f - %.0f days)",
            mean(ICER$h_m4 - ICER$h_m1)*-1,
            quantile(ICER$h_m4-ICER$h_m1, 0.975)*-1,
            quantile(ICER$h_m4-ICER$h_m1, 0.025)*-1
    )
  )

  print(
    sprintf("Deaths averted (M2 vs. M1) = %.0f deaths (95%% CrI %.0f - %.0f deaths)",
          mean(ICER$d_m2 - ICER$d_m1)*-1,
          quantile(ICER$d_m2-ICER$d_m1, 0.975)*-1,
          quantile(ICER$d_m2-ICER$d_m1, 0.025)*-1
    )
  )

  print(
    sprintf("Deaths averted (M3 vs. M1) = %.0f deaths (95%% CrI %.0f - %.0f deaths)",
          mean(ICER$d_m3 - ICER$d_m1)*-1,
          quantile(ICER$d_m3-ICER$d_m1, 0.975)*-1,
          quantile(ICER$d_m3-ICER$d_m1, 0.025)*-1
    )
  )

  print(
    sprintf("Deaths averted (M4 vs. M1) = %.0f deaths (95%% CrI %.0f - %.0f deaths)",
            mean(ICER$d_m4 - ICER$d_m1)*-1,
            quantile(ICER$d_m4-ICER$d_m1, 0.975)*-1,
            quantile(ICER$d_m4-ICER$d_m1, 0.025)*-1
    )
  )

  # Cost with 95% CrI
  print(
    sprintf("Incremental vaccine/screening doses used (M2 vs. M1) = %.0f doses (95%% CrI %.0f - %.0f doses)",
          mean(ICER$vac_m2 - ICER$vac_m1),
          quantile(ICER$vac_m2-ICER$vac_m1, 0.025),
          quantile(ICER$vac_m2-ICER$vac_m1, 0.975)
    )
  )

  print(
    sprintf("Incremental vaccine/screening doses used (M3 vs. M1) = %.0f doses (95%% CrI %.0f - %.0f doses)\n(Screening accounts for %.1f %% of incremental dose)",
          mean(ICER$vac_m3 +ICER$scr_m3 - ICER$vac_m1),
          quantile(ICER$vac_m3 +ICER$scr_m3 - ICER$vac_m1, 0.025),
          quantile(ICER$vac_m3 +ICER$scr_m3 - ICER$vac_m1, 0.975),
          mean(ICER$scr_m3/(ICER$vac_m3+ICER$scr_m3-ICER$vac_m1)*100)
    )
  )

  print(
    sprintf("Incremental vaccine/screening doses used (M4 vs. M1) = %.0f doses (95%% CrI %.0f - %.0f doses)\n(Screening accounts for %.1f %% of incremental dose)",
            mean(ICER$vac_m4 +ICER$scr_m4 - ICER$vac_m1),
            quantile(ICER$vac_m4 +ICER$scr_m4 - ICER$vac_m1, 0.025),
            quantile(ICER$vac_m4 +ICER$scr_m4 - ICER$vac_m1, 0.975),
            mean(ICER$scr_m4/(ICER$vac_m4+ICER$scr_m4-ICER$vac_m1)*100)
    )
  )

  # ICER (DALY averted per unit cost)
  print("ICER (M2 vs.M1):")

  print("Quarantine days averted")
  if(mean(ICER$vac_m2 - ICER$vac_m1)<0){
    print("Cost-saving (Incremental cost <0)")
    }else if(mean(ICER$vac_m2 - ICER$vac_m1)<0.1){
      print("Cannot calculate ICER (Incremental cost ==0)")
    }else{
      print(
        sprintf("Incremental Cost-effectiveness Ratio (M2 vs. M1) = %.1f quarantine days averted per 10000 doses (95%% CrI %.1f - %.1f)",
              mean((ICER$q_m2 - ICER$q_m1)*-10000/(ICER$vac_m2 - ICER$vac_m1)),
              quantile((ICER$q_m2 - ICER$q_m1)*-10000/(ICER$vac_m2 - ICER$vac_m1), 0.025),
              quantile((ICER$q_m2 - ICER$q_m1)*-10000/(ICER$vac_m2 - ICER$vac_m1), 0.975)
        )
      )
  }
  print("Hospitalization days averted")
  if(mean(ICER$vac_m2 - ICER$vac_m1)<0){
    print("Cost-saving (Incremental cost <0)")
  }else if(mean(ICER$vac_m2 - ICER$vac_m1)<0.1){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M2 vs. M1) = %.1f hospitalization days averted per 10000 doses (95%% CrI %.1f - %.1f)",
            mean((ICER$h_m2 - ICER$h_m1)*-10000/(ICER$vac_m2 - ICER$vac_m1)),
            quantile((ICER$h_m2 - ICER$h_m1)*-10000/(ICER$vac_m2 - ICER$vac_m1), 0.025),
            quantile((ICER$h_m2 - ICER$h_m1)*-10000/(ICER$vac_m2 - ICER$vac_m1), 0.975)
      )
    )
  }
  print("Deaths averted")
  if(mean(ICER$vac_m2 - ICER$vac_m1)<0){
    print("Cost-saving (Incremental cost <0)")
  }else if(mean(ICER$vac_m2 - ICER$vac_m1)<0.1){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M2 vs. M1) = %.1f deaths averted per 10000 doses (95%% CrI %.1f - %.1f)",
            mean((ICER$d_m2 - ICER$d_m1)*-10000/(ICER$vac_m2 - ICER$vac_m1)),
            quantile((ICER$d_m2 - ICER$d_m1)*-10000/(ICER$vac_m2 - ICER$vac_m1), 0.025),
            quantile((ICER$d_m2 - ICER$d_m1)*-10000/(ICER$vac_m2 - ICER$vac_m1), 0.975)
      )
    )
  }

  print("ICER (M3 vs. M1)")
  print("Quarantine days averted")
  if(mean(ICER$vac_m3 +ICER$scr_m3 - ICER$vac_m1)<0){
    print("Cost-saving (Incremental cost <0)")
  }else if(mean(ICER$vac_m3 + ICER$scr_m3 - ICER$vac_m1)<0.1){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M3 vs. M1) = %.1f quarantine days averted per 10000 doses (95%% CrI %.1f - %.1f)",
            mean((ICER$q_m3 - ICER$q_m1)*-10000/(ICER$vac_m3 + ICER$scr_m3 - ICER$vac_m1)),
            quantile((ICER$q_m3 - ICER$q_m1)*-10000/(ICER$vac_m3 + ICER$scr_m3 - ICER$vac_m1), 0.025),
            quantile((ICER$q_m3 - ICER$q_m1)*-10000/(ICER$vac_m3 + ICER$scr_m3 - ICER$vac_m1), 0.975)
      )
    )
  }
  print("Hospitalization days averted")
  if(mean(ICER$vac_m3 + ICER$scr_m3 - ICER$vac_m1)<0){
    print("Cost-saving (Incremental cost <0)")
  }else if(mean(ICER$vac_m3 + ICER$scr_m3 - ICER$vac_m1)<0.1){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M3 vs. M1) = %.1f hospitalization days averted per 10000 doses (95%% CrI %.1f - %.1f)",
            mean((ICER$h_m3 - ICER$h_m1)*-10000/(ICER$vac_m3 + ICER$scr_m3 - ICER$vac_m1)),
            quantile((ICER$h_m3 - ICER$h_m1)*-10000/(ICER$vac_m3 + ICER$scr_m3 - ICER$vac_m1), 0.025),
            quantile((ICER$h_m3 - ICER$h_m1)*-10000/(ICER$vac_m3 + ICER$scr_m3 - ICER$vac_m1), 0.975)
      )
    )
  }
  print("Deaths averted")
  if(mean(ICER$vac_m3 + ICER$scr_m3 - ICER$vac_m1)<0){
    print("Cost-saving (Incremental cost <0)")
  }else if(mean(ICER$vac_m3 + ICER$scr_m3 - ICER$vac_m1)<0.1){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M3 vs. M1) = %.2f deaths averted per 10000 doses (95%% CrI %.2f - %.2f)",
            mean((ICER$d_m3 - ICER$d_m1)*-10000/(ICER$vac_m3 + ICER$scr_m3 - ICER$vac_m1)),
            quantile((ICER$d_m3 - ICER$d_m1)*-10000/(ICER$vac_m3 + ICER$scr_m3 - ICER$vac_m1), 0.025),
            quantile((ICER$d_m3 - ICER$d_m1)*-10000/(ICER$vac_m3 + ICER$scr_m3 - ICER$vac_m1), 0.975)
      )
    )
  }

  print("ICER (M4 vs. M1)")
  print("Quarantine days averted")
  if(mean(ICER$vac_m4 +ICER$scr_m4 - ICER$vac_m1)<0){
    print("Cost-saving (Incremental cost <0)")
  }else if(mean(ICER$vac_m4 + ICER$scr_m4 - ICER$vac_m1)<0.1){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M4 vs. M1) = %.1f quarantine days averted per 10000 doses (95%% CrI %.1f - %.1f)",
              mean((ICER$q_m4 - ICER$q_m1)*-10000/(ICER$vac_m4 + ICER$scr_m4 - ICER$vac_m1)),
              quantile((ICER$q_m4 - ICER$q_m1)*-10000/(ICER$vac_m4 + ICER$scr_m4 - ICER$vac_m1), 0.025),
              quantile((ICER$q_m4 - ICER$q_m1)*-10000/(ICER$vac_m4 + ICER$scr_m4 - ICER$vac_m1), 0.975)
      )
    )
  }
  print("Hospitalization days averted")
  if(mean(ICER$vac_m4 + ICER$scr_m4 - ICER$vac_m1)<0){
    print("Cost-saving (Incremental cost <0)")
  }else if(mean(ICER$vac_m4 + ICER$scr_m4 - ICER$vac_m1)<0.1){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M4 vs. M1) = %.1f hospitalization days averted per 10000 doses (95%% CrI %.1f - %.1f)",
              mean((ICER$h_m4 - ICER$h_m1)*-10000/(ICER$vac_m4 + ICER$scr_m4 - ICER$vac_m1)),
              quantile((ICER$h_m4 - ICER$h_m1)*-10000/(ICER$vac_m4 + ICER$scr_m4 - ICER$vac_m1), 0.025),
              quantile((ICER$h_m4 - ICER$h_m1)*-10000/(ICER$vac_m4 + ICER$scr_m4 - ICER$vac_m1), 0.975)
      )
    )
  }
  print("Deaths averted")
  if(mean(ICER$vac_m4 + ICER$scr_m4 - ICER$vac_m1)<0){
    print("Cost-saving (Incremental cost <0)")
  }else if(mean(ICER$vac_m4 + ICER$scr_m4 - ICER$vac_m1)<0.1){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M4 vs. M1) = %.2f deaths averted per 10000 doses (95%% CrI %.2f - %.2f)",
              mean((ICER$d_m4 - ICER$d_m1)*-10000/(ICER$vac_m4 + ICER$scr_m4 - ICER$vac_m1)),
              quantile((ICER$d_m4 - ICER$d_m1)*-10000/(ICER$vac_m4 + ICER$scr_m4 - ICER$vac_m1), 0.025),
              quantile((ICER$d_m4 - ICER$d_m1)*-10000/(ICER$vac_m4 + ICER$scr_m4 - ICER$vac_m1), 0.975)
      )
    )
  }
}

collapse_res <- function(M_res){
  res_sum <- bind_rows(M_res, .id="df")

  res_sum %<>% group_by(time) %>%
    summarize(
      q_mean = mean(Q+QV1+QV2),
      q_low_ci = quantile(Q+QV1+QV2, 0.025),
      q_high_ci = quantile(Q+QV1+QV2, 0.975),

      h_mean = mean(H+HV1+HV2),
      h_low_ci = quantile(H+HV1+HV2, 0.025),
      h_high_ci = quantile(H+HV1+HV2, 0.975),

      d_mean = mean(D+DV1+DV2),
      d_low_ci = quantile(D+DV1+DV2, 0.025),
      d_high_ci = quantile(D+DV1+DV2, 0.975)
    )

  return(res_sum)
}

cal_prod <- function(ICER){

  # M2 vs. M1
  prod_m2<-
    mean(ICER$q_m2 - ICER$q_m1)*-1*prod_q +
    mean(ICER$h_m2 - ICER$h_m1)*-1*prod_h +
    mean(ICER$vac_m2 - ICER$vac_m1)*-1*prod_all

  prod_m2_low<-
    quantile(ICER$q_m2 - ICER$q_m1, 0.975)*-1*prod_q +
    quantile(ICER$h_m2 - ICER$h_m1, 0.975)*-1*prod_h +
    quantile(ICER$vac_m2 - ICER$vac_m1, 0.975)*-1*prod_all

  prod_m2_high <-
    quantile(ICER$q_m2 - ICER$q_m1, 0.025)*-1*prod_q +
    quantile(ICER$h_m2 - ICER$h_m1, 0.025)*-1*prod_h +
    quantile(ICER$vac_m2 - ICER$vac_m1, 0.025)*-1*prod_all

  print(
    sprintf("M2 reduces productivity loss by $ %.0f (95%% CI $ %.0f - $ %.0f) vs. M1",
            prod_m2,
            prod_m2_low,
            prod_m2_high)
  )

  # M3 vs. M1
  prod_m3<-
    mean(ICER$q_m3 - ICER$q_m1)*-1*prod_q +
    mean(ICER$h_m3 - ICER$h_m1)*-1*prod_h +
    mean(ICER$vac_m3 - ICER$vac_m1)*-1*prod_all

  prod_m3_low<-
    quantile(ICER$q_m3 - ICER$q_m1, 0.975)*-1*prod_q +
    quantile(ICER$h_m3 - ICER$h_m1, 0.975)*-1*prod_h +
    quantile(ICER$vac_m3 - ICER$vac_m1, 0.975)*-1*prod_all

  prod_m3_high <-
    quantile(ICER$q_m3 - ICER$q_m1, 0.025)*-1*prod_q +
    quantile(ICER$h_m3 - ICER$h_m1, 0.025)*-1*prod_h +
    quantile(ICER$vac_m3 - ICER$vac_m1, 0.025)*-1*prod_all

  print(
    sprintf("M3 reduces productivity loss by $ %.0f (95%% CI $ %.0f - $ %.0f) vs. M1",
            prod_m3,
            prod_m3_low,
            prod_m3_high)
  )

  # M4 vs. M1
  prod_m4<-
    mean(ICER$q_m4 - ICER$q_m1)*-1*prod_q +
    mean(ICER$h_m4 - ICER$h_m1)*-1*prod_h +
    mean(ICER$vac_m4 - ICER$vac_m1)*-1*prod_all

  prod_m4_low<-
    quantile(ICER$q_m4 - ICER$q_m1, 0.975)*-1*prod_q +
    quantile(ICER$h_m4 - ICER$h_m1, 0.975)*-1*prod_h +
    quantile(ICER$vac_m4 - ICER$vac_m1, 0.975)*-1*prod_all

  prod_m4_high <-
    quantile(ICER$q_m4 - ICER$q_m1, 0.025)*-1*prod_q +
    quantile(ICER$h_m4 - ICER$h_m1, 0.025)*-1*prod_h +
    quantile(ICER$vac_m4 - ICER$vac_m1, 0.025)*-1*prod_all

  print(
    sprintf("M3 reduces productivity loss by $ %.0f (95%% CI $ %.0f - $ %.0f) vs. M1",
            prod_m4,
            prod_m4_low,
            prod_m4_high)
  )
}

cal_ICER_mon <- function(ICER){

  print("Following ICER is calculated based on the provider perspectve (cost of vaccine, hospitalization)")

  # ICER (DALY averted per unit cost)
  print("ICER (M2 vs.M1):")

  print("Quarantine days averted")
  inc_cost <- ((ICER$vac_m2 - ICER$vac_m1)*c_vac +
                 (ICER$h_m2 - ICER$h_m1)*c_hos)
  if(mean(inc_cost) <0.1){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else if(mean(inc_cost) <0){
    print("Cost-saving (Incremental cost <0)")
  }else{
    inc_cost <- ((ICER$vac_m2 - ICER$vac_m1)*c_vac +
                   (ICER$h_m1 - ICER$h_m2)*c_hos)
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M2 vs. M1) = %.3f quarantine days averted per $ 10000 spent (95%% CrI %.3f - %.3f)",
              mean((ICER$q_m2 - ICER$q_m1)*-10000/inc_cost),
              quantile((ICER$q_m2 - ICER$q_m1)*-10000/inc_cost, 0.025),
              quantile((ICER$q_m2 - ICER$q_m1)*-10000/inc_cost, 0.975)
      )
    )
  }
  print("Hospitalization days averted")
  if(mean(inc_cost)<0.1 & mean(inc_cost)>=0){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else if(mean(inc_cost)<0){
    print("Cost-saving (Incremental cost <0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M2 vs. M1) = %.3f hospitalization days averted per $ 10000 spent (95%% CrI %.3f - %.3f)",
              mean((ICER$h_m2 - ICER$h_m1)*-10000/inc_cost),
              quantile((ICER$h_m2 - ICER$h_m1)*-10000/inc_cost, 0.025),
              quantile((ICER$h_m2 - ICER$h_m1)*-10000/inc_cost, 0.975)
      )
    )
  }
  print("Deaths averted")
  if(mean(inc_cost)<0.1 & mean(inc_cost)>=0){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else if(mean(inc_cost)<0){
    print("Cost-saving (Incremental cost <0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M2 vs. M1) = %.5f deaths averted per $ 10000 spent (95%% CrI %.5f - %.5f)",
              mean((ICER$d_m2 - ICER$d_m1)*-10000/inc_cost),
              quantile((ICER$d_m2 - ICER$d_m1)*-10000/inc_cost, 0.025),
              quantile((ICER$d_m2 - ICER$d_m1)*-10000/inc_cost, 0.975)
      )
    )
  }

  print("ICER (M3 vs. M1)")
  inc_cost2 <- ((ICER$vac_m3 - ICER$vac_m1)*c_vac +
                  ICER$scr_m3*c_scr +
                  (ICER$h_m3 - ICER$h_m1)*c_hos)

  print("Quarantine days averted")
  if(mean(inc_cost2)<0.1 & mean(inc_cost2)>=0){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else if(mean(inc_cost2)<0){
    print("Cost-saving (Incremental cost <0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M3 vs. M1) = %.3f quarantine days averted per $ 10000 spent (95%% CrI %.3f - %.3f)",
              mean((ICER$q_m3 - ICER$q_m1)*-10000/inc_cost2),
              quantile((ICER$q_m3 - ICER$q_m1)*-10000/inc_cost2, 0.025),
              quantile((ICER$q_m3 - ICER$q_m1)*-10000/inc_cost2, 0.975)
      )
    )
  }
  print("Hospitalization days averted")
  if(mean(inc_cost2)<0.1 & mean(inc_cost2)>=0){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else if(mean(inc_cost2)<0){
    print("Cost-saving (Incremental cost <0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M2 vs. M1) = %.3f hospitalization days averted per $ 10000 spent (95%% CrI %.3f - %.3f)",
              mean((ICER$h_m3 - ICER$h_m1)*-10000/inc_cost2),
              quantile((ICER$h_m3 - ICER$h_m1)*-10000/inc_cost2, 0.025),
              quantile((ICER$h_m3 - ICER$h_m1)*-10000/inc_cost2, 0.975)
      )
    )
  }
  print("Deaths averted")
  if(mean(inc_cost2)<0.1 & mean(inc_cost2)>=0){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else if(mean(inc_cost2)<0){
    print("Cost-saving (Incremental cost <0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M3 vs. M1) = %.5f deaths averted per $ 10000 spent (95%% CrI %.5f - %.5f)",
              mean((ICER$d_m3 - ICER$d_m1)*-10000/inc_cost2),
              quantile((ICER$d_m3 - ICER$d_m1)*-10000/inc_cost2, 0.025),
              quantile((ICER$d_m3 - ICER$d_m1)*-10000/inc_cost2, 0.975)
      )
    )
  }

  print("ICER (M4 vs. M1)")
  inc_cost3 <- ((ICER$vac_m4 - ICER$vac_m1)*c_vac +
                  ICER$scr_m4*c_scr +
                  (ICER$h_m4 - ICER$h_m1)*c_hos)

  print("Quarantine days averted")
  if(mean(inc_cost3)<0.1 & mean(inc_cost3)>=0){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else if(mean(inc_cost3)<0){
    print("Cost-saving (Incremental cost <0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M4 vs. M1) = %.3f quarantine days averted per $ 10000 spent (95%% CrI %.3f - %.3f)",
              mean((ICER$q_m4 - ICER$q_m1)*-10000/inc_cost3),
              quantile((ICER$q_m4 - ICER$q_m1)*-10000/inc_cost3, 0.025),
              quantile((ICER$q_m4 - ICER$q_m1)*-10000/inc_cost3, 0.975)
      )
    )
  }
  print("Hospitalization days averted")
  if(mean(inc_cost3)<0.1 & mean(inc_cost3)>=0){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else if(mean(inc_cost3)<0){
    print("Cost-saving (Incremental cost <0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M4 vs. M1) = %.3f hospitalization days averted per $ 10000 spent (95%% CrI %.3f - %.3f)",
              mean((ICER$h_m4 - ICER$h_m1)*-10000/inc_cost3),
              quantile((ICER$h_m4 - ICER$h_m1)*-10000/inc_cost3, 0.025),
              quantile((ICER$h_m4 - ICER$h_m1)*-10000/inc_cost3, 0.975)
      )
    )
  }
  print("Deaths averted")
  if(mean(inc_cost3)<0.1 & mean(inc_cost3)>=0){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else if(mean(inc_cost3)<0){
    print("Cost-saving (Incremental cost <0)")
  }else{
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M2 vs. M1) = %.5f deaths averted per $ 10000 spent (95%% CrI %.5f - %.5f)",
              mean((ICER$d_m4 - ICER$d_m1)*-10000/inc_cost3),
              quantile((ICER$d_m4 - ICER$d_m1)*-10000/inc_cost3, 0.025),
              quantile((ICER$d_m4 - ICER$d_m1)*-10000/inc_cost3, 0.975)
      )
    )
  }
}


cal_ICER_DALY <- function(ICER){

  print("Following ICER is calculated based on the provider perspectve (cost of vaccine, hospitalization)")

  # ICER (DALY averted per unit cost)
  print("ICER (M2 vs.M1):")

  inc_cost <- (ICER$vac_m2 - ICER$vac_m1)*(c_vac + prod_all) +
                 (ICER$h_m2 - ICER$h_m1)*(c_hos + prod_h)+
    (ICER$q_m2 - ICER$q_m1)*(c_mild + prod_q) +
    (ICER$d_m2 - ICER$d_m1)*(prod_death*1.015^(1:(life_exp - mean_age)))

  inc_util <- -1*((ICER$h_m2 - ICER$h_m1)*daly_h +
    (ICER$d_m2 - ICER$d_m1)*1.015^(1:(life_exp-mean_age)))

  if(mean(inc_cost)<0.1 & mean(inc_cost)>=0){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else if(mean(inc_cost)<0){
    print("Cost-saving (Incremental cost <0)")
  }else{
    icer <- inc_cost / inc_util
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M2 vs. M1) = %.0f (95%% CrI %.0f - %.0f)",
              mean(icer), quantile(icer, 0.025), quantile(icer, 0.975)
      )
    )
  }

  print("ICER (M3 vs. M1)")

  inc_cost <- (ICER$vac_m3 - ICER$vac_m1)*(c_vac + prod_all) +
    (ICER$h_m3 - ICER$h_m1)*(c_hos + prod_h)+
    (ICER$q_m3 - ICER$q_m1)*(c_mild + prod_q) +
    (ICER$scr_m3)*c_scr +
    (ICER$d_m3 - ICER$d_m1)*(prod_death*1.015^(1:(life_exp - mean_age)))

  inc_util <- -1*((ICER$h_m3 - ICER$h_m1)*daly_h +
                    (ICER$d_m3 - ICER$d_m1)*1.015^(1:(life_exp-mean_age)))

  if(mean(inc_cost)<0.1 & mean(inc_cost)>=0){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else if(mean(inc_cost)<0){
    print("Cost-saving (Incremental cost <0)")
  }else{
    icer <- inc_cost / inc_util
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M3 vs. M1) = %.0f (95%% CrI %.0f - %.0f)",
              mean(icer), quantile(icer, 0.025), quantile(icer, 0.975)
      )
    )
  }

  print("ICER (M4 vs. M1)")

  inc_cost <- (ICER$vac_m4 - ICER$vac_m1)*(c_vac + prod_all) +
    (ICER$h_m4 - ICER$h_m1)*(c_hos + prod_h)+
    (ICER$q_m4 - ICER$q_m1)*(c_mild + prod_q) +
    (ICER$scr_m4)*c_scr +
    (ICER$d_m4 - ICER$d_m1)*(prod_death*1.015^(1:(life_exp - mean_age)))

  inc_util <- -1*((ICER$h_m4 - ICER$h_m1)*daly_h +
                    (ICER$d_m4 - ICER$d_m1)*1.015^(1:(life_exp-mean_age)))

  if(mean(inc_cost)<0.1 & mean(inc_cost)>=0){
    print("Cannot calculate ICER (Incremental cost ==0)")
  }else if(mean(inc_cost)<0){
    print("Cost-saving (Incremental cost <0)")
  }else{
    icer <- inc_cost / inc_util
    print(
      sprintf("Incremental Cost-effectiveness Ratio (M4 vs. M1) = %.0f (95%% CrI %.0f - %.0f)",
              mean(icer), quantile(icer, 0.025), quantile(icer, 0.975)
      )
    )
  }
}

cal_nmb <- function(ICER){

  dat_nmb <- data.frame(
    strategy = c(rep("S2 vs. S1", 15),
                  rep("S3 vs. S1", 15),
                  rep("S4 vs. S1", 15)
    ),
    element = rep(c("Q", "H", "D", "Cost", "NMB"), 3),
    quant = rep(c(rep("mean",5), rep("low",5), rep("high", 5)), 3),
    value = rep(NaN, 45)
  )

  # M2 vs. M1
  prod_m2<-
    mean(ICER$q_m2 - ICER$q_m1)*-1*(prod_q + c_mild)  +
    mean(ICER$h_m2 - ICER$h_m1)*-1*(prod_h + c_hos) +
    mean(ICER$vac_m2 - ICER$vac_m1)*-1*(prod_all + c_vac) +
    mean(ICER$d_m2 - ICER$d_m1)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age)))

  dat_nmb$value[1:4] <- c(
    mean(ICER$q_m2 - ICER$q_m1)*-1*(prod_q + c_mild),
    mean(ICER$h_m2 - ICER$h_m1)*-1*(prod_h + c_hos),
    mean(ICER$d_m2 - ICER$d_m1)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age))),
    mean(ICER$vac_m2 - ICER$vac_m1)*-1*(prod_all + c_vac)
  )

  prod_m2_low<-
    quantile(ICER$q_m2 - ICER$q_m1, 0.975)*-1*(prod_q + c_mild)+
    quantile(ICER$h_m2 - ICER$h_m1, 0.975)*-1*(prod_h + c_hos)+
    quantile(ICER$vac_m2 - ICER$vac_m1, 0.975)*-1*(prod_all + c_vac) +
    quantile(ICER$d_m2 - ICER$d_m1, 0.975)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age)))

  dat_nmb$value[6:10] <- c(
    quantile(ICER$q_m2 - ICER$q_m1, 0.975)*-1*(prod_q + c_mild),
      quantile(ICER$h_m2 - ICER$h_m1, 0.975)*-1*(prod_h + c_hos),
    quantile(ICER$d_m2 - ICER$d_m1, 0.975)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age))),
      quantile(ICER$vac_m2 - ICER$vac_m1, 0.975)*-1*(prod_all + c_vac)
  )

  prod_m2_high <-
    quantile(ICER$q_m2 - ICER$q_m1, 0.025)*-1*(prod_q + c_mild)+
    quantile(ICER$h_m2 - ICER$h_m1, 0.025)*-1*(prod_h + c_hos)+
    quantile(ICER$vac_m2 - ICER$vac_m1, 0.025)*-1*(prod_all + c_vac)+
    quantile(ICER$d_m2 - ICER$d_m1, 0.025)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age)))

  dat_nmb$value[11:14] <- c(
    quantile(ICER$q_m2 - ICER$q_m1, 0.025)*-1*(prod_q + c_mild),
      quantile(ICER$h_m2 - ICER$h_m1, 0.025)*-1*(prod_h + c_hos),
    quantile(ICER$d_m2 - ICER$d_m1, 0.025)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age))),
      quantile(ICER$vac_m2 - ICER$vac_m1, 0.025)*-1*(prod_all + c_vac)
  )

  dat_nmb$value[c(5, 10, 15)] <- c(prod_m2, prod_m2_low, prod_m2_high)
  print(
    sprintf("M2 resulted in net monetary benefit of $ %.0f (95%% CI $ %.0f - $ %.0f) vs. M1",
            prod_m2,
            prod_m2_low,
            prod_m2_high)
  )

  # M3 vs. M1
  prod_m3<-
    mean(ICER$q_m3 - ICER$q_m1)*-1*(prod_q + c_mild)+
    mean(ICER$h_m3 - ICER$h_m1)*-1*(prod_h + c_hos) +
    mean(ICER$vac_m3 - ICER$vac_m1)*-1*(prod_all + c_vac) -
    mean(ICER$scr_m3)*c_scr +
    mean(ICER$d_m3 - ICER$d_m1)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age)))

  dat_nmb$value[16:19] <- c(
    mean(ICER$q_m3 - ICER$q_m1)*-1*(prod_q + c_mild),
    mean(ICER$h_m3 - ICER$h_m1)*-1*(prod_h + c_hos),
    mean(ICER$d_m3 - ICER$d_m1)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age))),
    mean(ICER$vac_m3 - ICER$vac_m1)*-1*(prod_all + c_vac) -
      mean(ICER$scr_m3)*c_scr
  )

  prod_m3_low<-
    quantile(ICER$q_m3 - ICER$q_m1, 0.975)*-1*(prod_q + c_mild)+
    quantile(ICER$h_m3 - ICER$h_m1, 0.975)*-1*(prod_h + c_hos)+
    quantile(ICER$vac_m3 - ICER$vac_m1, 0.975)*-1*(prod_all + c_vac) -
    quantile(ICER$scr_m3, 0.975)*c_scr +
    quantile(ICER$d_m3 - ICER$d_m1, 0.975)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age)))

  dat_nmb$value[21:24] <- c(
    quantile(ICER$q_m3 - ICER$q_m1, 0.975)*-1*(prod_q + c_mild),
      quantile(ICER$h_m3 - ICER$h_m1, 0.975)*-1*(prod_h + c_hos),
      quantile(ICER$d_m3 - ICER$d_m1, 0.975)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age))),
    quantile(ICER$vac_m3 - ICER$vac_m1, 0.975)*-1*(prod_all + c_vac) -
      quantile(ICER$scr_m3, 0.975)*c_scr
  )


  prod_m3_high <-
    quantile(ICER$q_m3 - ICER$q_m1, 0.025)*-1*(prod_q + c_mild)+
    quantile(ICER$h_m3 - ICER$h_m1, 0.025)*-1*(prod_h + c_hos)+
    quantile(ICER$vac_m3 - ICER$vac_m1, 0.025)*-1*(prod_all + c_vac) -
    quantile(ICER$scr_m3, 0.025)*c_scr +
    quantile(ICER$d_m3 - ICER$d_m1, 0.025)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age)))

  dat_nmb$value[26:29] <- c(
    quantile(ICER$q_m3 - ICER$q_m1, 0.025)*-1*(prod_q + c_mild),
      quantile(ICER$h_m3 - ICER$h_m1, 0.025)*-1*(prod_h + c_hos),
      quantile(ICER$d_m3 - ICER$d_m1, 0.025)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age))),
    quantile(ICER$vac_m3 - ICER$vac_m1, 0.025)*-1*(prod_all + c_vac) -
      quantile(ICER$scr_m3, 0.025)*c_scr

  )

  dat_nmb$value[c(20, 25, 30)] <- c(prod_m3, prod_m3_low, prod_m3_high)
  print(
    sprintf("M3 resulted in net monetary benefit of $ %.0f (95%% CI $ %.0f - $ %.0f) vs. M1",
            prod_m3,
            prod_m3_low,
            prod_m3_high)
  )

  # M4 vs. M1
  prod_m4<-
    mean(ICER$q_m4 - ICER$q_m1)*-1*(prod_q + c_mild)+
    mean(ICER$h_m4 - ICER$h_m1)*-1*(prod_h + c_hos)+
    mean(ICER$vac_m4 - ICER$vac_m1)*-1*(prod_all + c_vac) -
    mean(ICER$scr_m4)*c_scr +
    mean(ICER$d_m4 - ICER$d_m1)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age)))

  dat_nmb$value[31:34] <- c(
    mean(ICER$q_m4 - ICER$q_m1)*-1*(prod_q + c_mild),
    mean(ICER$h_m4 - ICER$h_m1)*-1*(prod_h + c_hos),
    mean(ICER$d_m4 - ICER$d_m1)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age))),
    mean(ICER$vac_m4 - ICER$vac_m1)*-1*(prod_all + c_vac) -
      mean(ICER$scr_m4)*c_scr
  )

  prod_m4_low<-
    quantile(ICER$q_m4 - ICER$q_m1, 0.975)*-1*(prod_q + c_mild) +
    quantile(ICER$h_m4 - ICER$h_m1, 0.975)*-1*(prod_h + c_hos) +
    quantile(ICER$vac_m4 - ICER$vac_m1, 0.975)*-1*(prod_all + c_vac)-
    quantile(ICER$scr_m4, 0.975)*c_scr +
    quantile(ICER$d_m4 - ICER$d_m1, 0.975)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age)))

  dat_nmb$value[36:39]<-c(
    quantile(ICER$q_m4 - ICER$q_m1, 0.975)*-1*(prod_q + c_mild),
      quantile(ICER$h_m4 - ICER$h_m1, 0.975)*-1*(prod_h + c_hos),
      quantile(ICER$d_m4 - ICER$d_m1, 0.975)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age))),
    quantile(ICER$vac_m4 - ICER$vac_m1, 0.975)*-1*(prod_all + c_vac)-
      quantile(ICER$scr_m4, 0.975)*c_scr
  )

  prod_m4_high <-
    quantile(ICER$q_m4 - ICER$q_m1, 0.025)*-1*(prod_q + c_mild)+
    quantile(ICER$h_m4 - ICER$h_m1, 0.025)*-1*(prod_h + c_hos)+
    quantile(ICER$vac_m4 - ICER$vac_m1, 0.025)*-1*(prod_all + c_vac)-
    quantile(ICER$scr_m4, 0.025)*c_scr +
    quantile(ICER$d_m4 - ICER$d_m1, 0.025)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age)))

  dat_nmb$value[41:44] <- c(
    quantile(ICER$q_m4 - ICER$q_m1, 0.025)*-1*(prod_q + c_mild),
      quantile(ICER$h_m4 - ICER$h_m1, 0.025)*-1*(prod_h + c_hos),
      quantile(ICER$d_m4 - ICER$d_m1, 0.025)*-1*sum((prod_death)*dis^(0:(life_exp - mean_age))),
    quantile(ICER$vac_m4 - ICER$vac_m1, 0.025)*-1*(prod_all + c_vac)-
      quantile(ICER$scr_m4, 0.025)*c_scr
  )

  dat_nmb$value[c(35, 40, 45)] <- c(prod_m4, prod_m4_low, prod_m4_high)
  print(
    sprintf("M4 resulted in net monetary benefit of $ %.0f (95%% CI $ %.0f - $ %.0f) vs. M1",
            prod_m4,
            prod_m4_low,
            prod_m4_high)
  )


  dat_nmb %<>%
    mutate(
      element = factor(element, levels = c("Q", "H", "D", "Cost", "NMB")),
      value = ifelse(element=="Cost", -value, value)
      )

  # summary plot
  sum_plot <-
    ggplot()+
    geom_bar(data = dat_nmb %>%
               filter(!quant %in% c("high", "low")),
             aes(element, value, fill = element), stat = "identity")+
    geom_errorbar(data = dat_nmb%>%
                    filter(quant %in% c("high", "low")) %>%
                    spread(key = quant, value = value),
                  aes(x=element, ymin = low, ymax=high), size = 0.3, color = "black")+
    geom_hline(yintercept = 0, color = "black")+
    facet_wrap(.~strategy) +
    xlab("Component") +
    ylab("Value ($)") +
    scale_y_continuous(labels = scales::label_comma(accuracy = NULL, scale = 1/1000000,
                                                    prefix = "", suffix = "MM",
                                                    big.mark = ",", decimal.mark = "."))+
    theme(legend.position = "right",
          legend.title = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),) +
    guides(fill=guide_legend(nrow=5,byrow=TRUE))+
    scale_fill_discrete(
      labels = c("Q" = "Q = Productivity savings from averted quarantine days",
                 "H" = "H = Productivity savgings from averted hospitalization days",
                 "D" = "D = Productivity savgings from averted death",
                 "Cost" = "Incremental cost",
                 "NMB"= " NMB = Net Monetary Benefit")
    )
  return(sum_plot)
}

nmb <- function(M2_res, M1_res, scr = F){
  nmb <-
    -1*(sum(M2_res$Q, M2_res$QV1, M2_res$QV2) - sum(M1_res$Q, M1_res$QV1, M1_res$QV2))*(prod_q + c_mild) -
    (sum(M2_res$H, M2_res$HV1, M2_res$HV2) - sum(M1_res$H, M1_res$HV1, M1_res$HV2))*(prod_h + c_hos) -
    (sum(M2_res$PI1, M2_res$PI2) - sum(M1_res$PI1, M1_res$PI2))*(prod_all + c_vac) -
    (sum(M2_res$D[tmax], M2_res$DV1[tmax], M2_res$DV2[tmax]) -
       sum(M1_res$D[tmax], M1_res$DV1[tmax], M1_res$DV2[tmax]))*sum(prod_death*1.015^(0:(life_exp-mean_age)))

  if(scr){
    nmb <- nmb - sum(M2_res$SCR)*c_scr
  }

  return(nmb)
}

icer <- function(M2_res, M1_res, scr = F){
  inc_cost <-
    (sum(M2_res$Q, M2_res$QV1, M2_res$QV2) - sum(M1_res$Q, M1_res$QV1, M1_res$QV2))*(prod_q + c_mild) +
    (sum(M2_res$H, M2_res$HV1, M2_res$HV2) - sum(M1_res$H, M1_res$HV1, M1_res$HV2))*(prod_h + c_hos) +
    (sum(M2_res$PI1, M2_res$PI2) - sum(M1_res$PI1, M1_res$PI2))*(prod_all + c_vac) +
    (sum(M2_res$D[tmax], M2_res$DV1[tmax], M2_res$DV2[tmax]) +
       sum(M1_res$D[tmax], M1_res$DV1[tmax], M1_res$DV2[tmax]))*sum(prod_death*1.015^(0:(life_exp-mean_age)))

  if(scr){
    inc_cost <- inc_cost + sum(M2_res$SCR*c_scr)
  }

  inc_util <-
    -(sum(M2_res$H, M2_res$HV1, M2_res$HV2) - sum(M1_res$H, M1_res$HV1, M1_res$HV2))*daly_h -
    sum((sum(M2_res$D[tmax], M2_res$DV1[tmax], M2_res$DV2[tmax]) -
       sum(M1_res$D[tmax], M1_res$DV1[tmax], M1_res$DV2[tmax]))*1.015^(0:(life_exp-mean_age)))

  if(inc_cost>0){
    return(inc_cost/inc_util)
  }else{
    return(0)
  }

}
