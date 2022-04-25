# Figure 1

ICER_base <- readRDS(here::here("simulation_objects", "base_scenario.RDS"))
ICER_naive <- readRDS(here::here("simulation_objects", "v_naive_result.RDS"))

sum_p_base <- cal_nmb(ICER_base)
sum_p_naive <- cal_nmb(ICER_naive)

sum_plot <-
  ggpubr::ggarrange(
  sum_p_base + labs(title ="A. Base scenario (Initial V1 = 51.6%, R = 3.5%)"),
  sum_p_naive + labs(title = "B. Vaccine-naive population ((Initial V1 = 0.0%, R = 3.5%)"),
  common.legend=T,
  legend = "right",
  ncol = 1,
  hjust=0.8
)

ggsave(plot = sum_plot,
       filename = here::here("results/Figure1.png"),
       width = 12,
       height = 6,
       dpi = 100
)

# Figure 2
dose <- "800K doses/day"

ICER_sen_1 <- readRDS(here::here("simulation_objects", "result_sen.RDS")) %>%
  mutate(
    vac_label = case_when(
      vac_daily == 640000 ~ "640K doses/day",
      vac_daily == 800000 ~ "800K doses/day",
      vac_daily == 1200000 ~ "1.2M doses/day",
      vac_daily ==1600000 ~ "1.6M doses/day",
      vac_daily == 4000000 ~ "4.0M doses/day",
      vac_daily == 8000000 ~ "8.0M doses/day",
      TRUE ~ NA_character_
    )
  ) %>% mutate(
    vac_label = factor(vac_label, levels = c("640K doses/day",
                                             "800K doses/day",
                                             "1.2M doses/day",
                                             "1.6M doses/day",
                                             "4.0M doses/day",
                                             "8.0M doses/day"))
  )%>%
  gather("res_type", "value", -"R", -"V1", -"vac_daily", -"vac_label") %>%
  mutate(
    facet = gsub("[[:punct:]][[:lower:]]+", " vs. S1", res_sen_long$res_type),
    facet = gsub("M", "S", facet)
  )


ICER_sen_5 <- readRDS(here::here("simulation_objects", "result_sen_5yrs.RDS")) %>%
  mutate(
  vac_label = case_when(
    vac_daily == 640000 ~ "640K doses/day",
    vac_daily == 800000 ~ "800K doses/day",
    vac_daily == 1200000 ~ "1.2M doses/day",
    vac_daily ==1600000 ~ "1.6M doses/day",
    vac_daily == 4000000 ~ "4.0M doses/day",
    vac_daily == 8000000 ~ "8.0M doses/day",
    TRUE ~ NA_character_
  )
) %>% mutate(
  vac_label = factor(vac_label, levels = c("640K doses/day",
                                           "800K doses/day",
                                           "1.2M doses/day",
                                           "1.6M doses/day",
                                           "4.0M doses/day",
                                           "8.0M doses/day"))
) %>%
  gather("res_type", "value", -"R", -"V1", -"vac_daily", -"vac_label") %>%
  mutate(
    facet = gsub("[[:punct:]][[:lower:]]+", " vs. S1", res_sen_long$res_type),
    facet = gsub("M", "S", facet)
  )


  dat <- ICER_sen_1 %>%
    filter(vac_label==dose,
           grepl("nmb", res_type))

  dat2 <- ICER_sen_5 %>%
    filter(vac_label==dose,
           grepl("nmb", res_type))

  col_break <- sort(unique(c(min(dat$value),
                             max(dat$value),
                             0)))

  if(min(col_break)==0) col_break <- col_break[2:length(col_break)]

  col_break2 <- sort(unique(c(min(dat2$value),
                             max(dat2$value),
                             0)))

  if(min(col_break2)==0) col_break2 <- col_break2[2:length(col_break2)]

  # NMB plane
  p_nmb_1<- ggplot(dat,
                 aes(x=R/S0*100, y=V1/S0*100)) +
    geom_tile(aes(fill=value)) +
    facet_wrap(facet~.,
               scales = "free")+
    scale_fill_viridis_c(option="A",
                         trans = scales::pseudo_log_trans(sigma = 10),
                         breaks = col_break,
                         labels = paste0(round(col_break/1000000000,2), "B")) +
    xlab("Initial prevalence recovered population (%)")+
    ylab("Initial coverage of the first dose (%)")+
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 8),
          axis.text = element_text(size = 8),
          axis.title=element_text(size = 8))+
    guides(fill = guide_colourbar(barwidth = 0.5, barheight = 5))+
    scale_x_continuous(expand=c(0,0))

  p_nmb_5 <- ggplot(dat2,
                                aes(x=R/S0*100, y=V1/S0*100)) +
    geom_tile(aes(fill=value)) +
    facet_wrap(facet~.,
               scales = "free")+
    scale_fill_viridis_c(option="A",
                         trans = scales::pseudo_log_trans(sigma = 10),
                         breaks = col_break2,
                         labels = paste0(round(col_break2/1000000000,2), "B")) +
    xlab("Initial prevalence recovered population (%)")+
    ylab("Initial coverage of the first dose (%)")+
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 8),
          axis.text = element_text(size = 8),
          axis.title=element_text(size = 8))+
    guides(fill = guide_colourbar(barwidth = 0.5, barheight = 5))+
    scale_x_continuous(expand=c(0,0))




fig2 <- ggpubr::ggarrange(p_nmb_1 + labs(title = "A. Net monetary benefit (NMB) from 1-year simulation"),
                  p_nmb_5 + labs(title = "B. Net monetary benefit (NMB) from 5-year simulation"),
                  ncol = 1,
                  legend = "right")

ggsave(plot = fig2,
       filename = here::here("results/Figure2.png"),
       width = 8,
       height = 6,
       dpi = 100
)
