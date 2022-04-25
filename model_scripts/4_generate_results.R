# print out results
spit_result(ICER)

ggthemr("fresh")


# CE pane
ymax_q <- ceiling(max(abs(ICER$q_m4 - ICER$q_m1), abs(ICER$q_m3 - ICER$q_m1), abs(ICER$q_m2-ICER$q_m1))/100)*100
ymax_h <- ceiling(max(abs(ICER$h_m4 - ICER$h_m1), abs(ICER$h_m3 - ICER$h_m1), abs(ICER$h_m2-ICER$h_m1))/100)*100
ymax_d <- ceiling(max(abs(ICER$d_m4 - ICER$d_m1), abs(ICER$d_m3 - ICER$d_m1), abs(ICER$d_m2-ICER$d_m1))/20)*20
xmax <- ceiling(max(abs(ICER$vac_m4+ICER$scr_m4 - ICER$vac_m1), abs(ICER$vac_m3+ICER$scr_m3 - ICER$vac_m1), abs(ICER$vac_m2-ICER$vac_m1))/100)*100

cep_q <-
  ggplot(ICER) +
  geom_hline(yintercept=0, color = "grey")+
  geom_vline(xintercept=0, color = "grey")+
  geom_point(aes(y=(q_m2 - q_m1)*-1,
                 x=(vac_m2 - vac_m1),
                 color = "M2 vs. M1"),
             size = .5) +
  geom_point(aes(y=(q_m3 - q_m1)*-1,
                 x=(vac_m3 + scr_m3 - vac_m1),
                 color = "M3 vs. M1"),
             size = .5) +
  geom_point(aes(y=(q_m4 - q_m1)*-1,
                 x=(vac_m4 + scr_m4 - vac_m1),
                 color = "M4 vs. M1"),
             size = .5) +

  ggtitle("Incremental Cost-Effectiveness Pane",
          subtitle = "Number of quarantine days averted")+
  scale_x_continuous(limits = c(-xmax, xmax))+
  scale_y_continuous(limits = c(-ymax_q, ymax_q)) +
  xlab("Incremental Doses (Vaccine + Screening)") +
  ylab("Number of Quarantine Days Averted")

cep_h <-
ggplot(ICER) +
  geom_hline(yintercept=0, color = "grey")+
  geom_vline(xintercept=0, color = "grey")+
  geom_point(aes(y=(h_m2 - h_m1)*-1,
                 x=(vac_m2 - vac_m1),
                 color = "M2 vs. M1"),
             size = .5) +
  geom_point(aes(y=(h_m3 - h_m1)*-1,
                 x=(vac_m3 + scr_m3 - vac_m1),
                 color = "M3 vs. M1"),
             size = .5) +
  geom_point(aes(y=(h_m4 - h_m1)*-1,
                 x=(vac_m4 + scr_m4 - vac_m1),
                 color = "M4 vs. M1"),
             size = .5) +
  ggtitle("Incremental Cost-Effectiveness Pane",
          subtitle = "Number of hospitalized days averted")+
  scale_x_continuous(limits = c(-xmax, xmax))+
  scale_y_continuous(limits = c(-ymax_h, ymax_h)) +
  xlab("Incremental Doses") +
  ylab("Number of Hospitalized Days Averted")

cep_d <-
ggplot(ICER) +
  geom_hline(yintercept=0, color = "grey")+
  geom_vline(xintercept=0, color = "grey")+
  geom_point(aes(y=(d_m2 - d_m1)*-1,
                 x=(vac_m2 - vac_m1),
                 color = "M2 vs. M1"),
             size = .5) +
  geom_point(aes(y=(d_m3 - d_m1)*-1,
                 x=(vac_m3 + scr_m3 - vac_m1),
                 color = "M3 vs. M1"),
             size = .5) +
  geom_point(aes(y=(d_m4 - d_m1)*-1,
                 x=(vac_m4 + scr_m4 - vac_m1),
                 color = "M4 vs. M1"),
             size = .5) +
  ggtitle("Incremental Cost-Effectiveness Pane",
          subtitle = "Number of deaths averted")+
  scale_x_continuous(limits = c(-xmax, xmax))+
  scale_y_continuous(limits = c(-ymax_d, ymax_d)) +
  xlab("Incremental Doses") +
  ylab("Number of Deaths Averted")

ggsave(plot = cep_q,
       filename = here::here("results/CE_plane_quarantine.png"),
       width = 6.5,
       height = 5.5,
       dpi = 100
)
ggsave(plot = cep_h,
       filename = here::here("results/CE_plane_hospitalized.png"),
       width = 6.5,
       height = 5.5,
       dpi = 100
)
ggsave(plot = cep_d,
       filename = here::here("results/CE_plane_death.png"),
       width = 6.5,
       height = 5.5,
       dpi = 100
)

# # of hospitalized, quarantined, death over time
M1_sum <- collapse_res(M1_res)
M2_sum <- collapse_res(M2_res)
M3_sum <- collapse_res(M3_res)
M4_sum <- collapse_res(M4_res)


# Quarantined
q<- ggplot()+
  geom_path(data = M1_sum, aes(x=time, y = log(q_mean), color = "M1"), size = 1) +
  geom_path(data = M1_sum, aes(x=time, y=log(q_low_ci), color="M1", linetype = "95% CI"))+
  geom_path(data = M1_sum, aes(x=time, y=log(q_high_ci), color="M1", linetype = "95% CI")) +

  geom_path(data = M2_sum, aes(x=time, y = log(q_mean), color = "M2"), size = 1) +
  geom_path(data = M2_sum, aes(x=time, y= log(q_low_ci), color="M2", linetype = "95% CI"))+
  geom_path(data = M2_sum, aes(x=time, y= log(q_high_ci), color="M2", linetype = "95% CI")) +

  geom_path(data = M3_sum, aes(x=time, y = log(q_mean), color = "M3"), size = 1) +
  geom_path(data = M3_sum, aes(x=time, y= log(q_low_ci), color="M3", linetype = "95% CI"))+
  geom_path(data = M3_sum, aes(x=time, y= log(q_high_ci), color="M3",linetype = "95% CI"))+

  geom_path(data = M4_sum, aes(x=time, y = log(q_mean), color = "M4"), size = 1) +
  geom_path(data = M4_sum, aes(x=time, y= log(q_low_ci), color="M4", linetype = "95% CI"))+
  geom_path(data = M4_sum, aes(x=time, y= log(q_high_ci), color="M4",linetype = "95% CI"))+

  scale_linetype_manual(values = c("95% CI" = "dotted"))+
  theme(legend.title = element_blank()) +
  xlab("Time") +
  ylab("# of People Quarantined (log)")

# Hospitalized
h <- ggplot()+
  geom_path(data = M1_sum, aes(x=time, y = log(h_mean), color = "M1"), size = 1) +
  geom_path(data = M1_sum, aes(x=time, y=log(h_low_ci), color="M1", linetype = "95% CI"))+
  geom_path(data = M1_sum, aes(x=time, y=log(h_high_ci), color="M1", linetype = "95% CI")) +

  geom_path(data = M2_sum, aes(x=time, y = log(h_mean), color = "M2"), size = 1) +
  geom_path(data = M2_sum, aes(x=time, y= log(h_low_ci), color="M2", linetype = "95% CI"))+
  geom_path(data = M2_sum, aes(x=time, y= log(h_high_ci), color="M2", linetype = "95% CI")) +

  geom_path(data = M3_sum, aes(x=time, y = log(h_mean), color = "M3"), size = 1) +
  geom_path(data = M3_sum, aes(x=time, y= log(h_low_ci), color="M3", linetype = "95% CI"))+
  geom_path(data = M3_sum, aes(x=time, y= log(h_high_ci), color="M3",linetype = "95% CI"))+

  geom_path(data = M4_sum, aes(x=time, y = log(h_mean), color = "M4"), size = 1) +
  geom_path(data = M4_sum, aes(x=time, y= log(h_low_ci), color="M4", linetype = "95% CI"))+
  geom_path(data = M4_sum, aes(x=time, y= log(h_high_ci), color="M4",linetype = "95% CI"))+

  scale_linetype_manual(values = c("95% CI" = "dotted")) +
  theme(legend.title = element_blank()) +
  xlab("Time") +
  ylab("# of People Hospitalized (log)")

# Death
d <- ggplot()+
  geom_path(data = M1_sum, aes(x=time, y = log(d_mean), color = "M1"), size = 1) +
  geom_path(data = M1_sum, aes(x=time, y=log(d_low_ci), color="M1", linetype = "95% CI"))+
  geom_path(data = M1_sum, aes(x=time, y=log(d_high_ci), color="M1", linetype = "95% CI")) +

  geom_path(data = M2_sum, aes(x=time, y = log(d_mean), color = "M2"), size = 1) +
  geom_path(data = M2_sum, aes(x=time, y= log(d_low_ci), color="M2", linetype = "95% CI"))+
  geom_path(data = M2_sum, aes(x=time, y= log(d_high_ci), color="M2", linetype = "95% CI")) +

  geom_path(data = M3_sum, aes(x=time, y = log(d_mean), color = "M3"), size = 1) +
  geom_path(data = M3_sum, aes(x=time, y= log(d_low_ci), color="M3", linetype = "95% CI"))+
  geom_path(data = M3_sum, aes(x=time, y= log(d_high_ci), color="M3",linetype = "95% CI"))+

  geom_path(data = M4_sum, aes(x=time, y = log(d_mean), color = "M4"), size = 1) +
  geom_path(data = M4_sum, aes(x=time, y= log(d_low_ci), color="M4", linetype = "95% CI"))+
  geom_path(data = M4_sum, aes(x=time, y= log(d_high_ci), color="M4",linetype = "95% CI"))+

  scale_linetype_manual(values = c("95% CI" = "dotted"))+
  theme(legend.title = element_blank()) +
  xlab("Time") +
  ylab("Cumulative # of Death (log)")


plot_tot <-
  ggpubr::ggarrange(q, h, d,
                  ncol = 3, nrow = 1,
                  labels = c("Quarantined (log)", "Hospitalized (log)", "   Death"),
                  common.legend=T)

ggsave(plot = plot_tot,
       filename = here::here("results/n_over_time.png"),
       width = 14,
       height = 6,
       dpi = 100
)

# Table 1
res <- apply(ICER, 2, function(x){
  sprintf("%.0f (95%% CI: %.0f - %.0f)",
          mean(x, na.rm=T),
          quantile(x, 0.025),
          quantile(x, 0.975))
})

tab1 <- matrix(c(res[1:4], NA, NA, res[5:18]),
         ncol = 4, byrow=T) %>% data.frame()
colnames(tab1) <- c("S1", "S2", "S3", "S4")
rownames(tab1) <- c("Total vaccine doses used",
                    "Total screenings",
                    "Total quarantine days",
                    "Total hospitalization days",
                    "Total death")
rio::export(tab1,
            here::here("results", "table1.xlsx"),
            row.names=T)
