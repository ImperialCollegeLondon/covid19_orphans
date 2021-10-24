library(cowplot)
library(xtable)

load("TheLancet_global_minimum_estimates_2021/data/extrapolate_primary.RData")
load("TheLancet_global_minimum_estimates_2021/data/extrapolate_primary_secondary.RData")
load("TheLancet_global_minimum_estimates_2021/data/extrapolate_parent.RData")

p_obs_pred <- cowplot::plot_grid(p_fit_pa_label, p_fit_p_label, p_fit_ps_label, labels = "AUTO", ncol=3)
cowplot::save_plot("TheLancet_global_minimum_estimates_2021/figures/fig4.pdf", p_obs_pred, base_width = 18, base_height = 6)

ggsave("TheLancet_global_minimum_estimates_2021/figures/primary_plot.pdf", p_fit_p_label)
p_all <- cowplot::plot_grid(p_fit_pa_label, p_obs_pred_pa, 
                            p_fit_p_label, p_obs_pred_p, 
                            p_fit_ps_label, p_obs_pred_ps, labels = "AUTO", ncol = 2)
cowplot::save_plot("TheLancet_global_minimum_estimates_2021/figures/logistic_fit.pdf", p_all, base_width = 12, base_height = 18)

p <- cowplot::plot_grid(p_loo_pa, p_loo_p, p_loo_ps, labels = "AUTO", ncol = 3)
cowplot::save_plot("TheLancet_global_minimum_estimates_2021/figures/loo_fit.pdf", p, base_width = 18, base_height =6)


parents <- readRDS("TheLancet_global_minimum_estimates_2021/data/country_estimates_pa.RDS")
primary <- readRDS("TheLancet_global_minimum_estimates_2021/data/country_estimates_p.RDS")
primary_secondary <- readRDS("TheLancet_global_minimum_estimates_2021/data/country_estimates_ps.RDS")

all <- left_join(parents, primary, by = c("country", "region"))
all <- left_join(all, primary_secondary, by = c("country", "region"))
all <- select(all, country, text_pa, text_p, text_ps)

tab<-xtable(all)
#print(tab, include.rownames=FALSE)