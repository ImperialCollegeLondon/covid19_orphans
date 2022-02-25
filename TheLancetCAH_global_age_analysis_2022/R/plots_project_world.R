library(cowplot)
library(xtable)

load("TheLancetCAH_global_age_analysis_2022/data/extrapolate_primary.RData")
load("TheLancetCAH_global_age_analysis_2022/data/extrapolate_primary_secondary.RData")
load("TheLancetCAH_global_age_analysis_2022/data/extrapolate_parent.RData")

p_obs_pred <- cowplot::plot_grid(p_fit_pa_label, p_fit_p_label, p_fit_ps_label, labels = "AUTO", ncol=3)
cowplot::save_plot("TheLancetCAH_global_age_analysis_2022/figures/model_fit.pdf", p_obs_pred, base_width = 18, base_height = 6)

ggsave("TheLancetCAH_global_age_analysis_2022/figures/primary_plot.pdf", p_fit_p_label)
p_all <- cowplot::plot_grid(p_fit_pa_label, p_obs_pred_pa, 
                            p_fit_p_label, p_obs_pred_p, 
                            p_fit_ps_label, p_obs_pred_ps, labels = "AUTO", ncol = 2)
cowplot::save_plot("TheLancetCAH_global_age_analysis_2022/figures/logistic_fit.pdf", p_all, base_width = 12, base_height = 18)

