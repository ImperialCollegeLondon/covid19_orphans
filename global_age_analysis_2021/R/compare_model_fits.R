load("data/data_model_comparison_parent_apr.Rdata")
p_parents <- ggplot(subset) + 
  geom_point(aes(tfr, parent_ratio)) + 
  geom_line(data = line_all, aes(x, y0), col = 'black') + 
  geom_line(data = line_all, aes(x, y1), col = 'red') + 
  xlab("Total fertility rate") + ylab("Ratio of children orphaned to deaths") + 
  geom_text_repel(aes(x = tfr, y = parent_ratio, label = country), size = 3, max.overlaps = 100) + 
  theme_bw()

load("data/data_model_comparison_parent_oct.Rdata")
p_parents <- p_parents + 
  geom_point(data = subset, aes(tfr, parent_ratio), col = "blue") + 
  geom_line(data = line_all, aes(x, y0), col = 'black', linetype = "dashed") + 
  geom_line(data = line_all, aes(x, y1), col = 'red', linetype = "dashed") + 
  xlab("Total fertility rate") + ylab("Ratio of children orphaned to deaths") + 
  #geom_text_repel(aes(x = tfr, y = parent_ratio, label = country), size = 3, max.overlaps = 100, col= "blue") + 
  theme_bw()


load("data/data_model_comparison_primary_apr.Rdata")
p_primary <- ggplot(subset) + 
  geom_point(aes(tfr, primary_ratio)) + 
  geom_line(data = line_all, aes(x, y0), col = 'black') + 
  geom_line(data = line_all, aes(x, y1), col = 'red') + 
  xlab("Total fertility rate") + ylab("Ratio of children orphaned to deaths") + 
  geom_text_repel(aes(x = tfr, y = primary_ratio, label = country), size = 3, max.overlaps = 100) + 
  theme_bw()

load("data/data_model_comparison_primary_oct.Rdata")
p_primary <- p_primary + 
  geom_point(data = subset, aes(tfr, primary_ratio), col = "blue") + 
  geom_line(data = line_all, aes(x, y0), col = 'black', linetype = "dashed") + 
  geom_line(data = line_all, aes(x, y1), col = 'red', linetype = "dashed") + 
  xlab("Total fertility rate") + ylab("Ratio of children orphaned to deaths") + 
  #geom_text_repel(aes(x = tfr, y = primary_ratio, label = country), size = 3, max.overlaps = 100, col= "blue") + 
  theme_bw()


load("data/data_model_comparison_primary_secondary_apr.Rdata")
p_primary_secondary <- ggplot(subset) + 
  geom_point(aes(tfr, ratio)) + 
  geom_line(data = line_all, aes(x, y0), col = 'black') + 
  geom_line(data = line_all, aes(x, y1), col = 'red') + 
  xlab("Total fertility rate") + ylab("Ratio of children losing primary &/or secondary caregivers to deaths") + 
  geom_text_repel(aes(x = tfr, y = ratio, label = country), size = 3, max.overlaps = 100) + 
  theme_bw()

load("data/data_model_comparison_primary_secondary_oct.Rdata")
p_primary_secondary <- p_primary_secondary + 
  geom_point(data = subset, aes(tfr, ratio), col = "blue") + 
  geom_line(data = line_all, aes(x, y0), col = 'black', linetype = "dashed") + 
  geom_line(data = line_all, aes(x, y1), col = 'red', linetype = "dashed") + 
  xlab("Total fertility rate") + ylab("Ratio of children losing primary &/or secondary caregivers to deaths") + 
  #geom_text_repel(aes(x = tfr, y = ratio, label = country), size = 3, max.overlaps = 100, col= "blue") + 
  theme_bw()


p <- cowplot::plot_grid(p_parents, p_primary, p_primary_secondary, ncol = 2)
ggsave("figures/compare_model_fits.pdf", width = 10, height = 10)
