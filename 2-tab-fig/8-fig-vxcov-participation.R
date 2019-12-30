########################################
# Shoo the Flu evaluation
# Analysis of student influenza vaccination coverage 

# Plot of association between vaccination coverage
# and school participation in intervention
########################################

rm(list = ls())
# define directories, load libraries
source(here::here("0-config.R"))

data = read.csv(file = coverage_participation_data_path)
sl_pred_df = readRDS(vax_coverage_participation)

data = data %>% mutate(
  coverage = coverage * 100,
  participation = participation * 100)

pdf(file=paste0(plot_path,"fig-vxcov-participation.pdf"),width=10,height=3)
ggplot(sl_pred_df, aes(x = participation, y = pred)) + 
  geom_line() + 
  geom_point(data = data, aes(x = participation, y = coverage), alpha = 0.5) + 
  facet_grid(~schoolyr) +
  ylab("Adjusted school-level\ninfluenza vaccination coverage") +
  xlab("Percent of students participating in school-located influenza vaccination") +
  theme_bw()
dev.off()