# Problem Set 4
# Question 1

# Name: Your Name
# Matrikelnummer: Your matriculation number
library(tidyverse)

basketball = read_csv2("basketball_complete.csv")

#a)
basketball %>%
  ggplot(aes(x=loc_x, y=loc_y)) + 
  stat_summary_hex(aes(z=as.numeric(shot_made_flag))) + theme_bw() + facet_grid()

basketball %>%
  ggplot(aes(x=loc_x, y=loc_y, fill=as.factor(shot_made_flag))) + 
  geom_bin2d() + theme_bw()
