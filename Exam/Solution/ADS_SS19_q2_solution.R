library(tidyverse)

economics = ggplot2::economics

# a)
economics %>%
  mutate(unemploymentRate = unemploy/pop) -> economics


# b)
economics %>%
  ggplot(aes(x=date, y=unemploy)) + geom_line() + 
  xlab("Year") + ylab("Number of unemployed people in thousands") + 
  theme_minimal() + theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16))


# c)
economics %>%
  gather(indicator, value, -date) %>%
  ggplot(aes(x=date, y=value)) + geom_line() + facet_wrap(.~indicator, scales = "free") +
  xlab("Year") + ylab("Value") + theme_minimal()
