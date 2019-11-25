require(dplyr)
require(ggplot2)

byacademy<-read.csv('byacademy.csv', header = TRUE, sep =',')

df <- byacademy

df

#df <- byacademy %>%
#  select(Belt, Age.Category) %>%
#  table() %>%
#  print()

#df2 <- data.frame(byacademy$Belt, byacademy$Age.Category)
#by_age_category <- df2 %>% group_by(byacademy$Belt)

#by_age_category %>% summarise()