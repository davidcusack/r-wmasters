library(readr)
byacademy <- read_csv("byacademy.csv")

library(ggplot2)
library(tidyverse)


#Academy & Belt Colour table

#abc <- table(byacademy$Academy, byacademy$Belt)

# Barplot
#barplot(table(byacademy$Belt))

bbs <- table(byacademy$Belt, byacademy$Sex)

#barplot(table(byacademy$Belt), beside=TRUE, legend=TRUE)
barplot(bbs, 
        col = "#000000",
        main = "Number of Belts for Males and Females",)

p<-ggplot(data=byacademy, aes(x=Belt, y=Sex)) +
  geom_bar(stat="identity")
p

# Horizontal bar plot
#p + coord_flip()
