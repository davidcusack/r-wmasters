require(dplyr)
require(ggplot2)


byacademy<-read.csv('byacademy.csv', header = TRUE, sep =',', stringsAsFactors = FALSE)

#Breaks down all the athelets by belt and age group 
beltAgeSummary =byacademy %>%
  group_by(Belt, Age.Category) %>%
  summarise(count = n(),na.rm=TRUE)

#Reorders the belt levels but colour hieracrchy 
beltAgeSummary$Belt=factor(beltAgeSummary$Belt,levels=c("BLACK","BROWN","PURPLE","BLUE"))  

#Plots the data
ggplot(beltAgeSummary, aes(x=Age.Category, y=count, fill=Belt)) +
  geom_bar(stat='identity', width = 0.9, position = position_dodge(preserve = 'single'))+
  scale_fill_manual(values = c("#181818", "#643E2C","#7A2871","#064DAB"))



beltAgeSexSummaryFemale =byacademy %>%
  group_by(Belt, Age.Category, Sex) %>%
  summarise(count = n(),na.rm=TRUE) %>%
  filter(Sex == "Female")
  
#Reorders the belt levels but colour hieracrchy 
beltAgeSexSummaryFemale$Belt=factor(beltAgeSexSummaryFemale$Belt,levels=c("BLACK","BROWN","PURPLE","BLUE"))  

#Plots the data
ggplot(beltAgeSexSummaryFemale, aes(x=Age.Category, y=count, fill=Belt)) +
  ggtitle("Number of Female Athlelets by Age Group & Belt") +
  xlab("Age Categories by Belt Rank") + ylab("Number of Atheletes") +
  geom_bar(stat='identity', width = 0.9, position = position_dodge(preserve = 'single'))+
  scale_fill_manual(values = c("#181818", "#643E2C","#7A2871","#064DAB"))

#Male Athelets

beltAgeSexSummaryMale =byacademy %>%
  group_by(Belt, Age.Category, Sex) %>%
  summarise(count = n(),na.rm=TRUE) %>%
  filter(Sex == "Male")

#Reorders the belt levels but colour hieracrchy 
beltAgeSexSummaryMale$Belt=factor(beltAgeSexSummaryMale$Belt,levels=c("BLACK","BROWN","PURPLE","BLUE"))  

#Plots the data
ggplot(beltAgeSexSummaryMale, aes(x=Age.Category, y=count, fill=Belt)) +
  ggtitle("Number of Male Athlelets by Age Group & Belt") +
  xlab("Age Categories by Belt Rank") + ylab("Number of Atheletes") +
  geom_bar(stat='identity', width = 0.9, position = position_dodge(preserve = 'single'))+
  scale_fill_manual(values = c("#181818", "#643E2C","#7A2871","#064DAB"))


topTenAcademys =byacademy %>%
  group_by(Academy) %>%
  summarise(count = n(),na.rm=TRUE) %>%
  arrange(desc(count)) %>%
  top_n(15) %>%

  #filter(Sex == "Male")

ggplot(topTenAcademys, aes(x=Academy, y=count)) +
  geom_point(alpha=0.7)




