require(dplyr)
require(ggplot2)
require(hrbrthemes)
require(viridis)


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

####### Top Ten Academeys

topTenAcademys =byacademy %>%
  group_by(Academy) %>%
  summarise(count = n(),na.rm=TRUE) %>%
  arrange(desc(count)) %>%
  top_n(15) %>%

  #filter(Sex == "Male")


ggplot(topTenAcademys, aes(x=Academy, y=count)) +
  geom_point(alpha=0.7)

###### Bubble Charts #######

sizeOfClub =byacademy %>%
  group_by(Belt, Age.Category, Sex) %>%
  summarise(count = n(),na.rm=TRUE) %>%
  filter(Sex == "Male")


# Most basic bubble plot
sizeOfClub %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size = pop)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Population (M)")

##### Line Graphs #####
lineGraphs =byacademy %>%
  group_by(Belt, Age.Category, Sex) %>%
  summarise(count = n(),na.rm=TRUE) %>%
  filter(Sex == "Male" & Belt == "Blue")


# Plot
  ggplot(lineGraphs, aes(x=Age.Category, y=count)) +
  geom_line()

