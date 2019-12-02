require(dplyr)
require(ggplot2)

require(hrbrthemes)
require(viridis)
library(magrittr)
install.packages('farver')

byacademy<-read.csv('byacademy.csv', header = TRUE, sep =',', stringsAsFactors = FALSE)
winners<-read.csv('wm-winners.csv', header = TRUE, sep =',', stringsAsFactors = FALSE)

#Breaks down all the athelets by belt and age group 
beltAgeSummary =byacademy %>%
  group_by(Belt, Age.Category) %>%
  summarise(count = n(),na.rm=TRUE)

#Reorders the belt levels but colour hieracrchy 
beltAgeSummary$Belt=factor(beltAgeSummary$Belt,levels=c("BLACK","BROWN","PURPLE","BLUE"))  

#Plots the data
ggplot(beltAgeSummary, aes(x=Age.Category, y=count, fill=Belt)) +
  ggtitle("Total Number of Athlelets by Age Group & Belt") +
  xlab("Age Categories by Belt Rank") + ylab("Number of Atheletes") +
  geom_bar(stat='identity', width = 0.9, position = position_dodge(preserve = 'single'))+
  scale_fill_manual(values = c("#181818", "#643E2C","#7A2871","#064DAB"))+
  geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-1, size=3)


############# Female Competitors #################


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
  scale_fill_manual(values = c("#181818", "#643E2C","#7A2871","#064DAB")) +
  geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-1, size=3)


ggplot(beltAgeSexSummaryFemale, aes(x=Belt, y=count)) +
  geom_line()
###############

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
  scale_fill_manual(values = c("#181818", "#643E2C","#7A2871","#064DAB"))+
  geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-1, size=3)

####### Academies ##############
topTenAcademys = byacademy %>%
  group_by(Academy) %>%
  summarise(count = n(),na.rm=TRUE) %>%
  arrange(desc(count)) %>%
  top_n(10, count)
  
ggplot(topTenAcademys, aes(x= reorder(Academy, -count), y=count)) +
  ggtitle ("Top Ten Academys by Number of Students") +
  xlab("Academys") + ylab("Number of Atheletes") +
  geom_bar(stat='identity', width = 0.9, position = position_dodge(preserve = 'single'), fill = "#FF6666") +
  geom_text(aes(label=count), position=position_dodge(width=0.9), hjust=-0.5) +
  coord_flip()

###### Scatter plot #####

#Breaks down all the athelets by belt and age group 
beltsByDivision =byacademy %>%
  group_by(Belt, Age.Category, Weight.Category) %>%
  summarise(count = n(),na.rm=TRUE) %>%
  arrange(desc(count))


beltsByDivision$Full.Category.Name = paste(beltsByDivision$Age.Category,beltsByDivision$Belt,beltsByDivision$Weight.Category)
beltsByDivision$newColumn <- NULL 
beltsByDivision
 

##### Academies under 10 Atheletes #####

under10Atheletes = byacademy %>%
  group_by(Academy) %>%
  summarise(count = n(),na.rm=TRUE) %>%
  arrange(desc(count)) %>%
  filter(count <= 10)

under25Atheletes = byacademy %>%
  group_by(Academy) %>%
  summarise(count = n(),na.rm=TRUE) %>%
  arrange(desc(count)) %>%
  filter(count >= 11 & count <= 25)

under75Atheletes = byacademy %>%
  group_by(Academy) %>%
  summarise(count = n(),na.rm=TRUE) %>%
  arrange(desc(count)) %>%
  filter(count >= 26 & count <= 75)

over75Atheletes = byacademy %>%
  group_by(Academy) %>%
  summarise(count = n(),na.rm=TRUE) %>%
  arrange(desc(count)) %>%
  filter(count >= 76)

academyMedalCount = winners %>%
  group_by(Academy) %>%
  summarise(count = n(),na.rm=TRUE) %>%
  arrange(desc(count))

medalsUnder10 <- inner_join(under10Atheletes, academyMedalCount, by = "Academy")
medalsUnder25 <- inner_join(under25Atheletes, academyMedalCount, by = "Academy")
medalsUnder75 <- inner_join(under75Atheletes, academyMedalCount, by = "Academy")
medalsOver75 <- inner_join(over75Atheletes, academyMedalCount, by = "Academy")


ggplot(medalsOver75, aes(x= Academy, y=count.y)) +
  ggtitle ("Academies with Over 75 Atheletes & Their Medal Account") +
  xlab("Academys") + ylab("Number of Medals") +
  geom_bar(stat='identity', width = 0.9, position = position_dodge(preserve = 'single'), fill = "#FF6666") +
  geom_text(aes(label=count.y), position=position_dodge(width=0.9), hjust=-0.5) +
  coord_flip()


###### Common Name #############
maleFirstName = byacademy %>%
  group_by(First.Name, Sex) %>%
  axis.text.x.top = 10 %>%
  filter(Sex == "Male") %>%
  summarise(count = n(), na.rm=TRUE) %>%
  arrange(desc(count)) %>%
  
commonMaleName <- maleFirstName[1,3]
print(commonMaleName)

totalMales = byacademy %>%
  group_by(Sex) %>%
  filter(Sex == "Male") %>%
  summarise(count = n(), na.rm=TRUE)

print((totalMales$count/10)/commonMaleName)
print(totalMales/commonMaleName)

maleNamePrevelance <- (commonMaleName + totalMales$count)
print(maleNamePrevelance)

