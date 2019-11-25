#Belt Colours
beltColours = c("Blue", "Purple", "Brown", "Black")

#beltSummary <- summarise(byacademy,
#                         count = n()
#                         )
#groupBeltCategory <- group_by(byacademy, Belt)
#groupBeltAgeCategory <- group_by(byacademy, Belt, Age.Category)


#Gives you all of the counts by Belt 
#beltAgeSummary <- summarise(groupBeltCategory, count = n())
beltAgeSummary2 <- summarise(groupBeltAgeCategory, count = n())
beltAgeSummary2=ungroup(beltAgeSummary2)

#ggplot(beltAgeSummary2) +
#  geom_bar(aes(x = Age.Category, fill = factor(Belt)), position = position_dodge(preserve = 'single'))
# d$Team2 <- factor(d$Team1, c("Cowboys", "Giants", "Eagles", "Redskins"))
#mutate(name = factor(name, levels=c("north", "north-east", "east", "south-east", "south", "south-west", "west", "north-west"))) %>%
#dat$V1 <- factor(dat$V1, levels = dat$V1)
beltAgeSummary2$Belt=factor(beltAgeSummary2$Belt,levels=c("BLACK","BROWN","PURPLE","BLUE"))  

#beltAgeSummary2=factor(beltAgeSummary2$Belt,c('BLACK','BROWN','PURPLE','BLUE'))

ggplot(beltAgeSummary2, aes(x=Age.Category, y=count, fill=Belt)) +
  geom_bar(stat='identity', position = position_dodge(preserve = 'single'))+
  scale_fill_manual(values = c("black", "brown","purple","blue"))


#Notes
#https://www.youtube.com/watch?v=6iE1iQgoi5Q&t=206s
#https://plot.ly/ggplot2/geom_bar/ 

#beltAgeSummary2$Belt %>% fct_relevel("BLACK ","BROWN ","PURPLE ","BLUE ") %>% levels()