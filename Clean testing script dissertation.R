
# COVID-19 VACCINE HESITANCY DISSERTATION R SCRIPT


#### Loading packages

library(tidyverse)
library(BSDA)


#### Loading data


# loading the survey level dataset
surveydata <- read.csv("C:/Users/motor/Google Drive/Politics and IR/Dissertation/Covid 19 inoculation hesitancy/DATA/DATASET/SURVEY LEVEL DATASET.csv")
names(surveydata)[names(surveydata) == "ï..countryname"] <- "countryname"


# loading the state level dataset
statedata <- read.csv("C:/Users/motor/Google Drive/Politics and IR/Dissertation/Covid 19 inoculation hesitancy/DATA/DATASET/STATE LEVEL DATASET.csv")
names(statedata)[names(statedata) == "ï..countryname"] <- "countryname"

# creating PC and non-PC subsets
nonpcstates <- filter(statedata, postcom == 0)
pcstates <- filter(statedata, postcom == 1)




#########################################################################################################################################################

#### DESCRIPTIVE STATISTICS #############################################################################################################################

summary(statedata$govtrustoecd17, na.rm = TRUE)

summary(statedata$provax, na.rm = TRUE)



#########################################################################################################################################################

#### STATISTICAL TESTS ##################################################################################################################################

### Test #1 - difference in mean vaccine confidence between PC and non-PC states

sd(nonpcstates$provax, na.rm = TRUE) # 11.02445
sd(pcstates$provax, na.rm = TRUE) # 7.816194

z.test(na.omit(nonpcstates$provax), na.omit(pcstates$provax), alternative = "greater", mu = 0, sigma.x = 11.02445,
       sigma.y = 7.816194, conf.level = 0.95)




#### difference in trust in authorities ####

### Test #2 - difference in mean gov. trust between PC and non-PC states

sd(nonpcstates$govtrustoecd17, na.rm = TRUE) # 0.7648959
sd(pcstates$govtrustoecd17, na.rm = TRUE) # 0.4357991

z.test(na.omit(nonpcstates$govtrustoecd17), na.omit(pcstates$govtrustoecd17), alternative = "greater", mu = 0, sigma.x = 0.7648959,
       sigma.y = 0.4357991, conf.level = 0.95)




### Test #3 - difference in mean sci. trust between PC and non-PC states

sd(nonpcstates$scitr2018, na.rm = TRUE) # 0.1460233
sd(pcstates$scitr2018, na.rm = TRUE) # 0.1953271

z.test(na.omit(nonpcstates$scitr2018), na.omit(pcstates$scitr2018), alternative = "greater", mu = 0, sigma.x = 0.1460233,
       sigma.y = 0.1953271, conf.level = 0.95)



### Test #4 - difference in mean hosp. and health clin. trust between PC and non-PC states

sd(nonpcstates$hostr2018, na.rm = TRUE) # 7.741958
sd(pcstates$hostr2018, na.rm = TRUE) # 9.207244

z.test(na.omit(nonpcstates$hostr2018), na.omit(pcstates$hostr2018), alternative = "greater", mu = 0, sigma.x = 7.741958,
       sigma.y = 9.207244, conf.level = 0.95)





#### Testing the association between trust in authorities and COVID 19 vaccine Hesitancy ####


cor.test(statedata$govtrustoecd17, statedata$provax, alternative = "greater")

cor.test(statedata$scitr2018, statedata$provax, alternative = "greater")

cor.test(statedata$hostr2018, statedata$provax, alternative = "greater")



#### Linear models used in the "discussion" section. These do not meet diagnostic assumptions,
#     they only carry illustrative value.

model1 <- lm(statedata$provax ~ as.factor(statedata$postcom))
summary(model1)


model2 <- lm(statedata$provax ~ as.factor(statedata$postcom) + statedata$hdi2019)
summary(model2)




#########################################################################################################################################################

#### PLOTS (made using ggplot2)


#vaccine hesitancy by political history

ggplot(data = subset(statedata,!is.na(provax)), aes(reorder(countryname, +provax), y = provax)) +
  geom_bar(stat = "identity", mapping = aes(fill = as.factor(postcom)))+
  scale_fill_manual(values=c("blue4", "red3"), labels = c("Non-post-communist", "Post-communist"))+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "Average COVID-19 vaccine confidence by country in Europe", x = "Country name", 
       y = "Proportion of citizens willing to take a Covid 19 vaccine", fill ="Political history")

### trust in authorities

# trust in gov.  by country

ggplot(data = subset(statedata,!is.na(govtrustoecd17)), aes(reorder(countryname, +govtrustoecd17), y = govtrustoecd17)) +
  geom_bar(stat = "identity", mapping = aes(fill = as.factor(postcom)))+
  scale_fill_manual(values=c("blue4", "red3"), labels = c("Non-post-communist", "Post-communist"))+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "Average trust in government (OECD 2017)", x = "Country name", 
       y = "Proportion of citizens who trust their government", fill ="Political history")

# trust in sci.  by country

ggplot(data = subset(statedata,!is.na(scitr2018)), aes(reorder(countryname, +scitr2018), y = scitr2018)) +
  geom_bar(stat = "identity", mapping = aes(fill = as.factor(postcom)))+
  scale_fill_manual(values=c("blue4", "red3"), labels = c("Non-post-communist", "Post-communist"))+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "Average Wellcome Global Monitor Trust in Scientists Index 2018", x = "Country name", 
       y = "Trust in Scientists Index 2018", fill ="Political history")+
  coord_cartesian(ylim = c(2,3.5)) 


# trust in hea.  by country

ggplot(data = subset(statedata,!is.na(hostr2018)), aes(reorder(countryname, +hostr2018), y = hostr2018)) +
  geom_bar(stat = "identity", mapping = aes(fill = as.factor(postcom)))+
  scale_fill_manual(values=c("blue4", "red3"), labels = c("Non-post-communist", "Post-communist"))+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "Average confidence in hospitals and health clinics by country (2018)", x = "Country name", 
       y = "Average trust hospitals and health clinics", fill ="Political history")+
  coord_cartesian(ylim = c(40,95))

# COVID-19 vaccine hesitancy by HDI

statedata %>%
ggplot(aes(x = hdi2019, y = provax)) +
  geom_point(mapping = aes(color = as.factor(postcom)), size = 2)+
  scale_color_manual(values=c("blue4", "red3"), labels = c("Non-post-communist", "Post-communist"))+
  labs(title = "Vaccine Confidence by Human Development Index (2019)", x = "2019 National HDI", 
       y = "Proportion of citizens willing to take a Covid 19 vaccine", color ="Political History")+
  theme(plot.title = element_text(hjust = 0.5))




# COVID-19 vaccine hesitancy by trust in authorities

ggplot(data = subset(statedata,!is.na(hostr2018)), aes(reorder(countryname, +hostr2018), y = hostr2018)) +
  geom_bar(stat = "identity", mapping = aes(fill = as.factor(postcom)))+
  scale_fill_manual(values=c("blue4", "red3"), labels = c("Non-post-communist", "Post-communist"))+
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "Average confidence in hospitals and health clinics by country (2018)", x = "Country name", 
       y = "Average trust hospitals and health clinics", fill ="Political history")+
  coord_cartesian(ylim = c(40,95))





