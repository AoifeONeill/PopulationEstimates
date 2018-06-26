library(dplyr)
library(readr)
library(reshape2)


##set the working area, otherwise you might overwrite ONS's LA-regional national data!!!

setwd("~/Population data/Scottish data zones")


##before begining to read, check if any previous population figures have been revised!!


#### SCOTLAND #####
##################

###before reading in, remove the commas in the numbers within excel! Or find the r code to do this
#year 2011
population_male_2011 <- read.csv("Scotland data zones male by age 2011.csv", skip = 5, header = TRUE)
population_female_2011 <- read.csv("Scotland data zones female by age 2011.csv", skip = 5,  header = TRUE)
##year 2012
population_male_2012 <- read.csv("Scotland data zones male by age 2012.csv", skip = 5,  header = TRUE)
population_female_2012 <- read.csv("Scotland data zones female by age 2012.csv", skip = 5,  header = TRUE)
#year 2013
population_male_2013 <- read.csv("Scotland data zones male by age 2013.csv", skip = 5, header = TRUE)
population_female_2013 <- read.csv("Scotland data zones female by age 2013.csv", header = TRUE)
#year 2014
population_male_2014 <- read.csv("Scotland data zones male by age 2014.csv", skip = 5,  header = TRUE)
population_female_2014 <- read.csv("Scotland data zones female by age 2014.csv", header = TRUE)
#year 2015
population_male_2015 <- read.csv("Scotland data zones male by age 2015.csv", skip = 5, header = TRUE)
population_female_2015 <- read.csv("Scotland data zones female by age 2015.csv", skip = 5,  header = TRUE)
#year 2016
population_male_2016 <- read.csv("Scotland data zones male by age 2016.csv", skip = 5, header = TRUE)
population_female_2016 <- read.csv("Scotland data zones female by age 2016.csv", skip = 5, header = TRUE)


##Update with new data
##population_male_#### <- read.csv("population_####_m.csv", header = TRUE)
##population_female_### <- read.csv("population_####_f.csv", header = TRUE)

?read.csv
##This is to remove the "All ages" column as we don't want it.
population_male_2011 <- population_male_2011[,-c(2:4)]
population_female_2011 <- population_female_2011[,-c(2:4)]
population_male_2012 <- population_male_2012[,-c(2:4)]
population_female_2012 <- population_female_2012[,-c(2:4)]
population_male_2013 <- population_male_2013[,-c(2:4)]
population_female_2013 <- population_female_2013[,-c(2:4)]
population_male_2014 <- population_male_2014[,-c(2:4)]
population_female_2014 <- population_female_2014[,-c(2:4)]
population_male_2015 <- population_male_2015[,-c(2:4)]
population_female_2015 <- population_female_2015[,-c(2:4)]
population_male_2016 <- population_male_2016[,-c(2:4)]
population_female_2016 <- population_female_2016[,-c(2:4)]

##Update with new data
##population_male_#### <- population_male_###[,-c(2:4)]
##population_female_#### <- population_female_####[,-c(2:4)]

##re-naming columns
colnames(population_female_2011) <- c("Data zone", 0:90)
colnames(population_female_2012) <- c("Data zone", 0:90)
colnames(population_female_2013) <- c("Data zone", 0:90)
colnames(population_female_2014) <- c("Data zone", 0:90)
colnames(population_female_2015) <- c("Data zone", 0:90)
colnames(population_female_2016) <- c("Data zone", 0:90)
colnames(population_male_2011) <- c("Data zone", 0:90)
colnames(population_male_2012) <- c("Data zone", 0:90)
colnames(population_male_2013) <- c("Data zone", 0:90)
colnames(population_male_2014) <- c("Data zone", 0:90)
colnames(population_male_2015) <- c("Data zone", 0:90)
colnames(population_male_2016) <- c("Data zone", 0:90)

##Melts the dataset down (aka it "un-pivots the data). Before, the ages were columns, now their made in to rows with
##corresponding regional values. Columns are also added to represent gender and year.

Male_melt_2011 <- population_male_2011 %>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "M")%>%
  mutate(Year = "2011")

Male_melt_2012 <- population_male_2012 %>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "M")%>%
  mutate(Year = "2012")

Male_melt_2013 <- population_male_2013 %>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "M")%>%
  mutate(Year = "2013")

Male_melt_2014 <- population_male_2014 %>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "M")%>%
  mutate(Year = "2014")   

Male_melt_2015 <- population_male_2015 %>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "M")%>%
  mutate(Year = "2015")

Male_melt_2016 <- population_male_2016 %>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "M")%>%
  mutate(Year = "2016")

Female_melt_2012 <- population_female_2012%>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "F")%>%
  mutate(Year = "2012")

Female_melt_2013 <- population_female_2013%>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "F")%>%
  mutate(Year = "2013")

Female_melt_2014 <- population_female_2014%>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "F")%>%
  mutate(Year = "2014")

Female_melt_2015 <- population_female_2015%>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "F")%>%
  mutate(Year = "2015")

Female_melt_2016 <- population_female_2016%>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "F")%>%
  mutate(Year = "2016")

###COPY ABOVE CODE TO MELT THE YEAR YOU HAVE UPDATED


##binds the male and female dataset together
##IF UPDATING ADD THE MALE AND FEMALE MELTED DATA FOR THE MOST RECENT DATA
data <- rbind(Male_melt_2016, Male_melt_2015, Male_melt_2014, Male_melt_2013, Male_melt_2012,Female_melt_2016, Female_melt_2015, Female_melt_2014, Female_melt_2013, Female_melt_2012)


##removes the X in front of the age variable##
data$variable <- sub("X", "", data$variable, fixed=TRUE)


ds <- data %>%
  group_by(Year) %>%
  summarize(sum_name = sum(value, na.rm = TRUE))

sum(ds$sum_name)
quick_check <- data %>%
  group_by(Name)%>%
  dplyr::count(value)

checkz <- summarise(quick_check)

sum(data$value)
countss <- group_by(data)

final_data <- data[,-2]

write.csv(final_data, "data.csv")
