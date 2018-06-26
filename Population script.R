##Population Estimates##

###Load required Libraries##

library(dplyr)
library(readr)
library(reshape2)

##set working directory

setwd("~/Population data")

##Read in new datasets downloaded from Nomie. 


data_2002_to_2011 <- read.csv("Nomis 2002-2011 data.csv", header = TRUE)
data_2012_to_2016 <- read.csv("Nomis 2012-2016 data.csv", header = TRUE)

View(data_2002_to_2011)

##update the below commands with the text in the rows you want to remove

data_2002_to_2011 <- data_2002_to_2011[!grepl("Population estimates - local authority based by single year of age", data_2002_to_2011$X),]
data_2002_to_2011 <- data_2002_to_2011[!grepl("ONS Crown Copyright Reserved [from Nomis on 14 June 2018]", data_2002_to_2011$X),]

data_2012_to_2016 <- data_2012_to_2016[!grepl("Population estimates - local authority based by single year of age", data_2012_to_2016$X),]
data_2012_to_2016 <- data_2012_to_2016[!grepl("ONS Crown Copyright Reserved [from Nomis on 25 June 2018]", data_2012_to_2016$X),]

population_male_2002 <- data_2002_to_2011[c(7:397), -1]
population_female_2002 <- data_2002_to_2011[c(413:803), -1]
population_male_2003 <- data_2002_to_2011[c(819:1209), -1]
population_female_2003 <- data_2002_to_2011[c(1225:1615), -1]
population_male_2004 <- data_2002_to_2011[c(1631:2021), -1]
population_female_2004 <- data_2002_to_2011[c(2037:2427), -1]
population_male_2005 <- data_2002_to_2011[c(2443:2833), -1]
population_female_2005 <- data_2002_to_2011[c(2849:3239), -1]
population_male_2006 <- data_2002_to_2011[c(3255:3645), -1]
population_female_2006 <- data_2002_to_2011[c(3661:4051), -1]
population_male_2007 <- data_2002_to_2011[c(4067:4457), -1]
population_female_2007 <- data_2002_to_2011[c(4473:4863), -1]
population_male_2008 <- data_2002_to_2011[c(4879:5269), -1]
population_female_2008 <- data_2002_to_2011[c(5285:5675), -1]
population_male_2009 <- data_2002_to_2011[c(5691:6081), -1]
population_female_2009 <- data_2002_to_2011[c(6097:6487), -1]
population_male_2010 <- data_2002_to_2011[c(6503:6893), -1]
population_female_2010 <- data_2002_to_2011[c(6909:7299), -1]
population_male_2011 <- data_2002_to_2011[c(7315:7705), -1]
population_female_2011 <- data_2002_to_2011[c(7721:8111), -1]
population_male_2012 <- data_2012_to_2016[c(7:397), -1]
population_female_2012 <- data_2012_to_2016[c(413:803), -1]
population_male_2013 <- data_2012_to_2016[c(819:1209), -1]
population_female_2013 <- data_2012_to_2016[c(1225:1615), -1]
population_male_2014 <- data_2012_to_2016[c(1631:2021), -1]
population_female_2014 <- data_2012_to_2016[c(2037:2427), -1]
population_male_2015 <- data_2012_to_2016[c(2443:2833), -1]
population_female_2015 <- data_2012_to_2016[c(2849:3239), -1]
population_male_2016 <- data_2012_to_2016[c(3255:3645), -1]
population_female_2016 <- data_2012_to_2016[c(3661:4051), -1]


colnames(population_male_2002) <- c("Code", 0:90)
colnames(population_male_2003) <- c("Code", 0:90)
colnames(population_male_2004) <- c("Code", 0:90)
colnames(population_male_2005) <- c("Code", 0:90)
colnames(population_male_2006) <- c("Code", 0:90)
colnames(population_male_2007) <- c("Code", 0:90)
colnames(population_male_2008) <- c("Code", 0:90)
colnames(population_male_2009) <- c("Code", 0:90)
colnames(population_male_2010) <- c("Code", 0:90)
colnames(population_male_2011) <- c("Code", 0:90)
colnames(population_female_2002) <- c("Code", 0:90)
colnames(population_female_2003) <- c("Code", 0:90)
colnames(population_female_2004) <- c("Code", 0:90)
colnames(population_female_2005) <- c("Code", 0:90)
colnames(population_female_2006) <- c("Code", 0:90)
colnames(population_female_2007) <- c("Code", 0:90)
colnames(population_female_2008) <- c("Code", 0:90)
colnames(population_female_2009) <- c("Code", 0:90)
colnames(population_female_2010) <- c("Code", 0:90)
colnames(population_female_2011) <- c("Code", 0:90)
colnames(population_male_2012) <- c("Code", 0:90)
colnames(population_male_2013) <- c("Code", 0:90)
colnames(population_male_2014) <- c("Code", 0:90)
colnames(population_male_2015) <- c("Code", 0:90)
colnames(population_male_2016) <- c("Code", 0:90)
colnames(population_female_2012) <- c("Code", 0:90)
colnames(population_female_2013) <- c("Code", 0:90)
colnames(population_female_2014) <- c("Code", 0:90)
colnames(population_female_2015) <- c("Code", 0:90)
colnames(population_female_2016) <- c("Code", 0:90)


##Melts the dataset down (aka it "un-pivots the data). Before, the ages were columns, now their made in to rows with
##corresponding regional values. Columns are also added to represent gender and year.

Male_melt_2002 <- population_male_2002 %>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "M")%>%
  mutate(Year = "2002")

Male_melt_2003 <- population_male_2003 %>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "M")%>%
  mutate(Year = "2003")

Male_melt_2004 <- population_male_2004 %>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "M")%>%
  mutate(Year = "2004")   

Male_melt_2005 <- population_male_2005 %>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "M")%>%
  mutate(Year = "2005")

Male_melt_2006 <- population_male_2006 %>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "M")%>%
  mutate(Year = "2006")

Male_melt_2007 <- population_male_2007 %>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "M")%>%
  mutate(Year = "2007")

Male_melt_2008 <- population_male_2008 %>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "M")%>%
  mutate(Year = "2008")

Male_melt_2009 <- population_male_2009 %>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "M")%>%
  mutate(Year = "2009")

Male_melt_2010 <- population_male_2010 %>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "M")%>%
  mutate(Year = "2010")

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

Female_melt_2002 <- population_female_2002%>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "F")%>%
  mutate(Year = "2002")

Female_melt_2003 <- population_female_2003%>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "F")%>%
  mutate(Year = "2003")

Female_melt_2004 <- population_female_2004%>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "F")%>%
  mutate(Year = "2004")

Female_melt_2005 <- population_female_2005%>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "F")%>%
  mutate(Year = "2005")

Female_melt_2006 <- population_female_2006%>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "F")%>%
  mutate(Year = "2006")

Female_melt_2007 <- population_female_2007%>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "F")%>%
  mutate(Year = "2007")

Female_melt_2008 <- population_female_2008%>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "F")%>%
  mutate(Year = "2008")

Female_melt_2009 <- population_female_2009%>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "F")%>%
  mutate(Year = "2009")

Female_melt_2010 <- population_female_2010%>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "F")%>%
  mutate(Year = "2010")

Female_melt_2011 <- population_female_2011%>%
  melt(measure.vars = c(2:92))%>% 
  mutate(Gender= "F")%>%
  mutate(Year = "2011")

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
final_data_2002_to_2011 <- rbind(Male_melt_2002, Male_melt_2003, Male_melt_2004, Male_melt_2005, Male_melt_2006, Male_melt_2007, Male_melt_2008, Male_melt_2009, Male_melt_2010, Male_melt_2011, Female_melt_2002, Female_melt_2003, Female_melt_2004, Female_melt_2005, Female_melt_2006, Female_melt_2007, Female_melt_2008, Female_melt_2009, Female_melt_2010, Female_melt_2011)
final_data_2012_to_2016 <- rbind(Male_melt_2012, Male_melt_2013, Male_melt_2014, Male_melt_2015, Male_melt_2016, Female_melt_2012, Female_melt_2013, Female_melt_2014, Female_melt_2015, Female_melt_2016)


##this changes the count value to a number, as when its first created R classifies it as a character
final_data_2002_to_2011$value <- as.numeric(as.character(final_data_2002_to_2011$value))
final_data_2012_to_2016$value <- as.numeric(as.character(final_data_2012_to_2016$value))


##Creates a table showing the population for each year. I use this code to make sure that the datasets are correct, by quickly comparing my yearly totals to ONS's
summary_data <- final_data_2002_to_2011 %>% 
  group_by(Year) %>% 
  summarise(value = sum(value, na.rm = TRUE))


ds <- data %>%
  group_by(Year) %>%
  summarize(sum_Year = sum(value, na.rm = TRUE))

##Create the two final csv files to be put on to SQL server. 

write.csv(final_data_2012_to_2016, "data_2012_to_2016.csv")

write.csv(final_data_2002_to_2011, "data_2002_to_2011.csv")
