library(dplyr)
library(readr)
library(reshape2)

setwd("~/Population data")

##before begining to read, check if any previous population figures have been revised!!


raw_data <- read.csv("RAW_data.csv", header = TRUE)
View(raw_data)


raw_data <- raw_data[!grepl("Population estimates - local authority based by single year of age", raw_data$X),]
raw_data <- raw_data[!grepl("ONS Crown Copyright Reserved [from Nomis on 14 June 2018]", raw_data$X),]

population_male_2002 <- raw_data[c(7:397), -1]
population_female_2002 <- raw_data[c(413:803), -1]
population_male_2003 <- raw_data[c(819:1209), -1]
population_female_2003 <- raw_data[c(1225:1615), -1]
population_male_2004 <- raw_data[c(1631:2021), -1]
population_female_2004 <- raw_data[c(2037:2427), -1]
population_male_2005 <- raw_data[c(2443:2833), -1]
population_female_2005 <- raw_data[c(2849:3239), -1]
population_male_2006 <- raw_data[c(3255:3645), -1]
population_female_2006 <- raw_data[c(3661:4051), -1]
population_male_2007 <- raw_data[c(4067:4457), -1]
population_female_2007 <- raw_data[c(4473:4863), -1]
population_male_2008 <- raw_data[c(4879:5269), -1]
population_female_2008 <- raw_data[c(5285:5675), -1]
population_male_2009 <- raw_data[c(5691:6081), -1]
population_female_2009 <- raw_data[c(6097:6487), -1]
population_male_2010 <- raw_data[c(6503:6893), -1]
population_female_2010 <- raw_data[c(6909:7299), -1]
population_male_2011 <- raw_data[c(7315:7705), -1]
population_female_2011 <- raw_data[c(7721:8111), -1]

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


###COPY ABOVE CODE TO MELT THE YEAR YOU HAVE UPDATED


##binds the male and female dataset together
##IF UPDATING ADD THE MALE AND FEMALE MELTED DATA FOR THE MOST RECENT DATA
data <- rbind(Male_melt_2002, Male_melt_2003, Male_melt_2004, Male_melt_2005, Male_melt_2006, Male_melt_2007, Male_melt_2008, Male_melt_2009, Male_melt_2010, Male_melt_2011, Female_melt_2002, Female_melt_2003, Female_melt_2004, Female_melt_2005, Female_melt_2006, Female_melt_2007, Female_melt_2008, Female_melt_2009, Female_melt_2010, Female_melt_2011)

final_data <- data

final_data$value <- as.numeric(as.character(final_data$value))

summary_data <- final_data %>% 
  group_by(Year) %>% 
  summarise(value = sum(value, na.rm = TRUE))


ds <- data %>%
  group_by(Year) %>%
  summarize(sum_Year = sum(value, na.rm = TRUE))

write.csv(final_data, "2002-2011_data.csv")

