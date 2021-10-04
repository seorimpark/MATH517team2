library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(UsingR)

df0 <- read_csv("Provisional_COVID-19_Deaths_by_Place_of_Death_and_Age.csv")

df0 %>% head()

# taking away the month and year since no info
df1= df0[,-c(5,6)]
df1 %>% head()


unique(df1[,1]) # no need for this one since all have been lastlyupdated at once
df2= df1[,-c(1)]
df2 %>% head() 


colnames(df2)
dim(df2)



help("as.Date.character")
df2[1,1]
dates<-df2[,1] 


# view column data class: simple trial
class(df2$`Start Date`)
dateOnly <- as.Date(df2$`Start Date`, format="%d/%m/%Y")
dateOnly 
class(dateOnly)

# transforming the columns start date and end date to date format:
df3<-df2
df3$`Start Date`<- as.Date(df2$`Start Date`, format="%m/%d/%Y")
df3
df3$`End Date`<- as.Date(df2$`End Date`, format="%m/%d/%Y")
df3

#transform the other columns ( "Group","HHS Region","State","Place of Death" "Age group") into factors:
df4<-df3
col_names <- colnames(df4[,3:7])
df4[col_names] <- lapply(df4[col_names] , factor)
df4

#final dataframe working with:
df<-df4
df

#df_general<-df %>% filter(`Age group` == "All Ages" & State == "United States" & Group == "By Total" ) 



# plot the data using ggplot
#require(gridExtra)

ggplot(data = df, aes(x = `Start Date` , y =  `Total Deaths`)) +
  geom_point(aes(colour = factor(`Group`)))+
  labs(x = "Start Date",
       y = "Total deaths")

ggplot(data = df, aes(x = `Start Date` , y =  `Total Deaths`)) +
  geom_point(aes(colour = factor(`Place of Death`)))+
  labs(x = "Start Date",
       y = "Total deaths")

ggplot(data = df, aes(x = `Start Date` , y =  `Total Deaths`)) +
  geom_point(aes(colour = factor(`HHS Region`)))+
  labs(x = "Start Date",
       y = "Total deaths")

ggplot(data = df, aes(x = `Start Date` , y =  `Total Deaths`)) +
  geom_point(aes(colour = factor(`State`)))+
  labs(x = "Start Date",
       y = "Total deaths")

ggplot(data = df, aes(x = `Start Date` , y =  `Total Deaths`)) +
  geom_point(aes(colour = factor(`Age group`)))+
  labs(x = "Start Date",
       y = "Total deaths")


# p <- ggplot(data = df, aes(x = `Start Date` , y =  `Total Deaths`, shape = factor(`Age group`)))
# p +
#   geom_point(aes(colour = factor(`Age group`)), size = 4) +
#   geom_point(colour = "grey90", size = 1.5)

# d <- ggplot(data = df, aes(x = `Start Date` , y =  `Total Deaths`,shape = factor(`Age group`)))
# d + geom_point(alpha = 1/10)
# 
# 
# ggplot(data = df, aes(x = `Start Date` , y =  `Total Deaths`)) +
#   geom_point(aes(colour = factor(`Age group`)), alpha = 1/10)+
#   labs(x = "Date",
#        y = "Total deaths")
