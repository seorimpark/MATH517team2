library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(UsingR)
library(ggcorrplot)
library(usmap)


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


##############################################################################################################
# plot the data using ggplot################################################################################

#  function to plot: pick the data set
LevelPlots <- function(dataframe, variable, name_variable, Chosen_Factor) {
  
  ggplot(data = dataframe, aes(x = `Start Date` , y =  variable)) +
    geom_point(aes(colour = factor(Chosen_Factor)))+
    labs(x = "Start Date",
         y = name_variable )
 

}

# 
HistoPlots <- function(dataframe, variable, name_variable) {
  
  ggplot(data=dataframe, aes(variable)) + 
    geom_histogram(breaks=seq(20, 50, by=2), 
                   col="red", 
                   aes(fill=..count..)) +
    labs( title =name_variable )
}

#require(gridExtra)

#######################################################################################################
# Total deaths : 
LevelPlots(df, df$`Total Deaths`,"Total Deaths",df$Group)
LevelPlots(df, df$`Total Deaths`,"Total Deaths",df$`HHS Region`)
LevelPlots(df, df$`Total Deaths`,"Total Deaths",df$State)
LevelPlots(df, df$`Total Deaths`,"Total Deaths",df$`Place of Death`)
LevelPlots(df, df$`Total Deaths`,"Total Deaths",df$`Age group`)

HistoPlots(df, df$`Total Deaths`,"Total Deaths")

# "Pneumonia Deaths"  
LevelPlots(df, df$`Pneumonia Deaths`,"Pneumonia Deaths",df$Group)
LevelPlots(df, df$`Pneumonia Deaths`,"Pneumonia Deaths",df$`HHS Region`)
LevelPlots(df, df$`Pneumonia Deaths`,"Pneumonia Deaths",df$State)
LevelPlots(df, df$`Pneumonia Deaths`,"Pneumonia Deaths",df$`Place of Death`)
LevelPlots(df, df$`Pneumonia Deaths`,"Pneumonia Deaths",df$`Age group`)

HistoPlots(df, df$`Pneumonia Deaths`,"Pneumonia Deaths")
# "Pneumonia and COVID-19 Deaths" 

LevelPlots(df, df$`Pneumonia and COVID-19 Deaths`,"Pneumonia and COVID-19 Deaths",df$Group)
LevelPlots(df, df$`Pneumonia and COVID-19 Deaths`,"Pneumonia and COVID-19 Deaths",df$`HHS Region`)
LevelPlots(df, df$`Pneumonia and COVID-19 Deaths`,"Pneumonia and COVID-19 Deaths",df$State)
LevelPlots(df, df$`Pneumonia and COVID-19 Deaths`,"Pneumonia and COVID-19 Deaths",df$`Place of Death`)
LevelPlots(df, df$`Pneumonia and COVID-19 Deaths`,"Pneumonia and COVID-19 Deaths",df$`Age group`)

HistoPlots(df, df$`Pneumonia and COVID-19 Deaths`,"Pneumonia and COVID-19 Deaths")
# "Influenza Deaths"    
LevelPlots(df, df$`Influenza Deaths`,"Influenza Deaths",df$Group)
LevelPlots(df, df$`Influenza Deaths`,"Influenza Deaths",df$`HHS Region`)
LevelPlots(df, df$`Influenza Deaths`,"Influenza Deaths",df$State)
LevelPlots(df, df$`Influenza Deaths`,"Influenza Deaths",df$`Place of Death`)
LevelPlots(df, df$`Influenza Deaths`,"Influenza Deaths",df$`Age group`)

HistoPlots(df, df$`Influenza Deaths`,"Influenza Deaths")

# "Pneumonia, Influenza, or COVID-19 Deaths" 

LevelPlots(df, df$`Pneumonia, Influenza, or COVID-19 Deaths`,"Pneumonia, Influenza, or COVID-19 Deaths",df$Group)
LevelPlots(df, df$`Pneumonia, Influenza, or COVID-19 Deaths`,"Pneumonia, Influenza, or COVID-19 Deaths",df$`HHS Region`)
LevelPlots(df, df$`Pneumonia, Influenza, or COVID-19 Deaths`,"Pneumonia, Influenza, or COVID-19 Deaths",df$State)
LevelPlots(df, df$`Pneumonia, Influenza, or COVID-19 Deaths`,"Pneumonia, Influenza, or COVID-19 Deaths",df$`Place of Death`)
LevelPlots(df, df$`Pneumonia, Influenza, or COVID-19 Deaths`,"Pneumonia, Influenza, or COVID-19 Deaths",df$`Age group`)

HistoPlots(df, df$`Pneumonia, Influenza, or COVID-19 Deaths`,"Pneumonia, Influenza, or COVID-19 Deaths")

####################################################################################################
# trying to understand the start/end date part:
unique(df$`End Date`)
unique(df$`Start Date`)
unique(df$`Age group`) # maybe delete the firt level
unique(df$Group)
unique(df$`HHS Region`)
unique(df$State) # maybe delete the firt level
unique(df$`Place of Death`)# maybe delete the firt level


df_specific<- df %>% filter(`Age group` != "All Ages" & State != "United States" & `Place of Death` != "Total - All Places of Death")
df_specific


### 
# Total deaths :

# scatter throuh time : 
LevelPlots(df_specific, df_specific$`Total Deaths`,"Total Deaths",df_specific$Group)
LevelPlots(df_specific, df_specific$`Total Deaths`,"Total Deaths",df_specific$`HHS Region`)
LevelPlots(df_specific, df_specific$`Total Deaths`,"Total Deaths",df_specific$State)
LevelPlots(df_specific, df_specific$`Total Deaths`,"Total Deaths",df_specific$`Place of Death`)
LevelPlots(df_specific, df_specific$`Total Deaths`,"Total Deaths",df_specific$`Age group`)



# Remarks:

# "Pneumonia Deaths"  
LevelPlots(df_specific, df_specific$`Pneumonia Deaths`,"Pneumonia Deaths",df_specific$Group)
LevelPlots(df_specific, df_specific$`Pneumonia Deaths`,"Pneumonia Deaths",df_specific$`HHS Region`)
LevelPlots(df_specific, df_specific$`Pneumonia Deaths`,"Pneumonia Deaths",df_specific$State)
LevelPlots(df_specific, df_specific$`Pneumonia Deaths`,"Pneumonia Deaths",df_specific$`Place of Death`)
LevelPlots(df_specific, df_specific$`Pneumonia Deaths`,"Pneumonia Deaths",df_specific$`Age group`)


# "Pneumonia and COVID-19 Deaths" 

LevelPlots(df_specific, df_specific$`Pneumonia and COVID-19 Deaths`,"Pneumonia and COVID-19 Deaths",df_specific$Group)
LevelPlots(df_specific, df_specific$`Pneumonia and COVID-19 Deaths`,"Pneumonia and COVID-19 Deaths",df_specific$`HHS Region`)
LevelPlots(df_specific, df_specific$`Pneumonia and COVID-19 Deaths`,"Pneumonia and COVID-19 Deaths",df_specific$State)
LevelPlots(df_specific, df_specific$`Pneumonia and COVID-19 Deaths`,"Pneumonia and COVID-19 Deaths",df_specific$`Place of Death`)
LevelPlots(df_specific, df_specific$`Pneumonia and COVID-19 Deaths`,"Pneumonia and COVID-19 Deaths",df_specific$`Age group`)

# "Influenza Deaths"    
LevelPlots(df_specific, df_specific$`Influenza Deaths`,"Influenza Deaths",df_specific$Group)
LevelPlots(df_specific, df_specific$`Influenza Deaths`,"Influenza Deaths",df_specific$`HHS Region`)
LevelPlots(df_specific, df_specific$`Influenza Deaths`,"Influenza Deaths",df_specific$State)
LevelPlots(df_specific, df_specific$`Influenza Deaths`,"Influenza Deaths",df_specific$`Place of Death`)
LevelPlots(df_specific, df_specific$`Influenza Deaths`,"Influenza Deaths",df_specific$`Age group`)

# "Pneumonia, Influenza, or COVID-19 Deaths" 

LevelPlots(df_specific, df_specific$`Pneumonia, Influenza, or COVID-19 Deathss`,"Pneumonia, Influenza, or COVID-19 Deathss",df_specific$Group)
LevelPlots(df_specific, df_specific$`Pneumonia, Influenza, or COVID-19 Deathss`,"Pneumonia, Influenza, or COVID-19 Deathss",df_specific$`HHS Region`)
LevelPlots(df_specific, df_specific$`Pneumonia, Influenza, or COVID-19 Deathss`,"Pneumonia, Influenza, or COVID-19 Deathss",df_specific$State)
LevelPlots(df_specific, df_specific$`Pneumonia, Influenza, or COVID-19 Deathss`,"Pneumonia, Influenza, or COVID-19 Deathss",df_specific$`Place of Death`)
LevelPlots(df_specific, df_specific$`Pneumonia, Influenza, or COVID-19 Deathss`,"Pneumonia, Influenza, or COVID-19 Deathss",df_specific$`Age group`)

# Remarks:



####################################################################################################
# Correlation Matrix:

#cor(df_specific$`Pneumonia, Influenza, or COVID-19 Deathss`, method = c("pearson", "kendall", "spearman"))

# model.matrix(~0+., data=df_specific) %>% 
#   cor(use="pairwise.complete.obs") %>% 
#   ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

#heatmap(df_specific)

# DF<-df_specific
# DF[] <- lapply(df_specific,as.integer)
# library(sjPlot)
# sjp.corr(DF)
# sjt.corr(DF)




####################################
# geographic plots 

list_states<-unique(df_specific$State)

TotalDeath_by_state<-df_specific %>% 
  group_by(State) %>% 
  summarise(`Total Deaths`= sum(`Total Deaths`,na.rm=TRUE))

PneumoniaDeath<-df_specific %>% 
  group_by(State) %>% 
  summarise(`Pneumonia Deaths`= sum(`Pneumonia Deaths`,na.rm=TRUE))

PneumoniaCOVIDDeaths<-df_specific %>% 
  group_by(State) %>% 
  summarise(`Pneumonia and COVID-19 Deaths`= sum(`Pneumonia and COVID-19 Deaths`,na.rm=TRUE))

InfluenzaDeaths<-df_specific %>% 
  group_by(State) %>% 
  summarise(`Influenza Deaths`= sum(`Influenza Deaths`,na.rm=TRUE))

PneumoniaInfluenza_or_COVIDDeaths<-df_specific %>% 
  group_by(State) %>% 
  summarise(`Pneumonia, Influenza, or COVID-19 Deaths`= sum(`Pneumonia, Influenza, or COVID-19 Deaths`,na.rm=TRUE))

# omitting for better view Newyork city and puerto rico 
TotalDeath_by_state<-TotalDeath_by_state[-c(34,41),]
PneumoniaDeath_by_state<-PneumoniaDeath[-c(34,41),]
PneumoniaCOVIDDeaths_by_state<-PneumoniaCOVIDDeaths[-c(34,41),]
InfluenzaDeaths_by_state<-InfluenzaDeaths[-c(34,41),]
PneumoniaInfluenza_or_COVIDDeaths_by_state<-PneumoniaInfluenza_or_COVIDDeaths[-c(34,41),]

# all fine now:
us_popul$full==PneumoniaInfluenza_or_COVIDDeaths_by_state$State

# right datasets
us_TotalDeath<-us_popul
us_TotalDeath$pop_2015<-TotalDeath_by_state$`Total Deaths`

us_PneumoniaDeath<-us_popul
us_PneumoniaDeath$pop_2015<-PneumoniaCOVIDDeaths_by_state$`Pneumonia and COVID-19 Deaths`

us_PneumoniaCOVIDDeaths<-us_popul
us_PneumoniaCOVIDDeaths$pop_2015<-PneumoniaCOVIDDeaths_by_state$`Pneumonia and COVID-19 Deaths`

us_InfluenzaDeaths<-us_popul
us_InfluenzaDeaths$pop_2015<-InfluenzaDeaths_by_state$`Influenza Deaths`

us_PneumoniaInfluenza_or_COVIDDeaths<-us_popul
us_PneumoniaInfluenza_or_COVIDDeaths$pop_2015<-PneumoniaInfluenza_or_COVIDDeaths_by_state$`Pneumonia, Influenza, or COVID-19 Deaths`

# plots

plot_usmap(data = us_TotalDeath, values = "pop_2015", color = "red") + 
  scale_fill_continuous(name = "us_TotalDeath", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = us_PneumoniaDeath, values = "pop_2015", color = "red") + 
  scale_fill_continuous(name = "us_PneumoniaDeath", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = us_PneumoniaCOVIDDeaths, values = "pop_2015", color = "red") + 
  scale_fill_continuous(name = "us_PneumoniaCOVIDDeaths)", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = us_InfluenzaDeaths, values = "pop_2015", color = "red") + 
  scale_fill_continuous(name = "us_InfluenzaDeaths", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = us_PneumoniaInfluenza_or_COVIDDeaths, values = "pop_2015", color = "red") + 
  scale_fill_continuous(name = "us_PneumoniaInfluenza_or_COVIDDeaths", label = scales::comma) + 
  theme(legend.position = "right")







