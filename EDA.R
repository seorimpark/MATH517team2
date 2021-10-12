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

us_popul<-statepop
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


####################################################################################################
# sort the date depending on the category:

Select_Age_Group<-function(DataFrame, agegroup)
{ 
  age_groups<-unique(df$`Age group`)
  if(agegroup %in% age_groups)
     {
      df1= DataFrame %>% filter(`Age group` == agegroup )
      return(df1)
     }
  else{
    warning("Age group selected not in the list, the returned dataframe has not been filtered")
    return (DataFrame)
     }
  }
#Test:
#Select_Age_Group(df,"0-17 years")

Select_Group<-function(DataFrame, group)
{ 
  all_groups<-unique(df$`Group`)
  if(group %in% all_groups)
  {
    df1= DataFrame %>% filter(`Group` == group )
    return(df1)
  }
  else{
    warning("Group selected not in the list, the returned dataframe has not been filtered")
    return (DataFrame)
  }
}
#Test:
#Select_Group(df, "By Total")

Select_HHSRegion<-function(DataFrame, region)
{ 
  all_regions<-unique(df$`HHS Region`)
  if(region %in% all_regions)
  {
    df1= DataFrame %>% filter(`HHS Region` == region )
    return(df1)
  }
  else{
    warning("HHS Region selected not in the list, the returned dataframe has not been filtered")
    return (DataFrame)
  }
}

#Test:
# Select_HHSRegion(df,4)
# Select_HHSRegion(df,-1)

Select_State<-function(DataFrame, state)
{ 
  all_states<-unique(df$State)
  if(state %in% all_states)
  {
    df1= DataFrame %>% filter(`State` == state )
    return(df1)
  }
  else{
    warning("State selected not in the list, the returned dataframe has not been filtered")
    return (DataFrame)
  }
}

#Test:
#Select_State(df,"Hawaii")
#Select_State(df,-1)

Select_PlaceDeath<-function(DataFrame, place_d)
{ 
  all_places<-unique(df$`Place of Death`)
  if(place_d %in% all_places)
  {
    df1= DataFrame %>% filter(`Place of Death` == place_d )
    return(df1)
  }
  else{
    warning("Place of death selected not in the list, the returned dataframe has not been filtered")
    return (DataFrame)
  }
}

#Test:
# Select_PlaceDeath(df,"Healthcare setting, inpatient")
# Select_PlaceDeath(df,-1)

Select_all<-function(DataFrame, agegroup,group,region,state,place_d)
{
  # I want to use %>% but not quite confortable, I ll use brute force first:
  df1=Select_Age_Group(DataFrame,agegroup)
  df2= Select_Group(df1,group)
  df3=Select_HHSRegion(df2, region)
  df4=Select_State(df3,state)
  df5=Select_PlaceDeath(df4,place_d)
  return(df5)
}

# eventually gives you 0 or 1 column
Select_all(df,"0-17 years","By Total",-1,"California","Healthcare setting, inpatient")


# Now studying some correlations:
# [1] All Ages          0-17 years        18-29 years       30-39 years      
# [5] 40-49 years       50-64 years       65-74 years       75-84 years      
# [9] 85 years and over

df_corr_agegroups1<-Select_Age_Group(df,"All Ages") [,8:13] 
df_corr_agegroups2<-Select_Age_Group(df,"0-17 years")[,8:13]
df_corr_agegroups3<-Select_Age_Group(df,"18-29 years")[,8:13]
df_corr_agegroups4<-Select_Age_Group(df,"30-39 years")[,8:13]
df_corr_agegroups5<-Select_Age_Group(df,"40-49 years")[,8:13]
df_corr_agegroups6<-Select_Age_Group(df,"50-64 years")[,8:13]
df_corr_agegroups7<-Select_Age_Group(df,"65-74 years")[,8:13]
df_corr_agegroups8<-Select_Age_Group(df,"75-84 years")[,8:13]
df_corr_agegroups9<-Select_Age_Group(df,"85 years and over")[,8:13]


# pairs(df_corr_agegroups2)
# library(corrplot)
# library(RColorBrewer)
# M <-cor(df_corr_agegroups1,method = "pearson",use = "complete.obs")
# corrplot(M, type="upper", order="hclust",
#          col=brewer.pal(n=8, name="RdYlBu"))
# 
# 
# # install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(df_corr_agegroups1, histogram = TRUE, method = "pearson")
chart.Correlation(df_corr_agegroups2, histogram = TRUE, method = "pearson")
chart.Correlation(df_corr_agegroups3, histogram = TRUE, method = "pearson")
chart.Correlation(df_corr_agegroups4, histogram = TRUE, method = "pearson")
chart.Correlation(df_corr_agegroups5, histogram = TRUE, method = "pearson")
chart.Correlation(df_corr_agegroups6, histogram = TRUE, method = "pearson")
chart.Correlation(df_corr_agegroups7, histogram = TRUE, method = "pearson")
chart.Correlation(df_corr_agegroups8, histogram = TRUE, method = "pearson")
chart.Correlation(df_corr_agegroups9, histogram = TRUE, method = "pearson")

###
library(ggcorrplot)
model.matrix(~0+., data=df) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)
# 
# 
# ####
# library(tidyverse)
# library(lsr)
# # function to get chi square p value and Cramers V
# f = function(x,y) {
#   tbl = df %>% select(x,y) %>% table()
#   chisq_pval = round(chisq.test(tbl)$p.value, 4)
#   cramV = round(cramersV(tbl), 4) 
#   data.frame(x, y, chisq_pval, cramV) }
# 
# # create unique combinations of column names
# # sorting will help getting a better plot (upper triangular)
# df_comb = data.frame(t(combn(sort(names(df)), 2)), stringsAsFactors = F)
# df_comb=data
# # apply function to each variable combination
# df_res = map2_df(df$`Total Deaths`, df$`Pneumonia, Influenza, or COVID-19 Deaths`, f)
# 
# # plot results
# df_res %>%
#   ggplot(aes(x,y,fill=chisq_pval))+
#   geom_tile()+
#   geom_text(aes(x,y,label=cramV))+
#   scale_fill_gradient(low="red", high="yellow")+
#   theme_classic()

library(vcd)
st1 <- structable(~Group+`Age group`, df)
#st1
pairs(st1)

st2 <- structable(~`HHS Region`+`State`+`Place of Death`, df)
#st2
pairs(st2)

st_age<- structable(~`COVID-19 Deaths`+`Pneumonia Deaths`+`Age group`, df)
pairs(st_age)



###
library(corrr)

library(tidyverse)
library(rcompanion)


# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# Adopted from https://stackoverflow.com/a/52557631/590437
mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
  # https://github.com/r-lib/rlang/issues/781
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x =  pull(df, xName)
    y =  pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
      cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
}

# df[,3:14] %>%
# #  select(,- c('Start Date','End Date')) %>%
#   mixed_assoc() %>%
# #  select(x, y, assoc) %>%
#   spread(y, assoc) %>%
#   #column_to_rownames("x") %>%
#   as.matrix %>%
#   as_cordf %>%
#   network_plot()


# now same as last week but standardised data:
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

# standardisind data per 10`000
us_popul<-statepop 

standardise_pop<-function(data)
{
  data[,2]=data[,2]*10000/us_popul$pop_2015
  return(data)
}

TotalDeath_by_state<-standardise_pop(TotalDeath_by_state)
TotalDeath_by_state
PneumoniaDeath_by_state<-standardise_pop(PneumoniaDeath_by_state)
PneumoniaDeath_by_state
PneumoniaCOVIDDeaths_by_state<-standardise_pop(PneumoniaCOVIDDeaths_by_state)
PneumoniaCOVIDDeaths_by_state
InfluenzaDeaths_by_state<-standardise_pop(InfluenzaDeaths_by_state)
InfluenzaDeaths_by_state
PneumoniaInfluenza_or_COVIDDeaths_by_state<-standardise_pop(PneumoniaInfluenza_or_COVIDDeaths_by_state)
PneumoniaInfluenza_or_COVIDDeaths_by_state


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


