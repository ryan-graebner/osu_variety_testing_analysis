####################################################################
#This code calculates BLUEs for yield data for a single location.
####################################################################
#Load libraries
library(lme4)
library(emmeans)

max_years <- 5 #The number of years to include in the analysis
timeframe <- 6 #The maximum number of years to go back if there is missing year. This should be equal to or greater than "max_years"

#Assemble Data
info<-read.csv("sample_info.csv") #This list includes updated variety names, internal program IDs, and other traits. This list also specifies which entries we report data for. In our program, we report data for entries that were included in the most recent year's trial.
data<-read.csv("sample_data.csv") #Loads data
data_a<-data[data$"Location"=="Condon",] #Selects data from Condon
data_b<-data_a[,c("Year","Code","Yield")] #Selects columns with the year, our program's internal ID for each entry, and yield data.

past_years<-sort(as.numeric(unique(data_b$Year)),decreasing=TRUE) #This makes a list of years available
past_years<-as.character(past_years[past_years>max(past_years-timeframe)]) #Selects the years in the allowed timeframe
if(length(past_years)>max_years) past_years<-past_years[1:max_years] #If there are more years than the max number of years allowed in the analysis, selects "max_years" most recent years.
print(past_years) #Prints the years used

data_c<-data_b[which(data_b$Year %in% past_years),] #Selects data from the years that will be used in this analysis.

mod <- lmer(Yield ~ Code + (1|Year), data=data_c) #Fits the mixed effects model
mod.lsm<-emmeans(mod,"Code") #Calculates BLUE values for each variety
lsm.contrast <- contrast(mod.lsm, method = "eff") #Calculates standard errors for each BLUE

#Combines output data into a dataframe & calculate 95% confidence intervals
answer<-cbind(summary(mod.lsm)$emmean,summary(lsm.contrast)$SE*1.96)
rownames(answer)<-summary(mod.lsm)$Code
colnames(answer)<-c("BLUE","CI")

output<-cbind(info,answer[as.character(info$Code),]) #Combines the info table with the corresponding BLUEs and CIs
