library(dplyr)
library(plotly)
library(readr)

incarceration_df <- read.csv("incarceration_trends.csv")
#CA California, WA Washington, AZ Arizona, NM New Mexico, TX Texas, FL Florida, OH Ohio, PA Pennsylvania, IL illinois, NY New York


#Organize the dataframe
newdf <- select(incarceration_df, 
                year, state, county_name, total_pop_15to64, total_jail_pop, 
                aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64, white_pop_15to64, 
                aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop)
newdf <- na.omit(newdf)
write.csv(newdf, "C:/Users/hwang/OneDrive/Desktop/INFO201/newdf.csv")

#Create a function to organize data
organizestatedf <- function(data){
  summarise(
    group_by(data, year), mean_tot_pop_15to64 = round(sum(total_pop_15to64)/(max(data$year)-min(data$year))),
    mean_jail_pop_15to64 = round(sum(total_jail_pop)/(max(data$year)-min(data$year))), 
    mean_aapi_pop_15to64 = round(sum(aapi_pop_15to64)/(max(data$year)-min(data$year))), 
    mean_black_pop_15to64 = round(sum(black_pop_15to64)/(max(data$year)-min(data$year))), 
    mean_latinx_pop_15to64 = round(sum(latinx_pop_15to64)/(max(data$year)-min(data$year))), 
    mean_native_pop_15to64 = round(sum(native_pop_15to64)/(max(data$year)-min(data$year))), 
    mean_white_pop_15to64 = round(sum(white_pop_15to64)/(max(data$year)-min(data$year))), 
    mean_aapi_jail_pop = round(sum(aapi_jail_pop)/(max(data$year)-min(data$year))), 
    mean_black_jail_pop = round(sum(black_jail_pop)/(max(data$year)-min(data$year))), 
    mean_latinx_jail_pop = round(sum(latinx_jail_pop)/(max(data$year)-min(data$year))), 
    mean_native_jail_pop = round(sum(native_jail_pop)/(max(data$year)-min(data$year))), 
    mean_white_jail_pop = round(sum(white_jail_pop)/(max(data$year)-min(data$year)))
  )
}

add_race_prop <- function(data){
  mutate(data, aapi_jail_prop = data$mean_aapi_jail_pop/data$mean_jail_pop_15to64, 
         black_jail_prop = data$mean_black_jail_pop/data$mean_jail_pop_15to64, 
         latinx_jail_prop = data$mean_latinx_jail_pop/data$mean_jail_pop_15to64, 
         native_jail_prop = data$mean_native_jail_pop/data$mean_jail_pop_15to64, 
         white_jail_prop = data$mean_white_jail_pop/data$mean_jail_pop_15to64
         )
}

#Create new dataframes of states
dfca <- filter(newdf, state == "CA")
dfca <- organizestatedf(dfca)
dfca <- add_race_prop(dfca)
dfca <- cbind(statename = "CA", dfca)
 

dfwa <- filter(newdf, state == "WA")
dfwa <- organizestatedf(dfwa)
dfwa <- add_race_prop(dfwa)
dfwa <- cbind(statename = "WA", dfwa)

dfaz <- filter(newdf, state == "AZ")
dfaz <- organizestatedf(dfaz)
dfaz <- add_race_prop(dfaz)
dfaz <- cbind(statename = "AZ", dfaz)

dfnm <- filter(newdf, state == "NM")
dfnm <- organizestatedf(dfnm)
dfnm <- add_race_prop(dfnm)
dfnm <- cbind(statename = "NM", dfnm)

dftx <- filter(newdf, state == "TX")
dftx <- organizestatedf(dftx)
dftx <- add_race_prop(dftx)
dftx <- cbind(statename = "TX", dftx)

dffl <- filter(newdf, state == "FL")
dffl <- organizestatedf(dffl)
dffl <- add_race_prop(dffl)
dffl <- cbind(statename = "FL", dffl)

dfoh <- filter(newdf, state == "OH")
dfoh <- organizestatedf(dfoh)
dfoh <- add_race_prop(dfoh)
dfoh <- cbind(statename = "OH", dfoh)

dfpa <- filter(newdf, state == "PA")
dfpa <- organizestatedf(dfpa)
dfpa <- add_race_prop(dfpa)
dfpa <- cbind(statename = "PA", dfpa)

dfil <- filter(newdf, state == "IL")
dfil <- organizestatedf(dfil)
dfil <- add_race_prop(dfil)
dfil <- cbind(statename = "IL", dfil)

dfny <- filter(newdf, state == "NY")
dfny <- organizestatedf(dfny)
dfny <- add_race_prop(dfny)
dfny <- cbind(statename = "NY", dfny)


statedf <- bind_rows(dfca, dfwa, dfaz, dfnm, dftx, dffl, dfoh, dfpa, dfil, dfny)
statedf <- filter(statedf, year >= as.numeric("1999"))
View(statedf)
write.csv(statedf, "C:/Users/hwang/OneDrive/Desktop/INFO201/statedf.csv")

#Create a dataframe contain new columns
statechosen <- c("CA", "WA", "AZ", "NM", "TX", "FL", "OH", "PA", "IL", "NY")
race <- c("Asian American/Pacific Islander", "Black", "Latinx", "Native American", "White")
total_pop_mean_allyear <- c(round(mean(dfca$mean_tot_pop_15to64)), round(mean(dfwa$mean_tot_pop_15to64)), 
                            round(mean(dfaz$mean_tot_pop_15to64)), round(mean(dfnm$mean_tot_pop_15to64)), 
                            round(mean(dftx$mean_tot_pop_15to64)), round(mean(dffl$mean_tot_pop_15to64)), 
                            round(mean(dfoh$mean_tot_pop_15to64)), round(mean(dfpa$mean_tot_pop_15to64)), 
                            round(mean(dfil$mean_tot_pop_15to64)), round(mean(dfny$mean_tot_pop_15to64)))
total_jail_popmean_allyear <- c(round(mean(dfca$mean_jail_pop_15to64)), round(mean(dfwa$mean_jail_pop_15to64)), 
                                round(mean(dfaz$mean_jail_pop_15to64)), round(mean(dfnm$mean_jail_pop_15to64)), 
                                round(mean(dftx$mean_jail_pop_15to64)), round(mean(dffl$mean_jail_pop_15to64)), 
                                round(mean(dfoh$mean_jail_pop_15to64)), round(mean(dfpa$mean_jail_pop_15to64)), 
                                round(mean(dfil$mean_jail_pop_15to64)), round(mean(dfny$mean_jail_pop_15to64)))
df <- data.frame(state = statechosen, 
           meanpop_allyear_15to64 = total_pop_mean_allyear, 
           jailpop_allyear = total_jail_popmean_allyear)
View(df)
write.csv(df, "C:/Users/hwang/OneDrive/Desktop/INFO201/df.csv")


#Answer questions
#1 What is the average value of my variable across all the counties (in the current year)?

#2 Where is my variable the highest / lowest?

#3 How much has my variable change over the last N years?


