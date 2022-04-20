library(tidyverse)
library(dplyr)

#Create MPG Stats Table
MPG_Stats_Table<- read.csv(file='Resources/MechaCar_mpg.csv')

#Generate linear regression model
lm(mpg ~ vehicle_length +  vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MPG_Stats_Table) 
#Determine p-value and r-squared values
summary(lm(mpg ~ vehicle_length +  vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MPG_Stats_Table))

#Create Suspension Coil Table
Suspension_Coil_Table<- read.csv(file='Resources/Suspension_Coil.csv')

# Create a total summary 
total_summary <- Suspension_Coil_Table %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance =var(PSI), SD = sd(PSI))

# Create a lot summary table
lot_summary <- Suspension_Coil_Table %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance =var(PSI), SD = sd(PSI))

#Create T-tests
t.test(Suspension_Coil_Table$PSI, mu=1500)

t.test(subset(Suspension_Coil_Table, Manufacturing_Lot=="Lot1")$PSI, mu=1500)

t.test(subset(Suspension_Coil_Table, Manufacturing_Lot=="Lot2")$PSI, mu=1500)

t.test(subset(Suspension_Coil_Table, Manufacturing_Lot=="Lot3")$PSI, mu=1500)
