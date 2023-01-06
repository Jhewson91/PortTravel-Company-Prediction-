install.packages("ggpubr")
library(ggplot2, Hmisc); library(ggpubr)

travelData <- read.csv("C:/Users/jhews/OneDrive/Documents/Data Analytics/Portfolio/Project 3 - Travel Company/PortTravel Customer Records Dataset.csv", header = TRUE)
travelData

names(travelData)
head(travelData, n=10)

##DATA CLEANING##

#1 Duplications: 
sum(duplicated(travelData)) #This is to check if any of the rows are duplicated entries, there are 0. 

#2 Inconsistent Variables - Gender:
unique(travelData$Gender) # Here we see there are 3 Genders, we need to change the fe Male. 
travelData$Gender <- gsub("Fe Male", "Female", travelData$Gender) #I use gsub() to change the "fe male" to "female"
unique(travelData$Gender) #I check again to see if it worked, there are now 2 genders not three. 
travelData$Gender #I can also see in this that it has worked.

#3 Outliers -  From my checks, all variables apart from Income and NumberOfTrips had no substantial outliers 
#Outliers (Income):
outlierCheck <- ggplot(travelData, aes(Income)) #Use boxplots to identify outliers
outlierCheck + geom_boxplot()
boxplot.stats(travelData$Income)$out #this shows me the outliers (outside of IQR), here I select the 4 outlier vairables
which(travelData$Income == 1000) #this is to identify row number of variable 1
which(travelData$Income == 4678) #this is to identify row number of variable 2
which(travelData$Income == 98678)#this is to identify row number of variable 3
which(travelData$Income == 95000)#this is to identify row number of variable 4
travelData <- travelData[-c(143, 2587, 2483, 39), ] #this is to remove these rows.
outlierCheck <- ggplot(travelData, aes(Income)) #Here we can check this has worked (substantial outliers removed)
outlierCheck + geom_boxplot()

#Outliers (NumberOfTrips)
which(travelData$NumberOfTrips == 22) #this is to identify row number of variable 1
which(travelData$NumberOfTrips == 21) #this is to identify row number of variable 2
which(travelData$NumberOfTrips == 20) #this is to identify row number of variable 3
which(travelData$NumberOfTrips == 19) #this is to identify row number of variable 4
travelData <- travelData[-c(3257,815, 2826, 384), ]#Remove substantial Outliers 
outlierCheck2 <- ggplot(travelData, aes(NumberOfTrips)) #Here we can check this has worked (substantial outliers removed)
outlierCheck2 + geom_boxplot()





#QUESTION 1: DESCRIPTIVE ANALYSIS#

#1 AGE: Histogram showing Frequency Distribution + Central Tendency Lines
age <- ggplot(travelData, aes(Age), na.rm = TRUE)
age + geom_histogram(binwidth = 1, position = "dodge", colour = "black", fill = "white") +
  stat_central_tendency(type = "mean", linetype = "dashed", colour = "red") + annotate("text", x = 50, y = 225, label = "Mean = 37.62", colour = "Red") +
  stat_central_tendency(type = "median", linetype = "dotted", colour = "orange") + annotate("text", x = 50, y = 245, label = "Median = 36", colour = "orange") +
  stat_central_tendency(type = "mode", linetype = "longdash", colour = "blue") + annotate("text", x = 50, y = 235, label = "Mode = 35", colour = "blue") +
  labs(x = "Customer Age", y = "Frequency") + 
  ggtitle("The Frequency of Customers by Age, Including Central Tendency") 

#Boxplot Showing Dispersion (Range, IQR, 1st Quartile, 3rd Quartile, Median)
boxplot(travelData$Age, ylab= "Age", main = "Boxplot showing the Dispersion of Ages")

#Central Tendency
median(travelData$Age, na.rm = TRUE)
mean(travelData$Age, na.rm = TRUE)

#Dispersion
range(travelData$Age, na.rm = TRUE)
IQR(travelData$Age, na.rm = TRUE)
sd(travelData$Age, na.rm = TRUE)
var(travelData$Age, na.rm = TRUE)

#Box plot - Marital Status & Age
maritalAge <- ggplot(travelData, aes(MaritalStatus, Age), na.rm = TRUE)
maritalAge + geom_boxplot() + labs(x = "Marital Status", y = "Age") + ggtitle("Box Plot showing Marital Status and Age")



#2 INCOME: 
#Histogram showing Frequency Distribution + Central Tendency Lines
income <- ggplot(travelData, aes(as.numeric(Income)), na.rm = TRUE)
income + geom_histogram(binwidth = 400, colour = "black", fill = "white") +
  stat_central_tendency(type = "mean", linetype = "dashed", colour = "red") + annotate("text", x = 28000, y = 300, label = "Mean = £23,599.22", colour = "Red") +
  stat_central_tendency(type = "median", linetype = "dotted", colour = "orange") + annotate("text", x = 28000, y = 280, label = "Median = £22,351", colour = "orange") +
  stat_central_tendency(type = "mode", linetype = "longdash", colour = "blue") + annotate("text", x = 28000, y = 260, label = "Mode = £21,288, £21,020, £20,855, and £17,342", colour = "blue") +
  labs(x = "Gross Yearly Income (£)", y = "Frequency") + 
  ggtitle("The Frequency of Customers by Income, Including Central Tendency") 

#Boxplot Showing Dispersion (Range, IQR, 1st Quartile, 3rd Quartile, Median)
boxplot(travelData$Income, ylab= "Gross Yearly Income (£)", main = "Boxplot showing the Dispersion of Income")
boxplot(travelData$Income, ylab= "Gross Yearly Income (£)", main = "Boxplot showing the Dispersion of Income (Substantial Outliers Removed)")

#Line Graph Occupation and Income
occIncome <- ggplot(travelData, aes(Occupation, Income), na.rm = TRUE)
occIncome + stat_summary(fun = "mean", geom = "point") + stat_summary(fun = "mean", geom = "line", aes(group = 1), colour = "blue", linetype = "dashed") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +labs(x = "Occupation Type", y = "Income (£)") + ggtitle("Line Graph Showing Average Income by Occupation Type")

#Central Tendency
median(travelData$Income, na.rm = TRUE)
mean(travelData$Income, na.rm = TRUE)

#Dispersion
range(travelData$Income, na.rm = TRUE)
IQR(travelData$Income, na.rm = TRUE)
sd(travelData$Income, na.rm = TRUE)
var(travelData$Income, na.rm = TRUE)

#3 NUMBEROFTRIPS
#Histogram showing Frequency Distribution + Central Tendency Lines
tripsNumber <- ggplot(travelData, aes(NumberOfTrips), na.rm = TRUE)
tripsNumber + geom_histogram(binwidth = 1, colour = "black", fill = "white") + 
  stat_central_tendency(type = "mean", linetype = "dashed", colour = "red") + 
  annotate("text", x = 10, y = 1000, label = "Mean = 3.22", colour = "Red") + 
  stat_central_tendency(type = "median", linetype = "dotted", colour = "orange") + 
  annotate("text", x = 10, y = 900, label = "Median = 3", colour = "orange") + 
  stat_central_tendency(type = "mode", linetype = "longdash", colour = "blue") + 
  annotate("text", x = 10, y = 800, label = "Mode = 2", colour = "blue") +
  labs(x = "Average Number of Trips in a Year", y = "Frequency") + 
  ggtitle("Average Number of Trips in a Year, Including Central Tendency") 

#Boxplot Showing Dispersion (Range, IQR, 1st Quartile, 3rd Quartile, Median)
boxplot(travelData$NumberOfTrips, ylab= "Average Number of Trips in a Year", main = "Boxplot Showing the Dispersion of Yearly Trips")

# Bar Chart - Number of Trips vs Occupation
occupationNumber <- ggplot(travelData, aes(Occupation, NumberOfTrips), na.rm = TRUE)
occupationNumber + stat_summary(fun = "mean", geom = "bar", colour = "blue", fill = "pink", na.rm = TRUE) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, na.rm = TRUE) + 
  labs(x = "Occupation Type", y = "Average Number of Trips") + 
  ggtitle("Mean Average Number of Trips by Occupation Type")

# Pie Chart Showing Gender & Number of Trips
genderTrips <- ggplot(travelData, aes(x = "", y = NumberOfTrips, fill = Gender, na.rm = TRUE))
genderTrips + geom_bar(width = 1, stat = "Identity") + coord_polar("y", start = 0, direction = 1) + 
  theme_classic() + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme_void() +
  labs(y = "Number of Trips", fill = "Gender") + ggtitle("Pie Chart Showing the Number of Trips by Gender")

#Central Tendency
median(travelData$NumberOfTrips, na.rm = TRUE)
mean(travelData$NumberOfTrips, na.rm = TRUE)

#Dispersion
range(travelData$NumberOfTrips, na.rm = TRUE)
IQR(travelData$NumberOfTrips, na.rm = TRUE)
sd(travelData$NumberOfTrips, na.rm = TRUE)
var(travelData$NumberOfTrips, na.rm = TRUE)



#4 NUMBEROFCHILDRENVISITING 
#Histogram showing Frequency Distribution + Central Tendency Lines
childVisits <- ggplot(travelData, aes(NumberOfChildrenVisiting))
childVisits + geom_histogram(binwidth = 1, colour = "black", fill = "white") +
  stat_central_tendency(type = "mean", linetype = "dashed", colour = "red") + 
  annotate("text", x = 2, y = 2000, label = "Mean = 1.18", colour = "Red") + 
  stat_central_tendency(type = "median", linetype = "dotted", colour = "orange") + 
  annotate("text", x = 2, y = 1800, label = "Median = 1", colour = "orange") + 
  stat_central_tendency(type = "mode", linetype = "longdash", colour = "blue") + 
  annotate("text", x = 2, y = 1600, label = "Mode = 1", colour = "blue") +
  labs(x = "Average Number of Children Visiting per Year", y = "Frequency") + 
  ggtitle("Average Number of Children Visiting per Year, Including Central Tendency") 

#Boxplot Showing Dispersion (Range, IQR, 1st Quartile, 3rd Quartile, Median)
boxplot(travelData$NumberOfChildrenVisiting, ylab= "Average Number of Children Visiting per Year", main = "Boxplot Showing the Dispersion of Child Visits")

#Bar - Marital Status vs. Number of Children Visiting:
maritalChild <- ggplot(travelData, aes(MaritalStatus, NumberOfChildrenVisiting))
maritalChild + stat_summary(fun = "mean", geom = "bar", fill = "white", colour = "blue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.5) + 
  labs(x = "Marital Status", y = "Number of Children Visiting") + ggtitle("Bar Chart Showing Number of Children Visiting by Marital Status")

#Regression Line - income vs number of Children Visiting
incomeChildren <- ggplot(travelData, aes(Income, NumberOfChildrenVisiting), na.rm = TRUE)
incomeChildren + geom_point(position = "jitter", alpha = 0.2) + geom_smooth(method = lm) + labs(x = "Income (£)", y = "Number of Children Visiting") + ggtitle("Regression Line Showing Income and Number of Children Visiting (Substantial Outliers Removed)")

#Central Tendency
median(travelData$NumberOfChildrenVisiting, na.rm = TRUE)
mean(travelData$NumberOfChildrenVisiting, na.rm = TRUE)

#Dispersion
range(travelData$NumberOfChildrenVisiting, na.rm = TRUE)
IQR(travelData$NumberOfChildrenVisiting, na.rm = TRUE)
sd(travelData$NumberOfChildrenVisiting, na.rm = TRUE)
var(travelData$NumberOfChildrenVisiting, na.rm = TRUE)




#5	PITCHSATISFACTIONSCORE 
#Histogram showing Frequency Distribution + Central Tendency Lines
pitchSat <- ggplot(travelData, aes(PitchSatisfactionScore), na.rm = TRUE)
pitchSat + geom_histogram(binwidth = 1, colour = "black", fill = "white") +
  stat_central_tendency(type = "mean", linetype = "dashed", colour = "red") + 
  annotate("text", x = 2, y = 2000, label = "Mean = 3.077", colour = "Red") + 
  stat_central_tendency(type = "median", linetype = "dotted", colour = "orange") + 
  annotate("text", x = 2, y = 1800, label = "Median = 3", colour = "orange") + 
  stat_central_tendency(type = "mode", linetype = "longdash", colour = "blue") + 
  annotate("text", x = 2, y = 1600, label = "Mode = 3", colour = "blue") +
  labs(x = "Sales Pitch Satisfaction Score", y = "Frequency") + 
  ggtitle("Sales Pitch Satisfaction Score, Including Central Tendency") 

#Boxplot Showing Dispersion (Range, IQR, 1st Quartile, 3rd Quartile, Median)
boxplot(travelData$PitchSatisfactionScore, ylab= "Sales Pitch Satisfaction Score", main = "Boxplot Showing the Dispersion of Sales Pitch Satisfaction Score")

# Bar Chart showing the Pitch Satisfaction Score by Marital Status
pitchMarital <- ggplot(travelData, aes(MaritalStatus, PitchSatisfactionScore))
pitchMarital + stat_summary(fun = "mean", geom = "bar", fill = "white", colour = "blue") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.5) + labs(x = "Marital Status", y = "Pitch Saitsfaction Score") + 
  ggtitle("Bar Chart Showing the Pitch Satisfaction Score by Marital Status")

#Central Tendency
median(travelData$PitchSatisfactionScore, na.rm = TRUE)
mean(travelData$PitchSatisfactionScore, na.rm = TRUE)

#Dispersion
range(travelData$PitchSatisfactionScore, na.rm = TRUE)
IQR(travelData$PitchSatisfactionScore, na.rm = TRUE)
sd(travelData$PitchSatisfactionScore, na.rm = TRUE)
var(travelData$PitchSatisfactionScore, na.rm = TRUE)




#6	PREFERREDPROPERTYSTAR: 
#Histogram showing Frequency Distribution + Central Tendency Lines
preferredStar <- ggplot(travelData, aes(PreferredPropertyStar), na.rm = TRUE)
preferredStar + geom_histogram(binwidth = 1, colour = "black", fill = "white") +
  stat_central_tendency(type = "mean", linetype = "dashed", colour = "red") + 
  annotate("text", x = 4, y = 2000, label = "Mean = 3.581", colour = "Red") + 
  stat_central_tendency(type = "median", linetype = "dotted", colour = "orange") + 
  annotate("text", x = 4, y = 1800, label = "Median = 3", colour = "orange") + 
  stat_central_tendency(type = "mode", linetype = "longdash", colour = "blue") + 
  annotate("text", x = 4, y = 1600, label = "Mode = 3", colour = "blue") +
  labs(x = "Preferred Property Star", y = "Frequency") + 
  ggtitle("Preferred Property Star, Including Central Tendency") 

#Boxplot Showing Dispersion (Range, IQR, 1st Quartile, 3rd Quartile, Median)
boxplot(travelData$PreferredPropertyStar, ylab= "Preferred Property Star", main = "Boxplot Showing the Dispersion of Preferred Property Star")

#Central Tendency
median(travelData$PreferredPropertyStar, na.rm = TRUE)
mean(travelData$PreferredPropertyStar, na.rm = TRUE)

#Dispersion
range(travelData$PreferredPropertyStar, na.rm = TRUE)
IQR(travelData$PreferredPropertyStar, na.rm = TRUE)
sd(travelData$PreferredPropertyStar, na.rm = TRUE)
var(travelData$PreferredPropertyStar, na.rm = TRUE)




#7 NUMBEROFFOLLOWUPS: 
#Histogram showing Frequency Distribution + Central Tendency Lines
followups <- ggplot(travelData, aes(NumberOfFollowups), na.rm = TRUE)
followups + geom_histogram(binwidth = 1, colour = "black", fill = "white") +
  stat_central_tendency(type = "mean", linetype = "dashed", colour = "red") + 
  annotate("text", x = 5, y = 2000, label = "Mean = 3.7071", colour = "Red") + 
  stat_central_tendency(type = "median", linetype = "dotted", colour = "orange") + 
  annotate("text", x = 5, y = 1800, label = "Median = 4", colour = "orange") + 
  stat_central_tendency(type = "mode", linetype = "longdash", colour = "blue") + 
  annotate("text", x = 5, y = 1600, label = "Mode = 4", colour = "blue") +
  labs(x = "Total Number of Follow-ups after Pitch", y = "Frequency") + 
  ggtitle("Total Number of Follow-ups after Pitch, Including Central Tendency") 

#Boxplot Showing Dispersion (Range, IQR, 1st Quartile, 3rd Quartile, Median)
boxplot(travelData$NumberOfFollowups, ylab= "Total Number of Follow-ups after Pitch", main = "Boxplot Showing the Dispersion of the Total Number of Follow-ups after Pitch")

#Scatter - Number of Follow Ups vs Persons Visiting:
followGraph <- ggplot(travelData, aes(NumberOfFollowups, NumberOfPersonVisiting), na.rm = TRUE)
followGraph + geom_point(position = "jitter", alpha = 0.3) + labs(x = "Number of Follow-ups", y = "Number of Persons Visiting") + ggtitle("Scatter Graph Showing Number of Follow-Ups and Number of Visitors")

#Central Tendency
median(travelData$NumberOfFollowups, na.rm = TRUE)
mean(travelData$NumberOfFollowups, na.rm = TRUE)

#Dispersion
range(travelData$NumberOfFollowups, na.rm = TRUE)
IQR(travelData$NumberOfFollowups, na.rm = TRUE)
sd(travelData$NumberOfFollowups, na.rm = TRUE)
var(travelData$NumberOfFollowups, na.rm = TRUE)

#8 CityTier Histogram showing Frequency Distribution + Central Tendency Lines
cityTier <- ggplot(travelData, aes(CityTier), na.rm = TRUE)
cityTier + geom_histogram(binwidth = 1, colour = "black", fill = "white") +
  stat_central_tendency(type = "mean", linetype = "dashed", colour = "red") + 
  annotate("text", x = 2, y = 2000, label = "Mean = 1.654", colour = "Red") + 
  stat_central_tendency(type = "median", linetype = "dotted", colour = "orange") + 
  annotate("text", x = 2, y = 1800, label = "Median = 1", colour = "orange") + 
  stat_central_tendency(type = "mode", linetype = "longdash", colour = "blue") + 
  annotate("text", x = 2, y = 1600, label = "Mode = 1", colour = "blue") +
  labs(x = "City Tier", y = "Frequency") + 
  ggtitle("City Tier, Including Central Tendency") 

#Boxplot Showing Dispersion (Range, IQR, 1st Quartile, 3rd Quartile, Median)
boxplot(travelData$CityTier, ylab= "City Tier", main = "Boxplot Showing the Dispersion of City Tiers")

#Histogram Showing the Frequency of City Tiers with Car Ownership
travelData$OwnCar <- factor(travelData$OwnCar, levels = c(0:1), labels = c("No", "Yes"))
travelData$OwnCar
cityLine <- ggplot(travelData, aes(CityTier, fill = OwnCar))
cityLine + geom_histogram(binwidth = 1) + labs(x = "City Tier", y = "Frequency", fill = "Car Ownership") + ggtitle("Histogram Showing the Frequency of City Tiers with Car Ownership")

#Central Tendency
median(travelData$CityTier, na.rm = TRUE)
mean(travelData$CityTier, na.rm = TRUE)

#Dispersion
range(travelData$CityTier, na.rm = TRUE)
IQR(travelData$CityTier, na.rm = TRUE)
sd(travelData$CityTier, na.rm = TRUE)
var(travelData$CityTier, na.rm = TRUE)





#9 NUMBEROFPERSONVISITING: 
#Histogram showing Frequency Distribution + Central Tendency Lines
visitors <- ggplot(travelData, aes(NumberOfPersonVisiting), na.rm = TRUE)
visitors + geom_histogram(binwidth = 1, colour = "black", fill = "white") +
  stat_central_tendency(type = "mean", linetype = "dashed", colour = "red") + 
  annotate("text", x = 2, y = 2000, label = "Mean = 2.905", colour = "Red") + 
  stat_central_tendency(type = "median", linetype = "dotted", colour = "orange") + 
  annotate("text", x = 2, y = 1800, label = "Median = 3", colour = "orange") + 
  stat_central_tendency(type = "mode", linetype = "longdash", colour = "blue") + 
  annotate("text", x = 2, y = 1600, label = "Mode = 3", colour = "blue") +
  labs(x = "Total number of persons on the Trip with the Customer", y = "Frequency") + 
  ggtitle("Total number of persons on the Trip with the Customer, Including Central Tendency") 

#Boxplot Showing Dispersion (Range, IQR, 1st Quartile, 3rd Quartile, Median)
boxplot(travelData$NumberOfPersonVisiting, ylab= "Total number of persons on the Trip with the Customer", main = "Boxplot Showing the Dispersion of Total number of persons on the Trip with the Customer")

#Central Tendency
median(travelData$NumberOfPersonVisiting, na.rm = TRUE)
mean(travelData$NumberOfPersonVisiting, na.rm = TRUE)

#Dispersion
range(travelData$NumberOfPersonVisiting, na.rm = TRUE)
IQR(travelData$NumberOfPersonVisiting, na.rm = TRUE)
sd(travelData$NumberOfPersonVisiting, na.rm = TRUE)
var(travelData$NumberOfPersonVisiting, na.rm = TRUE)


#10 PASSPORT: 
#Histogram showing Frequency Distribution 
#factor() variables for passport and OwnCar:
travelData$Passport <- factor(travelData$Passport, levels = c(0:1), labels = c("No", "Yes"))
travelData$Passport
pass <- ggplot(travelData, aes(Passport), na.rm = TRUE)
pass + geom_histogram(stat = "count", binwidth = 1, colour = "black", fill = "white") +
  labs(x = "Passport Holders", y = "Frequency") + 
  ggtitle("Total number of Passport Holders")

# Pie Chart Showing Passport Holder & Number of Trips
passTrips <- ggplot(travelData, aes(x = "", y = NumberOfTrips, fill = Passport, na.rm = TRUE))
passTrips + geom_bar(width = 1, stat = "Identity") + coord_polar("y", start = 0, direction = 1) + theme_classic() + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank()) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme_void() +labs(y = "Number of Trips", fill = "Passport Holder") + ggtitle("Pie Chart Showing the Number of Trips by Passport Holder")




#QUESTION 2: CORRELATION ANALYSIS#

#Install Packages
install.packages("Hmisc"); install.packages("ggm");install.packages("corrr");
install.packages("ggplot2"); install.packages("polycor"); install.packages("corrplot");
library(boot); library(ggm); library(ggplot2); library(Hmisc);
library(polycor); library("corrplot"); library(corrr);

#MISSING VALUES: IMPUTED MEAN#
colSums(is.na(travelData))#Shows missing values by column 
#For Loop to conduct Imputed Mean 
for(i in 1:ncol(travelData)) {
  travelData[ , i][is.na(travelData[ , i])] <- mean(travelData[ , i], na.rm = TRUE)
}
colSums(is.na(travelData))#Check to see all missing values are now replaced with Imputed mean
travelData

#Removing CustomerID variable, because it is not a variable of interest.
travelData <- travelData[, c("Age","TypeofContact", "CityTier", "Occupation", "Gender", "NumberOfPersonVisiting", "NumberOfFollowups", "PreferredPropertyStar", "MaritalStatus", "Passport", "PitchSatisfactionScore", "OwnCar", "NumberOfChildrenVisiting", "Income", "NumberOfTrips")]
travelData

#ENCODE CATEGORICAL VARIABLES TO NUMERIC USING DUMMY VARIABLES#

#Encode Marital Status using dummy variables
travelData$MaritalStatusMarried <- ifelse(travelData$MaritalStatus == "Married", 1, 0)
travelData$MaritalStatusSingle <- ifelse(travelData$MaritalStatus == "Single", 1, 0)
travelData$MaritalStatusUnmarried <- ifelse(travelData$MaritalStatus == "Unmarried", 1, 0)
#Test to make sure the dummy variable is correct:
travelData$MaritalStatusMarried
travelData$MaritalStatusSingle
travelData$MaritalStatusUnmarried

#Encode Occupation using dummy variables
travelData$OccupationSmallBusiness <- ifelse(travelData$Occupation == "Small Business", 1, 0)
travelData$OccupationSalaried <- ifelse(travelData$Occupation == "Salaried", 1, 0)
travelData$OccupationLargeBusiness <- ifelse(travelData$Occupation == "Large Business", 1, 0)
#Test to make sure the dummy variable is correct:
travelData$OccupationSmallBusiness
travelData$OccupationSalaried
travelData$OccupationLargeBusiness

#Encode Gender using dummy variables
travelData$GenderFemale <- ifelse(travelData$Gender == "Female", 1, 0)
#Test to make sure the dummy variable is correct:
travelData$GenderFemale

#Encode Type of Contact using dummy variables
travelData$TypeofContactSelfEnquiry <- ifelse(travelData$TypeofContact == "Self Enquiry", 1, 0)
#Test to make sure the dummy variable is correct:
travelData$TypeofContactSelfEnquiry

#Encode Passport using dummy variables
travelData$Passport <- ifelse(travelData$Passport == "Yes", 1, 0)
#Test to make sure the dummy variable is correct:
travelData$Passport

#Encode OwnCar using dummy variables
travelData$OwnCar <- ifelse(travelData$OwnCar == "Yes", 1, 0)
#Test to make sure the dummy variable is correct:
travelData$OwnCar

# Restructure travelData data frame with numeric / dummy variables ready for correlation analysis
travelData <- travelData[, c("Age", "CityTier", "NumberOfPersonVisiting", "NumberOfFollowups", "PreferredPropertyStar", "Passport", "PitchSatisfactionScore", "OwnCar", "NumberOfChildrenVisiting", "Income", "NumberOfTrips", "MaritalStatusMarried", "MaritalStatusSingle", "MaritalStatusUnmarried", "GenderFemale", "OccupationSmallBusiness", "OccupationLargeBusiness", "OccupationSalaried", "TypeofContactSelfEnquiry")]
travelData 


#EXAMPLES OF CORRELATION BETWEEN VARIABLES:

#1: Number of Children visiting and Number of Persons Visiting: 
visitgraph <- ggplot(travelData, aes(NumberOfPersonVisiting, NumberOfChildrenVisiting))
visitgraph + geom_point(position = "jitter") + geom_smooth(method = lm) +
  labs(x = "Number of People Visiting", y = "Number of Children Visiting") +
  ggtitle("Scatter plot with Linear Regression to show Correlation")

#Pearson's product-moment correlation method chosen as:
#both variables are continuous, 
#they are normally distributed via the central limit theorem, 
#they have a linear relationship and 
#no significant outliers.
#Cor.test() function used with alternative set to “greater” as the scatter plot implies positive correlation
cor.test(travelData$NumberOfPersonVisiting, travelData$NumberOfChildrenVisiting, alternative = "greater", method = "pearson")

#2: Number of follow-ups and Number of people visiting:
followGraph <- ggplot(travelData, aes(NumberOfFollowups, NumberOfPersonVisiting))
followGraph + geom_point(position = "jitter") + geom_smooth(method = lm) +
  labs(x = "Number of Follow-Ups", y = "Number of People Visiting") +
  ggtitle("Scatter plot with Linear Regression to show Correlation")

#Pearson's product-moment correlation method chosen as:
#both variables are continuous, 
#they are normally distributed via the central limit theorem, 
#they have a linear relationship and 
#no significant outliers.
#Cor.test() function used with alternative set to “greater” as the scatter plot implies positive correlation
cor.test(travelData$NumberOfFollowups, travelData$NumberOfPersonVisiting, alternative = "greater", method = "pearson")


# 3: Income and Age: they are continuous and large data set, but there are outliers so I used spearman. 
AgeIncomeGraph <- ggplot(travelData, aes(Income, Age))
AgeIncomeGraph + geom_point(position = "jitter") + geom_smooth(method = lm) +
  labs(x = "Income (£)", y = "Age") +
  ggtitle("Scatter plot with Linear Regression to show Correlation")

#Spearman's product-moment correlation method chosen as:
#There are some outliers, so decided to use Spearman rather than Pearson
cor.test(travelData$Income, travelData$Age, alternative = "greater", method = "spearman")


# 4: Number of Persons Visiting and Income:
personIncome <- ggplot(travelData, aes(NumberOfPersonVisiting, Income))
personIncome + geom_point(position = "jitter") + geom_smooth(method = lm) +
  labs(x = "Number of Persons Visiting", y = "Income (£)") +
  ggtitle("Scatter plot with Linear Regression to show Correlation")

#Spearman's product-moment correlation method chosen as:
#There are some outliers, so decided to use Spearman rather than Pearson
cor.test(travelData$NumberOfPersonVisiting, travelData$Income, alternative = "greater", method = "spearman")






#COMPREHENSIVE ANALYSIS OF FULL DATA SET VIA TABLE:

#Create a table of all the correlations using rcorr():
rcorr(as.matrix(travelData), type = "spearman")

#Create Correlogram to Visualize Correlation
head(travelData)
M <- cor(travelData)
head(round(M,2))
corrplot(M, method="circle", type="upper")






#NUMBEROFTRIPS CORRELATION ANALYSIS:

#1 NumberOfTrips and Age

tripsAge <- ggplot(travelData, aes(NumberOfTrips, Age))
tripsAge + geom_point(position = "jitter") + geom_smooth(method = lm) +
  labs(x = "NumberOfTrips", y = "Age") +
  ggtitle("Scatter plot with Linear Regression to show NumberOfTrips and Age")

cor.test(travelData$NumberOfTrips, travelData$Age, alternative = "greater", method = "pearson")

#2 NumberOfTrips and NumberOfPersonVisiting

tripsVisit <- ggplot(travelData, aes(NumberOfTrips, NumberOfPersonVisiting))
tripsVisit + geom_point(position = "jitter") + geom_smooth(method = lm) +
  labs(x = "NumberOfTrips", y = "NumberOfPersonVisiting") +
  ggtitle("Scatter plot with Linear Regression to show NumberOfTrips and NumberOfPersonVisiting")

cor.test(travelData$NumberOfTrips, travelData$NumberOfPersonVisiting, alternative = "greater", method = "pearson")

#3 NumberOfTrips and NumberOfFollowups

tripsFollow <- ggplot(travelData, aes(NumberOfTrips, NumberOfFollowups))
tripsFollow + geom_point(position = "jitter") + geom_smooth(method = lm) +
  labs(x = "NumberOfTrips", y = "NumberOfFollowups") +
  ggtitle("Scatter plot with Linear Regression to show NumberOfTrips and NumberOfFollowups")

cor.test(travelData$NumberOfTrips, travelData$NumberOfFollowups, alternative = "greater", method = "pearson")

#4 NumberOfTrips and NumberOfChildrenVisiting

tripsChilds <- ggplot(travelData, aes(NumberOfTrips, NumberOfChildrenVisiting))
tripsChilds + geom_point(position = "jitter") + geom_smooth(method = lm) +
  labs(x = "NumberOfTrips", y = "NumberOfChildrenVisiting") +
  ggtitle("Scatter plot with Linear Regression to show NumberOfTrips and NumberOfChildrenVisiting")

cor.test(travelData$NumberOfTrips, travelData$NumberOfChildrenVisiting, alternative = "greater", method = "pearson")

#5 NumberOfTrips and Income
#Identify Outliers 
tripsIncome <- ggplot(travelData, aes(NumberOfTrips, Income))
tripsIncome + geom_point(position = "jitter") + geom_smooth(method = lm) +
  labs(x = "NumberOfTrips", y = "Income (£)") +
  ggtitle("Scatter plot with Linear Regression to show NumberOfTrips and Income")






#QUESTION 3 & 4: DEVELOP THE REGRESSION MODEL + INTERPRET THE FINDINGS

#TEST MODELS to find the model of best fit:
#create a general model to see performance of all variables together:
generalModel <- lm(travelData$NumberOfTrips ~ .,data = travelData)
summary(generalModel)

#TEST MODEL 1:
testModel1 <- lm(formula = travelData$NumberOfTrips ~ Age + NumberOfPersonVisiting + 
                   NumberOfFollowups + MaritalStatusSingle,
                 data = travelData)
summary(testModel1)

#TEST MODEL 2:
testModel2 <- lm(formula = travelData$NumberOfTrips ~ Age + NumberOfPersonVisiting + 
                   NumberOfFollowups + NumberOfChildrenVisiting,
                 data = travelData)
summary(testModel2)

#TEST MODEL 3:
testModel3 <- lm(formula = travelData$NumberOfTrips ~ Age + NumberOfPersonVisiting + 
                   NumberOfFollowups + Income,
                 data = travelData)
summary(testModel3)

# Restructure travelData to hold correlated variables only, ready for stepwise method
travelData <- travelData[, c("NumberOfTrips", "Age", "NumberOfPersonVisiting", "NumberOfFollowups","Income")]
travelData 




#STEPWISE METHOD ANALYSIS

#Backward stepwise method
step(generalModel, direction = "backward")
backwardModel <- lm(formula = travelData$NumberOfTrips ~ Age + NumberOfPersonVisiting + 
                      NumberOfFollowups, data = travelData)
summary(backwardModel)

#Forward stepwise method
forwardModel <- lm(travelData$NumberOfTrips ~ 1, data = travelData)
summary(forwardModel)
step(forwardModel, direction = "forward", scope = formula(generalModel))
forwardModel <- lm(formula = travelData$NumberOfTrips ~ NumberOfPersonVisiting + 
                     Age + NumberOfFollowups, data = travelData)
summary(forwardModel)

#Both ways stepwise method:
step(forwardModel, direction = "both", scope = formula(generalModel))
bothWaysModel <- lm(formula = travelData$NumberOfTrips ~ NumberOfPersonVisiting + 
                      Age + NumberOfFollowups, data = travelData)
summary(bothWaysModel)

# Restructure travelData to hold regression variables only (remove income), ready for "goodness of fit" test
travelData <- travelData[, c("NumberOfTrips", "Age", "NumberOfPersonVisiting", "NumberOfFollowups")]
travelData 






#VALIDATION TECHNIQUES:

#outlier - Standardised Residuals analysis:
travelData$standardResidual <- rstandard(bothWaysModel)
travelData

#Outlier - Isolate large and very large standard residual values
#Isolate > +/- 2
travelData$largeResidual <- travelData$standardResidual > 2 | travelData$standardResidual < -2
travelData$largeResidual
sum(travelData$largeResidual)
296/4880
#Isolate > +/- 2.5
travelData$largerResidual <- travelData$standardResidual > 2.5 | travelData$standardResidual < -2.5
travelData$largerResidual
sum(travelData$largerResidual)
106/4880
#Isolate > +/- 3
travelData$veryLargeResidual <- travelData$standardResidual > 3 | travelData$standardResidual < -3
travelData$veryLargeResidual
sum(travelData$veryLargeResidual)

#Influence - cook distance
cooks.distance(bothWaysModel)
travelData$cook <- cooks.distance(bothWaysModel)
travelData [travelData$cook >1, ]

#Leverage (Hat-Values)
travelData$leverage <- hatvalues(bothWaysModel)
travelData [travelData$leverage >0.00245902, ]

#Covariance Ratio
travelData$covarianceRatio <- covratio(bothWaysModel)
travelData [travelData$covarianceRatio >1.0024902, ]

#Influential and outliers:
travelData [travelData$veryLargeResidual, ]

#Multicollinearity (VIF):
vif(bothWaysModel)
mean(vif(bothWaysModel))





#QUESTION 5: PREDICTION#

#Insert Test Scenarios into the Model to find NumberOfTrips
testScenarios <- read.csv("C:/Users/jhews/OneDrive/Documents/Data Analytics/Portfolio/Project 3 - Travel Company/Additional Customer Dataset.csv", header = TRUE)
testScenarios

head(testScenarios, n=5)

#Predict NumberOfTrips via predict() function
predict(bothWaysModel, newdata = testScenarios)

