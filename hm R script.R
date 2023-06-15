# load data using GUI

#Take a look at the data:
View(hm)

#import libray
library(tidyverse)

#check data types
str(hm)

#check for missing values
colSums(is.na(hm))

#Drop missing values
hm <- hm %>%
drop_na()

#check to make sure that rows have been removed
colSums(is.na(hm))

#Check for duplicates
hm <- hm[!duplicated(hm), ]
dim(hm)

#round off values to 2 places
hm$Profitability <- round(hm$Profitability ,digit=2)
hm$Worldwide.Gross <- round(hm$Worldwide.Gross ,digit=2)

#View(hm)
dim(hm)
#Check for outliers using a boxplot
library(ggplot2)

#Create a boxplot that highlights the outliers

ggplot(hm, aes(x=Profitability, y=Worldwide.Gross))+ geom_boxplot(outlier.colour = "green", outlier.shape = 1)+ scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 1000))

#Remove outliers in 'Profitability'
Q1 <- quantile(hm$Profitability,.25)
Q3 <- quantile(hm$Profitability,.75)
IQR <- IQR(hm$Profitability)

no_outliers <- subset(hm, hm$Profitability> (Q1 - 1.5*IQR) & hm$Profitability< (Q3 + 1.5*IQR))
dim(no_outliers)

# Remove outliers in 'Worldwide.Gross'
Q1 <- quantile(no_outliers$Worldwide.Gross, .25)
Q3 <- quantile(no_outliers$Worldwide.Gross, .75)
IQR <- IQR(no_outliers$Worldwide.Gross)

hm1 <- subset(no_outliers, no_outliers$Worldwide.Gross>
                (Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))
dim(hm1)

#Summary Statistics?Univarate Analysis:
summary(hm1)

#bivariate analysis

#Scatterplot
ggplot(hm1, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0,110))+theme(axis.text.x = element_text(angle = 90))

# Bar Chart
ggplot(hm1, aes(x=Year)) + geom_bar()

#Export clean data
write.csv(hm1, "clean_hm.csv")
