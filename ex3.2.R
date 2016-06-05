## Exercise 3.2

## Load the dataset:
titanic<-read.csv("titanic_original.csv")
View(titanic)

# Replace the empty 'embarked' rows with an "S"
titanic$embarked[titanic$embarked == ""] <-"S"

names(titanic)
# Calculate mean of the Age column
# Download dplyr package in order to use commands
install.packages(dplyr)
average_age_df<-titanic %>% summarize(avg_age=mean(age, na.rm=TRUE))
unique(titanic$age)
# Average age comes out as 29.88113. We assign this to a variable:
aveage<-average_age_df[1,1]
# Now that we have a number for average age, we can assign it to the 'age' column of 
# the dataframe where values are missing.
titanic$age[is.na(titanic$age)] <-aveage


## Lifeboat
View(titanic)
# Fill in the missing values in boat with a "NA":
titanic$boat[titanic$boat == ""] <- "NA"

## Cabin
# Creating column 'has_cabin_number', set to 1 if cabin is not empty and 0 if it is.
titanic$has_cabin_number[titanic$cabin == ""] <- 0
titanic$has_cabin_number[titanic$cabin != ""] <- 1

