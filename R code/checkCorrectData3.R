# Load Data
getwd()
undergrad_and_postgrad_2007_2018 <- read_csv("Row data/Tuition and additional fees (canada 2007-2018).csv", col_types = cols(Coordinate = col_skip(), Value = col_number(), Vector = col_skip()))
undergrad_and_postgrad_2007_2018 <- data.frame(undergrad_and_postgrad_2007_2018) #converting to a dataframe


#step1: understand the data
head(undergrad_and_postgrad_2007_2018) #Print the first few rows of a dataset
str(undergrad_and_postgrad_2007_2018) #structure of data
anyDuplicated(undergrad_and_postgrad_2007_2018) # check for duplicates 
names(undergrad_and_postgrad_2007_2018)
View(undergrad_and_postgrad_2007_2018) # view as table
length(undergrad_and_postgrad_2007_2018) # number of elements or components

#step2: correcting 
colnames(undergrad_and_postgrad_2007_2018)[4] <- "averageTuitionFee"  # rename a column name
colnames(undergrad_and_postgrad_2007_2018)[3] <- "STUDYLEVEL"  # rename a column name
# change to correct data type 
attach(undergrad_and_postgrad_2007_2018)
undergrad_and_postgrad_2007_2018$Ref_Date<-as.ordered(Ref_Date) # change from character to ordinal
# convert from character to factor 
undergrad_and_postgrad_2007_2018[,c('GEO','STUDYLEVEL')]<- lapply(undergrad_and_postgrad_2007_2018[,c('GEO','STUDYLEVEL')], as.factor)


#data description and summary after correction  
str(undergrad_and_postgrad_2007_2018) #structure of data after correction
summary(undergrad_and_postgrad_2007_2018)  # summary statistics of the dataset
summary(undergrad_and_postgrad_2007_2018$averageTuitionFee) # summary statistics for the numeric value
sd(undergrad_and_postgrad_2007_2018$averageTuitionFee)  # standard dev of the the numeric value

#step 3:  see undergrad and postgrad separtately

#data with only undergraduate program
undergrad_2007_2018<- undergrad_and_postgrad_2007_2018[which(undergrad_and_postgrad_2007_2018$STUDYLEVEL == "Canadian undergraduate"),]
summary(undergrad_2007_2018)
summary(undergrad_2007_2018$averageTuitionFee) # summary statistics for the numeric value
sd(undergrad_2007_2018$averageTuitionFee)  # standard dev of the the numeric value

#data with only graduate program
grad_2007_2018<- undergrad_and_postgrad_2007_2018[which(undergrad_and_postgrad_2007_2018$STUDYLEVEL== "Canadian graduate"),]
summary(grad_2007_2018)
summary(grad_2007_2018$averageTuitionFee) # summary statistics for the numeric value
sd(grad_2007_2018$averageTuitionFee)  # standard dev of the the numeric value

#Boxplot group function
boxplotfunctionFinal <- function(df, attribute){
  tempValue = "null"
  tempCount = 0
  summaryReturned = array()
  par(mfrow = c(1,11))
  for (i in attribute) {
    
    if (!identical( tempValue, i) ){
      #print (i)
      
      boxplotgroup(df, df$expenditureAmount, i)
    }
    tempValue = i
  }
}

#Boxplot single function 
boxplotgroup <- function(df, attribute,value){
  boxplot(attribute, ylim = c(50000, 25000000),xlab=value )
  title("Boxplot for Numeric Attributes",line=-2,outer=TRUE)
}

