# Load Data
Revenues <- read_csv("~/Documents/ProjectGit/student-debt-proj/universities and degree-granting colleges Revenues.csv",col_types = cols(Coordinate = col_skip(),
                                                                                                       Value = col_number(), Vector = col_skip()))
RevenuesDF <- data.frame(Revenues) #converting Revenues to a dataframe

#step1: understand the data
head(RevenuesDF) #Print the first few rows of a dataset
str(RevenuesDF) #structure of data
anyDuplicated(RevenuesDF) # check for duplicates 
names(RevenuesDF)
View(RevenuesDF) # view as table
length(RevenuesDF) # number of elements or components

#step2: correcting 
colnames(RevenuesDF)[6] <- "revenueAmount"  # rename a column name
# change to correct data type 
attach(RevenuesDF)
RevenuesDF$Ref_Date<-as.ordered(Ref_Date) # change from character to ordinal
# convert from character to factor 
RevenuesDF[,c('GEO','SCHOOL','REVENUE','FUND')]<- lapply(RevenuesDF[,c('GEO','SCHOOL','REVENUE','FUND')], as.factor)


#data description ad summary after correction  
str(RevenuesDF) #structure of data after correction
summary(revenueAmount) # summary statistics for value
sd(revenueAmount)  # standard dev of the value

#step 3: extract the relevant data 
#total univ and college (total expenditure and salary  from total fund ) 
RevenuesDF<- RevenuesDF[FUND == "Total funds (x 1,000)" & SCHOOL == "Total universities and colleges" 
                                & (REVENUE == "Total revenues" | REVENUE == "Tuition and other fees"),]

summary(RevenuesDF) # summary statistics after keeping the relevant data 

#step 4: data with only total revenues
RevenuesDF_total_revenues<- RevenuesDF[which(RevenuesDF$REVENUE == "Total revenues"),]
summary(RevenuesDF_total_revenues)

#step 5: subset data with only Tuition and other fees
RevenuesDF_tuition_revenues<- RevenuesDF[which(RevenuesDF$REVENUE == "Tuition and other fees"),]
summary(RevenuesDF_tuition_revenues)

# Function to get summary by attribute 
summaryfunction <- function(df, attribute, value) {
  eachSummaryResult = lapply(df[attribute == value,], summary)
  return(eachSummaryResult)
}

# function that returns each standard deviation
sdfunction <- function(df, attribute, value){
  eachSummaryResult = lapply(df[attribute == value,], sd)
  return(eachSummaryResult)
}

# function that returns each summary with standard deviation
summaryfunctionFinalRevenue <- function(df, attribute){
  tempValue = "null"
  tempCount = 0
  summaryReturned = array()
  for (i in attribute) {
    if (!identical( tempValue, i) ){
      tempCount = tempCount + 1
      summaryReturned = summaryfunction(df, attribute, i)
      sdReturned = sdfunction(df, attribute, i)
      #Print value summary and standard deviation 
      print (i)
      print (summaryReturned$revenueAmount)
      print("standard deviation")
      print(sdReturned$revenueAmount)
    }
    tempValue = i
  }
}

#Boxplot group function
boxplotfunctionFinalRevenue <- function(df, attribute){
  tempValue = "null"
  tempCount = 0
  summaryReturned = array()
  par(mfrow = c(1,11))
  for (i in attribute) {
    
    if (!identical( tempValue, i) ){
      #print (i)
      
      boxplotgroup(df, df$revenueAmount, i)
    }
    tempValue = i
  }
}

#Boxplot single function 
boxplotgroup <- function(df, attribute,value){
  boxplot(attribute, ylim = c(50000, 25000000),xlab=value )
  title("Boxplot for Numeric Attributes",line=-2,outer=TRUE)
}




#calling the function to print each countries summary total Revenue
summaryfunctionFinalRevenue(RevenuesDF_total_revenues, RevenuesDF_total_revenues$GEO)


#calling the function to print each countries summary tuition Revenue
summaryfunctionFinalRevenue(RevenuesDF_tuition_revenues, RevenuesDF_tuition_revenues$GEO)

#step 6: Write Tuition and other fees Revenue CSV
write.csv(RevenuesDF_tuition_revenues, file = "universities and degree-granting colleges Tuition Revenue (2000-2016) corrected .csv")

#step 7: Write Total revenues CSV
write.csv(RevenuesDF_total_revenues, file = "universities and degree-granting colleges total revenue (2000-2016) corrected .csv")

#step 8: write relevant data csv
write.csv(RevenuesDF, file = "universities and degree-granting colleges Revenue (2000-2016) corrected .csv")

# call box plot rev
boxplotfunctionFinalRevenue(RevenuesDF_total_revenues, RevenuesDF_total_revenues$GEO)



