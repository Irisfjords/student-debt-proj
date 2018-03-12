# Load Data
Expenditures <- read_csv("universities and degree-granting colleges Expenditures.csv",col_types = cols(Coordinate = col_skip(),
                                                                                   Value = col_number(), Vector = col_skip()))
ExpendituresDF <- data.frame(Expenditures) #converting Expenditures to a dataframe
ExpendituresDF

#step1: understand the data
head(ExpendituresDF) #Print the first few rows of a dataset
str(ExpendituresDF) #structure of data
anyDuplicated(ExpendituresDF) # check for duplicates 
names(ExpendituresDF)
View(ExpendituresDF) # view as table
length(ExpendituresDF) # number of elements or components

#step2: correcting 
colnames(ExpendituresDF)[6] <- "expenditureAmount"  # rename a column name
# change to correct data type 
attach(ExpendituresDF)
ExpendituresDF$Ref_Date<-as.ordered(Ref_Date) # change from character to ordinal
# convert from character to factor 
ExpendituresDF[,c('GEO','SCHOOL','EXPENDITURE','FUNDFUNCTION')]<- lapply(ExpendituresDF[,c('GEO','SCHOOL','EXPENDITURE','FUNDFUNCTION')], as.factor)


#data description ad summary after correction  
str(ExpendituresDF) #structure of data after correction
summary(`expenditureAmount($)`) # summary statistics for value
sd(`expenditureAmount($)`)  # standard dev of the value

#step 3: extract the relevant data 
#total univ and college (total expenditure and salary  from total fund ) 
ExpendituresDF<- ExpendituresDF[FUNDFUNCTION == "Total funds (x 1,000)" & SCHOOL == "Total universities and colleges" 
                                & (EXPENDITURE == "Total expenditures" | EXPENDITURE == "Salaries and benefits") ,]

summary(ExpendituresDF) # summary statistics after keeping the relevant data 
summary(EXPENDITURE == "Total expenditures") # summary statistics for total exp 

#step 4: data with only total expenditures
ExpendituresDF_total_expenditures<- ExpendituresDF[which(ExpendituresDF$EXPENDITURE == "Total expenditures"),]
summary(ExpendituresDF_total_expenditures)

#step 5: subset data with only Salaries and benefits
ExpendituresDF_salary_expenditures<- ExpendituresDF[which(ExpendituresDF$EXPENDITURE == "Salaries and benefits"),]
summary(ExpendituresDF_salary_expenditures)

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
summaryfunctionFinal <- function(df, attribute){
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
      print (summaryReturned$expenditureAmount)
      print("standard deviation")
      print(sdReturned$expenditureAmount)
    }
   tempValue = i
  }
}

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




#calling the function to print each countries summary total Expend
summaryfunctionFinal(ExpendituresDF_total_expenditures, ExpendituresDF_total_expenditures$GEO)


#calling the function to print each countries summary salary Expend
summaryfunctionFinal(ExpendituresDF_salary_expenditures, ExpendituresDF_salary_expenditures$GEO)

#step 6: Write Salaries and benefits CSV
write.csv(ExpendituresDF_salary_expenditures, file = "universities and degree-granting colleges salary only Expenditure (2000-2016) corrected .csv")

#step 7: Write Total expenditures CSV
write.csv(ExpendituresDF_total_expenditures, file = "universities and degree-granting colleges total only Expenditure (2000-2016) corrected .csv")

#step 8: write relevant data csv
write.csv(ExpendituresDF, file = "universities and degree-granting colleges Expenditure (2000-2016) corrected .csv")

# call box plot exp
boxplotfunctionFinal(ExpendituresDF_salary_expenditures, ExpendituresDF_salary_expenditures$GEO)



