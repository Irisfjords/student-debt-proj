# Load Data
getwd()
undergraduate_students_by_field_of_study <- data.frame(read_csv("Row data/Weighted average tuition fees for full-time Canadian undergraduate students, by field of study.csv", col_types = cols(Coordinate = col_skip(), Value = col_number(), Vector = col_skip())))

#step1: understand the data
head(undergraduate_students_by_field_of_study) #Print the first few rows of a dataset
str(undergraduate_students_by_field_of_study) #structure of data
anyDuplicated(undergraduate_students_by_field_of_study) # check for duplicates 
names(undergraduate_students_by_field_of_study)
View(undergraduate_students_by_field_of_study) # view as table
length(undergraduate_students_by_field_of_study) # number of elements or components

#step2: correcting 
colnames(undergraduate_students_by_field_of_study)[4] <- "averageTuitionFee"  # rename a column name
colnames(undergraduate_students_by_field_of_study)[3] <- "STUDYFIELD"  # rename a column name
# change to correct data type 
attach(undergrad_and_postgrad_2007_2018)
undergraduate_students_by_field_of_study$Ref_Date<-as.ordered(undergraduate_students_by_field_of_study$Ref_Date) # change from character to ordinal
# convert from character to factor 
undergraduate_students_by_field_of_study[,c('GEO','STUDYFIELD')]<- lapply(undergraduate_students_by_field_of_study[,c('GEO','STUDYFIELD')], as.factor)


# step 3:data description and summary after correction  
str(undergraduate_students_by_field_of_study) #structure of data after correction
summary(undergraduate_students_by_field_of_study)  # summary statistics of the dataset
summary(undergraduate_students_by_field_of_study$averageTuitionFee) # summary statistics for the numeric value
sd(undergraduate_students_by_field_of_study$averageTuitionFee)  # standard dev of the the numeric value

# step 4:filling the missing values 
meanFee <- mean(undergraduate_students_by_field_of_study$averageTuitionFee,na.rm=TRUE)
missingFee <-is.na(undergraduate_students_by_field_of_study$averageTuitionFee)
undergraduate_students_by_field_of_study$averageTuitionFee[missingFee]=meanFee
summary(undergraduate_students_by_field_of_study)  # summary statistics of the dataset after filling NA's
summary(undergraduate_students_by_field_of_study$averageTuitionFee) # summary statistics for the numeric value
sd(undergraduate_students_by_field_of_study$averageTuitionFee)  # standard dev of the the numeric value

# 
#step 5: Write to  CSV
write.csv(undergraduate_students_by_field_of_study, file = "Tution fee undergraduate students by field of study (2006-2018) corrected .csv")

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

