# Load Data
getwd()
graduate_students_by_field_of_study <- data.frame(read_csv("Row data/Weighted average tuition fees for full-time Canadian graduate students, by field of study.csv", col_types = cols(Coordinate = col_skip(), Value = col_number(), Vector = col_skip())))

#step1: understand the data
head(graduate_students_by_field_of_study) #Print the first few rows of a dataset
str(graduate_students_by_field_of_study) #structure of data
anyDuplicated(graduate_students_by_field_of_study) # check for duplicates 
names(graduate_students_by_field_of_study)
View(graduate_students_by_field_of_study) # view as table
length(graduate_students_by_field_of_study) # number of elements or components

#step2: correcting 
colnames(graduate_students_by_field_of_study)[4] <- "averageTuitionFee"  # rename a column name
colnames(graduate_students_by_field_of_study)[3] <- "STUDYFIELD"  # rename a column name
# change to correct data type 
attach(graduate_students_by_field_of_study)
graduate_students_by_field_of_study$Ref_Date<-as.ordered(graduate_students_by_field_of_study$Ref_Date) # change from character to ordinal
# convert from character to factor 
graduate_students_by_field_of_study[,c('GEO','STUDYFIELD')]<- lapply(graduate_students_by_field_of_study[,c('GEO','STUDYFIELD')], as.factor)


# #step 3:data description and summary after correction  
str(graduate_students_by_field_of_study) #structure of data after correction
summary(graduate_students_by_field_of_study)  # summary statistics of the dataset
summary(graduate_students_by_field_of_study$averageTuitionFee) # summary statistics for the numeric value
sd(graduate_students_by_field_of_study$averageTuitionFee)  # standard dev of the the numeric value

# #step 4:remove missing values 
graduate_students_by_field_of_study<- graduate_students_by_field_of_study[complete.cases(graduate_students_by_field_of_study), ]
str(graduate_students_by_field_of_study)
summary(graduate_students_by_field_of_study)  # summary statistics of the dataset after removing NA's
summary(graduate_students_by_field_of_study$averageTuitionFee) # summary statistics for the numeric value
sd(graduate_students_by_field_of_study$averageTuitionFee)  # standard dev of the the numeric value


#step 5: Write to  CSV
write.csv(graduate_students_by_field_of_study, file = "Tution fee graduate students by field of study (2006-2018) corrected .csv")


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

