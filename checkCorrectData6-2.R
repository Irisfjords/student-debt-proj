library(readr)
# Load OSAP Data                                                                                            

University_OSAP14 <- data.frame(read_csv("Row data/OSAP repayment assist_usage 2014/University-Table 1.csv",  
                                          col_types = cols(`2014 Repayment Assistance Participation Rate (3)` = col_number(), 
                                                 X5 = col_skip())))
                        

University_program_OSAP14 <- data.frame(read_csv("Row data/OSAP repayment assist_usage 2014/University Program-Table 2014.csv", 
                                                 col_types = cols(`# of 11-12 Borrowers who received Repayment Assistance (2)` = col_number(), 
                                                                           `# of Borrowers 2011-12 (1)` = col_number(), 
                                                                           `2014 Repayment Assistance Participation Rate (3)` = col_number())))

#step1: understand the data

head(University_OSAP14) #Print the first few rows of a dataset
str(University_OSAP14) #structure of data
anyDuplicated(University_OSAP14) # check for duplicates 
names(University_OSAP14)
View(University_OSAP14) # view as table
length(University_OSAP14) # number of elements or components

head(University_program_OSAP14) #Print the first few rows of a dataset
str(University_program_OSAP14) #structure of data
anyDuplicated(University_program_OSAP14) # check for duplicates 
names(University_program_OSAP14)
View(University_program_OSAP14) # view as table
length(University_program_OSAP14) # number of elements or components



#step2: correcting 
attach(University_OSAP14)
detach(University_OSAP14)


colnames(University_OSAP14)[1] <- "institutionName"  # rename a column name
colnames(University_OSAP14)[2] <- "numOfBorrowers"  # rename a column name
colnames(University_OSAP14)[3] <- "receivedAssistance"  # rename a column name
colnames(University_OSAP14)[4] <- "AssistanceParticipationRate"  # rename a column name
University_OSAP14$institutionName<- as.factor(University_OSAP14$institutionName)
#University_OSAP14$AssistanceParticipationRate <- sapply(University_OSAP14$AssistanceParticipationRate, function(AssistanceParticipationRate) as.double(gsub("%", "", University_OSAP14$AssistanceParticipationRate)))

#
attach(University_program_OSAP14)
detach(University_program_OSAP14)


University_program_OSAP14$INSTITUTION.NAME<- as.factor(University_program_OSAP14$INSTITUTION.NAME)

colnames(University_program_OSAP14)[1] <- "institutionName"  # rename a column name
colnames(University_program_OSAP14)[2] <- "programName"  # rename a column name
colnames(University_program_OSAP14)[3] <- "numOfBorrowers"  # rename a column name
colnames(University_program_OSAP14)[4] <- "receivedAssistance"  # rename a column name
colnames(University_program_OSAP14)[5] <- "AssistanceParticipationRate"  # rename a column name
University_program_OSAP14$programName<- as.factor(University_program_OSAP14$programName)
University_program_OSAP14$AssistanceParticipationRate <- sapply(University_program_OSAP14$AssistanceParticipationRate, function(AssistanceParticipationRate) as.double(gsub("%", "", AssistanceParticipationRate)))

University_program_OSAP14 <- na.omit(University_program_OSAP14) 


#data description and summary after correction  
str(University_OSAP14) #structure of data after correction
summary(University_OSAP14)
summary(University_OSAP14) # summary universty RAP
summary(University_OSAP14$numOfBorrowers) # summary statistics for value
sd(University_OSAP14$numOfBorrowers)  # standard dev of the value
summary(University_OSAP14$receivedAssistance) # summary statistics for value
sd(University_OSAP14$receivedAssistance)  # standard dev of the value
summary(University_OSAP14$AssistanceParticipationRate) # summary statistics for value
sd(University_OSAP14$AssistanceParticipationRate)  # standard dev of the value

#data description and summary after correction  
str(University_program_OSAP14) #structure of data after correction
summary(University_program_OSAP14) # summary universty RAP
summary(University_program_OSAP14$numOfBorrowers) # summary statistics for value
sd(University_program_OSAP14$numOfBorrowers)  # standard dev of the value
summary(University_program_OSAP14$receivedAssistance) # summary statistics for value
sd(University_program_OSAP14$receivedAssistance)  # standard dev of the value
summary(University_program_OSAP14$AssistanceParticipationRate) # summary statistics for value
sd(University_program_OSAP14$AssistanceParticipationRate)  # standard dev of the value




# Write to  CSV
write.csv(University_OSAP14, file = "OSAP repayment assistance 2014 universty corrected .csv")

write.csv(University_program_OSAP14, file = "OSAP repayment assistance 2014 universty-program corrected .csv")



