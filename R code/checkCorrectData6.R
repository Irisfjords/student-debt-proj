# Load OSAP Data
                                                                                                       Value = col_number(), Vector = col_skip()))
University_OSAP <- data.frame(read_csv("Row data/OSAP Repayment Assistance Plan usage row data/University-Table 1.csv", col_types = cols(X5 = col_skip())))

University_program_OSAP <- data.frame(read_csv("Row data/OSAP Repayment Assistance Plan usage row data/University Program-Table 1.csv", 
                                    col_types = cols(`# of 12-13 Borrowers who received Repayment Assistance (2)` = col_number(), 
                                      `# of Borrowers 2012-13 (1)` = col_number(), 
                                        X10 = col_skip(), X6 = col_skip(), 
                                         X7 = col_skip(), X8 = col_skip(), 
                                         X9 = col_skip())))

#step1: understand the data
head(University_program_OSAP) #Print the first few rows of a dataset
str(University_program_OSAP) #structure of data
anyDuplicated(University_program_OSAP) # check for duplicates 
names(University_program_OSAP)
View(University_program_OSAP) # view as table
length(University_program_OSAP) # number of elements or components


head(University_OSAP) #Print the first few rows of a dataset
str(University_OSAP) #structure of data
anyDuplicated(University_OSAP) # check for duplicates 
names(University_OSAP)
View(University_OSAP) # view as table
length(University_OSAP) # number of elements or components

#step2: correcting 
attach(University_OSAP)
University_OSAP$institutionName<- as.factor(University_OSAP$institutionName)

colnames(University_OSAP)[1] <- "institutionName"  # rename a column name
colnames(University_OSAP)[2] <- "numOfBorrowers"  # rename a column name
colnames(University_OSAP)[3] <- "receivedAssistance"  # rename a column name
colnames(University_OSAP)[4] <- "AssistanceParticipationRate"  # rename a column name
University_OSAP$AssistanceParticipationRate <- sapply(AssistanceParticipationRate, function(AssistanceParticipationRate) as.double(gsub("%", "", AssistanceParticipationRate)))

#
attach(University_program_OSAP)


University_program_OSAP$INSTITUTION.NAME<- as.factor(University_program_OSAP$INSTITUTION.NAME)

colnames(University_program_OSAP)[1] <- "institutionName"  # rename a column name
colnames(University_program_OSAP)[2] <- "programName"  # rename a column name
colnames(University_program_OSAP)[3] <- "numOfBorrowers"  # rename a column name
colnames(University_program_OSAP)[4] <- "receivedAssistance"  # rename a column name
colnames(University_program_OSAP)[5] <- "AssistanceParticipationRate"  # rename a column name
University_program_OSAP$programName<- as.factor(University_program_OSAP$programName)
University_program_OSAP$AssistanceParticipationRate <- sapply(University_program_OSAP$AssistanceParticipationRate, function(AssistanceParticipationRate) as.double(gsub("%", "", AssistanceParticipationRate)))


#takeout total
University_OSAP <- University_OSAP[-23,]

#data description and summary after correction  
str(University_OSAP) #structure of data after correction
summary(University_OSAP) # summary universty RAP
summary(University_OSAP$numOfBorrowers) # summary statistics for value
sd(University_OSAP$numOfBorrowers)  # standard dev of the value
summary(University_OSAP$receivedAssistance) # summary statistics for value
sd(University_OSAP$receivedAssistance)  # standard dev of the value
summary(University_OSAP$AssistanceParticipationRate) # summary statistics for value
sd(University_OSAP$AssistanceParticipationRate)  # standard dev of the value

#data description and summary after correction  
str(University_program_OSAP) #structure of data after correction
summary(University_program_OSAP) # summary universty RAP
summary(University_program_OSAP$numOfBorrowers) # summary statistics for value
sd(University_program_OSAP$numOfBorrowers)  # standard dev of the value
summary(University_program_OSAP$receivedAssistance) # summary statistics for value
sd(University_program_OSAP$receivedAssistance)  # standard dev of the value
summary(University_program_OSAP$AssistanceParticipationRate) # summary statistics for value
sd(University_program_OSAP$AssistanceParticipationRate)  # standard dev of the value




# Write to  CSV
write.csv(University_OSAP, file = "OSAP repayment assistance 2015 universty corrected .csv")

write.csv(University_program_OSAP, file = "OSAP repayment assistance 2015 universty-program corrected .csv")



