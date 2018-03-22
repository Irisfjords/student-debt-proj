
"**** Step1: Load Revenue Datasets **** "

#load cleaned over all revenue data
Revenue <- data.frame(read_csv("universities and degree-granting colleges Revenue (2000-2016) corrected .csv", col_types = cols(X1 = col_skip(), revenueAmount = col_number())))
#load cleaned Total Revenue data
TotalRevenue <- data.frame(read_csv("universities and degree-granting colleges total revenue (2000-2016) corrected .csv", 
                                    col_types = cols(FUND = col_skip(), REVENUE = col_skip(), X1 = col_skip(), revenueAmount = col_number())))
#load cleaned Tuition Revenue data
TuitionRevenue <- data.frame(read_csv("universities and degree-granting colleges tuition revenue (2000-2016) corrected .csv", 
                                      col_types = cols(FUND = col_skip(), REVENUE = col_skip(), X1 = col_skip(), revenueAmount = col_number())))


"**** Step2: Load Expenditure Datasets ****"
#load cleaned overall Expenditure data
Expenditure <- data.frame(read_csv("universities and degree-granting colleges Expenditure (2000-2016) corrected .csv", col_types = cols(X1 = col_skip(), expenditureAmount = col_number())))

#load cleaned Total Expenditure data
TotalExpenditure <- data.frame(read_csv("universities and degree-granting colleges total only Expenditure (2000-2016) corrected .csv", 
                                        col_types = cols(FUNDFUNCTION = col_skip(), EXPENDITURE = col_skip(), X1 = col_skip(), expenditureAmount = col_number())))
#load cleaned Salary Expenditure data
SalaryExpenditure <- data.frame(read_csv("universities and degree-granting colleges salary only Expenditure (2000-2016) corrected .csv", 
                                         col_types = cols(FUNDFUNCTION = col_skip(), EXPENDITURE = col_skip(), X1 = col_skip(), expenditureAmount = col_number())))

# view column names of the dataset
names(Revenue)
names(Expenditure)
names(TotalRevenue)
names(TuitionRevenue)
names(TotalExpenditure)
names(SalaryExpenditure)


"**** Step3: Modify ****"
# change from character to ordinal
Revenue$Ref_Date<-as.ordered(Revenue$Ref_Date) 
Expenditure$Ref_Date<-as.ordered(Expenditure$Ref_Date) 
TotalRevenue$Ref_Date<-as.ordered(TotalRevenue$Ref_Date) 
TotalExpenditure$Ref_Date<-as.ordered(TotalExpenditure$Ref_Date) 
TuitionRevenue$Ref_Date<-as.ordered(TuitionRevenue$Ref_Date) 
SalaryExpenditure$Ref_Date<-as.ordered(SalaryExpenditure$Ref_Date) 

# convert from character to factor 
Revenue[,c('GEO','SCHOOL','REVENUE','FUND')]<- lapply(Revenue[,c('GEO','SCHOOL','REVENUE','FUND')], as.factor)
Expenditure[,c('GEO','SCHOOL','EXPENDITURE','FUNDFUNCTION')]<- lapply(Expenditure[,c('GEO','SCHOOL','EXPENDITURE','FUNDFUNCTION')], as.factor)

TotalRevenue[,c('GEO','SCHOOL')]<- lapply(TotalRevenue[,c('GEO','SCHOOL')], as.factor)
TotalExpenditure[,c('GEO','SCHOOL')]<- lapply(TotalExpenditure[,c('GEO','SCHOOL')], as.factor)
TuitionRevenue[,c('GEO','SCHOOL')]<- lapply(TuitionRevenue[,c('GEO','SCHOOL')], as.factor)
SalaryExpenditure[,c('GEO','SCHOOL')]<- lapply(SalaryExpenditure[,c('GEO','SCHOOL')], as.factor)

#Rename 
colnames(TotalRevenue)[4] <- "TotalRevAmt"  # rename revenueAmount column name to represent total revenue
colnames(TotalExpenditure)[4] <- "TotalExpAmt"  # rename expenditureAmount column name  to represent total expenditure
colnames(TuitionRevenue)[4] <- "TuitionRevAmt"  # rename revenueAmount column name to represent tuition revenue
colnames(SalaryExpenditure)[4] <- "SalaryExpAmt"  # rename expenditureAmount column name  to represent salary expenditure

"**** Step4: Merge ****"
# Merge the two data.frames total revenue and expenditure
Total_Revenue_Expenditure <- merge.data.frame(x = TotalRevenue, y = TotalExpenditure,  all.x=TRUE)
# Merge the two data.frames tuition revenue and salary expenditure
Tuition_Revenue_Salary_Expenditure <- merge.data.frame(x = TuitionRevenue, y = SalaryExpenditure,  all.x=TRUE)
head(Revenue_Expenditure)
head(Tuition_Revenue_Salary_Expenditure)

#Merge all revenue and Expenditure datas 
Revenue_Expenditure_Final <- merge.data.frame(x = Total_Revenue_Expenditure, y = Tuition_Revenue_Salary_Expenditure ,  all.x=TRUE)
head(Revenue_Expenditure_Final)
