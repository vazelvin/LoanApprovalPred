#Big Data 2 - Project 1
#Loan Amount Analysis and Prediction based on Bank Dataset



#Installing the required libraries
install.packages("sqldf")
library("sqldf")
install.packages("tidyverse")
library("tidyverse")
install.packages("tidyr")
library("tidyr")
install.packages("readxl")
library("readxl")
install.packages("corrplot")
library("corrplot")
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
install.packages("plotly")                                        
# Install plotly package
library("plotly")       
install.packages("pheatmap")        
library("pheatmap")
install.packages("caTools")    # For Linear regression 
library(caTools)


#Loading the Data File
Loan1 <- read_excel("C:/Users/sanja/Downloads/Project_LoanData.xlsx")


#Removing rows with null values to determine correlation
Loan2 <- na.omit(Loan1)
Loanplot <- na.omit(Loan1)

#Making Strings to Numeric in different dataframe
Loan2$Gender[Loan2$Gender == "Male"] <- 1
Loan2$Gender[Loan2$Gender == "Female"] <- 0
Loan2$Gender <- as.numeric(Loan2$Gender)
summary(Loan2$Gender)

Loan2$Married[Loan2$Married == "Yes"] <- 1
Loan2$Married[Loan2$Married == "No"] <- 0
Loan2$Married <- as.numeric(Loan2$Married)
summary(Loan2$Married)

Loan2$Education[Loan2$Education == "Graduate"] <- 1
Loan2$Education[Loan2$Education == "Not Graduate"] <- 0
Loan2$Education <- as.numeric(Loan2$Education)
summary(Loan2$Education)

Loan2$Self_Employed[Loan2$Self_Employed == "Yes"] <- 1
Loan2$Self_Employed[Loan2$Self_Employed == "No"] <- 0
Loan2$Self_Employed <- as.numeric(Loan2$Self_Employed)
summary(Loan2$Self_Employed)

Loan2$Loan_Status[Loan2$Loan_Status == "Y"] <- 1
Loan2$Loan_Status[Loan2$Loan_Status == "N"] <- 0
Loan2$Loan_Status <- as.numeric(Loan2$Loan_Status)
summary(Loan2$Property_Area)

Loan2$Property_Area[Loan2$Property_Area == "Urban"] <- 1
Loan2$Property_Area[Loan2$Property_Area == "Semiurban"] <- 0
Loan2$Property_Area[Loan2$Property_Area == "Rural"] <- -1
Loan2$Property_Area <- as.numeric(Loan2$Property_Area)
summary(Loan2)

#Removing index values to plot correlation matrix
Loan3 <- subset(Loan2, select = -Loan_ID)


#creating correlation matrix from dataset
Loancor <- cor(Loan3)
Loancor
round(Loancor, digits = 1) #rounding correlation value to 1 decimal place

#generating heatmap from correlation matrix
plot_ly(z = Loancor,x = c("Gender", "Married", "Dependents", "Education", "Self_Employed", "ApplicantIncome", "CoapplicantIncome", "LoanAmount", "LoanTerm_Month", "Credit_History", "Property_Area", "Loan_Status"), y = c("Gender", "Married", "Dependents", "Education", "Self_Employed", "ApplicantIncome", "CoapplicantIncome", "LoanAmount", "LoanTerm_Month", "Credit_History", "Property_Area", "Loan_Status"), type = "heatmap",showlegend=TRUE) 



#learning and modeling

summary(Loan3)

#selecting only highly correlated data
Loan5 <- select(Loan3, "LoanAmount", "ApplicantIncome", "CoapplicantIncome", "LoanTerm_Month")
summary(Loan5)
plot(Loan5)

#dividing data into training and testing dataset
smp_size <- floor(0.80 * nrow(Loan5))
train_ind <- sample(seq_len(nrow(Loan5)), size = smp_size)
train <- Loan5[train_ind, ]
test <- Loan5[-train_ind, ]

#generating multi linear regression model
model_all <- lm(LoanAmount ~ ApplicantIncome + CoapplicantIncome + LoanTerm_Month, data=train)

summary(model_all) # summary gives the summary result of training model  

#predicting on test data using generated regression model
pred <- predict(model_all,test)
pred # predicted values expected

#type + o plots a line graph, we plotted the Forcasted Loan Amount that was calculated using the intercepts and coefficients
plot(pred, type = "o", main = "Forcasted Loan Amount in USD")

#data$variablename, we plotted the Actual Loan Amount 
plot(test$ApplicantIncome, type = "o", main = "Actual Loan Amount in USD")


#calculating error
mape_val <- mean(abs((test$LoanAmount-pred)/test$LoanAmount)) * 100
mape_val



#Insights graph Loan Amount By Maritial status
Loanplot%>%
  ggplot(aes(ApplicantIncome/100, LoanAmount,      
             colour = Married))+
  geom_point(size = 3, alpha= 0.5 )+  
  geom_smooth(method = lm, se=F)+
  facet_wrap(~Property_Area)+
  labs(title = "Loan Amount By Marriage Status")+
  theme_bw()


#Insights graph LoanStatus By Property area
Loanplot%>%
  ggplot(aes(Loan_Status, ApplicantIncome/100,      
             colour = Property_Area))+
  geom_point(size = 4, alpha= 0.5 )+  
  geom_smooth(method = lm, se=F)+
  facet_wrap(~LoanTerm_Month)+
  labs(title = "Loan Term By Location")+
  theme_light()




#Horizontal boxplot using coord_flip() and Income divided by 100
Loanplot%>%
  ggplot(aes(Credit_History, ApplicantIncome/100))+
  geom_boxplot()+  
  geom_point(alpha= 0.5,
             aes(colour = LoanAmount))+
  facet_wrap(~Property_Area)+
  coord_flip()+
  theme_bw()+
  labs(title = "Credit History vs No Credit History")




cor(Loan3$LoanAmount, Loan3$ApplicantIncome)


#graphs

#Bargraph for Property area vs Income
ggplot() +   aes(x = Loanplot$Property_Area, fill = Loanplot$ApplicantIncome/100, weight = Loanplot$ApplicantIncome/100) +   geom_bar()  +   labs(x = "Property Area", y = "SALARY", title = "AVERAGE INCOME OF PEOPLE")

#Bargraph for Loan approval status by Property area
ggplot() + aes(x = Loanplot$Property_Area, y=Loanplot$Loan_Status, fill=Loanplot$Loan_Status) + geom_col() +  labs(x = "Property Area", y = "Frequency", title = "Property Area vs Loan Status", fill = "Loan Status")

#Histogram for Applicant Income
ggplot() + aes(x = Loanplot$ApplicantIncome/100) + geom_histogram(fill="Blue") + labs(x = "Applicant Income", title = "Histogram for Applicant Income")

#Histogram for Coapplicant Income
ggplot() + aes(x = Loanplot$CoapplicantIncome) + geom_histogram(fill="Green")+ labs(x = "Coapplicant Income", title = "Histogram for Coapplicant Income")

#Histogram for Loan amount
ggplot() + aes(x = Loanplot$LoanAmount) + geom_histogram(fill="Red")+ labs(x = "Loan Amount", title = "Histogram for Loan Amount")

#Histogram for Marital Status by Gender
ggplot() +   aes(x = Loanplot$Married, fill = Loanplot$Gender) +   geom_bar()  +   labs(x = "Mrital Status", y = "Count", title = "Marital status by Gender", fill = "Gender")

#Bargraph for Salary by Education and Employment Status
ggplot() +   aes(x = Loanplot$Education, fill = Loanplot$Self_Employed, weight = Loanplot$ApplicantIncome/100) +   geom_bar()  +   labs(x = "Gender", y = "SALARY", title = "AVERAGE INCOME OF PEOPLE BY GENDER AND EMPLOYMENT STATUS", fill = "Self-Employed")


             
#Bargraph based on Loan Approval by Credit history
Loanplot2 = Loanplot
Loanplot2$Credit_History <- as.character(Loanplot$Credit_History)
Loanplot2$Credit_History[Loan2$Credit_History == "1"] <- "Yes"
Loanplot2$Credit_History[Loan2$Credit_History == "0"] <- "No"

ggplot() +   aes(x = Loanplot$Credit_History, fill = Loanplot2$Loan_Status) +   geom_bar()  +   labs(x = "Credit History", y = "Count", title = "Effect of Credit History on approval rate", fill = "LoanStatus")




#Bar Chart for Education categorized by Loan_Status
Loanplot %>% 
  #drop_na(Education)%>%
  ggplot(aes(Education,fill = Loan_Status))+
  geom_bar()+
  facet_wrap(~Loan_Status)+
  labs(x="Education",
       y="Count of Values",
       title="Bar Chart for Education categorized by Loan_Status")+
  geom_text(aes(label = ..count..), stat = 'count', hjust = 1)+
  theme_bw()

#Histogram for LoanAmount categorized by Gender
Loanplot %>% 
  #drop_na(Gender)%>%
  ggplot(aes(LoanAmount))+
  geom_histogram(fill="#6C61A1")+
  facet_wrap(~Gender)+
  labs(x="LoanAmount",
       y="Count of Values",
       title="Histogram for LoanAmount categorized by Gender")+
  theme_bw() 


#Histogram for LoanAmount categorised by Loan_Status
Loanplot %>% 
  #drop_na(Loan_Status)%>%
  ggplot(aes(LoanAmount))+
  geom_histogram(fill="#6C61A1")+
  facet_wrap(~Loan_Status)+
  labs(x="LoanAmount",
       y="Count of Values",
       title="Histogram for LoanAmount categorized by Loan_Status")+
  theme_bw()


#Histogram for LoanAmount categorized by Credit_History
Loanplot %>% 
  #drop_na(Credit_History)%>%
  ggplot(aes(LoanAmount))+
  geom_histogram(fill="#6C61A1")+
  facet_wrap(~Credit_History)+
  labs(x="LoanAmount",
       y="Count of Values",
       title="Histogram for LoanAmount categorized by Credit_History")+
  theme_bw()


