library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)

getwd()

#### Case 1- Loans from Lending Club ####

####DATA IMPORT####
loans = read_csv("loans_full_schema.csv")

####DATA SUMMARY####
summary(loans) 
# dataset is 55 variables of both quantitative and qualitative data. 
#The data covers various attributes of loans, 

####VISUALIZATIONS####
#title = "Comparison of Average Interest Rates between Loan Sub-Grades",
# Visualize average of subgrades - appears rather consistent
loans %>% group_by(sub_grade) %>% summarize(avg_rate = mean(interest_rate)) %>%
  ggplot(aes(x = sub_grade, y = avg_rate)) + geom_col(fill = "blue", color = "black") +
  labs(x = "Loan Sub-Grade", y = "Interest Rate Average (%)") +
  theme_minimal()

# title = "Comparison of Standard Deviation within Interest Rates between Loan Sub-Grades", 
# significant deviation in grade D loans and subgrade E5 loans, requiring interest rate prediction
#however, it also indicates that to an extent, loan subgrade is correlated to interest rate
loans %>% group_by(sub_grade) %>% summarize(sd_rate = sd(interest_rate)) %>%
  ggplot(aes(x = sub_grade, y = sd_rate)) + geom_col(fill = "blue", color = "black") +
  labs(x = "Loan Sub-Grade", y = "Interest Rate Standard Deviation(%)") +
  theme_minimal()

# title = "Annual Income Distribution for Loan Sub-Grades w/o Initial Outliers", 
# income distribution shows that median income across all loan sub-grades are quite similar
# without outliers, there isn't an obvious significant difference between median annual income
# with outliers: marked decrease in significance of outliers the lower the subgrade goes
quantile(loans$annual_income)
loans %>% filter(annual_income < 1.5*(95000-45000)+95000) %>%
ggplot(aes(x = sub_grade, y = annual_income)) + geom_boxplot() +
  labs(x = "Loan Sub-Grade", y = "Annual Income ($)") +
  theme_minimal()

# title = "Interest Rate over Total Credit Limit w/o Inital Outliers", 
# Tendency for lower interest rate% the higher the debit limit according to fitted line
# could however also be a function of higher debit limit > more rich > more likely to not default
quantile(loans$total_credit_limit)
loans %>% filter(total_credit_limit < 1.5*(267550.00  - 51593.75  ) + 267550.00 ) %>%
ggplot(aes(x = total_credit_limit, y = interest_rate)) + geom_point(color = "grey") +
  geom_smooth(method = "lm")+
  labs(x = "Total Credit Limit ($)", y = "Interest Rate (%)") +
  theme_minimal()

# title = "Loan Status Distribution of each Loan Sub-Grade",
# Percent Stacked barchart - as subgrade gets worse, greater proportion of loans are in
  #grace period or are late
ggplot(aes(x = sub_grade, fill = loan_status), data = loans) + geom_bar(position = "fill") +
  labs(x = "Loan Sub-Grade", y= "Proportion") +
  scale_fill_discrete(name = "Loan Status") + theme_minimal()

# Distribution of Interest Rate over Loan Purpose
#understand the distribution of interest rate for loans with different purposes
loans %>% ggplot(aes(x = loan_purpose, y = interest_rate, fill = loan_purpose)) +
  geom_violin(trim=TRUE) + 
  labs(y = "Interest Rate") + 
  scale_fill_discrete(name = "Loan Purpose")+
  theme_minimal() + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

####DATA CLEANING ####
#NA COUNT IN DATA
colSums(is.na(loans))

#variables with NAs
#emp_title, emp_length, debt_to_income, annual_income_joint, verirfication_income_joint, debt_to_income_joint,
#months_since_last_delinq , months_since_90d_late, months_since_last_credit_inquiry, num_accounts_120d_past_due,

#most significant ones to deal with- annual_income_joint, verification_income_joint, debt_to_income_joint,
#months_since_last_delinq, months_since_90d_late, months_since_last_credit_inquiry

#approach-
#variables with significant amount of NAs should be checked for removal- 
#too much missing data that cannot be estimated
#variables that are not particularly useful as it for data analysis- inconsistent character-based data
#attempt to estimate NA values if they do not compose a significant proportion of the dataset

loans_clean = loans %>% 
  select(-c(emp_title, state, annual_income_joint, verification_income_joint,
            debt_to_income_joint,months_since_last_delinq , months_since_90d_late, 
            months_since_last_credit_inquiry)) %>% 
  mutate(emp_length = if_else(is.na(emp_length),median(emp_length, na.rm=TRUE), emp_length),
         debt_to_income = if_else(is.na(debt_to_income),median(debt_to_income, na.rm=TRUE),debt_to_income),
         num_accounts_120d_past_due = if_else(is.na(num_accounts_120d_past_due),
                                              median(num_accounts_120d_past_due, na.rm=TRUE),
                                              num_accounts_120d_past_due))

loans_clean <- as.data.frame(unclass(loans_clean),stringsAsFactors = TRUE)
colSums(is.na(loans_clean))
str(loans_clean)

####MODELING####
#linear regression
lm1 = lm(interest_rate ~., data = loans_clean)
summary(lm1)

lm2 = lm(interest_rate ~total_credit_lines + num_total_cc_accounts + term + installment + 
           grade + sub_grade + issue_month + initial_listing_status, data = loans_clean)
summary(lm2)

lm3 = lm(interest_rate ~ grade + sub_grade + issue_month + initial_listing_status, data = loans_clean)
summary(lm3)

plot(lm3$residuals)

#robust regression 
# why robust? b/c outliers within the data require us to utilize a methodlogy resistant to the effects of outliers.
library(MASS)
rlm1 <- rlm(interest_rate ~ sub_grade+issue_month + initial_listing_status, data=loans_clean, maxit = 500)
summary(rlm1)
plot(rlm1$residuals)



#### Case 2- Orders ####

####DATA IMPORT####
orders = read_csv("casestudy.csv")

str(orders)
colSums(is.na(orders))

orders2015 = orders %>% filter(year == 2015)
orders2016 = orders %>% filter(year == 2016)
orders2017 = orders %>% filter(year ==2017)

# Total revenue for the current year
orders_answer = orders %>% group_by(year) %>% summarise(total_rev = sum(net_revenue))

#	New Customer Revenue e.g., new customers not present in previous year only
a = as.numeric(orders2016 %>% anti_join(orders2015, by = "customer_email") %>% 
                 summarise(total_rev = sum(net_revenue)))
b = as.numeric(orders2017 %>% anti_join(orders2016, by = "customer_email") %>% 
                 summarise(total_rev = sum(net_revenue)))
orders_answer$new_cust_rev = c(orders_answer$total_rev[1], a, b)

#	Existing Customer Revenue Current Year
orders_answer$exist_cust_rev = orders_answer$total_rev - orders_answer$new_cust_rev

#	Existing Customer Revenue Prior Year
orders_answer$exist_cust_rev_pr = c(0,orders_answer$exist_cust_rev[1],orders_answer$exist_cust_rev[2])

#	Existing Customer Growth. To calculate this, use the Revenue of existing customers for current year -(minus) 
  #Revenue of existing customers from the previous year
orders_answer$exist_cust_grow = c(0,orders_answer$exist_cust_rev[2]-orders_answer$exist_cust_rev[1],
                                  orders_answer$exist_cust_rev[3]-orders_answer$exist_cust_rev[2])

#	Revenue lost from attrition
a = as.numeric(orders2015 %>% anti_join(orders2016, by = "customer_email") %>% 
                 summarise(total_rev = sum(net_revenue)))
b = as.numeric(orders2016 %>% anti_join(orders2017, by = "customer_email") %>% 
                 summarise(total_rev = sum(net_revenue)))
orders_answer$rev_attrit = c(0,a,b)

#	Total Customers Current Year
df1 = orders %>% group_by(year) %>% summarise(total_cust = n())
orders_answer = orders_answer %>% add_column(as.data.frame(df1[,2]))

#	Total Customers Previous Year
orders_answer$total_cust_pr = c(0,orders_answer$total_cust[1],orders_answer$total_cust[2])

#	New Customers
a = as.numeric(orders2016 %>% anti_join(orders2015, by = "customer_email") %>% 
                 summarise(new_cust = n()))
b = as.numeric(orders2017 %>% anti_join(orders2016, by = "customer_email") %>% 
                 summarise(new_cust = n()))
orders_answer$new_cust = c(orders_answer$total_cust[1],a,b)

#	Lost Customers
a = as.numeric(orders2015 %>% anti_join(orders2016, by = "customer_email") %>% 
                 summarise(lost_cust = n()))
b = as.numeric(orders2016 %>% anti_join(orders2017, by = "customer_email") %>% 
                 summarise(lost_cust = n()))
orders_answer$lost_cust = c(0,a,b)

####VISUALIZATIONS####
#line plot - Change of Total Revenue over Time
ggplot(aes(x = year, y = total_rev), data = orders_answer)+
  geom_line(color="blue", size = 1) +
  scale_x_continuous(breaks=c(2015,2016,2017))+
  labs(x = "Year", y = "Total Revenue")+
  theme_minimal()

#line plot - Revenue gain and loss over time
orders_answer %>% ggplot(aes(x = year))+
  geom_line(aes(y = new_cust_rev, color = "New Customer"), size = 1) +
  geom_line(aes(y = exist_cust_rev, color = "Existing Customer"), size = 1) +
  geom_line(aes(y = rev_attrit, color = "Attritioned (Lost)"), size = 1) +
  scale_x_continuous(breaks=c(2015,2016,2017))+
  labs(x = "Year", y = "Revenue", color = "Source")+
  theme_minimal()

#not particularly useful or descriptive on their own- see line char below
#bar plot - Change of Total customers over Time
orders_answer %>% ggplot(aes(x = factor(year)))+
  geom_col(aes(y = total_cust), fill = "blue") +
  labs(x = "Year", y = "Customers")+
  theme_minimal()
#bar plot - Change in New Customers over Time
orders_answer %>% ggplot(aes(x = factor(year)))+
  geom_col(aes(y = new_cust), fill = "green") +
  labs(x = "Year", y = "Customers")+
  theme_minimal()
#bar plot - Change in Lost Customers over Time
orders_answer %>% ggplot(aes(x = factor(year)))+
  geom_col(aes(y = lost_cust), fill = "red") +
  labs(x = "Year", y = "Customers")+
  theme_minimal()

#line plot - Customer Type over Time
orders_answer %>% ggplot(aes(x = year))+
  geom_line(aes(y = total_cust, color = "Total"), size = 1) +
  geom_line(aes(y = new_cust, color = "New"), size = 1) +
  geom_line(aes(y = lost_cust, color = "Lost"), size = 1) +
  scale_x_continuous(breaks=c(2015,2016,2017))+
  labs(x = "Year", y = "Customers", color = "Type")+
  theme_minimal()


