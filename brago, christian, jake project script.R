library(rpart)
library(Metrics)
library(caret)
library(lubridate)
library(corrplot)
library(tidyverse)

# reading in data ----
data <- read.csv('teaching_training_data.csv')
cft <- read.csv('teaching_training_data_cft.csv')
com <- read.csv('teaching_training_data_com.csv')
grit <- read.csv('teaching_training_data_grit.csv')
num <- read.csv('teaching_training_data_num.csv')
opt <- read.csv('teaching_training_data_opt.csv')

# data cleaning for question 1 ----
cft <- cft %>% 
  dplyr::select(unid, cft_score) %>% 
  distinct(unid, .keep_all = TRUE)
com <- com %>% 
  dplyr::select(unid, com_score) %>% 
  distinct(unid, .keep_all = TRUE)
grit <- grit %>% 
  dplyr::select(unid, grit_score) %>% 
  distinct(unid, .keep_all = TRUE)
num <- num %>% 
  dplyr::select(unid, num_score) %>% 
  distinct(unid, .keep_all = TRUE)
opt <- opt %>% 
  dplyr::select(unid, opt_score) %>% 
  distinct(unid, .keep_all = TRUE)

data <- data %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now) %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>% 
  mutate(age = floor(age_at_survey))

# merge scores data
df_assess <- full_join(cft, com, by ="unid")
df_assess <- df_assess %>% 
  full_join(grit, by ="unid") %>% 
  full_join(num, by ="unid") %>% 
  full_join(opt, by ="unid")
data <- full_join(data, df_assess, by ="unid")

survey1 <- filter(data, survey_num == 1) %>% 
  dplyr::select(-c(survey_date_month, job_start_date, job_leave_date, company_size, monthly_pay, financial_situation_now, financial_situation_5years))

# response variable is 'working'
summary(survey1$working) #F: 40179, T: 11102
prop.table(table(survey1$working)) #F: 78%, T: 22%

# visualizations for question 1----
ggplot(survey1, aes(x= gender, fill= working)) + geom_bar(position= 'fill') +
  labs(x= 'Gender', y= 'Proportion', fill= 'Working')
# significant because distributions are not the same

ggplot(survey1, aes(x= province, fill= working)) + geom_bar(position= 'fill') + coord_flip() +
  labs(x= 'Province', y= 'Proportion', fill= 'Working')
# maybe significant but there are too many NA

ggplot(survey1, aes(x= volunteer, fill= working)) + geom_bar(position= 'fill') +
  labs(x= 'Volunteer', y= 'Proportion', fill= 'Working')
# probably not because they are similar

ggplot(survey1, aes(x= leadershiprole, fill= working)) + geom_bar(position= 'fill') +
  labs(x= 'Leadership Role', y= 'Proportion', fill= 'Working')
# probably not because they are similar

ggplot(survey1, aes(x= anygrant, fill= working)) + geom_bar(position= 'fill') +
  labs(x= 'Receive Grant', y= 'Proportion', fill= 'Working')
# probably not because they are similar

ggplot(survey1, aes(x= anyhhincome, fill= working)) + geom_bar(position= 'fill') +
  labs(x= 'Other Household Income', y= 'Proportion', fill= 'Working')
# maybe significant

ggplot(survey1, aes(x= givemoney_yes, fill= working)) + geom_bar(position= 'fill') +
  labs(x= 'Give Money to Others', y= 'Proportion', fill= 'Working')
# maybe significant

ggplot(survey1, aes(x= age, fill= working)) + geom_bar() + facet_grid(working~.) + xlim(15, 35) +
  geom_vline(xintercept= mean(filter(survey1, working== TRUE)$age, na.rm=TRUE), color= 'black') +
  geom_vline(xintercept= mean(filter(survey1, working== FALSE)$age, na.rm=TRUE), color= 'blue') + 
  labs(x= 'Age', y= 'Count', fill= 'Working')
# not because distribution look to be the same 

scores_data <- dplyr::select(survey1, cft_score:opt_score)
corrplot(cor(scores_data, use= "pairwise.complete.obs"), method = "square")
# cft, com, num are pairwise highly correlated so probably only need one

working_cft <- survey1 %>% filter(working == TRUE)
working_cft_mean <- mean(working_cft$cft_score, na.rm= TRUE)
not_working_cft <- survey1 %>% filter(working == FALSE)
not_working_cft_mean <- mean(not_working_cft$cft_score, na.rm= TRUE)
ggplot(survey1, aes(x= cft_score, fill= working)) + geom_bar() + facet_grid(working~.) +
  geom_vline(xintercept= working_cft_mean, color= 'green') +
  geom_vline(xintercept= not_working_cft_mean, color= 'blue') + 
  labs(x= 'CFT Score', y= 'Count', fill= 'Working')
# mean cft score is slightly higher for working -> probably significant

working_opt <- survey1 %>% filter(working == TRUE)
working_opt_mean <- mean(working_opt$opt_score, na.rm= TRUE)
not_working_opt <- survey1 %>% filter(working == FALSE)
not_working_opt_mean <- mean(not_working_opt$opt_score, na.rm= TRUE)
ggplot(survey1, aes(x= opt_score, fill= working)) + geom_bar() + facet_grid(working~.) +
  geom_vline(xintercept= working_opt_mean, color= 'green') +
  geom_vline(xintercept= not_working_opt_mean, color= 'blue') + 
  labs(x= 'OPT Score', y= 'Count', fill= 'Working')
# not significant because they are essentially the same

working_grit <- survey1 %>% filter(working == TRUE)
working_grit_mean <- mean(working_grit$grit_score, na.rm= TRUE)
not_working_grit <- survey1 %>% filter(working == FALSE)
not_working_grit_mean <- mean(not_working_grit$grit_score, na.rm= TRUE)
ggplot(survey1, aes(x= grit_score, fill= working)) + geom_bar() + facet_grid(working~.) +
  geom_vline(xintercept= working_grit_mean, color= 'green') +
  geom_vline(xintercept= not_working_grit_mean, color= 'blue') +
  labs(x= 'GRIT Score', y= 'Count', fill= 'Working')
# not significant

ggplot(survey1, aes(x= as.factor(fin_situ_now), fill= working)) + geom_bar(position= 'fill') +
  labs(x= 'Current Financial Situation', y= 'Proportion', fill= 'Working')
# probably significant

# modeling for question 1 ----
# checking significance of explanatory variables
gender_table <- table(survey1$gender, survey1$working)
chisq.test(gender_table) 
# significant

hhincome_table <- table(survey1$anyhhincome, survey1$working)
chisq.test(hhincome_table) 
# significant

givemoney_table <- table(survey1$givemoney_yes, survey1$working)
chisq.test(givemoney_table) 
# significant

fin_now_table <- table(survey1$fin_situ_now, survey1$working)
chisq.test(fin_now_table) 
# significant

# splitting test and train
train_index = sample(c(T, F), nrow(survey1), prob = c(0.8, 0.2), replace = TRUE)
survey1_train <- survey1[train_index,]
survey1_test <- survey1[!train_index,]

# setting train control
my_control <- trainControl(method = "cv", number = 5, savePredictions = "final", allowParallel = TRUE, verboseIter= TRUE)

# training first model
model <- train(as.factor(working) ~ gender + anyhhincome + givemoney_yes + fin_situ_now + cft_score, 
               data= survey1_train, method= 'xgbTree', trControl= my_control, na.action= na.pass)

# model accuracy
(model_accuracy <- mean(model$results$Accuracy))

# predictions + confusion matrix
predictions <- predict(model, survey1_test)

confusions_matrix <- table(predictions, survey1_test$working) 
# not working because the lengths are unequal. predictions has 4564, working has 10221

# THINGS TO DO FOR QUESTION 1 ----
' 1. confusion matrix, accuracy, specificity, sensitivity'

# data cleaning for question 2 ----
# reordering monthly_pay 
levels(data$monthly_pay) <- c("Zero", "R500 or less", "Between R501 and R1000", "Between R1001 and R2000", 
                              "Between R2001 and 3000", "Between R3001 and R3500", "Between R3501 and R4000", 
                              "Between R4001 and R6000", "Between R6001 and R8000", "Above R8000", "I prefer not to answer")

# removing repeated levels in numchildren
levels(data$numchildren)[levels(data$numchildren) == "4more"] <- "4 or more"

# adding work length and over_6mons variables
data <- data %>% mutate(job_start_date = as.Date(job_start_date),
                        job_leave_date = as.Date(job_leave_date),
                        survey_date_month = as.Date(survey_date_month))

for(i in 1:nrow(data)){
  if(data[i, 'working'] == TRUE & is.na(data[i, 'job_leave_date']))
    data[i, 'job_leave_date'] <- data[i, 'survey_date_month']
}

data$work_length <- abs(interval(data$job_start_date, data$job_leave_date)/months(1))
data$over_6mons <- data$work_length >= 6

# subsetting only those who are working at the time of the survey
working <- filter(data, working == TRUE)

# response variable is 'over_6mons'
summary(working$over_6mons) # F: 8390, T: 7518, NA: 3952
prop.table(table(working$over_6mons)) # F: 53%, T: 47% (na's not included)

# visualizations for question 2 ----
ggplot(working, aes(x= work_length)) + geom_histogram(binwidth= 5, fill= 'white', color= 'black')
ggplot(working, aes(y= work_length)) + geom_boxplot()
# work length is heavily skewed

ggplot(working, aes(x= over_6mons, fill= over_6mons)) + geom_bar()

ggplot(working, aes(x= company_size, fill= over_6mons)) + geom_bar(position= 'fill')
# not significant

ggplot(working, aes(x= monthly_pay, fill= over_6mons))  + geom_bar(position= 'fill') + coord_flip()
# significant

ggplot(working, aes(y= age, color= over_6mons)) + geom_boxplot()
# not significant

ggplot(working, aes(x= leadershiprole, fill= over_6mons)) + geom_bar(position= 'fill')
# maybe significant

ggplot(working, aes(x= province, fill= over_6mons)) + geom_bar(position= 'fill') + coord_flip()
# significant

ggplot(working, aes(x= gender, fill= over_6mons)) + geom_bar(position= 'fill')
# significant

ggplot(working, aes(x= as.factor(fin_situ_now), fill= over_6mons)) + geom_bar(position= 'fill')
# maybe significant

ggplot(working, aes(x= givemoney_yes, fill= over_6mons)) + geom_bar(position= 'fill')
# maybe significant

ggplot(working, aes(x= numchildren, fill= over_6mons)) + geom_bar(position= 'fill')
# significant

ggplot(working, aes(x= anyhhincome, fill= over_6mons)) + geom_bar(position= 'fill')
# significant

ggplot(working, aes(x= anygrant, fill= over_6mons)) + geom_bar(position= 'fill')
# significant

# modeling for question 2 ----
# checking significance of explanatory variables
pay_table <- table(working$monthly_pay, working$over_6mons)
chisq.test(pay_table)
# significant

leadership_table <- table(working$leadershiprole, working$over_6mons)
chisq.test(leadership_table)
# not significant

province_table <- table(working$province, working$over_6mons)
chisq.test(province_table)
# significant

gender_table <- table(working$gender, working$over_6mons)
chisq.test(gender_table)
# not significant

fin_table <- table(working$fin_situ_now, working$over_6mons)
chisq.test(fin_table)
# not significant

money_table <- table(working$givemoney_yes, working$over_6mons)
chisq.test(money_table)
# significant

children_table <- table(working$numchildren, working$over_6mons)
chisq.test(children_table)
# not significant

hhincome_table <- table(working$anyhhincome, working$over_6mons)
chisq.test(hhincome_table) 
# significant

grant_table <- table(working$anygrant, working$over_6mons)
chisq.test(grant_table)
# significant

# splitting test and train
train_index = sample(c(T, F), nrow(working), prob = c(0.8, 0.2), replace = TRUE)
working_train <- working[train_index,]
working_test <- working[!train_index,]

# training first model
model <- train(as.factor(over_6mons) ~ monthly_pay + anyhhincome + anygrant, data= working_train, 
               method= 'glm', trControl= my_control, na.action= na.pass)

model <- glm(over_6mons ~ monthly_pay + anyhhincome + anygrant, data= working_train, family= 'binomial')


# THINGS TO DO FOR QUESTION 2 ----
' 1. create a model, test it, confusion matrix'