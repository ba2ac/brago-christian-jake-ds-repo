library(tidyverse)
library(rpart)
library(Metrics)
library(caret)
library(lubridate)
library(corrplot)

# reading in data ----
data <- read.csv('data/raw/teaching_training_data.csv')
cft <- read.csv('data/raw/teaching_training_data_cft.csv')
com <- read.csv('data/raw/teaching_training_data_com.csv')
grit <- read.csv('data/raw/teaching_training_data_grit.csv')
num <- read.csv('data/raw/teaching_training_data_num.csv')
opt <- read.csv('data/raw/teaching_training_data_opt.csv')

# data cleaning ----
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
summary(data$working) #F: 63409, T: 19861
prop.table(table(data$working)) #F: 76%, T: 24%

# visualizations ----
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
# mean cft score is slightly higher for working -> significant

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

# modeling ----
# checking significance of explanatory variables
gender_table <- table(survey1$gender, survey1$working)
gender_chisq <- chisq.test(gender_table)
gender_chisq # significant

hhincome_table <- table(survey1$anyhhincome, survey1$working)
hhincome_chisq <- chisq.test(hhincome_table)
hhincome_chisq # significant

givemoney_table <- table(survey1$givemoney_yes, survey1$working)
givemoney_chisq <- chisq.test(givemoney_table)
givemoney_chisq # significant

fin_now_table <- table(survey1$fin_situ_now, survey1$working)
fin_now_chisq <- chisq.test(fin_now_table)
fin_now_chisq # significant

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
model_accuracy <- mean(model$results$Accuracy)
model_accuracy

# things to do ----
# predictions
# confusion matrix
# accuracy, specificity, sensitivity