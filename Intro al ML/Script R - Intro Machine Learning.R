require(tidyverse)

fitness <- read.csv('~/Downloads/fitness_data.csv',
                    sep = ';', dec = ',')
summary(fitness)

fitness <- fitness %>%
  mutate(weight = ifelse(is.na(weight),
                         mean(na.omit(fitness$weight)),
                         weight))

unique(fitness$days_before)

fitness <- fitness %>% 
  mutate(days_before = trimws(gsub("days","",days_before))) %>% 
  mutate(days_before = as.integer(days_before))

fitness <- fitness %>% 
  mutate(day_of_week = ifelse(day_of_week == 'Wednesday',
                              'Wed',
                              ifelse(day_of_week == 'Monday',
                                     'Mon',
                                     ifelse(day_of_week == 'Fri.',
                                            'Fri',
                                            day_of_week))))

fitness <- fitness %>% 
  mutate_if(is.character,as.factor)

fitness <- fitness %>% 
  mutate(attended = as.factor(attended))


fitness %>% 
  group_by(attended) %>% 
  summarise(avg_weight = mean(months_as_member),
            med_weight = median(months_as_member)) %>% 
  ungroup() %>% 
  View()

fitness <- fitness %>% 
  select(-booking_id)
require(rpart)
require(caret)
set.seed(1)
index <- createDataPartition(
  y = fitness$attended,
  times = 1,
  p = 0.8,
  list = FALSE
)

train_set <- fitness[index,]
test_set <- fitness[-index,]

tree_model <- rpart(attended ~ .,
                    data = train_set)

require(rpart.plot)

rpart.plot(tree_model)

pred_values <- predict(tree_model,
                       newdata = test_set,
                       type = 'class')
confusionMatrix(pred_values, 
                test_set$attended,
                positive = '1')
