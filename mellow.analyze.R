#mellow analysis

library(tidyverse)
library(corrplot)
library(psych)
library(MASS)
library(patchwork)

options(scipen = 999)

raw <- read_csv("/Users/sagepletka/Documents/GitHub/mellow.analysis/channeldata.csv")


#eda

ggplot(raw, aes(viewCount)) + geom_histogram()
ggplot(raw, aes(log(viewCount))) + geom_histogram()
ggplot(raw, aes(likeCount)) + geom_histogram()
ggplot(raw, aes(vgrade)) + geom_bar()
ggplot(raw, aes(climb_type)) + geom_bar()
ggplot(raw, aes(group)) + geom_bar()
ggplot(raw, aes(length_film)) + geom_bar()
ggplot(raw, aes(gender)) + geom_bar()
ggplot(raw, aes())

describe(log(raw$viewCount))
#encode variables into numeric 


prep <- raw %>% mutate(uncut = if_else(uncut == "no", 0, 1)) %>%
  mutate(vgrade = as.numeric(str_sub(vgrade, 2, 3))) %>%
  mutate(vgrade = if_else(is.na(vgrade), 1, vgrade)) %>%
  mutate(vgrade = as.factor(vgrade)) %>%
  mutate(length_film = if_else(length_film == "no", 0, 1)) %>%
  mutate(gender = case_match(gender, "Male" ~ 1,
                             "Female" ~ 2,
                             "Mix" ~ 3)) %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(sport = if_else(climb_type == "sport", 1, 0)) %>%
  mutate(boulder = if_else(climb_type == "boulder", 1, 0)) %>% 
  mutate(multi_climbs = if_else(multi_climbs == "no", 0, 1)) %>%
  mutate(group = if_else(group == "solo", 0, 1)) %>%
  mutate(days_since_upload = if_else(!is.na(date), as.numeric(as.Date("2023-10-01")-date), NA))

colnames(prep[7:16])
  
cor(prep[ , c(3, 4, 5, 14)])
#views/likes/comments are extremely highly correlated, small negative correlation with time
ggplot(prep, aes(days_since_upload, viewCount)) + geom_point()
ggplot(prep, aes(days_since_upload, likeCount)) + geom_point()


ggplot(prep, aes(viewCount)) + geom_histogram(aes(fill = vgrade))
ggplot(raw, aes(viewCount)) + geom_histogram(aes(fill = climb_type))
ggplot(raw, aes(viewCount)) + geom_histogram(aes(fill = gender))
ggplot(raw, aes(viewCount)) + geom_histogram(aes(fill = uncut))

psych::describe(raw$viewCount)



#poisson regression

all_pois <- glm(viewCount ~ uncut + gender + climb_type + days_since_upload + group + multi_climbs + length_film + vgrade,
                data = prep, family = "poisson")

summary(all_pois)


pois <- glm(viewCount ~ uncut + vgrade + gender + sport + days_since_upload, data = prep,
            family = "poisson")

summary(pois)
pois_coef <- tibble(coef(pois), variable.names(pois)) %>%
  mutate(exponentiated = exp(coef(pois)))


#now to try random forest for prediction and to compare models
library(randomForest)
library(caTools)
library(Metrics)

rf_data <- prep %>% select(viewCount,uncut, gender, climb_type, days_since_upload, group, multi_climbs, vgrade)


set.seed(17)
sample_rf <- sample(1:nrow(rf_data), size = .75 * nrow(rf_data), replace = FALSE)
train_rf <- rf_data[sample_rf, ]
test_rf <- rf_data[-sample_rf, ]

set.seed(500)
rf <- randomForest(viewCount ~ ., data = train_rf)
rf
plot(rf)


#reducing trees to 100 based on rfplot
set.seed(500)
rf_reduced <- randomForest(viewCount ~ ., data = train_rf, ntree = 100) 
rf_reduced
plot(rf_reduced)
varImpPlot(rf_reduced)


rf_data
test_num <- tibble(predict(rf_reduced, test_rf[ , -1]))
comp <- test_num %>% mutate(actual = test_rf[ , 1])

mean_test_rf <- sum(comp$actual)/nrow(comp$actual)
rss_rf <- sum((comp$actual - comp$`predict(rf_reduced, test_rf[, -1])`)^2)
tss_rf <-sum((comp$actual - mean_test_rf)^2)
actual_rf <- pull(comp$actual)
predicted_rf <- comp$`predict(rf_reduced, test_rf[, -1])`
rmse_rf <- rmse(actual_rf, predicted_rf)
rmse_rf


#recreate poission model with hold out sample to test
poisson_train <- glm(viewCount ~ ., data = train_rf, family = "poisson")
summary(poisson_train)

pois_test <- tibble(predict(poisson_train, test_rf[ , -1], type = "response"))
pois_compare <- pois_test %>% mutate(actual = test_rf[ , 1])

actual_pois <- pull(pois_compare$actual)
predicted_pois <- pois_compare$`predict(poisson_train, test_rf[, -1], type = "response")`
rmse_pois <- rmse(actual_pois, predicted_pois)
rmse_pois


#comapare residual plots
rf_residual <- tibble(actual_rf, predicted_rf) %>% mutate(residual = actual_rf - predicted_rf)
rf_residual_plot <- ggplot(rf_residual, aes(actual_rf, residual)) + geom_point()
rf_residual_plot

pois_residual <- tibble(actual_pois, predicted_pois) %>% mutate(residual = actual_pois - predicted_pois)
pois_residual_plot <- ggplot(pois_residual, aes(actual_pois, residual)) + geom_point()
pois_residual_plot

residual_compare <- rf_residual_plot + pois_residual_plot
residual_compare
