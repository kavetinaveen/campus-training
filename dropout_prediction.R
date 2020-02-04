rm(list = ls())
setwd("~/Documents/Kaggle/KDDCup 2015/")

# Installing/Loading required libraries
load_packages <- function(packages_list){
  for(i in packages_list){
    if(i %in% installed.packages()){
      library(eval(i), character.only = TRUE)
      print(paste0("Loaded installed library: ", i))
    }else{
      install.packages(i)
      library(i, character.only = TRUE)
      print(paste0("Installed and loaded library: ", i))
    }
  }
}
requirements <- c('data.table', 'dplyr', 'lubridate', 'reshape2', 'ggplot2', 'h2o', 'Metrics', 'PRROC')
load_packages(requirements)

# Function to process raw data
process_data <- function(log, truth, enrollment, object, date, final_data){
  # Joining enrollments with date
  enrollment <- enrollment %>% 
                  left_join(date) %>% 
                  mutate(from = as.Date(from, format = "%Y-%m-%d"),
                         to = as.Date(to, format = "%Y-%m-%d"))
  
  # Joining log data with object
  log <- log %>%
          left_join(object, by = c("object" = "module_id")) %>% 
          mutate(time = as.POSIXct(time, format = '%Y-%m-%dT%H:%M:%S'))
  
  # Number of courses by each user (All courses duration: 29 days)
  nCourses <- enrollment %>% 
                group_by(username) %>% 
                summarise(nCourses = n())
  
  final_data <- final_data %>% 
                  left_join(enrollment) %>% 
                  left_join(nCourses)
  
  # Number of enrollments in each course
  nEnrollments <- enrollment %>% 
                    group_by(course_id) %>% 
                    summarise(nEnrollments = n())
  
  final_data <- final_data %>% 
                  left_join(nEnrollments)
  
  # Average delay between chapters (w.r.t user)
  avg_delay_features <- log %>% 
                          filter(category == 'chapter') %>% 
                          arrange(enrollment_id, time) %>% 
                          group_by(enrollment_id) %>% 
                          summarise(avg_delay_chapters = mean(as.integer(diff.Date(time))))
  
  final_data <- final_data %>%
                  left_join(avg_delay_features)
  
  # Average delay between chapters (w.r.t course)
  delay_chapters <- object %>% 
                      mutate(start = as.POSIXct(start, format = '%Y-%m-%dT%H:%M:%S')) %>% 
                      filter(category == 'chapter') %>% 
                      arrange(course_id, start) %>% 
                      group_by(course_id) %>%
                      summarise(avg_delay_chapters_course = mean(as.integer(diff.Date(start)))) %>% 
                      mutate(avg_delay_chapters_course = ifelse(is.na(avg_delay_chapters_course), 0, avg_delay_chapters_course))
  
  final_data <- final_data %>% 
                  left_join(delay_chapters)
  
  # Lifetime of the user
  tmp <- log %>% 
          left_join(enrollment[, c("enrollment_id", "username")])
  
  lifetime_tbl <- tmp %>% 
                    group_by(username) %>% 
                    summarise(lifetime = as.integer(max(time) - min(time)))
  
  final_data <- final_data %>% 
                  left_join(lifetime_tbl)
  
  # Features based on between course gap
  btw_course_gap_features <- final_data %>% 
                                group_by(username) %>% 
                                summarise(btw_course_gap_min = min(as.integer(diff.Date(sort(from)))),
                                          btw_course_gap_max = max(as.integer(diff.Date(sort(from)))),
                                          btw_course_gap_mean = mean(as.integer(diff.Date(sort(from))))) %>% 
                                mutate(btw_course_gap_min = ifelse(is.infinite(btw_course_gap_min), 999, btw_course_gap_min),
                                       btw_course_gap_max = ifelse(is.infinite(btw_course_gap_max), 999, btw_course_gap_max),
                                       btw_course_gap_mean = ifelse(is.na(btw_course_gap_mean), 999, btw_course_gap_mean))
  
  final_data <- final_data %>% 
                  left_join(btw_course_gap_features)
  
  
  # Event count feature
  event_counts <- dcast(log, enrollment_id ~ event, value.var = "event", fun = length)
  
  final_data <- final_data %>% 
                  left_join(event_counts)
  
  # Time spent on problem, discussion, video, mean and median interarrival times
  event_duration <- log %>% 
                      arrange(enrollment_id, time) %>% 
                      group_by(enrollment_id) %>% 
                      mutate(time_diff = c(0, as.integer(diff.Date(time))),
                             is_problem = ifelse(event == 'problem', 1, 0),
                             is_video = ifelse(event == 'video', 1, 0),
                             is_discussion = ifelse(event == 'discussion', 1, 0),
                             is_problem_duration = is_problem*time_diff,
                             is_video_duration = is_video*time_diff,
                             is_discussion_duration = is_discussion*time_diff) %>% 
                      group_by(enrollment_id) %>% 
                      summarise(problem_duration = sum(is_problem_duration),
                                video_duration = sum(is_video_duration),
                                discussion_duration = sum(is_discussion_duration),
                                md_interarrival_time = median(time_diff),
                                mean_interarrival_time = mean(time_diff)) 
  
  final_data <- final_data %>% 
                  left_join(event_duration) %>% 
                  mutate(problem_duration_per_course = problem_duration/nCourses,
                         video_duration_per_course = video_duration/nCourses,
                         discussion_duration_per_course = discussion_duration/nCourses)
  
  return(final_data)
}

# Processing train data
date <- fread("./data/ObjectData/date.csv")
object <- fread("./data/ObjectData/object.csv", na.strings = c("null"))
enrollment_train <- fread("./data/train/enrollment_train.csv")
log_train <- fread("./data/train/log_train.csv")
truth_train <- fread("./data/train/truth_train.csv")

final_data_train <- truth_train
colnames(final_data_train) <- c("enrollment_id", "dropout")

final_data_train <- process_data(log_train, truth_train, enrollment_train, object, date, final_data_train)
fwrite(final_data_train, "final_data_train.csv")

# Processing test data
enrollment_test <- fread("./data/test/enrollment_train.csv")
log_test <- fread("./data/test/log_train.csv")
truth_test <- fread("./data/test/truth_train.csv")

final_data_test <- truth_test
colnames(final_data_test) <- c("enrollment_id", "dropout")

final_data_test <- process_data(log_test, truth_test, enrollment_test, object, date, final_data_test)
fwrite(final_data_test, "final_data_test.csv")

# Function to get boxplot
get_box_plot <- function(df, y, params, x = 'dropout'){
  g <- ggplot(data = df) + geom_boxplot(aes_string(x = as.factor(df[[x]]), y = y)) + ggtitle(params$title) + xlab(params$xlab) + ylab(params$ylab)
  print(g)
}

params <- list(title = 'Boxplot of lifetime', xlab = 'Dropout Indicator', ylab = 'Lifetime of a user')
get_box_plot(final_data, 'lifetime', params)

# Training GBM Model
h2o.init()
train_df <- h2o.uploadFile(path = 'final_data_train.csv')
train_df$dropout <- as.factor(train_df$dropout)
dependent <- 'dropout'
independent <- setdiff(colnames(train_df), c('enrollment_id', dependent, 'username', 'course_id', 'from', 'to'))
model <- h2o.gbm(y = dependent, x = independent, training_frame = train_df, ntrees = 100, max_depth = 5, min_rows = 10)

# Predict the test data
test_df <- h2o.uploadFile(path = 'final_data_test.csv')
test_df$dropout <- as.factor(test_df$dropout)
predictions <- predict(model, test_df)
predictions <- as.data.frame(predictions)
test_df <- as.data.frame(test_df)
predictions$dropout <- test_df$dropout
table(predictions$predict, predictions$dropout)

# AUC-ROC 
roc <- roc.curve(scores.class0 = predictions[predictions$dropout == 1, 'p1'], scores.class1 = predictions[predictions$dropout == 0, 'p1'], curve = T)
plot(roc)

# AUC-PR
pr <- pr.curve(scores.class0 = predictions[predictions$dropout == 1, 'p1'], scores.class1 = predictions[predictions$dropout == 0, 'p1'], curve = T)
plot(pr)







