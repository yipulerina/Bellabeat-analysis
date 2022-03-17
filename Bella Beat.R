library(tidyverse)
library(lubridate)
library(janitor)
library(here)

##Importing datasets and renaming it. ----

activity <- read_csv(here("Data","dailyActivity_merged.csv"))
sleep <- read_csv(here("Data", "sleepDay_merged.csv"))
steps <- read_csv(here("Data", "hourlySteps_merged.csv"))



##Clean dataset  ----
#making necessary changes for easier exploration of data 
activity <- activity%>%
  clean_names()%>%
  drop_na()%>%
  distinct()

sleep <- sleep%>%
  clean_names()%>%
  distinct()%>%
  drop_na()

steps<-steps%>%
  distinct()%>%
  clean_names()%>%
  drop_na()

##Formatting dates----

activity <- activity %>%
  rename(date = activity_date) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))
head(activity)

sleep<-sleep%>%
  separate(col = sleep_day, into  = c('date', 'time'), sep = ' ')

sleep$date = mdy(sleep$date)
sleep$time = hms(sleep$time)
head(sleep)

steps <- steps%>%
  rename(date_time = activity_hour)%>%
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))


## merge dataset----

activity_sleep <- merge(activity, sleep, by = c("id", "date"))
glimpse(activity_sleep)

#droping columns that consist mainly of null values and are not suitable for our analysis----

activity_sleep <- activity_sleep%>%
  select(-c("logged_activities_distance", "tracker_distance", "sedentary_active_distance"))

##Visualizations ----

ggplot(data=activity_sleep, aes(x=total_steps, y=calories)) + 
  geom_point(color= '#66b2b2') + 
  geom_smooth(color = 'purple') + 
  labs(title="Total Steps vs. Calories", x="Total Steps", y= "Calories")
# Great !! There is a positive correlation betweeen total steps and calories burned.
# The more we excercise the healthier we get:)

#let's create a new dataframe for further analysis----
new_average <- activity_sleep%>%
  group_by(id)%>%
  summarise(mean_hours_sleep = mean(total_minutes_asleep)/60,
            mean_daily_steps = mean(total_steps),
            mean_daily_calories = mean(calories))
head(new_average)  

#Quality Of Sleep----

sleep_quality <- new_average%>%
  mutate(sleep_quality = case_when(
    mean_hours_sleep <5 ~ "poor",
    mean_hours_sleep >=5 & mean_hours_sleep <7 ~ "moderate",
    mean_hours_sleep >=7 ~ "healthy"
  ))
head(sleep_quality)   

sleep_quality_percent <- sleep_quality%>%
  group_by(sleep_quality)%>%
  summarise(total = n())%>%
  mutate(totals = sum(total))%>%
  group_by(sleep_quality)%>%
  summarise(total_percent = scales::percent(total/totals))

head(sleep_quality_percent)

# change 'chr' format to 'factor'
sleep_quality_percent$sleep_quality<- factor(sleep_quality_percent$sleep_quality,
                                             levels = c("healthy", "moderate", "poor"))

#now, the visualization:)

sleep_quality_percent%>%
  ggplot(aes(x = "", y=total_percent, fill= sleep_quality))+
  geom_bar( stat = "identity", width = 1)+
  coord_polar("y", start = 0)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust =0.5, size=14, face= "bold"))+
  scale_fill_manual(values = c("#85e085", "#ffd480","#ff8080"),
                    labels = c("Healthy", "Moderate", "Poor"))+
  geom_text(aes(label = total_percent),
            position = position_stack(vjust=0.5))+
  labs(title = "Sleep Quality of users")

#Types of users----

user_type<- new_average%>%
  mutate(user_type = case_when(
    mean_daily_steps <5000 ~"sedentary",
    mean_daily_steps >=5000 & mean_daily_steps <7500 ~"lightly active",
    mean_daily_steps >=7500 & mean_daily_steps <10000 ~"fairly active",
    mean_daily_steps >=10000 ~"very active"
  ))
head(user_type)

user_type_percent <- user_type%>%
  group_by(user_type)%>%
  summarise(total = n())%>%
  mutate(totals = sum(total))%>%
  group_by(user_type)%>%
  summarise(total_percent = scales::percent(total/totals))
head(user_type_percent) 

user_type_percent$user_type <- factor(user_type_percent$user_type, 
                                      levels = c("fairly active", "lightly active", "sedentary", "very active"))

user_type_percent%>%
  ggplot(aes(x="",y=total_percent, fill = user_type))+
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start= 0)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))+
  scale_fill_manual(values = c("#85e085","#0075ab","#ff6583","#ffa600"))+
  geom_text(aes(label = total_percent), position = position_stack(vjust = 0.5))+
  labs(title = "Types Of User")
        
  # Steps per hour----
steps <- steps%>%
  separate(col = "date_time", into = c('date','time'), sep =' ')
 

steps%>%
  group_by(time)%>%
  summarise(mean_steps = mean(step_total))%>%
  ggplot()+
  geom_col(aes(x = time, y= mean_steps, fill= mean_steps))+
  labs(title = "Steps VS Hour", x="", y="")+
  scale_fill_gradient(low = "yellow", high = "red")+
  theme(axis.text.x = element_text(angle =90))
# Users are most active between 12 - 2pm and between 5-7 pm

#Sleep VS sedentary

head(activity_sleep)
ggplot(activity_sleep, aes(x = total_minutes_asleep ,y= sedentary_minutes))+
  geom_jitter(color ='#66b2b2')+ 
  geom_smooth(color = "blue")+
  labs(title = "Sedentary Minutes VS Minutes Asleep", x = "Minutes Asleep", y= "Sedentary Minutes")

 
