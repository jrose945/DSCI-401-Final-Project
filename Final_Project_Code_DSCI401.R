# Libraries used for the project
library(tidyverse)
library(ggplot2)
library(gridExtra)



### Preprocessing COVID and Vaacination Datasets-------------------------------------



covid_data <- read.csv("data/WHO-COVID-19-global-daily-data.csv")

Qatar_covid_cases <- covid_data %>% 
  select(Date_reported, Country, New_cases) %>% 
  filter(Country == "Qatar")  


Qatar_covid_cases$Date_reported <- ymd(Qatar_covid_cases$Date_reported)

Qatar_covid_cases_WC <- Qatar_covid_cases %>% 
  filter(Date_reported > as.Date("2022-08-01") & Date_reported < as.Date("2023-08-01")) %>%
  mutate(New_cases = replace_na(New_cases, 0))

vaccine_data <- read.csv("data/vaccinations.csv")

qatar_vaccine_data <- vaccine_data %>%
  select(location, daily_vaccinations, date) %>% 
  filter(location == "Qatar", date > as.Date("2022-08-01") & date < as.Date("2023-08-01"))


qatar_vaccine_data$date <- ymd(qatar_vaccine_data$date)


### Preprocessing WC and Copa America datasets-------------------------------------


WC_attendance_data<- read.csv("data/WC2022_Attendance_Sheet.csv")
copa_2024_matches <- read.csv("data/Copa_2024_Matches.csv")

WC_attendance_data_arg_braz <- WC_attendance_data  %>% 
  filter(Home == "Argentina" | Away == "Argentina" | Home == "Brazil" | Away == "brazil") %>% 
  mutate(Attendance = str_remove_all(Attendance, ","), 
         Attendance = as.double(Attendance), 
         Tournament = "World Cup", 
         Team = case_when(
           Home == "Argentina" | Away  == "Argentina" ~ "Argentina", 
           Home == "Brazil" | Away == "Brazil" ~ "Brazil"
         )) %>% 
  select(Home, Away, Attendance, Tournament, Team)



copa_2024_matches_arg_braz <- copa_2024_matches %>%
  filter(home_team == "Argentina" | away_team == "Argentina" | home_team == "Brazil" | away_team == "Brazil") %>% 
  mutate(attendance = str_remove_all(attendance, ","), 
         attendance = as.double(attendance),
         Tournament = "Copa America", 
         Team = case_when(
           home_team == "Argentina" | away_team == "Argentina" ~ "Argentina", 
           home_team == "Brazil" | away_team == "Brazil" ~ "Brazil"
         )) %>% 
  rename(Home = home_team, Away = away_team, Attendance = attendance) %>% 
  select(Home, Away, Attendance, Tournament, Team) 

arg_brazil_stats <- bind_rows(WC_attendance_data_arg_braz, copa_2024_matches_arg_braz)


arg_braz_covid_data <- covid_data %>% 
  filter(Country == "Argentina" | Country == "Brazil", year(Date_reported) == 2022 | year(Date_reported) == 2024 ) %>% 
  mutate(Date_reported = as.Date(Date_reported),
         New_cases = replace_na(New_cases, 0))



### Qatar COVID plots-------------------------------------------------------


line_plot1 <- ggplot(qatar_vaccine_data, aes(x = date, y = daily_vaccinations)) +  
  geom_line() +   
  xlab("Date") + 
  ylab("New Cases \n per Individual") + 
  annotate("rect", xmin = as.Date("2022-11-20"),
           xmax = as.Date("2022-12-18"),
           ymin = -Inf, 
           ymax = Inf,
           alpha = 0.2, fill = "red") + 
  ggtitle("Daily COVID-19 Vaccinations Over Time in Qatar")


line_plot2 <- ggplot(Qatar_covid_cases_WC, aes(x = Date_reported, y = New_cases)) + 
  geom_line() + 
  xlab("Date") + 
  ylab("New Cases \n per Individual") + 
  annotate("rect", xmin = as.Date("2022-11-20"),
           xmax = as.Date("2022-12-18"),
           ymin = -Inf, 
           ymax = Inf,
           alpha = 0.2, fill = "red") + 
  ggtitle("New COVID-19 Cases Over Time in Qatar")

final_line_plot <- grid.arrange(line_plot1, line_plot2, nrow = 2)

ggsave(
  filename = "line_plot_of_vac_cov.png",
  plot = final_line_plot,
  width = 8, height = 6, dpi = 300
  
)

### Summary statistics of covid casses and vaccination dosages in Qatar-------------------------------------


Qatar_covid_cases_WC %>% 
  summary()

qatar_vaccine_data %>% 
  summary()


### Summary statistics of world cup attendance-------------------------------------


arg_brazil_stats %>% 
  group_by(Tournament, Team) %>% 
  summarise(avg_attendance = mean(Attendance)) 


### Argentina and Brazil new COVID-19 cases after WC and Copa America-------------------------------------



arg_plot1 <- arg_braz_covid_data %>% 
  filter(Date_reported > as.Date("2024-07-14") & Date_reported < as.Date("2024-08-14")) %>% 
  mutate(New_cases = replace_na(New_cases, 0)) %>% 
  ggplot(aes(x = Date_reported, y = New_cases,color = Country, group = Country)) + 
  geom_line() + 
  xlab("Date") + 
  ylab("New Cases Per Individual") + 
  ggtitle("New COVID-19 Cases After Copa America 2024 \n In Argentina and Brazil")


arg_plot2 <- arg_braz_covid_data %>% 
  filter(Date_reported > as.Date("2022-11-20") & Date_reported < as.Date("2022-12-30")) %>% 
  ggplot(aes(x = Date_reported, y = New_cases, color = Country, group = Country)) +
  geom_line() + 
  xlab("Date") + 
  ylab("New Cases Per Individual") + 
  ggtitle("New COVID-19 Cases After World Cup 2022 \n In Argentina and Brazil")

final_line_plot2 <- grid.arrange(arg_plot1, arg_plot2, nrow = 2)

ggsave(
  filename = "line_plot_of_arg_braz.png",
  plot = final_line_plot2,
  width = 8, height = 6, dpi = 600
)