library(readr)
library(tidyverse)
install.packages("ggthemes")
library(ggthemes)
#Importing Formula 1 Datasets
#https://www.kaggle.com/rohanrao/formula-1-world-championship-1950-2020?select=status.csv
circuits <- read_csv("~/Downloads/R-Day Project/circuits.csv")
constructor_results <- read_csv("~/Downloads/R-Day Project/constructor_results.csv")
constructor_standings <- read_csv("~/Downloads/R-Day Project/constructor_standings.csv")
constructors <- read_csv("~/Downloads/R-Day Project/constructors.csv")
driver_standings <- read_csv("~/Downloads/R-Day Project/driver_standings.csv")
drivers <- read_csv("~/Downloads/R-Day Project/drivers.csv")
lap_times <- read_csv("~/Downloads/R-Day Project/lap_times.csv")
pit_stops <- read_csv("~/Downloads/R-Day Project/pit_stops.csv")
qualifying <- read_csv("~/Downloads/R-Day Project/qualifying.csv")
races <- read_csv("~/Downloads/R-Day Project/races.csv")
results <- read_csv("~/Downloads/R-Day Project/results.csv")
seasons <- read_csv("~/Downloads/R-Day Project/seasons.csv")
status <- read_csv("~/Downloads/R-Day Project/status.csv")

#Selecting Important Variables for Joining Datasets
drivernames <- drivers %>%
  select(driverId, surname)
constructors <- constructors %>%
  mutate(team = name)
constructornames <- constructors %>%
  select(constructorId, team)
#Joining Circuits to Races 
racenames <- races %>%
  select(raceId, circuitId, name, year)
circuits_and_races <- circuits %>% 
  dplyr::left_join(racenames,by="circuitId")
circuits_and_races1 <- circuits_and_races %>%
  select(circuitId, raceId, country, name.y, year)
#Joining Datasets to Final Dataset to use for Project
#Drivers
driver_to_result <- results %>% 
  dplyr::left_join(drivernames,by="driverId")
#Constructors
constructor_to_result <- driver_to_result %>% 
  dplyr::left_join(constructornames,by="constructorId")
#Status
status_to_result <- constructor_to_result %>%
  dplyr::left_join(status,by="statusId")
#Final dataset
Final_data <- status_to_result %>%
  dplyr::left_join(circuits_and_races1,by="raceId")
#Mutating Decades for final data
Final_data <- Final_data %>%
  mutate(Decade = if_else(year >= 1950,
                          paste0(year  %/% 10 * 10, "'s"),
                          paste0((year - 1900) %/% 10 * 10, "'s")))
#Adding Podium to final dataset
Final_data <- Final_data%>%
  dplyr::mutate(podium = if_else(positionOrder == 1 | positionOrder == 2 |positionOrder == 3,"Podium","No Podium"))

Final_data

#Writing into Excel
write.csv(Final_data,"F1_Final_data.csv", row.names = FALSE)

#Now we can Begin Data Analysis

best_drivers <- Final_data %>%
  group_by(team,year) %>%
  count(podium)
best_driver1 <- best_drivers %>%
  filter(podium == "Podium")%>%
  filter(n > 1)%>%
  filter(year >= 2015, year < 2021)
#Graph 2
ggplot(best_driver1, aes(x = year, y = n))+
  geom_point(aes(x = year, y = n, colour = team))+
  geom_line(aes(x = year, y = n, colour = team))+
  theme_bw()+
  labs(x= "Year", y = "Number of Podiums", title = "Graph 2: Number of Team Podiums from 2015 to 2020")+
  theme(plot.title = element_text(hjust=0.5))+
  guides(color = guide_legend("Team", order = 1))+
  scale_y_continuous(breaks = c(5,10,15,20,25,30,35))

#Table 1
Final_data %>%
  dplyr::group_by(Decade) %>%
  filter(year > "1999")%>%
  dplyr::summarize(Fastest_mean = mean(fastestLapSpeed,na.rm=T),Fastest_sd = sd(fastestLapSpeed,na.rm=T),
                   Fastest_1quantile = quantile(fastestLapSpeed, probs = 0.25, na.rm = T), 
                   Fastest_3quantile = quantile(fastestLapSpeed, probs = 0.75, na.rm = T))

#Change lapspeed to numerical value
Final_data$fastestLapSpeed <- as.double(Final_data$fastestLapSpeed)
#Graph 1
Final_data%>%
  filter(year > "1999")%>%
  ggpubr::ggdensity(x="fastestLapSpeed", add="mean", rug= TRUE,
                      color="Decade", fill="Decade")+
  labs(title = "Graph 1: Density Chart of Fastest Speed (past 3 Decades)", x = "Fastest Speed", y = "Density")+
  theme(plot.title = element_text(hjust=0.5))

Final_data$grid <- as.numeric(Final_data$grid)
Final_data_reg <- Final_data %>%
  filter(year > "2018")
F1_mod<- lm(positionOrder~grid + team , data=Final_data_reg)
#Check Multicollinearity
car::vif(F1_mod)

Final_data$team <- relevel(factor(Final_data$team), "Mercedes")

F1_mod %>%
  moderndive::get_regression_table()
#Residual Plot
plot(F1_mod,1)

F1_residuals = resid(F1_mod)
plot(fitted(F1_mod), F1_residuals,
     ylab = "Residuals", xlab = "Predicted Finish Position",
     main = "Predicted Placement by Starting Position and Team")
abline(0,0)

#Graph 3
F1_mod %>%
  broom::tidy(conf.int=T,exponentiate=T) %>%
  dplyr::mutate(Sig = if_else(p.value < 0.05,"Significant",
                              "Non-Significant")) %>%
  ggplot(aes(x=term,y=estimate)) +
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high,color=factor(Sig)),width=0.1) +
  geom_point(aes(color=factor(Sig))) +
  geom_hline(aes(yintercept = 0),color="black",linetype="dashed") +
  labs(x = "Variable",
       y = "Estimated Exponentiated Coefficient",
       color = "Statistical \n Significance") +
  theme_bw() + coord_flip() +
  ggtitle("Graph 3: Confidence Intervals for Regression Analysis") +
  theme(plot.title = element_text(hjust=0.5))





