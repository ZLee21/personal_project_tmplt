#p1_visualization

library(tidyverse)
library(dplyr)
install.packages("ggthemes")
library(ggthemes)

#terminal install.packages("gapminder")
library(gapminder)

scale_y_continuous(trans = "sqrt")
theme_bw()

dat <- gapminder::gapminder %>%
filter(country != "Kuwait")

#firstchart
gg <- ggplot(dat, aes(x=lifeExp, y=gdpPercap)) + 
  geom_point(aes(col=continent, size=pop))+
  facet_wrap(~ year, nrow=1)


  ggsave(gg,width = 15)


#inclass 2nd chart help
(dat_cont <- dat %>%
group_by(year,continent) %>%
summarise(
    gdpPercap = weighted.mean(gdpPercap, pop),
    pop = sum(pop)
) %>%
ungroup() %>%
arrange(continent, year))

#second chart
dat %>%
ggplot(aes( x= year, y = gdpPercap))+
geom_point()+
geom_line(aes(group = country))+
geom_point(data = dat_cont,color = "#000705", size = 3)+
geom_line(data = dat_cont)+
geom_point(aes(color = continent))+
facet_wrap(~ continent, nrow = 1)


#############################
