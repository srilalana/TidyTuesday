
# Load the #TidyTuesday dataset
## Read Week 38 Data
tuesdata <- tidytuesdayR::tt_load(2020, week = 38)

##Selecting the kids dataset
kids=tuesdata$kids

## Load required packages
install.packages("tidyverse")
library(tidyverse)

## View the dataset 
View(kids)

## summary
ggplot(data = kids) + 
  stat_summary(
    mapping = aes(x = raw, y = state),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median)


## Box plot--To detect the outliers
ggplot(data = kids, mapping = aes(x = raw, y = state)) + 
  geom_boxplot()



## Filtering

kids %>% 
  filter(year <= 2015 & year >= 2000) %>% 
  select(state,year,raw,inf_adj,inf_adj_perchild)


kids %>%
  filter(year >=2011) %>%
  group_by(year) %>%
  summarise(mean_inf_adj_per_child = mean(inf_adj_perchild))




#### Aggregate functions
## Average of Raw data year wise
e<- kids %>%
  group_by(year) %>% 
  summarise(raw_mean= mean(raw),.groups='drop')
head(e)



# Plot the graph for Average of Raw data year wise
ggplot(e, aes(x = year, y = raw_mean)) +
  geom_line() +
  theme_classic() +
  labs(
    x = "Year",
    y = "Raw Average",
    title = paste(
      "Raw Average from 1997 to 2015"
    )
  )



#### Plotting for inf_per_child
e1<- kids %>%
  group_by(year) %>% 
  summarise(inflation_mean_per_child= mean(inf_adj_perchild),.groups='drop')
head(e1)

ggplot(e1, aes(x = year, y = inflation_mean_per_child)) +
  geom_line() +
  theme_classic() +
  labs(
    x = "Year",
    y = " Inflation adjusted Average per child",
    title = paste(
      "Inflation adjusted Average per child from 1997 to 2015"
    )
  )




# Min and max values for inflation adjusted variable wise
kids %>%
  group_by(variable)%>%
  summarise(min_inf_adj = min(inf_adj),
            max_inf_adj= max(inf_adj))




# dropping of na  values
kids=kids%>% drop_na()




#sum,min,max,mean
sum(kids$raw)
min(kids$raw)
max(kids$raw)
mean(kids$raw)


## A simple ggplot between state and inf_adj on year
ggplot(data = kids) + 
  geom_point(mapping = aes(x = inf_adj, y = state, color = year))

ggplot(data = kids) + 
  geom_point(mapping = aes(x = inf_adj, y = state, size = year))

ggplot(data = kids) + 
  geom_point(mapping = aes(x = inf_adj, y = year, shape = state))



### (Raw)Spending on kids year wise
ggplot(data = kids) + 
  geom_smooth(mapping = aes(x = year, y =raw))
ggplot(data = kids) + 
  geom_point(mapping = aes(x = year, y =raw))




