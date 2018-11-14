#MANIPULATING ANALYZING AND EXPORTING DATA 



#load tidyverse package
library(tidyverse)

#readr (package in tidyverse) and read_csv
surveys <- read_csv("data/protal_data_joined.csv")

str(surveys)

#show surveys
surveys

#view our data
View(surveys)

#pull out rows -filteR (R = rows)
filter()

#create new columns based on data already in dataframe
mutate()

#create summary statistics on grouped data
group_by%>% summarize()

#sorting results
arragne()

#count discrete values
count()

#pull out plot_id...
select(surveys,plot_id,species_id, weight)

#select columns except for ones you don't want
select(surveys, -record_id, -species_id)


#pulling out specific rows
filter(surveys, year==1990)

#how to select AND filter on the same dataset
#1. intermediate steps
surveys2 <- filter(surveys, weight < 5)
surveys2
surveys_sml <- select(surveys2, species_id,sex,weight)
surveys_sml

#2. nested functions
surveys_sml <- select(filter(surveys, weight < 5), species_id,sex,weight)
surveys_sml

#3. pipes - shoot the output from one function to another
# a pipe looks like %>% and the shortcut is shift+ctrl(strg)+m %>% 
surveys %>% filter(weight < 5) %>% select(species_id,sex,weight)

#saving new output
#not printed out when it is assigned to a new object
surveys_sml <- surveys %>% filter(weight < 5) %>% select(species_id,sex,weight)
surveys_sml

##challenge
#using pipes subset surveys to inclued animals collected before 1995
#only include columns year, sex, weight

head(surveys)
surveys %>% filter(year < 1995) %>% select(year,sex,weight)

#mutate to create a new column of weight in kg
surveys %>% mutate(weight_kg = weight/1000)

#create multiple columns in one go using mutate
surveys %>% mutate(weight_kg = weight/1000, weight_kg2 = weight_kg^2) %>% head

#filtering out NAs?
# ! = not, is.na = is it NA? !is.na = is NOT NA
surveys %>% filter(!is.na(weight)) %>% mutate(weight_kg = weight/1000) %>% head

#group_by is the column name that you want to claculate summary statistics for
#summarize is what you want to summarize
surveys %>% group_by(sex) %>% summarize(mean_weight = mean(weight,na.rm=TRUE))

#group by multiple columns
surveys %>% group_by(sex,species,species_id) %>% summarize(mean_weight = mean(weight,na.rm=TRUE)) %>% head()

#see more lines by using print
surveys %>% group_by(sex,species,species_id) %>% summarize(mean_weight = mean(weight,na.rm=TRUE)) %>% print(n=24)

#unexpected NAs one method is
surveys %>% filter(!is.na(weight)) %>% group_by(sex,species_id) %>% summarize(mean_weight = mean(weight)) %>% print(n=24)

#sort by descending weight
surveys %>% filter(!is.na(weight)) %>% group_by(sex,species_id) %>% summarize(mean_weight = mean(weight), min_weight = min(weight)) %>% arrange(desc(mean_weight)) %>% head()

#counting to work out numbers of rows
surveys %>% count(sex)
surveys %>% count(sex, sort=TRUE)

#don't want to sort by count
surveys %>% count(sex,species) %>% arrange(species,desc(n))

#CHALLENGE
#1. how many animals were caught in each plot_type?
head(surveys)
surveys %>% count(plot_type)

#2. what mean, min and max hindfoot length for each species (species_id). 
#group_by, summarize()
surveys %>% filter(!is.na(hindfoot_length)) %>% group_by(species,species_id) %>% summarize(mean_hindfoot = mean(hindfoot_length), min_hindfoot = min(hindfoot_length, max_hindfoot = max(hindfoot_length))) 

#filtering out a bunch of NA values so we can plot our data
surveys_complete <- surveys %>% filter(!is.na(weight),!is.na(hindfoot_length), !is.na(sex))

species_counts <- surveys_complete %>% count(species_id) %>% filter(n >= 50)
species_counts %>% head()

surveys_complete <- surveys_complete %>% filter(species_id %in% species_counts$species_id)
surveys_complete
dim(surveys_complete)




####PLOTTING

#basic ggplot template
#ggplot(data = <DATA>, mapping=aes(<MAPPINGS>)) + <GEOM_FUNCTION>()

ggplot(surveys_complete)
#define what to display on axes
head(surveys_complete)
ggplot(surveys_complete, aes(x = weight, y = hindfoot_length))
#getting dots on graphs
ggplot(surveys_complete, aes(x = weight, y = hindfoot_length)) + geom_point()

#different options - geom_point(scattergraph)
#boxplots
geom_boxplots()
#line graphs
geom_line()

#assign a plot to a variable/object
surveys_plot <- ggplot(surveys_complete, aes(x = weight, y = hindfoot_length))
surveys_plot

#use plus option to tweak how we want to display data
#plus has to be at the end of the previous line (not at beginning of next line)
surveys_plot + geom_point()
surveys_plot + 
  geom_point()

#display data using hexagonal bins?
#insatll a package
install.packages("hexbin")
library(hexbin)

surveys_plot + geom_hex()

#scatterplot with adding of transperancy
surveys_plot + geom_point(alpha = 0.1)
#different colour
surveys_plot + geom_point(alpha = 0.1, colour = "blue")
#dots different colours depending on species_id
surveys_plot + geom_point(aes(colour=species_id))


##CHALLENGE
#create a scatter plot of weight over species_id and colour your points by species_id

surveys_plot <- ggplot(surveys_complete, aes(x = species_id, y = weight, colour=species_id))
surveys_plot + geom_point()

#change to a boxplot
surveys_plot <- ggplot(surveys_complete, aes(x = species_id, y = weight)) + 
  geom_boxplot(aes(colour=species_id))
surveys_plot

ggplot(surveys_complete, aes(x = species_id, y = weight)) + 
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, colour = "tomato")


#boxplots on top of data
ggplot(surveys_complete, aes(x = species_id, y = weight)) + 
  geom_jitter(alpha = 0.3, colour = "tomato") +
  geom_boxplot(alpha = 0)

#colouring sexes
ggplot(surveys_complete, aes(x = species_id, y = weight)) + 
  geom_jitter(alpha = 0.3,aes(colour = sex)) +
  geom_boxplot(alpha = 0)

#new tibble to plot with our time data
yearly_counts <- surveys_complete %>% count(year,species_id)
yearly_counts %>% head()

ggplot(yearly_counts, aes(x = year, y = n)) + geom_line()

#different line for each species
ggplot(yearly_counts, aes(x = year, y = n, group = species_id)) + geom_line()
#add colur
ggplot(yearly_counts, aes(x = year, y = n, colour = species_id)) + geom_line()

#bunch of little graph together (faceting)
ggplot(yearly_counts, aes(x = year, y = n)) +
  geom_line() +
  facet_wrap(~species_id)

yearly_counts %>% head()
#we need to make a new tibble that has sex in it
yearly_sex_counts <- surveys_complete %>% count(year, species_id, sex)
head(yearly_sex_counts)

#splitting or data even further
ggplot(yearly_sex_counts, aes(x = year, y = n, colour = sex)) +
  geom_line(lwd = 0.5) +
  facet_wrap(~species_id)

#change background colours using themes
ggplot(yearly_sex_counts, aes(x = year, y = n, colour = sex)) +
  geom_line(lwd = 0.5) +
  facet_wrap(~species_id) +
  theme_bw() +
  theme(panel.grid = element_blank())

