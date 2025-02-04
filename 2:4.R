library(tidyverse)
d <- read_csv("/Users/juneburke/Downloads/KamilarAndCooperData.csv", col_names = TRUE)
attach(d)
(p <- ggplot(data = d, aes(x = log(Body_mass_female_mean),
                          y = log(Brain_Size_Female_Mean))) + 
  geom_point(na.rm = TRUE) + 
    geom_smooth(method = "lm", na.rm=TRUE, color = "pink") +
    geom_vline(xintercept = 7)+
    geom_hline(yintercept = 3)+
    geom_point(data = d, aes( x=log(Body_mass_female_mean),y=log(Body_mass_male_mean)))+
    geom_smooth(data = d, aes(
      x=log(Body_mass_female_mean), 
      y=log(Body_mass_male_mean)
      ), method = "lm", na.rm = TRUE, color = "cyan"))
install.packages("cowplot")
library(cowplot) #lets you load multiple plots together
####Data Wrangling
s <- select(d, Family, Genus, Body_mass_male_mean)
s <- arrange(d, Family, Genus, desc(Body_mass_male_mean))
s <- summarize(
  group_by(d, Family),
  avgF = mean(Body_mass_female_mean, na.rm = TRUE)
)
####looping
df <- read_csv("/Users/juneburke/Downloads/IMDB-movies.csv", col_names = TRUE)
attach(df)
head(df)
df <- mutate(df, comedy = if(genres = "Comedy") {} "TRUE", ifelse = "FALSE"))

