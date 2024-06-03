# installing/loading packages:
if(!require(pacman)) { install.packages("pacman"); library(pacman)} #load / install+load installr
pacman::p_load(dplyr,
               tidyr,
               knitr,
               readxl,
               haven,
               tidyverse,
               data.table,
               janitor,
               gtsummary,
               likert,
               stringr,
               rlang,
               lubridate,
               ggrepel,
               cowplot,
               hrbrthemes,
               viridis,
               plotly,
               osmdata,
               sf,
               networkD3,
               formattable,
               finalfit,
               purrr,
               flextable,
               car,
               questionr,
               effectsize,
               lattice,
               psych,
               psychTools,
               gplots,   
               vcd,
               aplpack,
               gridExtra)




#install.packages("units", type='binary') #neeed for sf
#library(units)


setwd("/Users/abeleung/Documents/Work/Griffith PhD URP/Conferences/WCTR/2023/Genoa Seniors DRT/Data")
df <- read.csv("silverbus.csv")

str(df)

describevar <- describe(df)
write.csv(describevar, file = "describevar.csv")



#bivar
ggplot(df) +
  geom_bar(mapping = aes(x=impt_boardcomfort, fill = gender), position = "dodge") +
  xlab("X") +
  ylab("Y") +
  scale_fill_manual(values = c("pink2","steelblue"))


#twocats
dep <- xtabs(~ gender + willuse, data = df)
prop.table(dep, NULL)
assocstats(dep)

#two level var
ggplot(df) +
  geom_boxplot(mapping = aes(x = gender, y = impt_onemode), fill="steelblue") + 
  coord_flip() +
  labs(x = "X", y = "Y")

#multi-level var
ggplot(df) +
  geom_boxplot(mapping = aes(x = willuse, y = impt_onemode ), fill="steelblue") +  #One mode Importance relates to WTP and routetype
  coord_flip() +
  labs(x = "X", y = "Y")

#Somee effects by impt_lowmobaccess impt_boardsafety impt_stopsafety impt_tripcomfort impt_stopcomfort impt_crowding impt_punctu

ggplot(df) +
  geom_boxplot(mapping = aes(x = traveltechuse, y = impt_boardcomfort ), fill="steelblue") + 
  coord_flip() +
  labs(x = "X", y = "Y")



