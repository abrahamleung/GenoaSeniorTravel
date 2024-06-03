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
               gridExtra,
               yarrr)




#install.packages("units", type='binary') #neeed for sf
#library(units)


setwd("/Users/abeleung/Documents/Work/Griffith PhD URP/Conferences/WCTR/2023/Genoa Senior Travel/Data")
df <- read.csv("2019WTPDataset_fixheader.csv")

str(df)

#DescribeVar====
describevar <- describe(df)
write.csv(describevar, file = "describevar2.csv")


#Find Duplicates====
#dups <- df %>% 
#  janitor::get_dupes(2:73)

#write.csv(dups, file = "dubs.csv")

#Keep only unique rows
# added to a chain of pipes (e.g. data cleaning)
df <- df %>% 
  distinct(across(2:76), # reduces data frame to only unique rows (keeps first one of any duplicates)
           .keep_all = TRUE) 

# if outside pipes, include the data as first argument 
# distinct(obs)


#Recoding=====
#Recode Gender
df$Sex = factor(df$Sex, 
                levels = c(0,1),
                labels = c("Female",
                           "Male"
                             ))

#table(df$Sex)


df$StatusFullEN = factor(df$StatusFull, 
                   levels = c("PENSIONATO ex lavoratore dipendente settore privato",
                              "PENSIONATO ex Lavoratore dipendente settore pubblico",
                              "PENSIONATO ex lavoratore autonomo",
                              "PENSIONATO ex libero professionista",
                              "PENSIONATO ex altro",
                              "OCCUPATO lavoratore dipendente settore privato",
                              "OCCUPATO lavoratore dipendente settore pubblico",
                              "OCCUPATO lavoratore autonomo",
                              "OCCUPATO libero professionista",
                              "OCCUPATO altro"), 
                   labels = c("PENSIONER former private sector employee",
                              "PENSIONER former public sector employee",
                              "PENSIONER formerly self-employed",
                              "PENSIONER former freelancer",
                              "PENSIONER former other",
                              "EMPLOYED private sector employee",
                              "EMPLOYED public sector employee",
                              "EMPLOYED self-employed",
                              "EMPLOYED freelancer",
                              "EMPLOYED other"))

df$Status = factor(df$StatusFull, 
                     levels = c("PENSIONATO ex lavoratore dipendente settore privato",
                                "PENSIONATO ex Lavoratore dipendente settore pubblico",
                                "PENSIONATO ex lavoratore autonomo",
                                "PENSIONATO ex libero professionista",
                                "PENSIONATO ex altro",
                                "OCCUPATO lavoratore dipendente settore privato",
                                "OCCUPATO lavoratore dipendente settore pubblico",
                                "OCCUPATO lavoratore autonomo",
                                "OCCUPATO libero professionista",
                                "OCCUPATO altro"), 
                     labels = c("Pensioner",
                                "Pensioner",
                                "Pensioner",
                                "Pensioner",
                                "Pensioner",
                                "Employed",
                                "Employed",
                                "Employed",
                                "Employed",
                                "Employed"))



df$Occupation = factor(df$StatusFull, 
                     levels = c("PENSIONATO ex lavoratore dipendente settore privato",
                                "PENSIONATO ex Lavoratore dipendente settore pubblico",
                                "PENSIONATO ex lavoratore autonomo",
                                "PENSIONATO ex libero professionista",
                                "PENSIONATO ex altro",
                                "OCCUPATO lavoratore dipendente settore privato",
                                "OCCUPATO lavoratore dipendente settore pubblico",
                                "OCCUPATO lavoratore autonomo",
                                "OCCUPATO libero professionista",
                                "OCCUPATO altro"), 
                     labels = c("Private sector employee",
                                "Public sector employee",
                                "Self-employed",
                                "Freelancer",
                                "Other",
                                "Private sector employee",
                                "Public sector employee",
                                "Self-employed",
                                "Freelancer",
                                "Other"))
#table(df$StatusFullEN)
#table(df$Status)
#table(df$Occupation)

df$EduQual = factor(df$EduQual,
                       levels = c("Laurea, diploma universitario, titolo post-laurea",
                                  "Diploma di scuola media superiore",
                                  "Licenza meda inferiore o avviamento",
                                  "Licenza elementare",
                                  "Senza titolo di studio"), 
                       labels = c("University",
                                  "High school",
                                  "Lower-middle school",
                                  "Primary school",
                                  "None"))

#table(df$EduQual)
#table(df$EduQual_rec)



df$Health = factor(df$Health,
                    levels = c("Ottimo",
                               "Buono",
                               "Neutrale",
                               "Scandente",
                               "Per nulla buono"), 
                    labels = c("Optimal",
                               "Good",
                               "Neutral",
                               "Poor",
                               "Not good at all"))

table(df$Health)





df$LivesWith = factor(df$LivesWith,
                    levels = c("Da solo",
                               "Con coniuge o convivente",
                               "Con coniuge o convivente più figli"), 
                    labels = c("Alone",
                               "With spouse or partner",
                               "With spouse or partner plus children"))

table(df$LivesWith)

df$Tenure = factor(df$Tenure,
                      levels = c("Di proprietà",
                                 "In affitto"), 
                      labels = c("Homeowner",
                                 "Renter"))

table(df$Tenure)

df$Income = factor(df$Income,
                   levels = c("Inferiore a 8000 euro",
                              "Da 8001 a 15000 euro",
                              "Da 15001 a 28000 euro",
                              "Da 28001 a 55000 euro",
                              "Da 55001 a 75000 euro",
                              "Oltre 75000 euro"), 
                   labels = c("<8000€",
                              "8001-15000 €",
                              "15001-28000 €",
                              "28001-55000 €",
                              "55001-75000 €",
                              ">75000 €"))

table(df$Income)


df$DriversLic = factor(df$DriversLic,
                   levels = c("ATTUALMENTE IN POSSESSO",
                              "NON PIU IN POSSESSO",
                              "NO, NON L'HO MAI AVUTA"), 
                   labels = c("Current",
                              "Formerly had",
                              "Never had"))

table(df$DriversLic)


df$PastForgo_PoorPT = factor(df$PastForgo_PoorPT,
                       levels = c("Molto spesso (almeno 2 volte la settimana)",
                                  "Spesso (almeno 1 volta la settimana) ",
                                  "Raramente (qualche volta l‘anno)",
                                  "Mai"), 
                       labels = c("Very often (at least 2 times a week)",
                                  "Often (at least once a week)",
                                  "Rarely (sometimes a year)",
                                  "Never"))
table(df$PastForgo_PoorPT)



df$NeedAssist = factor(df$NeedAssist,
                             levels = c("No, sono completamente autonomo",
                                        "No, ma avrei bisogno di un aiuto",
                                        "Sì, parenti o amici",
                                        "Sì, associaizoni di volontariato",
                                        "Sì, il servizio comunale",
                                        "Altro"), 
                             labels = c("No, I'm completely autonomous",
                                        "No, but I need some help",
                                        "Yes, relatives or friends",
                                        "Other",
                                        "Other",
                                        "Other"
                             ))
table(df$NeedAssist)




#table(df$HomeQuartier)

df$YrsLived = factor(df$YrsLived,
                       levels = c("Meno di 5 anni",
                                  "Da 6 a 15 anni",
                                  "Da 16 a 25 anni",
                                  "Da 26 a 35 anni",
                                  "Da 36 a 45 anni",
                                  "Oltre 46 anni"), 
                       labels = c("<5 years",
                                  "6-15 years",
                                  "16-25 years",
                                  "26-35 years",
                                  "36-45 years",
                                  ">46 years"
                       ))
table(df$YrsLived)

df$TravelTechUse = factor(df$TravelTechUse,
                     levels = c("Sì",
                                "No",
                                "Non personalmente, ma chiedo informazioni ad altri che le utilizzano"), 
                     labels = c("Yes",
                                "No",
                                "No, but I asked for information"   
                     ))



table(df$TravelTechUse)


df$NonTechReason = factor(df$NonTechReason,
                          levels = c("Mancanza di utilità per la sua tipologia di spostamento",
                                     "Difficoltà nell'uso della tecnologia"), 
                          labels = c("Not useful",
                                     "Difficult to use"
                          ))

table(df$NonTechReason)    


df$PTPassUse = factor(df$PTPassUse,
                          levels = c("Abbonamento annuale",
                                     "Abbonamento mensile",
                                     "Abbonamento settimanale",
                                     "Biglietti singoli (cartacei)",
                                     "Biglietti singoli (via SMS)",
                                     "Biglietto singolo integrato",
                                     "Non uso il trasporto publico locale"), 
                          labels = c("Annual subscription",
                                     "Monthly subscription",
                                     "Weekly subscription",
                                     "Single tickets (paper)",
                                     "Single tickets (via SMS)",
                                     "Integrated single ticket",
                                     "Do not use public transport"
                          ))



table(df$PTPassUse)
table(df$PTPassUse_bin)



df$IfPTNeedsTransfer = factor(df$IfPTNeedsTransfer,
                          levels = c("Effettuerei comunque il viaggio con i mezzi pubblici e con la stessa frequenza",
                                     "Effettuerei comunque il viaggio con i mezzi pubblici ma diminuirei la frequenza di quello spostamento",
                                     "Userei l’auto privata senza variare i miei orari",
                                     "Userei l’auto privata variando i miei orari",
                                     "Non effettuerei più quello spostamento"), 
                          labels = c("Continue with PT with the same frequency",
                                     "Continue with PT but with reduced frequency",
                                     "Use private car without changing my schedule",
                                     "Use private car with changed schedules",
                                     "I would never make that trip ever again"
                          ))

table(df$IfPTNeedsTransfer)



df$WhatActivForgo_PoorPT = factor(df$WhatActivForgo_PoorPT,
                          levels = c("Attività di tempo libero",
                                     "Attività di gestione familiare",
                                     "Lavoro"), 
                          labels = c("Leisure activities",
                                     "Family activities",
                                     "Work"
                          ))

table(df$WhatActivForgo_PoorPT)




df$When_ActivForgo_PoorPT = factor(df$When_ActivForgo_PoorPT,
                                  levels = c("Mattino (prima delle 14)",
                                             "Pomeriggio (tra le 14 e le 20)",
                                             "Sera (dopo le 20)"), 
                                  labels = c("Morning (before 2pm)",
                                             "Afternoon (between 2 and 8 pm)",
                                             "Evening (after 8pm)"
                                  ))

table(df$When_ActivForgo_PoorPT)



df$When_MainMovement = factor(df$When_MainMovement,
                                   levels = c("7to9",
                                              "9to11",
                                              "11to13",
                                              "13to15",
                                              "15to17",
                                              "17to19"))

table(df$When_MainMovement)






df$Purpose_MainMovement = factor(df$Purpose_MainMovement,
                                  levels = c("Tempo libero",
                                             "Gestione familiare",
                                             "Lavoro"), 
                                  labels = c("Leisure activities",
                                             "Family activities",
                                             "Work"
                                  ))

table(df$Purpose_MainMovement)

df$Mode_MainMovement = factor(df$Mode_MainMovement,
                                  levels = c("Auto/Moto",
                                             "Autobus",
                                             "Treno",
                                             "Piedi/Bicicletta"), 
                                  labels = c("Car or Motorcycle",
                                             "Public Transport",
                                             "Public Transport",
                                             "Walk or cycle"
                                  ))

table(df$Mode_MainMovement)



#Descriptives ====

#statistic = list(all_continuous() ~ "{mean} ({sd})",
#                 all_categorical() ~ "{n} / {N} ({p}%)"),
#                 digits = list(all_categorical() ~ c(0, 0, 2), all_continuous() ~ 2),

Desc <- df %>% 
  select(
         #Demo
         Sex, Age, StatusFullEN, Status, Occupation, EduQual, Health, LivesWith, Tenure, Income, DriversLic,
         NeedAssist, HomeQuartier, YrsLived,
         #Travel
         PTPassUse, PastForgo_PoorPT, WhatActivForgo_PoorPT, When_ActivForgo_PoorPT,
         When_MainMovement, Purpose_MainMovement, Mode_MainMovement, UseDist_MainMovement, UseTime_MainMovement,
         TravelTechUse, NonTechReason,
         #WTP
         WTP_Imp, WTP_Same
  ) %>% # keep only columns of interest
  tbl_summary(     
    by = Sex,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = list(all_categorical() ~ c(0, 0, 2), all_continuous() ~ 2),
    missing = "always",
    missing_text = "(Missing)"
  ) %>%
  as_flex_table()
Desc 


#Demo
Sex, Age, StatusFullEN, Status, Occupation, EduQual, Health, LivesWith, Tenure, Income, DriversLic,
NeedAssist, YrsLived,
#Travel
PTPassUse, PastForgo_PoorPT, WhatActivForgo_PoorPT, When_ActivForgo_PoorPT,
When_MainMovement, Purpose_MainMovement, Mode_MainMovement, UseDist_MainMovement, UseTime_MainMovement,
TravelTechUse, NonTechReason,
#WTP
WTP_Imp, WTP_Same

#WTP Reasons
WTP_Imp_why, WTP_Same_why

#Geographic
HomeQuartier

#for Likert
Satif_CommCen, Satif_GreenSpace, Satif_CommActiv, Satif_NearPT, Satif_FeelSecure
IntendPrivCar, IntendPT, IntendBike, IntendWalk
Rate_PedPath, Rate_MaintClean, Rate_PedXing, Rate_Lighting, Rate_WalkSafe, Rate_Stairs,
Rate_TravelComf, Rate_StopComf, Rate_BoardingComf, Rate_AscDesComf, Rate_InfoAvailBoards, Rate_InfoAvailStops, Rate_TPLCost, Rate_TravelTime, 
Rate_WaitTime, Rate_Freq, Rate_Punct, Rate_SafetyStop, Rate_SafetyBoard, Rate_GuideCourtesy, 
Relev_TravelCost, Relev_TravelTime

#Trips
Trips_pday, TripsWkly_Drive, TripsWkly_Pax, TripsWkly_Bus, TripsWkly_Train, TripsWkly_CycPed, 
TripsWkly_Work, TripsWkly_Family, TripsWkly_Leisure



#Cat tests ====


#  histogram with density plot
c1 <- ggplot(df, aes(x=WTP_Imp)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

c2 <- ggplot(df, aes(x=WTP_Same)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

c <- plot_grid(c1, c2,
               align = "v",
               ncol = 1)
c

#Calculate the mean of each group 
mu <- df %>%
  group_by(Mode_MainMovement) %>%
  summarise_at(c("WTP_Imp", "WTP_Same"), mean, na.rm = TRUE)

#by mode
p<-ggplot(df, aes(x=WTP_Imp, fill=Mode_MainMovement, color=Mode_MainMovement)) +
  geom_histogram(binwidth=10, alpha=0.1, position="identity") +
  geom_vline(data=mu, aes(xintercept=WTP_Imp, color=Mode_MainMovement),
             linetype="dashed")+
  theme(legend.position="top")
p

p1 <- ggplot(df, aes(x=WTP_Imp, color=Mode_MainMovement, fill=Mode_MainMovement)) +
  geom_density(alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=WTP_Imp, color=Mode_MainMovement),
             linetype="dashed")+
  labs(title="WTP Improved histogram plot",x="WTP", y = "Density")+
  theme_classic()

p2 <- ggplot(df, aes(x=WTP_Same, color=Mode_MainMovement, fill=Mode_MainMovement)) +
  geom_density(alpha=0.2)+
  geom_vline(data=mu, aes(xintercept=WTP_Same, color=Mode_MainMovement),
             linetype="dashed")+
  labs(title="WTP Status Quo histogram plot",x="WTP", y = "Density")+
  theme_classic()

p1 <- p1 + coord_cartesian(xlim = c(0,1000), ylim = c(0,0.005))
p2 <- p2 + coord_cartesian(xlim = c(0,1000), ylim = c(0,0.005))

plot_grid(p2, p1,
          nrow = 2,
          align = "v"
)
                      
#bivar
ggplot(df) +
  geom_bar(mapping = aes(x=WTP_Imp, fill = Sex), position = "dodge") +
  xlab("X") +
  ylab("Y") +
  scale_fill_manual(values = c("pink2","steelblue"))

ggplot(df) +
  geom_bar(mapping = aes(x=WTP_Same, fill = Sex), position = "dodge") +
  xlab("X") +
  ylab("Y") +
  scale_fill_manual(values = c("pink2","steelblue"))


#twocats
dep <- xtabs(~ Sex + WTP_Imp, data = df)
prop.table(dep, NULL)
assocstats(dep)

#two level var
ggplot(df) +
  geom_boxplot(mapping = aes(x = NonTechReason, y = WTP_Imp), fill="steelblue") + 
  coord_flip() +
  labs(x = "X", y = "Y")

ggplot(df) +
  geom_boxplot(mapping = aes(x = NonTechReason, y = WTP_Same), fill="steelblue") + 
  coord_flip() +
  labs(x = "X", y = "Y")


#Linear regression====



p1 <- ggplot(df, aes(x=WTP_Imp, y=Age)) + 
  geom_point() +
  geom_smooth() +
  theme_classic()

p2 <- ggplot(df, aes(x=WTP_Same, y=Age)) + 
  geom_point() +
  geom_smooth() +
  theme_classic()

p1 <- p1 + coord_cartesian(xlim = c(0,1000))
p2 <- p2 + coord_cartesian(xlim = c(0,1000))

plot_grid(p2, p1,
          nrow = 2,
          align = "v")



scatter.smooth(x=df$WTP_Imp, y=df$UseDist_MainMovement)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$UseTime_MainMovement)  # scatterplot

scatter.smooth(x=df$WTP_Imp, y=df$Satif_CommCen)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Satif_GreenSpace)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Satif_CommActiv)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Satif_NearPT)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Satif_FeelSecure)  # scatterplot

scatter.smooth(x=df$WTP_Imp, y=df$IntendPrivCar)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$IntendPT)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$IntendBike)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$IntendWalk)  # scatterplot

scatter.smooth(x=df$WTP_Imp, y=df$Rate_PedPath)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_MaintClean)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_PedXing)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_Lighting)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_WalkSafe)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_Stairs)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_TravelComf)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_StopComf)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_BoardingComf)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_AscDesComf)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_InfoAvailBoards)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_InfoAvailStops)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_TPLCost)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_TravelTime)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_WaitTime)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_Freq)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_Punct)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_SafetyStop)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_SafetyBoard)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Rate_GuideCourtesy)  # scatterplot

scatter.smooth(x=df$WTP_Imp, y=df$Relev_TravelCost)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$Relev_TravelTime)  # scatterplot

scatter.smooth(x=df$WTP_Imp, y=df$Trips_pday)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$TripsWkly_Drive)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$TripsWkly_Pax)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$TripsWkly_Bus)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$TripsWkly_Train)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$TripsWkly_CycPed)  # scatterplot

scatter.smooth(x=df$WTP_Imp, y=df$TripsWkly_Work)  # scatterplot ERROR
scatter.smooth(x=df$WTP_Imp, y=df$TripsWkly_Family)  # scatterplot
scatter.smooth(x=df$WTP_Imp, y=df$TripsWkly_Leisure)  # scatterplot







#wide to long
df_satif <- df %>% select(ID, Satif_CommCen, Satif_GreenSpace, Satif_CommActiv, Satif_NearPT, Satif_FeelSecure)
df_satif <- reshape2::melt(df_satif, id.var = 'ID')






#for Likert
Satif_CommCen, Satif_GreenSpace, Satif_CommActiv, Satif_NearPT, Satif_FeelSecure
IntendPrivCar, IntendPT, IntendBike, IntendWalk
Rate_PedPath, Rate_MaintClean, Rate_PedXing, Rate_Lighting, Rate_WalkSafe, Rate_Stairs,
Rate_TravelComf, Rate_StopComf, Rate_BoardingComf, Rate_AscDesComf, Rate_InfoAvailBoards, Rate_InfoAvailStops, Rate_TPLCost, Rate_TravelTime, 
Rate_WaitTime, Rate_Freq, Rate_Punct, Rate_SafetyStop, Rate_SafetyBoard, Rate_GuideCourtesy, 
Relev_TravelCost, Relev_TravelTime

#Trips
Trips_pday, TripsWkly_Drive, TripsWkly_Pax, TripsWkly_Bus, TripsWkly_Train, TripsWkly_CycPed, 
TripsWkly_Work, TripsWkly_Family, TripsWkly_Leisure




ggplot(df_melt, aes(x = factor(ID), y = value, colour = variable)) + 
  geom_point() + xlab('ZIP Code')


# rerun by adding 3-4 zeros to the N

#Binary Logit Regression ====

#Tobit or not (only cat)
#Linear for likert

#Cumulative distrubiton plot for both WTP

#log or not?
