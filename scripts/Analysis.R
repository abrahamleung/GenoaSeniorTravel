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
               yarrr,
               fastDummies,
               statsr,
               GGally,
               olsrr,
               corrplot,
               funModeling,
               ggpubr,
               jtools,
               ggstance,
               broom,
               kableExtra,
               pixiedust,
               lmtest,
               tableHTML,
               huxtable,
               CRediTas)




#install.packages("units", type='binary') #neeed for sf
#library(units)

#LOAD data ====
setwd("/Users/abeleung/Documents/Work/Griffith PhD URP/Journal/Research in Transportation Business & Management/Genoa Senior Travel/Data/")

df <- read.csv("2019WTPDataset_fixheader.csv")

#str(df)

#Find Duplicates====
#dups <- df %>% 
#  janitor::get_dupes(2:73)

#write.csv(dups, file = "dubs.csv")


# if outside pipes, include the data as first argument 
# distinct(obs)




#Keep only unique rows
# added to a chain of pipes (e.g. data cleaning)
df <- df %>% 
  dplyr::distinct(across(2:76), # reduces data frame to only unique rows (keeps first one of any duplicates)
           .keep_all = TRUE) 

#Remove outlier rows
#df <- subset(df, ID!= 285)






#Recoding=====

#Age split
df <- df %>% 
  mutate(Age_cat = cut(Age, 
                   c(-Inf, quantile(Age, c(.30, .66666)), Inf), 
                   labels = c("Low", "Mid", "High"))) #62-70 #71-80 #81+

df <- df %>% relocate(Age_cat, .after = Age)



#Recode Gender
df$Sex = factor(df$Sex, 
                levels = c(0,1),
                labels = c("Female",
                           "Male"
                             ))

table(df$Sex)


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
table(df$StatusFullEN)
table(df$Status)
table(df$Occupation)

df$EduQual = factor(df$EduQual,
                       levels = c("Laurea, diploma universitario, titolo post-laurea",
                                  "Diploma di scuola media superiore",
                                  "Licenza meda inferiore o avviamento",
                                  "Licenza elementare",
                                  "Senza titolo di studio"), 
                       labels = c("University",
                                  "High school",
                                  "Lower middle school",
                                  "Primary school",
                                  "None"))

table(df$EduQual)
table(df$EduQual_rec)



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

#recode as numeric
df$Income_rec = car::recode(df$Income,"
                              'Inferiore a 8000 euro' = 1;
                              'Da 8001 a 15000 euro' = 2;
                              'Da 15001 a 28000 euro' = 3;
                              'Da 28001 a 55000 euro' = 4;
                              'Da 55001 a 75000 euro' = 5;
                              'Oltre 75000 euro' = 6 ")


df$Income_rec <- as.numeric(df$Income_rec)
table(df$Income_rec)
str(df$Income_rec)

df$Income = factor(df$Income,
                   levels = c("Inferiore a 8000 euro",
                              "Da 8001 a 15000 euro",
                              "Da 15001 a 28000 euro",
                              "Da 28001 a 55000 euro",
                              "Da 55001 a 75000 euro",
                              "Oltre 75000 euro"), 
                   labels = c("Under 8000€",
                              "8001 to 15000€",
                              "15001 to 28000€",
                              "28001 to 55000€",
                              "55001 to 75000€",
                              "Over 75000€"))

table(df$Income)



df$DriversLic = factor(df$DriversLic,
                   levels = c("ATTUALMENTE IN POSSESSO",
                              "NON PIU IN POSSESSO",
                              "NO, NON L'HO MAI AVUTA"), 
                   labels = c("Current",
                              "Formerly had",
                              "Never had"))


table(df$DriversLic)



#recode NA as 0
#df$PastForgo_PoorPT %>% replace_na("Mai")

df$PastForgo_PoorPT = factor(df$PastForgo_PoorPT,
                       levels = c("Molto spesso (almeno 2 volte la settimana)",
                                  "Spesso (almeno 1 volta la settimana) ",
                                  "Raramente (qualche volta l‘anno)",
                                  "Mai"), 
                       labels = c("Very often",
                                  "Often",
                                  "Rarely",
                                  "Never"))


table(df$PastForgo_PoorPT)
str(df$PastForgo_PoorPT)
summary(df$PastForgo_PoorPT)


#recode as numbers
df$PastForgo_PoorPT_rec = car::recode(df$PastForgo_PoorPT,"
                                  'Very often' = 3;
                                  'Often' = 2;
                                  'Rarely' = 1;
                                  'Never' = 0")

#remove NA
#df <- df[!is.na(df$PastForgo_PoorPT_rec), ]

#replace NA in factor var
df$PastForgo_PoorPT_rec[is.na(df$PastForgo_PoorPT_rec)] = 0

df$PastForgo_PoorPT_rec <- as.integer(as.factor(df$PastForgo_PoorPT_rec)) -1

df$PastForgo_PoorPT_rec[df$PastForgo_PoorPT_rec==2] <- 3

str(df$PastForgo_PoorPT_rec)
summary(df$PastForgo_PoorPT_rec)
table(df$PastForgo_PoorPT_rec)



df$NeedAssist = factor(df$NeedAssist,
                             levels = c("No, sono completamente autonomo",
                                        "No, ma avrei bisogno di un aiuto",
                                        "Sì, parenti o amici",
                                        "Sì, associaizoni di volontariato",
                                        "Sì, il servizio comunale",
                                        "Altro"), 
                             labels = c("No completely autonomous",
                                        "No but need some help",
                                        "Yes",
                                        "Yes",
                                        "Yes",
                                        "Other"
                             ))


table(df$NeedAssist)
summary(df$NeedAssist)

#recode as numeric
df$NeedAssist_rec = car::recode(df$NeedAssist,"
                                  'No completely autonomous' = 0;
                                  'No but need some help' = 1;
                                  'Yes' = 2;
                                  'Other' = NA")

df$NeedAssist_rec <- as.integer(as.factor(df$NeedAssist_rec)) -1
summary(df$NeedAssist_rec)

str(df$NeedAssist_rec)
table(df$NeedAssist_rec)
summary(df$NeedAssist_rec)






df$YrsLived = factor(df$YrsLived,
                       levels = c("Meno di 5 anni",
                                  "Da 6 a 15 anni",
                                  "Da 16 a 25 anni",
                                  "Da 26 a 35 anni",
                                  "Da 36 a 45 anni",
                                  "Oltre 46 anni"), 
                       labels = c("Less than 5 years",
                                  "6 to 15 years",
                                  "16 to 25 years",
                                  "26 to 35 years",
                                  "36 to 45 years",
                                  "Over 46 years"
                       ))
table(df$YrsLived)


#recode as numeric
df$YrsLived_rec = car::recode(df$YrsLived,"
                                  'Less than 5 years' = 1;
                                '6 to 15 years' = 2;
                                '16 to 25 years' = 3;
                                '26 to 35 years' = 4;
                                '36 to 45 years' = 5;
                                'Over 46 years' = 6"
                                )


table(df$YrsLived_rec)
df$YrsLived_rec <- as.numeric(df$YrsLived_rec)
str(df$YrsLived_rec)

df$TravelTechUse = factor(df$TravelTechUse,
                     levels = c("Sì",
                                "No",
                                "Non personalmente, ma chiedo informazioni ad altri che le utilizzano"), 
                     labels = c("Yes",
                                "No",
                                "No but I asked for information"   
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
                                     "MonthWeek subscription",
                                     "MonthWeek subscription",
                                     "Single tickets",
                                     "Single tickets",
                                     "Single tickets",
                                     "Do not use public transport"
                          ))


table(df$PTPassUse)


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
                                  labels = c("Morning before 2pm",
                                             "Afternoon between 2 and 8 pm",
                                             "Evening after 8pm"
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


#Intend
#NA treatment remove, not good effect
#df <- df %>% 
#  mutate_at(c("IntendPrivCar", "IntendPT",	"IntendBike",	"IntendWalk"), ~replace_na(.,3))

#recode numeric variables
df <- df %>%
  mutate(across(
    starts_with("Intend"),
    ~dplyr::recode(., `1`=-2,`2`=-1,`3`=0,`4`=1,`5`=2)
  ))

summary(df$IntendPrivCar)
summary(df$IntendPT)
summary(df$IntendBike)
summary(df$IntendWalk)


#Calculate UseDist_MainMovement by mode specific average

#cal speed
df$UseSpeed_MainMovement = df$UseDist_MainMovement / df$UseTime_MainMovement

#get speed
df %>%
  group_by(Mode_MainMovement) %>%
  summarise_at(vars(UseSpeed_MainMovement),
               funs(mean = mean), 
               na.rm = TRUE)

#duplicate speed column
df$EstSpeed_MainMovement <- df$Mode_MainMovement

#recode mode name to est speed values
df <- df %>% mutate(EstSpeed_MainMovement=dplyr::recode(EstSpeed_MainMovement,
                                                "Car or Motorcycle" = 0.404,
                                                "Public Transport" = 0.238,
                                                "Walk or cycle" = 0.125))

summary(df$EstSpeed_MainMovement)
summary(df$Mode_MainMovement)
summary(df$UseDist_MainMovement2)

#duplicate dist column
df$UseDist_MainMovement2 <- df$UseDist_MainMovement

#select and fill missing with estimated dist by mode avg speed
df$EstDist_MainMovement <- df$EstSpeed_MainMovement * df$UseTime_MainMovement

df$UseDist_MainMovement2[is.na(df$UseDist_MainMovement2)] <- df$EstDist_MainMovement[is.na(df$UseDist_MainMovement2)]

df <- df %>% relocate(UseSpeed_MainMovement:EstDist_MainMovement, .after = UseTime_MainMovement)
  



#Dummify====
#Create dummy from values (WTP)


df$WTP_Same_cat <-ifelse(df$WTP_Same > 221.356275303644,"SQHigh","SQLow") #value from n=247 no missing, from desc table
df$WTP_Imp_cat <-ifelse(df$WTP_Imp > 304.068825910931,"ImpHigh","ImpLow")

df$WTP_Same_zero <-ifelse(df$WTP_Same == 0,1,0) #zero WTP
df$WTP_Imp_zero <-ifelse(df$WTP_Imp == 0,1,0) 

df$WTP_Same_NA <- ifelse(is.na(df$WTP_Same), 1, 0) #NA
df$WTP_Imp_NA <- ifelse(is.na(df$WTP_Imp), 1, 0) 

table(df$WTP_Same_cat)
table(df$WTP_Imp_cat)
table(df$WTP_Same_zero)
table(df$WTP_Imp_zero)
table(df$WTP_Same_NA)
table(df$WTP_Imp_NA)


#join 4 cats

df$WTP_All_cat <- str_c(df$WTP_Same_cat,"-", df$WTP_Imp_cat)

table(df$WTP_All_cat)



#Categories
df <- dummy_cols(df, 
                 select_columns = c("Sex", "Age_cat","Status", "Occupation", "Tenure","DriversLic", "EduQual", "LivesWith",
                                    "PTPassUse", "IfPTNeedsTransfer", "PastForgo_PoorPT_rec",
                                    "TravelTechUse", "NonTechReason",
                                    "WhatActivForgo_PoorPT", "When_ActivForgo_PoorPT", 
                                    "When_MainMovement", "Purpose_MainMovement","Mode_MainMovement",
                                    "WTP_Same_cat", "WTP_Imp_cat", "WTP_All_cat", "WTP_Same_zero", "WTP_Imp_zero", "WTP_Same_NA", "WTP_Imp_NA"),
                 ignore_na = T)


colnames(df) <- gsub(" ","_",colnames(df))




#For reference
write.csv(df, file = "dummy.csv")

#DescribeVar====
describevar <- psych::describe(df)
write.csv(describevar, file = "describevar.csv")



#Select complete====
df2 <- df %>% select(WTP_Same, WTP_Imp,
                     Sex, Sex_Female, Age,
                     HomeQuartier,
                     EduQual_rec, Income_rec, Health_rec, NeedAssist_rec, LivesWith_Alone, DriversLic_Current, YrsLived_rec, 
                     PTPassUse, PTPassUse_Annual_subscription, 
                     When_MainMovement,When_MainMovement_7to9, When_MainMovement_9to11, When_MainMovement_11to13, When_MainMovement_13to15, When_MainMovement_15to17, When_MainMovement_17to19, 
                     UseDist_MainMovement2, 
                     TravelTechUse_Yes, 
                     PastForgo_PoorPT_rec, PastForgo_PoorPT_rec_3,
                     TripsWkly_Drive, TripsWkly_Pax, TripsWkly_Bus, 
                     TripsWkly_Work, TripsWkly_Family, TripsWkly_Leisure, 
                     Relev_TravelCost, Relev_TravelTime, Relev_TravelComf, 
                     Satif_GreenSpace, Satif_CommCen, Satif_BizActiv, Satif_NearPT, Satif_FeelSecure, 
                     Rate_TPLCost, Rate_Freq, Rate_Punct, 
                     Rate_StopComf, 
                     Rate_InfoAvailStops, Rate_InfoAvailBoards, 
                     Rate_PedPath, Rate_PedXing, Rate_Lighting, Rate_Stairs,
                     WTP_Same_cat, WTP_Imp_cat, WTP_All_cat, WTP_Same_zero, WTP_Imp_zero, WTP_Same_NA, WTP_Imp_NA)

df2 <- df2 %>% drop_na()

#For reference
write.csv(df2, file = "df2.csv")

#Crosstabs====
df %>% tabyl(When_MainMovement, Sex)


df %>% tabyl(When_MainMovement, Tenure)
df %>% tabyl(When_MainMovement, Age)


df %>% 
  group_by(EduQual_rec) %>% 
  dplyr::summarise(mean = mean(WTP_Imp, na.rm = TRUE), n = n())


out_table <- df2 %>% 
  group_by(PastForgo_PoorPT_rec) %>% 
  dplyr::summarise(n = n(), meanWTPSame = mean(WTP_Same, na.rm = T), meanWTPImp = mean(WTP_Imp, na.rm = T))
tableHTML(out_table)

out_table <- df2 %>% 
  group_by(WTP_All_cat) %>% 
  dplyr::summarise(n = n(), 
                   WTPSame = mean(WTP_Same, na.rm = T), 
                   WTPImp = mean(WTP_Imp, na.rm = T),
                   Sex_Female = mean(Sex_Female, na.rm = T),    
                   Age = mean(Age, na.rm = T),
                   LivesWith_Alone = mean(LivesWith_Alone, na.rm = T),
                   DriversLic_Current = mean(DriversLic_Current, na.rm = T),
                   EduQual_rec = mean(EduQual_rec, na.rm = T),
                   Income_rec = mean(Income_rec, na.rm = T),            
                   Health_rec = mean(Health_rec, na.rm = T),
                   NeedAssist_rec = mean(NeedAssist_rec, na.rm = T),
                   YrsLived_rec = mean(YrsLived_rec, na.rm = T),
                   When_MainMovement_7to9 = mean(When_MainMovement_7to9, na.rm = T),
                   When_MainMovement_9to11 = mean(When_MainMovement_9to11, na.rm = T),
                   When_MainMovement_11to13 = mean(When_MainMovement_11to13, na.rm = T),
                   When_MainMovement_13to15 = mean(When_MainMovement_13to15, na.rm = T),
                   When_MainMovement_15to17 = mean(When_MainMovement_15to17, na.rm = T),
                   When_MainMovement_17to19 = mean(When_MainMovement_17to19, na.rm = T),
                   PTPassUse_Annual_subscription = mean(PTPassUse_Annual_subscription, na.rm = T),
                   PastForgo_PoorPT_rec = mean(PastForgo_PoorPT_rec, na.rm = T),
                   TravelTechUse_Yes = mean(TravelTechUse_Yes, na.rm = T),
                   UseDist_MainMovement2 = mean(UseDist_MainMovement2, na.rm = T),
                   TripsWkly_Drive = mean(TripsWkly_Drive, na.rm = T),
                   TripsWkly_Pax = mean(TripsWkly_Pax, na.rm = T),
                   TripsWkly_Bus = mean(TripsWkly_Bus, na.rm = T),
                   TripsWkly_Work = mean(TripsWkly_Work, na.rm = T),
                   TripsWkly_Family = mean(TripsWkly_Family, na.rm = T),
                   TripsWkly_Leisure = mean(TripsWkly_Leisure, na.rm = T),
                   Relev_TravelCost = mean(Relev_TravelCost, na.rm = T),
                   Relev_TravelTime = mean(Relev_TravelTime, na.rm = T),
                   Relev_TravelComf = mean(Relev_TravelComf, na.rm = T),
                   Satif_GreenSpace = mean(Satif_GreenSpace, na.rm = T),
                   Satif_CommCen = mean(Satif_CommCen, na.rm = T),
                   Satif_BizActiv = mean(Satif_BizActiv, na.rm = T),
                   Satif_NearPT = mean(Satif_NearPT, na.rm = T),
                   Satif_FeelSecure = mean(Satif_FeelSecure, na.rm = T),
                   Rate_TPLCost = mean(Rate_TPLCost, na.rm = T),
                   Rate_Freq = mean(Rate_Freq, na.rm = T),
                   Rate_Punct = mean(Rate_Punct, na.rm = T),
                   Rate_StopComf = mean(Rate_StopComf, na.rm = T),
                   Rate_InfoAvailStops = mean(Rate_InfoAvailStops, na.rm = T),
                   Rate_InfoAvailBoards = mean(Rate_InfoAvailBoards, na.rm = T),
                   Rate_PedPath = mean(Rate_PedPath, na.rm = T),
                   Rate_PedXing = mean(Rate_PedXing, na.rm = T),
                   Rate_Lighting = mean(Rate_Lighting, na.rm = T),
                   Rate_Stairs = mean(Rate_Stairs, na.rm = T))
tableHTML(out_table)


                   
                   
#Descriptive ====

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


Relev_TravelCost , Relev_TravelTime , Relev_TravelComf , 
Satif_GreenSpace , Satif_BizActiv , Satif_NearPT , Satif_FeelSecure ,
Rate_TPLCost , Rate_Freq , Rate_Punct ,
Rate_StopComf , 
Rate_InfoAvailStops , Rate_InfoAvailBoards ,
Rate_PedPath , Rate_PedXing , Rate_Lighting , Rate_Stairs,


#Complete only
Desc <- df2 %>% 
  select(
    Sex , Age , LivesWith_Alone , DriversLic_Current, HomeQuartier,
    #WTP
    WTP_Same, WTP_Imp, WTP_Same_cat, WTP_Imp_cat, WTP_All_cat,
    EduQual_rec , Income_rec , Health_rec , NeedAssist_rec , YrsLived_rec ,
    When_MainMovement, 
    PTPassUse , PastForgo_PoorPT_rec , 
    TravelTechUse_Yes ,
    UseDist_MainMovement2 ,
    TripsWkly_Drive , TripsWkly_Pax , TripsWkly_Bus , 
    TripsWkly_Work , TripsWkly_Family , TripsWkly_Leisure
  ) %>% # keep only columns of interest
  tbl_summary(     
    #by = Sex,
    statistic = list(all_continuous() ~ "{mean}",
                     all_categorical() ~ "{n}"),
    digits = list(all_categorical() ~ c(0, 0, 2), all_continuous() ~ 2),
    missing = NULL,#"always",
    missing_text = "(Missing)"
  ) %>%
  as_flex_table()
Desc 



#statistic = list(all_continuous() ~ "{mean} ({sd})",
#                 all_categorical() ~ "{n} / {N} ({p}%)"),





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
Satif_CommCen, Satif_GreenSpace, Satif_BizActiv, Satif_NearPT, Satif_FeelSecure
IntendPrivCar, IntendPT, IntendBike, IntendWalk
Rate_PedPath, Rate_MaintClean, Rate_PedXing, Rate_Lighting, Rate_WalkSafe, Rate_Stairs,
Relev_TravelComf, Rate_StopComf, Rate_BoardingComf, Rate_AscDesComf, Rate_InfoAvailBoards, Rate_InfoAvailStops, Rate_TPLCost, Rate_TravelTime, 
Rate_WaitTime, Rate_Freq, Rate_Punct, Rate_SafetyStop, Rate_SafetyBoard, Rate_GuideCourtesy, 
Relev_TravelCost, Relev_TravelTime

#Trips
Trips_pday, TripsWkly_Drive, TripsWkly_Pax, TripsWkly_Bus, TripsWkly_Train, TripsWkly_CycPed, 
TripsWkly_Work, TripsWkly_Family, TripsWkly_Leisure

#LIKERT====

#Factorise
#Relevance: How relevant is the X to you? 
df3 <- df2 %>% 
  mutate_at(.vars = c(32:34), 
            .funs = function(x) factor(x, 
                                       levels = c(1,2,3,4,5,6,7), 
                                       labels = c("1 (Irrelevant)","2","3","4","5", "6", "7 (Very Relevant)")))

#Satisfaction: How satisfied are you with your neighborhood as regards the presence of X?
df3 <- df3 %>% 
  mutate_at(.vars = c(35:39), 
            .funs = function(x) factor(x, 
                                       levels = c(1,2,3,4,5,6,7), 
                                       labels = c("1 (Not Satisfied)","2","3","4","5", "6", "7 (Very Satisfied)")))

#Ratings: What is your opinion on X?
df3 <- df3 %>% 
  mutate_at(.vars = c(40:49), 
            .funs = function(x) factor(x, 
                                       levels = c(1,2,3,4,5,6,7,8,9,10), 
                                       labels = c("1 (Very Poor)","2","3","4","5", "6", "7", "8", "9", "10 (Very Good)")))


summary(df3$Relev_TravelCost)
str(df3$Relev_TravelTime)
str(df3$Relev_TravelComf)

summary(df3$Satif_CommCen)
summary(df3$Satif_GreenSpace)
summary(df3$Satif_BizActiv)
summary(df3$Satif_NearPT)

summary(df3$Rate_TPLCost)
summary(df3$Rate_Freq)
summary(df3$Rate_Punct)





#Likert Plot
#Relevance: How relevant is X to you
LikertRel <- as.data.frame(df3[ , c(32:34)]) #select col from df
colnames(LikertRel) <- c("1. Travel cost",
                         "2. Travel time",
                         "3. Comfort")

LikertRel <- likert(LikertRel, grouping=df3$WTP_All_cat)
#LikertRel <- likert(LikertRel)
summary(LikertRel)
p1 <- plot(LikertRel) + theme(strip.text.x = element_text(size = 10, face = "bold")) + 
  theme(axis.title.x= element_text(size = 9))
p1




#Satisfaction
LikertSat <- as.data.frame(df3[ , c(35:39)]) #select col from df
colnames(LikertSat) <- c("1. Green space",
                         "2. Community centers",
                         "3. Business activity",
                         "4. Public transport",
                         "5. General security")

LikertSat <- likert(LikertSat, grouping=df3$WTP_All_cat)
#LikertSat <- likert(LikertSat)
summary(LikertSat)
p2 <- plot(LikertSat) + theme(strip.text.x = element_text(size = 10, face = "bold")) + 
  theme(axis.title.x= element_text(size = 9))
p2


#Combine plots
plot_grid(
  p1, p2,
  labels = c('A', 'B'),
  ncol = 1,
  rel_heights = c(3,4.3)
)

#Land pdf  9x 11


#Ratings
LikertRate <- as.data.frame(df3[ , c(40:49)]) #select col from df
colnames(LikertRate) <- c("a. LPT Cost",
                          "b. LPT Frequency",
                          "c. LPT Punctuality",
                          "d. LPT Stop Comfort",
                          "e. Information at LPT stops",
                          "f. Information on board LPT",
                          "g. Condition of pedestrian paths",
                          "f. Condition of pedestrian crossings",
                          "i. Condition of lighting",
                          "j. Condition of stairs")

LikertRate <- likert(LikertRate, grouping=df3$WTP_All_cat)
#LikertRate <- likert(LikertRate)
summary(LikertRate)
p3 <- plot(LikertRate) + theme(strip.text.x = element_text(size = 10, face = "bold")) + 
  theme(axis.title.x= element_text(size = 9))
p3

#Port  pdf 9 x 12


#histogram  ====
Sex_Female, Age, Age_cat_Low, Age_cat_Mid, Age_cat_High, 
EduQual_rec, Income_rec, Health_rec, NeedAssist_rec, LivesWith_Alone, DriversLic_Current, YrsLived_rec,
PTPassUse_Annual_subscription, PTPassUse_Single_tickets,
When_MainMovement_7to9, When_MainMovement_9to11, When_MainMovement_11to13, When_MainMovement_13to15, When_MainMovement_17to19, 
UseDist_MainMovement2,
TravelTechUse_Yes, 
IfPTNeedsTransfer_Continue_with_PT_with_the_same_frequency,
TripsWkly_Drive, TripsWkly_Pax, TripsWkly_Bus, TripsWkly_Train, TripsWkly_CycPed,
TripsWkly_Family, TripsWkly_Leisure,
Relev_TravelCost, Relev_TravelTime, Relev_TravelComf, 
Satif_GreenSpace, Satif_BizActiv, Satif_NearPT, Satif_FeelSecure,
Rate_StopComf, Rate_BoardingComf, Rate_InfoAvailBoards, Rate_InfoAvailStops, 
Rate_TPLCost, Rate_TravelTime, Rate_WaitTime, Rate_Freq, Rate_Punct,
Rate_SafetyStop, Rate_SafetyBoard, Rate_GuideCourtesy, 
Rate_PedPath, Rate_MaintClean, Rate_PedXing, Rate_Lighting, Rate_Stairs)






hist(df$Age)
summary(df$Age)

#  histogram with density plot
c1 <- ggplot(df, aes(x=WTP_Imp)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 25,)+
  geom_density(alpha=.2, fill="#FF6666") + coord_cartesian(xlim = c(0,800), ylim = c(0,0.0075))

c2 <- ggplot(df, aes(x=WTP_Same)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 25,)+
  geom_density(alpha=.2, fill="#FF6666") + coord_cartesian(xlim = c(0,800), ylim = c(0,0.0075))
c <- plot_grid(c2, c1,
               align = "v",
               ncol = 1)
c

#pivot longer for facet USE THIS!
df_WTP <- df3 %>% select(WTP_Imp,WTP_Same)

df_WTP <- df_WTP %>% pivot_longer(c(WTP_Imp, WTP_Same), names_to = "var", values_to = "value")

df_WTP %>%
  ggplot(aes(value, fill = var)) +
  geom_histogram(aes(y=..density..), binwidth = 25)+
  geom_density(alpha=.2) + facet_wrap(~var)


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


#Violin====
df %>% ggplot(aes(x = WTP_Imp, y = Sex, fill = Sex)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()
  

df %>% ggplot(aes(x = WTP_Imp, y = Status, fill = Status)) + 
    geom_violin(trim=FALSE)+
    geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = Occupation, fill = Occupation)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = EduQual, fill = EduQual)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = Health, fill = Health)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = LivesWith, fill = LivesWith)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = Tenure, fill = Tenure)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = Income, fill = Income)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = DriversLic, fill = DriversLic)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = PastForgo_PoorPT, fill = PastForgo_PoorPT)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = NeedAssist, fill = NeedAssist)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = HomeQuartier, fill = HomeQuartier)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = YrsLived, fill = YrsLived)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = TravelTechUse, fill = TravelTechUse)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = NonTechReason, fill = NonTechReason)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = PTPassUse, fill = PTPassUse)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = IfPTNeedsTransfer, fill = IfPTNeedsTransfer)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = WhatActivForgo_PoorPT, fill = WhatActivForgo_PoorPT)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = When_ActivForgo_PoorPT, fill = When_ActivForgo_PoorPT)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = When_MainMovement, fill = When_MainMovement)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()


df %>% ggplot(aes(x = WTP_Imp, y = Purpose_MainMovement, fill = Purpose_MainMovement)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

df %>% ggplot(aes(x = WTP_Imp, y = Mode_MainMovement, fill = Mode_MainMovement)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white") +
  theme_classic()

#Scatter plots====



df_demo <- df %>% select(Age, Sex, WTP_Imp,WTP_Same,
                         Sex_Male, EduQual_rec, Health_rec, NeedAssist_rec, Income_rec, Status_Pensioner, Occupation_Public_sector_employee,
                         Tenure_Homeowner, DriversLic_Current, LivesWith_Alone, YrsLived_rec)
df_m <- df %>% select(Age, Sex, WTP_Imp,WTP_Same,
                      UseDist_MainMovement, UseTime_MainMovement)
df_sat <- df %>% select(Age, Sex, WTP_Imp, WTP_Same,
                        Satif_CommCen, Satif_GreenSpace, Satif_BizActiv, Satif_NearPT, Satif_FeelSecure)
df_int <- df %>% select(Age, Sex, WTP_Imp, WTP_Same,
                        IntendPrivCar, IntendPT, IntendBike, IntendWalk)
df_rate1 <- df %>% select(Age, Sex, WTP_Imp, WTP_Same,
                          Rate_PedPath, Rate_PedXing, Rate_MaintClean, Rate_Lighting, Rate_Stairs)
df_rate2 <- df %>% select(Age, Sex, WTP_Imp, WTP_Same,
                          Rate_BoardingComf, Rate_StopComf, Rate_AscDesComf)
df_rate3 <- df %>% select(Age, Sex, WTP_Imp, WTP_Same,
                          Rate_WalkSafe, Rate_SafetyStop, Rate_SafetyBoard)
df_rate4 <- df %>% select(Age, Sex, WTP_Imp, WTP_Same,
                          Rate_InfoAvailBoards, Rate_InfoAvailStops)
df_rate5 <- df %>% select(Age, Sex, WTP_Imp, WTP_Same,
                          Rate_TPLCost, Rate_TravelTime)
df_rate6 <- df %>% select(Age, Sex, WTP_Imp, WTP_Same,
                          Rate_WaitTime, Rate_Freq, Rate_Punct,  Rate_GuideCourtesy)
df_rel <- df %>% select(Age, Sex, WTP_Imp, WTP_Same, Relev_TravelCost, Relev_TravelTime, Relev_TravelComf)
df_trip1 <- df %>% select(Age, Sex, WTP_Imp, WTP_Same,
                          Trips_pday, TripsWkly_Work, TripsWkly_Family, TripsWkly_Leisure,
                          Purpose_MainMovement_Leisure_activities, Purpose_MainMovement_Family_activities, Purpose_MainMovement_Work)
df_trip2 <- df %>% select(Age, Sex, WTP_Imp, WTP_Same,
                          TripsWkly_Drive, TripsWkly_Pax, TripsWkly_Bus, TripsWkly_Train, TripsWkly_CycPed)
df_trip3 <- df %>% select(Age, Sex, WTP_Imp, WTP_Same,
                          PTPassUse_Annual_subscription, PastForgo_PoorPT_rec, PastForgo_PoorPT_bin,
                          TravelTechUse_No, NonTechReason_Not_useful, NonTechReason_Difficult_to_use)
df_trip4 <- df %>% select(Age, Sex, WTP_Imp, WTP_Same,
                          WhatActivForgo_PoorPT_Leisure_activities, WhatActivForgo_PoorPT_Family_activities,
                          WhatActivForgo_PoorPT_Work,
                          When_ActivForgo_PoorPT_Morning_before_2pm, When_ActivForgo_PoorPT_Afternoon_between_2_and_8_pm, When_ActivForgo_PoorPT_Evening_after_8pm)
df_trip5 <- df %>% select(Age, Sex, WTP_Imp, WTP_Same,
                          When_MainMovement_7to9, When_MainMovement_9to11, When_MainMovement_11to13, When_MainMovement_13to15,
                          When_MainMovement_15to17, When_MainMovement_17to19)

plot_sc <- function(input)
  {dp <- input %>% pivot_longer(c(-Age, -Sex, -WTP_Imp, -WTP_Same), names_to = "var", values_to = "value")
  
  p1 <- dp %>% ggplot(aes(x = WTP_Imp, y = value)) +
    geom_jitter() +
    geom_smooth(method=lm,  linetype="dashed",
                color="darkred", fill="blue") +
    theme_classic() +
    facet_wrap(~ var, scales = "free")
  
  p2 <- dp %>% ggplot(aes(x = WTP_Same, y = value)) +
    geom_jitter() +
    geom_smooth(method=lm,  linetype="dashed",
                color="darkred", fill="blue") +
    theme_classic() +
    facet_wrap(~ var, scales = "free")
  
  p1 <- p1 + coord_cartesian(xlim = c(0,1000))
  p2 <- p2 + coord_cartesian(xlim = c(0,1000))
  
  plot_grid(p2, p1,
            nrow = 2,
            align = "v",
            axis = "tblr")
}



plot_sc (df_demo)
plot_sc (df_m)
plot_sc (df_sat)
plot_sc (df_int)
plot_sc (df_rate1)
plot_sc (df_rate2)
plot_sc (df_rate3)
plot_sc (df_rate4)
plot_sc (df_rate5)
plot_sc (df_rate6)
plot_sc (df_rel)
plot_sc (df_trip1)
plot_sc (df_trip2)
plot_sc (df_trip3)
plot_sc (df_trip4)
plot_sc (df_trip5)






       
#Logistic Regression====
# Fit the model

model <- glm(PastForgo_PoorPT_bin ~ WTP_Imp,
             data = df,
             family = binomial,
             na.action = na.exclude)


model <- glm( diabetes ~ glucose + mass + pregnant, 
              data = train.data, family = binomial)
summary(model)$coef

summary(model)$coef




#Linear Regression for pos WTP values====

#values
IntendBike + IntendWalk  + PTPassUse_bin
Rate_PedPath + Rate_MaintClean + Rate_PedXing + Rate_Lighting + Rate_WalkSafe + Rate_Stairs Relev_TravelComf + Rate_StopComf + Rate_BoardingComf + Rate_AscDesComf + Rate_InfoAvailBoards + Rate_InfoAvailStops + Rate_TPLCost + Rate_TravelTime + Rate_WaitTime + Rate_Freq + Rate_Punct + Rate_SafetyStop + Rate_SafetyBoard + Rate_GuideCourtesy
PTPassUse_Annual_subscription + PTPassUse_Monthly_subscription + PTPassUse_Weekly_subscription + PTPassUse_Single_tickets_paper + PTPassUse_Single_tickets_via_SMS + PTPassUse_Integrated_single_ticket + PTPassUse_Do_not_use_public_transport
IfPTNeedsTransfer_Continue_with_PT_with_the_same_frequency + IfPTNeedsTransfer_Continue_with_PT_but_with_reduced_frequency + IfPTNeedsTransfer_Use_private_car_without_changing_my_schedule + IfPTNeedsTransfer_Use_private_car_with_changed_schedules + IfPTNeedsTransfer_I_would_never_make_that_trip_ever_again
TravelTechUse_Yes
When_MainMovement_7to9 + When_MainMovement_9to11 + When_MainMovement_11to13 + When_MainMovement_13to15 + When_MainMovement_15to17 + When_MainMovement_17to19 + Purpose_MainMovement_Leisure_activities + Purpose_MainMovement_Family_activities + Purpose_MainMovement_Work
Mode_MainMovement_Car_or_Motorcycle + Mode_MainMovement_Public_Transport + Mode_MainMovement_Walk_or_cycle
Relev_TravelCost, Relev_TravelTime

#examples
ggplot(data=df, aes(x=WTP_Same, y=Sex_Male))+
    geom_jitter()+
  geom_smooth(method='lm') +
  theme_classic()
  
lm = lm(WTP_Imp ~ Rate_PedPath, data = df)
summary(lm)

lm = lm(WTP_Imp ~ Sex + Rate_PedPath, data = df)
summary(lm)

#Stepwise https://www.guru99.com/r-simple-multiple-linear-regression.html

#stepwise OLSRR
#https://olsrr.rsquaredacademy.com/articles/variable_selection.html
#https://rpubs.com/tskam/Session06

#demo only
model <- lm(WTP_Same ~ Sex_Male + Age + EduQual_rec + Income_rec + Health_rec + LivesWith_Alone + Tenure_Homeowner + DriversLic_Current + PastForgo_PoorPT_rec +
              NeedAssist_rec + YrsLived_rec, data = df)
k <- ols_step_all_possible(model)
Sex_Male + Age + EduQual_rec + Health_rec + Tenure_Homeowner + DriversLic_Current + YrsLived_rec


model <- lm(WTP_Imp ~ Sex_Male + Age + EduQual_rec + Income_rec + Health_rec + LivesWith_Alone + Tenure_Homeowner + DriversLic_Current + PastForgo_PoorPT_rec +
              NeedAssist_rec + YrsLived_rec, data = df)
k <- ols_step_all_possible(model)

Sex_Male + Age + Health_rec + Tenure_Homeowner + DriversLic_Current + YrsLived_rec

plot(k)

#Demo + satif
model <- lm(WTP_Same ~ Sex_Male + Age + EduQual_rec + Health_rec + Tenure_Homeowner + DriversLic_Current + YrsLived_rec + Satif_CommCen	+ Satif_GreenSpace	+ Satif_BizActiv	+ Satif_NearPT	+ Satif_FeelSecure, data = df)
k <- ols_step_all_possible(model)


Sex_Male + Age + EduQual_rec + Health_rec + Tenure_Homeowner + DriversLic_Current + YrsLived_rec + Satif_CommCen + Satif_NearPT + Satif_FeelSecure

model <- lm(WTP_Imp ~ Sex_Male + Age + Health_rec + Tenure_Homeowner + DriversLic_Current + YrsLived_rec + 
              Satif_CommCen	+ Satif_GreenSpace	+ Satif_BizActiv	+ Satif_NearPT	+ Satif_FeelSecure, data = df)
k <- ols_step_all_possible(model)

Sex_Male + Age + Health_rec + Tenure_Homeowner + DriversLic_Current + YrsLived_rec + Satif_GreenSpace + Satif_NearPT

#Intend
model <- lm(WTP_Same ~ 
              Sex_Male + Age + EduQual_rec + Health_rec + Tenure_Homeowner + DriversLic_Current +
              IntendBike + IntendWalk  + PTPassUse_bin +
              Satif_CommCen	+ Satif_GreenSpace	+ Satif_BizActiv	+ Satif_NearPT	+ Satif_FeelSecure,
            data = df)
k <- ols_step_all_possible(model)


#rate
model <- lm(WTP_Same ~ 
              Rate_PedPath + Rate_MaintClean + Rate_PedXing + Rate_Lighting + Rate_WalkSafe + Rate_Stairs +
              Rate_StopComf + Rate_BoardingComf + Rate_AscDesComf + Rate_InfoAvailBoards +
              Rate_InfoAvailStops + Rate_TPLCost + Rate_TravelTime + Rate_WaitTime + Rate_Freq + Rate_Punct +
              Rate_SafetyStop + Rate_SafetyBoard + Rate_GuideCourtesy
            , data = df)
k <- ols_step_best_subset(model)

#all
model <- lm(WTP_Same ~ 
              Sex_Male + Age + EduQual_rec + Income_rec + Health_rec + LivesWith_Alone + Tenure_Homeowner + DriversLic_Current + 
              Mode_MainMovement_Public_Transport + Mode_MainMovement_Walk_or_cycle + 
              When_MainMovement_7to9 + When_MainMovement_9to11 + When_MainMovement_11to13 + When_MainMovement_13to15 + When_MainMovement_15to17 + When_MainMovement_17to19 + 
              Purpose_MainMovement_Leisure_activities + Purpose_MainMovement_Family_activities + Purpose_MainMovement_Work +
              TravelTechUse_Yes +
              PastForgo_PoorPT_rec +
              IntendBike + IntendWalk  +
              Satif_CommCen + Satif_GreenSpace + Satif_BizActiv + Satif_NearPT + Satif_FeelSecure + 
              Rate_PedPath + Rate_MaintClean + Rate_PedXing + Rate_Lighting + Rate_WalkSafe + Rate_Stairs +
              Rate_StopComf + Rate_BoardingComf + Rate_AscDesComf + Rate_InfoAvailBoards +
              Rate_InfoAvailStops + Rate_TPLCost + Rate_TravelTime + Rate_WaitTime + Rate_Freq + Rate_Punct +
              Rate_SafetyStop + Rate_SafetyBoard + Rate_GuideCourtesy +
              PTPassUse_bin + PTPassUse_Annual_subscription + PTPassUse_Monthly_subscription + PTPassUse_Weekly_subscription + PTPassUse_Single_tickets_paper + PTPassUse_Single_tickets_via_SMS + PTPassUse_Integrated_single_ticket + PTPassUse_Do_not_use_public_transport + 
              IfPTNeedsTransfer_Continue_with_PT_with_the_same_frequency + IfPTNeedsTransfer_Continue_with_PT_but_with_reduced_frequency + IfPTNeedsTransfer_Use_private_car_without_changing_my_schedule + IfPTNeedsTransfer_Use_private_car_with_changed_schedules + IfPTNeedsTransfer_I_would_never_make_that_trip_ever_again
            , data = df)

model <- lm(WTP_Same ~ 
              Sex_Male + Age + EduQual_rec + Income_rec + Health_rec + LivesWith_Alone + Tenure_Homeowner + DriversLic_Current + 
              Mode_MainMovement_Public_Transport + Mode_MainMovement_Walk_or_cycle + 
              When_MainMovement_7to9 + When_MainMovement_9to11 + When_MainMovement_11to13 + When_MainMovement_13to15 + When_MainMovement_15to17 + When_MainMovement_17to19 + 
              Purpose_MainMovement_Leisure_activities + Purpose_MainMovement_Family_activities + Purpose_MainMovement_Work +
              TravelTechUse_Yes +
              PastForgo_PoorPT_rec +
              IntendBike + IntendWalk  +
              Satif_CommCen + Satif_GreenSpace + Satif_BizActiv + Satif_NearPT + Satif_FeelSecure + 
              Rate_PedPath + Rate_MaintClean + Rate_PedXing + Rate_Lighting + Rate_WalkSafe + Rate_Stairs +
              Rate_StopComf + Rate_BoardingComf + Rate_AscDesComf + Rate_InfoAvailBoards +
              Rate_InfoAvailStops + Rate_TPLCost + Rate_TravelTime + Rate_WaitTime + Rate_Freq + Rate_Punct +
              Rate_SafetyStop + Rate_SafetyBoard + Rate_GuideCourtesy +
              PTPassUse_bin + PTPassUse_Annual_subscription + PTPassUse_Monthly_subscription + PTPassUse_Weekly_subscription + PTPassUse_Single_tickets_paper + PTPassUse_Single_tickets_via_SMS + PTPassUse_Integrated_single_ticket + PTPassUse_Do_not_use_public_transport + 
              IfPTNeedsTransfer_Continue_with_PT_with_the_same_frequency + IfPTNeedsTransfer_Continue_with_PT_but_with_reduced_frequency + IfPTNeedsTransfer_Use_private_car_without_changing_my_schedule + IfPTNeedsTransfer_Use_private_car_with_changed_schedules + IfPTNeedsTransfer_I_would_never_make_that_trip_ever_again
            , data = df)




df2 <- df  %>%
  select(c("WTP_Same", "WTP_Imp",
           "Sex_Male" , "Age" , "EduQual_rec" , "Income_rec" , "Health_rec" , "LivesWith_Alone" , "Tenure_Homeowner" , "DriversLic_Current" , 
           "Mode_MainMovement_Car_or_Motorcycle",
           "Relev_TravelCost", "Relev_TravelTime",
           "When_MainMovement_9to11" , "When_MainMovement_11to13" , "When_MainMovement_13to15" , "When_MainMovement_15to17" , "When_MainMovement_17to19" , 
           "Purpose_MainMovement_Leisure_activities" , "Purpose_MainMovement_Family_activities" , "Purpose_MainMovement_Work",
           "TravelTechUse_Yes" ,
           "PastForgo_PoorPT_rec" ,
           "IntendBike" , "IntendWalk" , "IntendPrivCar", "IntendPT",
           "Satif_CommCen" , "Satif_GreenSpace" , "Satif_BizActiv" , "Satif_NearPT" , "Satif_FeelSecure" , 
           "Rate_PedPath" , "Rate_MaintClean" , "Rate_PedXing" , "Rate_Lighting" , "Rate_WalkSafe" , "Rate_Stairs" ,
           "Relev_TravelComf" , "Rate_StopComf" , "Rate_BoardingComf" , "Rate_AscDesComf" , "Rate_InfoAvailBoards" ,
           "Rate_InfoAvailStops" , "Rate_TPLCost" , "Rate_TravelTime" , "Rate_WaitTime" , "Rate_Freq" , "Rate_Punct" ,
           "Rate_SafetyStop" , "Rate_SafetyBoard" , "Rate_GuideCourtesy" ,
           "PTPassUse_Annual_subscription" ,
           "IfPTNeedsTransfer_Continue_with_PT_with_the_same_frequency"))

#Correlation====
df %>% 
  summarise(cor(WTP_Imp, EduQual_rec))

ggpairs(df, columns = c(32,34,3,85,81:83,6,8,86,89,93,95,103,106,113,118,121,135:137)) #demo
ggpairs(df, columns = c(32,34,19:23)) #satif
ggpairs(df, columns = c(32,34,26:29)) #int
ggpairs(df, columns = c(32, 34)) #WTP
ggpairs(df, columns = c(32,34,36:41, 53:66)) #rate
ggpairs(df, columns = c(32,34,43:50)) #trips
ggpairs(df, columns = c(32,34,51:52)) #rel
ggpairs(df, columns = c(32,34,74:75)) #travellength
ggpairs(df, columns = c(32,34,123:128)) #whenforgo


#Multi Corr Test
corrplot(cor(df4,use = "complete.obs"),
         tl.cex = 0.4,
         number.cex = 0.6,
         number.font = 2,
         number.digits = 1,
         order = "original", method = "number", type = "upper", tl.pos = "td")


df4 <- df2  %>%
  select(c(1:2,4:12,14,22:23,25:31))

names(df4)[names(df4) == "WTP_Same"] <- "WTP Status Quo"
names(df4)[names(df4) == "WTP_Imp"] <- "WTP Improved"
names(df4)[names(df4) == "Sex_Female"] <- "Sex: Female"
names(df4)[names(df4) == "EduQual_rec"] <- "Education"
names(df4)[names(df4) == "Income_rec"] <- "Income"
names(df4)[names(df4) == "Health_rec"] <- "Health"
names(df4)[names(df4) == "NeedAssist_rec"] <- "Need assist"
names(df4)[names(df4) == "LivesWith_Alone"] <- "Lives alone"
names(df4)[names(df4) == "DriversLic_Current"] <- "Drivers Licience"
names(df4)[names(df4) == "YrsLived_rec"] <- "Years lived" #in neighbourhood
names(df4)[names(df4) == "PTPassUse_Annual_subscription"] <- "Annual LPT ticket"
names(df4)[names(df4) == "UseDist_MainMovement2"] <- "Usual travel distance"
names(df4)[names(df4) == "TravelTechUse_Yes"] <- "Digital travel apps"
names(df4)[names(df4) == "PastForgo_PoorPT_rec_3"] <- "Forgone trip due to poor PLT"
names(df4)[names(df4) == "TripsWkly_Drive"] <- "Trips weekly mode: Car driver"
names(df4)[names(df4) == "TripsWkly_Pax"] <- "Trips weekly mode: Car pax"
names(df4)[names(df4) == "TripsWkly_Bus"] <- "Trips weekly mode: Bus"
names(df4)[names(df4) == "TripsWkly_Work"] <- "Trips weekly mode: Work"
names(df4)[names(df4) == "TripsWkly_Family"] <- "Trips weekly mode: Family"
names(df4)[names(df4) == "TripsWkly_Leisure"] <- "Trips weekly mode: Leisure"



M = cor(df4)
#ord = corrMatOrder(M, order = 'AOE')
#M2 = M[ord, ord] #if wanting AOE order

corrplot.mixed(
  M,
  lower = "number",
  upper = "circle",
  tl.pos = c("lt"),
  diag = c("l"),
  order = 'hclust',
  bg = "white",
  addgrid.col = "grey",
  lower.col = NULL,
  upper.col = NULL,
  tl.cex = 1.25,
  number.cex = 1,
  number.digits = 1,
  cl.cex = 0.9,
  tl.col = "grey50"
)
#1700x1700

#improved
corrplot(M,
         method="color",
         col = COL2('PiYG'),
         type="lower",
         order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", #Text label color and rotation
         tl.cex = 1,
         number.cex = 0.8,
         number.digits = 1,
         tl.pos = c("lt"),
         # hide correlation coefficient on the principal diagonal
         diag=T 
)

corrplot(M,
         method="color",
         type = 'lower',
         order = 'hclust',
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = 'black',
         tl.cex = 0.8,
         number.cex = 0.6,
         number.digits = 1,
         cl.ratio = 0.1,
         tl.srt = 45,
         col = COL2('PiYG', 10))

#1000x1000png


Rate_PedPath Rate_MaintClean <- pick 1 only
Rate_WalkSafe corr with many, pick this
Rate_SafetyBoard and Rate_AscDesComf and Rate_BoardingComf pick 1
Rate_Freq vs Rate_TravelTime vs Rate_WaitTime vs Rate_Punct pick 1
Mode_MainMovement_Walk_or_cycle vs Mode_MainMovement_Public_Transport #or pick drive instead
Purpose_MainMovement_Leisure_activities vs Purpose_MainMovement_Family_activities pick 1
Male vs Drivers
Imcome vs Edu
PTPassUse_Annual_subscription vs Mode_MainMovement_Public_Transport

#fuller
Sex_Female + Age + EduQual_rec + Income_rec + Health_rec + NeedAssist_rec + LivesWith_Alone + DriversLic_Current + YrsLived_rec +
  PTPassUse_Annual_subscription + PTPassUse_Single_tickets +
  When_MainMovement_7to9 + When_MainMovement_9to11 + When_MainMovement_11to13 + When_MainMovement_13to15 + When_MainMovement_17to19 + 
  UseDist_MainMovement2 +
  TravelTechUse_Yes + 
  IfPTNeedsTransfer_Continue_with_PT_with_the_same_frequency +
  TripsWkly_Drive + TripsWkly_Pax + TripsWkly_Bus + TripsWkly_Train + TripsWkly_CycPed +
  TripsWkly_Family + TripsWkly_Leisure +
  Relev_TravelCost + Relev_TravelTime + Relev_TravelComf + 
  Satif_GreenSpace + Satif_BizActiv + Satif_NearPT + Satif_FeelSecure +
  Rate_StopComf + Rate_BoardingComf + Rate_InfoAvailBoards + Rate_InfoAvailStops + 
  Rate_TPLCost + Rate_TravelTime + Rate_WaitTime + Rate_Freq + Rate_Punct +
  Rate_SafetyStop + Rate_SafetyBoard + Rate_GuideCourtesy + 
  Rate_PedPath + Rate_MaintClean + Rate_PedXing + Rate_Lighting + Rate_Stairs
, data = df2)


#Selected variables for LM after VIF and remove corr====
#USE THIS



model_same <- lm((WTP_Same) ~ 
                   Sex_Female + Age + LivesWith_Alone + DriversLic_Current +
                   EduQual_rec + Income_rec + Health_rec + NeedAssist_rec + YrsLived_rec +
                   When_MainMovement_7to9 + When_MainMovement_11to13 + When_MainMovement_13to15 + When_MainMovement_15to17 + When_MainMovement_17to19 + 
                   PTPassUse_Annual_subscription + PastForgo_PoorPT_rec + 
                   TravelTechUse_Yes +
                   UseDist_MainMovement2 +
                   TripsWkly_Drive + TripsWkly_Pax + TripsWkly_Bus + 
                   TripsWkly_Work + TripsWkly_Family + TripsWkly_Leisure +
                   Relev_TravelCost + Relev_TravelTime + Relev_TravelComf + 
                   Satif_CommCen + Satif_GreenSpace + Satif_BizActiv + Satif_NearPT + Satif_FeelSecure +
                   Rate_TPLCost + Rate_Freq + Rate_Punct +
                   Rate_StopComf + 
                   Rate_InfoAvailStops + Rate_InfoAvailBoards +
                   Rate_PedPath + Rate_PedXing + Rate_Lighting + Rate_Stairs
                 , data = df2)

model_imp <- lm((WTP_Imp) ~ 
                  Sex_Female + Age + LivesWith_Alone + DriversLic_Current +
                  EduQual_rec + Income_rec + Health_rec + NeedAssist_rec + YrsLived_rec +
                  When_MainMovement_7to9 + When_MainMovement_11to13 + When_MainMovement_13to15 + When_MainMovement_15to17 + When_MainMovement_17to19 + 
                  PTPassUse_Annual_subscription + PastForgo_PoorPT_rec + 
                  TravelTechUse_Yes +
                  UseDist_MainMovement2 +
                  TripsWkly_Drive + TripsWkly_Pax + TripsWkly_Bus + 
                  TripsWkly_Work + TripsWkly_Family + TripsWkly_Leisure +
                  Relev_TravelCost + Relev_TravelTime + Relev_TravelComf + 
                  Satif_CommCen + Satif_GreenSpace + Satif_BizActiv + Satif_NearPT + Satif_FeelSecure +
                  Rate_TPLCost + Rate_Freq + Rate_Punct +
                  Rate_StopComf + 
                  Rate_InfoAvailStops + Rate_InfoAvailBoards +
                  Rate_PedPath + Rate_PedXing + Rate_Lighting + Rate_Stairs
                , data = df2)


#model_same2 <- lm(log1p(WTP_Same) ~  swap for log
#model_imp2 <- lm(log1p(WTP_Imp) ~ 



tableHTML(ols_vif_tol(model_same))
tableHTML(ols_vif_tol(model_imp))



#choose best with step forward *reduced R2

model_sfw <- ols_step_forward_p(model_same, 
                                penter = 0.1, 
                                details = TRUE)

model_ifw <- ols_step_forward_p(model_imp, 
                                penter = 0.1, 
                                details = TRUE)


plot(model_fw)


#Best for WTP_Same
model_sbest <- lm(WTP_Same ~ Sex_Male + Age + EduQual_rec + Income_rec + Health_rec + LivesWith_Alone + DriversLic_Current +
                   PTPassUse_Annual_subscription +
                   Rate_Freq +
                   When_MainMovement_13to15 +
                   Rate_TPLCost +
                   Satif_BizActiv +
                   IntendBike +
                   Satif_FeelSecure + Satif_NearPT
                 , data = df)

model_ibest <- lm(WTP_Imp ~ Sex_Male + Age + EduQual_rec + Income_rec + Health_rec + LivesWith_Alone + DriversLic_Current +
                    PTPassUse_Annual_subscription +
                    Rate_Freq + Relev_TravelComf +
                    When_MainMovement_13to15 +
                    Rate_TPLCost +
                    Satif_BizActiv +
                    IntendBike +
                    Satif_FeelSecure + Satif_NearPT
                  , data = df)


#https://cran.r-project.org/web/packages/jtools/vignettes/summ.htmL
#LM summary and plot====
summary(lm(model_same))
summary(lm(model_imp))


anova(lm(model_same))
anova(lm(model_imp))


summ(model_same, confint = TRUE, vifs = TRUE, digits = 3)
summ(model_imp, confint = TRUE, vifs = TRUE,  digits = 3)

#VIF test
v1 <- ols_vif_tol(model_same)
v2 <- ols_vif_tol(model_imp)

tableHTML(v1)
tableHTML(v2)


broom::tidy(summary(lm(model_same, df)))

#detailed
dust(model_same) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"),
           round = 3) %>% 
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames(term = "Term", p.value = "P-value", 
                    std.error = "SE", statistic = "T-stats.",
                    estimate = "Coeff.") %>%
  sprinkle_print_method("html")

dust(model_imp) %>% 
  sprinkle(cols = c("estimate", "std.error", "statistic"),
           round = 3) %>% 
  sprinkle(cols = "p.value", fn = quote(pvalString(value))) %>% 
  sprinkle_colnames(term = "Term", p.value = "P-value", 
                    std.error = "SE", statistic = "T-stats.",
                    estimate = "Coeff.") %>%
  sprinkle_print_method("html")



#compare two models concise

export_summs(model_same, model_imp, 
             robust = "HC3",
             scale = TRUE,
             to.file = "html")

export_summs(model_same, model_imp, scale = T, robust = T,
             #error_format = "[{conf.low}-{conf.high}]", 
             error_format = "{p.value}", 
             stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
             statistics = c(N = "nobs", R2 = "r.squared"),
             bold_signif = 0.05,
             digits = 3,
             to.file = "html")

#AIC
glance(model_same,
       confint = TRUE,
       scale = TRUE, n.sd = 1,
       vifs = TRUE,
       digits = 3)

glance(model_imp,
       confint = TRUE,
       scale = TRUE, n.sd = 1,
       vifs = TRUE,
       digits = 3)

#huxreg
huxreg(model_same, model_imp, error_pos = "same")

#plots
#https://jtools.jacob-long.com/index.html

e1 <- effect_plot(model_same, pred = Rate_StopComf, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05)
e2 <- effect_plot(model_imp, pred = Rate_StopComf, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05)


plot_grid(e1, e2,
          nrow = 1,
          align = "h",
          axis = "tblr")


plot_summs(model_sbest, model_ibest, plot.distributions = TRUE)

plot_summs(model_same, model_imp,
           ci_level = 0.95,
           model.names = NULL,
           plot.distributions = F)

plot_coefs(model_same, model_imp,
           ci_level = 0.95,
           model.names = NULL,
           plot.distributions = F)

#force OLS
plot_summs(model_same, model_imp,
           ci_level = 0.95,
           robust = list(T, T),
           model.names = c("WTP - Status Quo", "WTP - Improved"))

plot_summs(model_same, model_same, model_same, model_same, robust = list(T, "HC0", "HC3", "HC5"),
           model.names = c("OLS", "HC0", "HC3", "HC5"))



plot(model_same, 1)
plot(model_imp, 1)
plot(model_same, 2)
plot(model_imp, 2)
plot(model_same, 3)
plot(model_imp, 3)
plot(model_same, 4)
plot(model_imp, 4)


#validating 
#https://bookdown.org/jimr1603/Intermediate_R_-_R_for_Survey_Analysis/testing-regression-assumptions.html#testing-the-homoscedasticity-assumption

ols_test_normality(model_same)
ols_test_normality(model_imp)
ols_test_correlation(model_same)
ols_test_correlation(model_imp)
ols_plot_resid_hist(model_same)
ols_plot_resid_hist(model_imp)


#Breusch-Pagan Test to test heteroskedasticity
lmtest::bptest(model_same)
lmtest::bptest(model_imp)

#Residual plot
df$resi <- model_same$residuals
df2$resi2 <- model_same2$residuals
ggplot(data = df2, aes(y = resi, x = Income_rec)) + geom_point(col = 'blue') + geom_abline(slope = 0)
ggplot(data = df2, aes(y = resi2, x = Income_rec)) + geom_point(col = 'blue') + geom_abline(slope = 0)

df3$resi <- model_imp$residuals
df3$resi2 <- model_imp2$residuals
ggplot(data = df3, aes(y = resi, x = Income_rec)) + geom_point(col = 'blue') + geom_abline(slope = 0)
ggplot(data = df3, aes(y = resi2, x = Income_rec)) + geom_point(col = 'blue') + geom_abline(slope = 0)

ncvTest(model_same) #homoscedasticity
ncvTest(model_imp) 

test <- dplyr::select_if(df3, is.numeric)
test <- test %>% select(!ID)
                 
                 
ols_plot_resid_fit(model_same)
ols_plot_resid_fit(model_imp)

ols_plot_resid_qq(model_same)
ols_plot_resid_qq(model_imp)

ols_plot_resid_hist(model_same)
ols_plot_resid_hist(model_imp)



#templates
summ(fit, robust = "HC3")
summ(fit, scale = TRUE)
summ(fit, scale = TRUE, n.sd = 2)
summ(fit, center = TRUE)
summ(fit, confint = TRUE, digits = 3)
summ(fit, confint = TRUE, ci.width = .5)

effect_plot(fit, pred = imdb_rating, interval = TRUE, plot.points = TRUE, 
            jitter = 0.05)

plot_summs(fit)
plot_summs(fit, robust = TRUE)
plot_summs(fit, inner_ci_level = .9)
plot_summs(fit, plot.distributions = TRUE, inner_ci_level = .9)

fit2 <- lm(metascore ~ imdb_rating + log(us_gross) + log(budget) + genre5,
           data = movies)
plot_summs(fit, fit2)
plot_summs(fit, fit2, plot.distributions = TRUE)

plot_summs(fit, fit, fit, robust = list(FALSE, "HC0", "HC5"),
           model.names = c("OLS", "HC0", "HC5"))

export_summs(fit, fit2, scale = TRUE)

export_summs(fit, fit2, scale = TRUE,
             error_format = "[{conf.low}, {conf.high}]")

export_summs(fit, fit2, scale = TRUE, to.file = "docx", file.name = "test.docx")

#CRediTas====
# Read the template back (in real life once it has been populated)


cras_table <- template_create(authors = c("Abraham Leung", "Claudia Burlando", "Tiziano Pavanini"))

write.csv(cras_table, file="cras_table.csv")

knitr::kable(cras_table)
fix(cras_table)

cras_table <- read.csv("cras_table.csv")

textfile <- tempfile()

cras_write(cras_table, "CRediT.txt", markdown = F, quiet = TRUE)

textfile



#Ideas ====

#Tobit or not (only cat)
#Linear for likert

#Cumulative distrubiton plot for both WTP

#log or not?
