library(meta)
library(metafor)
source('C:/Users/yanna/Desktop/Submission/code/functions.R')

####################
#### preprocess ####
####################

# load data
data <- read.csv('C:/Users/yanna/Desktop/Submission/data/meta_other_variables.csv')

sex_defined_14 <- subset(data, data$type=='defined'&data$variable=='sex' & data$lungrads=='Lung-RADS 1-4')
sex_anytime_12 <- subset(data, data$type=='anytime'&data$variable=='sex' & data$lungrads=='Lung-RADS 1-2')

race_defined_14 <- subset(data, data$type=='defined'&data$variable=='race' & data$lungrads=='Lung-RADS 1-4')
race_defined_12 <- subset(data, data$type=='defined'&data$variable=='race' & data$lungrads=='Lung-RADS 1-2')
race_anytime_14 <- subset(data, data$type=='anytime'&data$variable=='race' & data$lungrads=='Lung-RADS 1-4')
race_anytime_12 <- subset(data, data$type=='anytime'&data$variable=='race' & data$lungrads=='Lung-RADS 1-2')

smoke_defined_14 <- subset(data, data$type=='defined'&data$variable=='smoking' & data$lungrads=='Lung-RADS 1-4')

##########################
#### generate results ####
##########################

# sex - defined 14
m_sex_defined_14 <- metaprop_model_sub(sex_defined_14$n_adherent,
                               sex_defined_14$n_total,
                               sex_defined_14$ï..study_lungrads,
                               sex_defined_14$category)
forest_plot_sub(m_sex_defined_14, sex_defined_14$ï..study_lungrads, "Defined \nAdherence rate")
sum(sex_defined_14$n_total) #2079
sum(sex_defined_14$n_total[sex_defined_14$category=='Sex: Male']) #1255
sum(sex_defined_14$n_total[sex_defined_14$category=='Sex: Female']) #824


# race - defined 14
m_race_defined_14 <- metaprop_model_sub(race_defined_14$n_adherent,
                                        race_defined_14$n_total,
                                        race_defined_14$ï..study_lungrads,
                                        race_defined_14$category)
forest_plot_sub(m_race_defined_14, race_defined_14$ï..study_lungrads, "Defined \nAdherence rate")
sum(race_defined_14$n_total) #1607
sum(race_defined_14$n_total[race_defined_14$category=='Race: White']) #1133
sum(race_defined_14$n_total[race_defined_14$category=='Race: Non-White']) #474

# race - defined_12
m_race_defined_12 <- metaprop_model_sub(race_defined_12$n_adherent,
                                        race_defined_12$n_total,
                                        race_defined_12$ï..study_lungrads,
                                        race_defined_12$category)
forest_plot_sub(m_race_defined_12, race_defined_12$ï..study_lungrads, "Defined \nAdherence rate")
sum(race_defined_12$n_total) #1218
sum(race_defined_12$n_total[race_defined_12$category=='Race: White']) #973
sum(race_defined_12$n_total[race_defined_12$category=='Race: Non-White']) #245


# smoke_defined_14
m_smoke_defined_14 <- metaprop_model_sub(smoke_defined_14$n_adherent,
                                         smoke_defined_14$n_total,
                                         smoke_defined_14$ï..study_lungrads,
                                         smoke_defined_14$category)
forest_plot_sub(m_smoke_defined_14, smoke_defined_14$ï..study_lungrads, "Defined \nAdherence rate")
sum(smoke_defined_14$n_total) #2079
sum(smoke_defined_14$n_total[smoke_defined_14$category=='Smoking status: Current smokers']) #1051
sum(smoke_defined_14$n_total[smoke_defined_14$category=='Smoking status: Former smokers']) #1028
  
# funnel plots
funnel(m_sex_defined_14, studlab = F)
funnel(m_race_defined_14, studlab = F)
funnel(m_race_defined_12, studlab = F)
funnel(m_smoke_defined_14, studlab = F)

# egger's test
# sex - defined 14
egger_test(sex_defined_14$n_adherent, sex_defined_14$n_total) #  p = 0.3922
# race - defined 14
egger_test(race_defined_14$n_adherent, race_defined_14$n_total) #  p = 0.4812
# race - defined 12
egger_test(race_defined_12$n_adherent, race_defined_12$n_total) # p = 0.4846
# smoke - defined 14
egger_test(smoke_defined_14$n_adherent, smoke_defined_14$n_total) #  p = 0.6442








