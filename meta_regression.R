library(meta)
library(metafor)
library(lme4)
source('C:/Users/yanna/Desktop/Submission/code/functions.R')

####################
#### preprocess ####
####################

# load data
data <- read.csv('C:/Users/yanna/Desktop/Submission/data/meta_regression.csv', header = T)

data_defined <- subset(data, data$type=='defined')
data_anytime <- subset(data, data$type=='anytime')

##########################
#### generate results ####
##########################

# defined 
# lung-rads
lr_defined <-meta_regression(data_defined$n_adherent, data_defined$n_total, data_defined$lungrads)
lr_defined
# setting
setting_defined <- meta_regression(data_defined$n_adherent, data_defined$n_total, data_defined$setting)
setting_defined
# coordinator
coord_defined <- meta_regression(data_defined$n_adherent, data_defined$n_total, data_defined$coordinator)
coord_defined
# sdm
sdm_defined <- meta_regression(data_defined$n_adherent, data_defined$n_total, data_defined$SDM)
sdm_defined
# cessation
cessation_defined <- meta_regression(data_defined$n_adherent, data_defined$n_total, data_defined$smoking.cessation)
cessation_defined
# intervention
intervention_defined <- meta_regression(data_defined$n_adherent, data_defined$n_total, data_defined$intervention)
intervention_defined
# type
type_defined <- meta_regression(data_defined$n_adherent, data_defined$n_total, data_defined$publication)
type_defined 

# all
dat_1 <- escalc(measure="PFT", xi=data_defined$n_adherent, ni=data_defined$n_total)
res_1 <- rma(yi, vi, mods = ~ data_defined$lungrads +
                data_defined$setting+
                data_defined$coordinator+
                data_defined$SDM+
                data_defined$smoking.cessation+
                data_defined$intervention+
                data_defined$publication, data=dat_1)
res_1   

# unsepcified
# lung-rads
lr_anytime <- meta_regression(data_anytime$n_adherent, data_anytime$n_total, data_anytime$lungrads)
lr_anytime
# setting
setting_anytime <- meta_regression(data_anytime$n_adherent, data_anytime$n_total, data_anytime$setting)
setting_anytime
# coordinator
coord_anytime <- meta_regression(data_anytime$n_adherent, data_anytime$n_total, data_anytime$coordinator)
coord_anytime
# sdm
sdm_anytime <- meta_regression(data_anytime$n_adherent, data_anytime$n_total, data_anytime$SDM)
sdm_anytime
# cessation
cessation_anytime <- meta_regression(data_anytime$n_adherent, data_anytime$n_total, data_anytime$smoking.cessation)
cessation_anytime
# intervention
intervention_anytime <- meta_regression(data_anytime$n_adherent, data_anytime$n_total, data_anytime$intervention)
intervention_anytime
# type
type_anytime <- meta_regression(data_anytime$n_adherent, data_anytime$n_total, data_anytime$publication)
type_anytime
# all
dat_2 <- escalc(measure="PFT", xi=data_anytime$n_adherent, ni=data_anytime$n_total)
res_2 <- rma(yi, vi, mods = ~ data_anytime$lungrads +
               data_anytime$setting+
               data_anytime$coordinator+
               data_anytime$SDM+
               data_anytime$smoking.cessation+
               data_anytime$intervention+
               data_anytime$publication, data=dat_2)
res_2  


# subgroup analysis for mention of smoking cessation
smoke <- metaprop_model_sub(data_defined$n_adherent,
   data_defined$n_total,
   data_defined$study_lungrads,
   data_defined$smoking.cessation)
forest(smoke)

