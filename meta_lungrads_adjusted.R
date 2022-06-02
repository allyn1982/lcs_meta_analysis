library(meta)
library(metafor)
source('C:/Users/yanna/Desktop/Submission/code/functions.R')

####################
#### preprocess ####
####################

# load data
data <- read.csv('C:/Users/yanna/Desktop/Submission/data/meta_lungrads_adjusted.csv')

# subset data
defined_data <- subset(data, data$type=='defined')
anytime_data <- subset(data, data$type=='anytime')

# defined
# adjust for average age 
age_data <- subset(defined_data, !is.na(defined_data$mean_age))
dat <- escalc(measure="PFT", xi=age_data$n_adherent, ni=age_data$n_total) 
age_res <- rma(yi, vi, 
               mods = ~ age_data$mean_age, 
               data=dat, 
               slab = paste(age_data$ï..study, age_data$lungrads, sep="\n"))
age_res
forest(age_res)

# adjust for percent of female 
female_data <- subset(defined_data, !is.na(defined_data$percent_female))
dat <- escalc(measure="PFT", xi=female_data$n_adherent, ni=female_data$n_total) 
female_res <- rma(yi, vi, 
               mods = ~ female_data$percent_female, 
               data=dat, 
               slab = paste(female_data$ï..study, female_data$lungrads, sep="\n"))
female_res
forest(female_res)

# adjust for percent of white 
white_data  <- subset(defined_data, !is.na(defined_data$percent_white))
dat <- escalc(measure="PFT", xi=white_data$n_adherent, ni=white_data$n_total) 
white_res <- rma(yi, vi, 
                  mods = ~ white_data$percent_white, 
                  data=dat, 
                  slab = paste(white_data$ï..study, white_data$lungrads, sep="\n"))
white_res
forest(white_res)

# adjust for percent of former smoker 
former_data  <- subset(defined_data, !is.na(defined_data$percent_former_smoker))
dat <- escalc(measure="PFT", xi=former_data$n_adherent, ni=former_data$n_total) 
former_res <- rma(yi, vi, 
                 mods = ~ former_data$percent_former_smoker, 
                 data=dat, 
                 slab = paste(former_data$ï..study, former_data$lungrads, sep="\n"))
former_res
forest(former_res)

# adjust for percent of lungrads 12 
lr12_data  <- subset(defined_data, !is.na(defined_data$percent_lr_12))
dat <- escalc(measure="PFT", xi=lr12_data$n_adherent, ni=lr12_data$n_total) 
lr12_res <- rma(yi, vi, 
                  mods = ~ lr12_data$percent_lr_12, 
                  data=dat, 
                  slab = paste(lr12_data$ï..study, lr12_data$lungrads, sep="\n"))
lr12_res
forest(lr12_res)

# adjust for all except mean age (otherwise error: # of predictors large than # of observations)
all_data <- subset(defined_data, !is.na(defined_data$percent_former_smoker) &
                     !is.na(defined_data$percent_female) &
                     !is.na(defined_data$percent_white) &
                     !is.na(defined_data$percent_lr_12)
                     )
dat <- escalc(measure="PFT", xi=all_data$n_adherent, ni=all_data$n_total) 
all_res <- rma(yi, vi, 
                  mods = ~ all_data$percent_former_smoker + 
                 all_data$percent_lr_12+
                 all_data$percent_female + 
                 all_data$percent_white,
                  data=dat, 
                  slab = paste(all_data$ï..study, all_data$lungrads, sep="\n"))
all_res
forest(all_res)
###################################################
# anytime
age_data <- subset(anytime_data, !is.na(anytime_data$mean_age))
dat <- escalc(measure="PFT", xi=age_data$n_adherent, ni=age_data$n_total) 
age_res <- rma(yi, vi, 
               mods = ~ age_data$mean_age + age_data$percent_lr_12, 
               data=dat, 
               slab = paste(age_data$ï..study, age_data$lungrads, sep="\n"))
age_res
forest(age_res)
# forest(age_res,
#        showweights=FALSE, 
#        header= c('Study', 'Defined adherence rates [95% CI]')
#        )

# adjust for percent of female 
female_data <- subset(anytime_data, !is.na(anytime_data$percent_female))
dat <- escalc(measure="PFT", xi=female_data$n_adherent, ni=female_data$n_total) 
female_res <- rma(yi, vi, 
                  mods = ~ female_data$percent_female, 
                  data=dat, 
                  slab = paste(female_data$ï..study, female_data$lungrads, sep="\n"))
female_res
forest(female_res)

# adjust for percent of white 
white_data  <- subset(anytime_data, !is.na(anytime_data$percent_white))
dat <- escalc(measure="PFT", xi=white_data$n_adherent, ni=white_data$n_total) 
white_res <- rma(yi, vi, 
                 mods = ~ white_data$percent_white, 
                 data=dat, 
                 slab = paste(white_data$ï..study, white_data$lungrads, sep="\n"))
white_res
forest(white_res)

# adjust for percent of former smoker 
former_data  <- subset(anytime_data, !is.na(anytime_data$percent_former_smoker))
dat <- escalc(measure="PFT", xi=former_data$n_adherent, ni=former_data$n_total) 
former_res <- rma(yi, vi, 
                  mods = ~ former_data$percent_former_smoker, 
                  data=dat, 
                  slab = paste(former_data$ï..study, former_data$lungrads, sep="\n"))
former_res
forest(former_res)

# adjust for percent of lungrads 12 
lr12_data  <- subset(anytime_data, !is.na(anytime_data$percent_lr_12))
dat <- escalc(measure="PFT", xi=lr12_data$n_adherent, ni=lr12_data$n_total) 
lr12_res <- rma(yi, vi, 
                mods = ~ lr12_data$percent_lr_12, 
                data=dat, 
                slab = paste(lr12_data$ï..study, lr12_data$lungrads, sep="\n"))
lr12_res
forest(lr12_res)

# adjust for all except mean age (otherwise error: # of predictors large than # of observations)
all_data <- subset(anytime_data, !is.na(anytime_data$percent_former_smoker) &
                     !is.na(anytime_data$percent_female) &
                     !is.na(anytime_data$percent_white) &
                     !is.na(anytime_data$percent_lr_12)
)
dat <- escalc(measure="PFT", xi=all_data$n_adherent, ni=all_data$n_total) 
all_res <- rma(yi, vi, 
               mods = ~ all_data$percent_former_smoker + 
                 all_data$percent_lr_12+
                 all_data$percent_female + 
                 all_data$percent_white,
               data=dat, 
               slab = paste(all_data$ï..study, all_data$lungrads, sep="\n"))
all_res
forest(all_res)