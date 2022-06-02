library(meta)
library(metafor)
source('/Users/yannanlin/Desktop/YL/Submission/code/functions.R')

####################
#### preprocess ####
####################

# load data
data <- read.csv('/Users/yannanlin/Desktop/YL/Submission/data/meta_lungrads.csv')


# subset data
defined_data <- subset(data, data$type=='defined')
anytime_data <- subset(data, data$type=='anytime')
defined_data_12_34 <- subset(defined_data, defined_data$lungrads!='Lung-RADS 1-4')
anytime_data_12_34 <- subset(anytime_data, anytime_data$lungrads!='Lung-RADS 1-4' 
                                 & anytime_data$lungrads!='Lung-RADS 4')

##########################
#### generate results ####
##########################

##### Figure 2. (a) defined all lungrads #####
m1 <- metaprop_model(defined_data$n_adherent,
                     defined_data$n_total,
                     defined_data$study_lungrads)
forest_plot(m1, defined_data$index, "Defined \nAdherence rate")
sum(defined_data$n_total) #6689

#### Figure 2. (b) anytime all lungrads ####
m2 <- metaprop_model(anytime_data$n_adherent, 
                 anytime_data$n_total, 
                 anytime_data$study_lungrads)
forest_plot(m2, anytime_data$index, "Anytime \nAdherence Rate")
sum(anytime_data$n_total) #5085

#### Figure 2. (c) defined 1-2 and 3-4
m3 <- metaprop_model_sub(defined_data_12_34$n_adherent, 
                     defined_data_12_34$n_total, 
                     defined_data_12_34$study_lungrads, 
                     defined_data_12_34$lungrads)
#Test for subgroup differences (random effects model):
#                       Q d.f. p-value
# Between groups   7.89    1  0.0050
forest_plot_sub(m3, defined_data_12_34$i..study, "Defined \nAdherence Rate")
sum(defined_data_12_34$n_total) #3985
sum(defined_data_12_34$n_total[defined_data_12_34$lungrads=='Lung-RADS 1-2']) #3428
sum(defined_data_12_34$n_total[defined_data_12_34$lungrads=='Lung-RADS 3-4']) #557


#### Figure 2. (d) anytime 1-2 and 3-4
m4 <- metaprop_model_sub(anytime_data_12_34$n_adherent, 
                         anytime_data_12_34$n_total, 
                         anytime_data_12_34$study_lungrads, 
                         anytime_data_12_34$lungrads)
# Test for subgroup differences (random effects model):
#                      Q d.f. p-value
# Between groups   11.20    1  0.0008
forest_plot_sub(m4, anytime_data_12_34$study, "Anytime \nAdherence Rate")
sum(anytime_data_12_34$n_total) #4375
sum(anytime_data_12_34$n_total[anytime_data_12_34$lungrads=='Lung-RADS 1-2']) #3847
sum(anytime_data_12_34$n_total[anytime_data_12_34$lungrads=='Lung-RADS 3-4']) #528

# funnel plots
funnel(m1, studlab = FALSE)
funnel(m2, studlab = FALSE)
funnel(m3, studlab = FALSE)
funnel(m4, studlab = FALSE)

# Egger's test
# defined - 1-4
egger_test(defined_data$n_adherent, defined_data$n_total) # p = 0.6089
# undefiend - 1-4
egger_test(anytime_data$n_adherent, anytime_data$n_total) # p = 0.5278
# defined - 12_34
egger_test(defined_data_12_34$n_adherent, defined_data_12_34$n_total) # p = 0.3930
# anytime - 12_34
egger_test(anytime_data_12_34$n_adherent, anytime_data_12_34$n_total) # p = 0.7712

####################################################################################

# definition of adherence - within 3 months of recommended date
def_data <- subset(data, data$def_3_mo=="Yes" & data$lungrads!='Lung-RADS 1-4')
m_def <- metaprop_model_sub(def_data$n_adherent,
                            def_data$n_total,
                            def_data$study_lungrads,
                            def_data$lungrads)
forest_plot_sub(m_def, def_data$study, "Defined \nAdherence Rate")
sum(def_data$n_total) #2836
sum(def_data$n_total[def_data$lungrads=='Lung-RADS 1-2']) #2414
sum(def_data$n_total[def_data$lungrads=='Lung-RADS 3-4']) #422
funnel(m_def)
egger_test(def_data$n_adherent, def_data$n_total) #  p = 0.7485

###################################################################################

# only journal articles

# subset data
defined_article_data <- subset(data, data$article_type=='Journal article' & 
                                 data$type=='defined')
anytime_article_data <- subset(data, data$article_type=='Journal article' & 
                                   data$type=='anytime')
defined_article_data_12_34 <- subset(defined_article_data, 
                                     defined_article_data$lungrads!='Lung-RADS 1-4')
anytime_article_data_12_34 <- subset(anytime_article_data, 
                                       anytime_article_data$lungrads!='Lung-RADS 1-4' 
                                       & anytime_article_data$lungrads!='Lung-RADS 4')

# defined all lungrads

m_defined_article_14 <- metaprop_model(defined_article_data$n_adherent,
                                       defined_article_data$n_total,
                                       defined_article_data$study_lungrads)
forest_plot(m_defined_article_14, defined_article_data$index, "Defined \nAdherence rate")
sum(defined_article_data$n_total) #3582

# anytime all lungrads

m_anytime_article_14 <- metaprop_model(anytime_article_data$n_adherent,
                                       anytime_article_data$n_total,
                                       anytime_article_data$study_lungrads)
forest_plot(m_anytime_article_14, anytime_article_data$index, "Anytime \nAdherence rate")
sum(anytime_article_data$n_total) # 4636

# defined 12 34
m_defined_article_12_34 <- metaprop_model_sub(defined_article_data_12_34$n_adherent,
                                              defined_article_data_12_34$n_total,
                                              defined_article_data_12_34$study_lungrads,
                                              defined_article_data_12_34$lungrads)
forest_plot_sub(m_defined_article_12_34, defined_article_data_12_34$study, "Defined \nAdherence rate")
sum(defined_article_data_12_34$n_total) #3582
sum(defined_article_data_12_34$n_total[defined_article_data_12_34$lungrads=='Lung-RADS 1-2']) #3084
sum(defined_article_data_12_34$n_total[defined_article_data_12_34$lungrads=='Lung-RADS 3-4']) #534

# undefiend 12 34
m_anytime_article_12_34 <- metaprop_model_sub(anytime_article_data_12_34$n_adherent,
                                              anytime_article_data_12_34$n_total,
                                              anytime_article_data_12_34$study_lungrads,
                                              anytime_article_data_12_34$lungrads)
forest_plot_sub(m_anytime_article_12_34, anytime_article_data_12_34$study, "Anytime \nAdherence rate")
sum(anytime_article_data_12_34$n_total) #3926
sum(anytime_article_data_12_34$n_total[anytime_article_data_12_34$lungrads=='Lung-RADS 1-2']) #3446
sum(anytime_article_data_12_34$n_total[anytime_article_data_12_34$lungrads=='Lung-RADS 3-4']) #480

# funnel
funnel(m_defined_article_14)
funnel(m_anytime_article_14)
funnel(m_defined_article_12_34)
funnel(m_anytime_article_12_34)

# egger's test
# defined 1-4
egger_test(defined_article_data$n_adherent, defined_article_data$n_total) # p = 0.4083
# anytime 1-4
egger_test(anytime_article_data$n_adherent, anytime_article_data$n_total) # p = 0.0151
# defined 12 34
egger_test(defined_article_data_12_34$n_adherent, defined_article_data_12_34$n_total) # p = 0.4083
# anytime 12 34
egger_test(anytime_article_data_12_34$n_adherent, anytime_article_data_12_34$n_total) # p= 0.0818


#### additional analysis in response to Letter to Editor #########
defined_data_12 <- subset(defined_data_12_34, defined_data_12_34$lungrads == 'Lung-RADS 1-2')
defined_data_34 <- subset(defined_data_12_34, defined_data_12_34$lungrads == 'Lung-RADS 3-4')
anytime_data_12 <- subset(anytime_data_12_34, anytime_data_12_34$lungrads == 'Lung-RADS 1-2')
anytime_data_34 <- subset(anytime_data_12_34, anytime_data_12_34$lungrads == 'Lung-RADS 3-4')

# Classic fail-safe N
# defined 1-4
fsn_classic(defined_data$n_adherent, defined_data$n_total)
# anytime 1-4 
fsn_classic(anytime_data$n_adherent, anytime_data$n_total)
# defined 1-2
fsn_classic(defined_data_12$n_adherent, defined_data_12$n_total)
# defined 3-4
fsn_classic(defined_data_34$n_adherent, defined_data_34$n_total)
# anytime 1-2
fsn_classic(anytime_data_12$n_adherent, anytime_data_12$n_total)
# anytime 3-4
fsn_classic(anytime_data_34$n_adherent, anytime_data_34$n_total)

# Orwin's fail-safe N
fsn_orwin(defined_data$n_adherent, defined_data$n_total, target_adherence_rate = 0.05)
# anytime 1-4 
fsn_orwin(anytime_data$n_adherent, anytime_data$n_total)
# defined 1-2
fsn_orwin(defined_data_12$n_adherent, defined_data_12$n_total)
# defined 3-4
fsn_orwin(defined_data_34$n_adherent, defined_data_34$n_total)
# anytime 1-2
fsn_orwin(anytime_data_12$n_adherent, anytime_data_12$n_total)
# anytime 3-4
fsn_orwin(anytime_data_34$n_adherent, anytime_data_34$n_total)

# begg test
# defined 1-4
begg_test(defined_data$n_adherent, defined_data$n_total)
# anytime 1-4 
begg_test(anytime_data$n_adherent, anytime_data$n_total)
# defined 1-2
begg_test(defined_data_12$n_adherent, defined_data_12$n_total)
# defined 3-4
begg_test(defined_data_34$n_adherent, defined_data_34$n_total)
# anytime 1-2
begg_test(anytime_data_12$n_adherent, anytime_data_12$n_total)
# anytime 3-4
begg_test(anytime_data_34$n_adherent, anytime_data_34$n_total)

# trim and fill method
# defined 1-4
trim_test(defined_data$n_adherent, defined_data$n_total) 
funnel(m1, studlab = FALSE)
# anytime 1-4 
trim_test(anytime_data$n_adherent, anytime_data$n_total)
funnel(m2, studlab = FALSE)
# defined 1-2
trim_test(defined_data_12$n_adherent, defined_data_12$n_total)

# defined 3-4
trim_test(defined_data_34$n_adherent, defined_data_34$n_total)
# anytime 1-2
trim_test(anytime_data_12$n_adherent, anytime_data_12$n_total)
# anytime 3-4
trim_test(anytime_data_34$n_adherent, anytime_data_34$n_total)


# hypothesis test - fixed effect
# defined 1-4
hypothesis_test(defined_data$n_adherent, defined_data$n_total, type = 'FE')
# anytime 1-4 
hypothesis_test(anytime_data$n_adherent, anytime_data$n_total, type = 'FE')
# defined 1-2
hypothesis_test(defined_data_12$n_adherent, defined_data_12$n_total, type = 'FE')
# defined 3-4
hypothesis_test(defined_data_34$n_adherent, defined_data_34$n_total, type = 'FE')
# anytime 1-2
hypothesis_test(anytime_data_12$n_adherent, anytime_data_12$n_total, type = 'FE')
# anytime 3-4
hypothesis_test(anytime_data_34$n_adherent, anytime_data_34$n_total, type = 'FE')

# hypothesis test - random effect
# defined 1-4
hypothesis_test(defined_data$n_adherent, defined_data$n_total, type = 'REML')
# anytime 1-4 
hypothesis_test(anytime_data$n_adherent, anytime_data$n_total, type = 'REML')
# defined 1-2
hypothesis_test(defined_data_12$n_adherent, defined_data_12$n_total, type = 'REML')
# defined 3-4
hypothesis_test(defined_data_34$n_adherent, defined_data_34$n_total, type = 'REML')
# anytime 1-2
hypothesis_test(anytime_data_12$n_adherent, anytime_data_12$n_total, type = 'REML')
# anytime 3-4
hypothesis_test(anytime_data_34$n_adherent, anytime_data_34$n_total, type = 'REML')




dat <- escalc(measure="PFT", xi=defined_data_34$n_adherent,ni = defined_data$n_total) #PLO: logit transformed proportion. #https://rdrr.io/cran/metafor/man/escalc.html
res <- rma(yi, vi, data=dat, method = 'REML')
trimfill(res)
print(res)


metaprop(defined_data$n_adherent,
         defined_data$n_total,
         comb.fixed=T, comb.random=F,
         method="Inverse", sm = "PFT")


















