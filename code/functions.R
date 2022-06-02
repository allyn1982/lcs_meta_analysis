# function to generate metaprop instance - overall 
metaprop_model <- function(adherent_n, total_n, studlab){
  return(metaprop(adherent_n, 
                  total_n, 
                  studlab = studlab, 
                  fixed=F, random=T,
                  method="Inverse", sm = "PFT",
                  method.ci = 'CP')
  ) 
}

# function to generate metraprop instance - subgroup
metaprop_model_sub <- function(adherent_n, total_n, studlab, subgroup){
  return(metaprop(adherent_n, 
                  total_n, 
                  studlab = studlab, 
                  fixed=F, random=T,
                  subgroup = subgroup,
                  method="Inverse", sm = "PFT")
  ) 
}

# function to generate forest plot - overall
forest_plot <- function(metaprop_model, sort_index, adherence_type){
  return(forest(metaprop_model, 
                 overall = TRUE,
                 sortvar = sort_index, 
                 col.by="black",
                 fixed = FALSE,
                 random = TRUE,
                 leftcols = c("studlab", "effect","ci"), 
                 leftlabs = c("Study", adherence_type, "95% CI"),
                 rightcols = c("w.random"),
                 rightlabs = c("Weight"),
                 resid.hetstat = F,
                 addfit=FALSE,
                 print.Q = T,
                 print.pval.Q = T,
                 print.tau2 = T
  ))
}

# function to generate forest plot - subgroup
forest_plot_sub <- function(metaprop_model_sub, sort_index, adherence_type){
  return(forest(metaprop_model_sub, 
                overall = TRUE,
                sortvar = sort_index, 
                col.by="black",
                fixed = FALSE,
                random = TRUE,
                leftcols = c("studlab", "effect","ci"), 
                leftlabs = c("Study", adherence_type, "95% CI"),
                rightcols = c("w.random"),
                rightlabs = c("Weight"),
                resid.hetstat = F,
                addfit=FALSE,
                print.Q = T,
                print.pval.Q = T,
                print.subgroup = F, # do not print "lungrads="
                test.subgroup.random = T,
                label.test.subgroup.random = 'Test for subgroup differences: ',
                print.tau2 = T
  ))
}

# function for egger's test
egger_test <- function(adherent_n, total_n){
  dat <- escalc(measure="PFT", xi=adherent_n, ni=total_n) #PLO: logit transformed proportion. #https://rdrr.io/cran/metafor/man/escalc.html
  res <- rma(yi, vi , data=dat) 
  return(regtest(res))
  
}

# function for meta regression
meta_regression <- function(adherent_n, total_n, covariate){
  dat <- escalc(measure="PFT", xi=adherent_n, ni=total_n) #PLO: logit transformed proportion. #https://rdrr.io/cran/metafor/man/escalc.html
  res <- rma(yi, vi, mods = ~ covariate, data=dat)
  return(res)
}

# function for classic fail-safe n
fsn_classic <- function(adherent_n, total_n){
  dat <- escalc(measure="PFT", xi=adherent_n, ni=total_n) #PLO: logit transformed proportion. #https://rdrr.io/cran/metafor/man/escalc.html
  res <- rma(yi, vi, data=dat)
  test <- fsn(yi, vi, data = dat, type = "Rosenthal")
  print(test)
}

# function for Orwin's fail-safe N
fsn_orwin <- function(adherent_n, total_n, target_adherence_rate = 0.05){
  dat <- escalc(measure="PFT", xi=adherent_n, ni=total_n) #PLO: logit transformed proportion. #https://rdrr.io/cran/metafor/man/escalc.html
  res <- rma(yi, vi, data=dat)
  fsn_defined_12_34 <- fsn(yi, vi, data = dat, type = "Orwin",
                           target = target_adherence_rate)
  print(fsn_defined_12_34)
}

# function for begg's test
begg_test <- function(adherent_n, total_n){
  dat <- escalc(measure="PFT", xi=adherent_n, ni=total_n) #PLO: logit transformed proportion. #https://rdrr.io/cran/metafor/man/escalc.html
  res <- rma(yi, vi, data=dat)
  test <- ranktest(res)
  print(test)
}

# trim fill method
trim_test <- function(adherent_n, total_n){
  dat <- escalc(measure="PFT", xi=adherent_n, ni=total_n) #PLO: logit transformed proportion. #https://rdrr.io/cran/metafor/man/escalc.html
  res <- rma(yi, vi, data=dat, method = 'REML')
  test <- trimfill(res, comb.random = TRUE, fill = TRUE)
  print(funnel(test, legend= TRUE))
  print(test)
  
}

# hypothesis testing
hypothesis_test <- function(adherent_n, total_n, type = 'FE'){
  dat <- escalc(measure="PFT", xi=adherent_n, ni=total_n) #PLO: logit transformed proportion. #https://rdrr.io/cran/metafor/man/escalc.html
  res <- rma(yi, vi, data=dat, method = type)
  print(res)
}
  



