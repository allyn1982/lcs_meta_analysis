####### table 1 #######
# median # of included patients 
n_1 = c(901, 1444, 260, 268, 631, 3428, 32, 1241, 166, 275, 259,
        680, 1181, 477, 370, 554, 825, 421, 145, 242, 319, 1954,
        668, 2274)
min(n_1) #32
max(n_1) #3248
median(n_1) #449

# sex 
# number of studies reported sex distribution = 20
# number of studies with more male patiens = 15

# race 
# number of studies reported race distribution = 17
# number of studies with >50% white patiens = 12 (Rodriguez not counted as only specified Black proportion)

# smoking status
# number of studies reported smoking status distribution = 16
# number of studies with more current smokers = 12

# insurance
# number of studies reported insurance distribution = 6
# number of studies with most patients using Medicare/Medicaid = 5

# mean age
age_mean <- c(65.5, 64.8, 59, 64.1, 64, 64.3,60,63,60,64.1)
length(age_mean) #10
min(age_mean) #59
max(age_mean) #65.5

# median age
age_median <- c(66, 64, 66, 67, 63)
length(age_median) #5
min(age_median) #63
max(age_median) #67


##### table 2 #####
# median # of included patients 
n =  c(901, 1444, 260, 268, 631, 1546, 32, 511, 166, 32, 
      259, 133, 663, 477, 271, 42, 629, 258, 145, 179,
      319, 1954, 668, 375, 102, 2274)
median(n) #295
min(n) #32
max(n) #2274

# sex 
# number of studies reported sex distribution = 13
# number of studies with more male patiens = 11

# race 
# number of studies reported race distribution = 13
# number of studies with >50% white patiens = 11

# smoking status
# number of studies reported smoking status distribution = 9
# number of studies with more current smokers = 7

# insurance
# number of studies reported insurance distribution = 5
# number of studies with most patients using Medicare/Medicaid = 4

# mean age
age_mean <- c(64.8,64.1,64.3, 64, 64.1)
length(age_mean) #5
max(age_mean)#64.8
min(age_mean) #64

# median age
age_median <- c(66, 63)
