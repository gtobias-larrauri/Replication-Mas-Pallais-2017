library(xtable) #Tables to Latex
library(stargazer) #Not used I think
library(haven) #Import data from stata
library(patchwork) #Not used I think
library(ggplot2) #Not used I think
library(dplyr) #To collapse data
library(nnet) #Not used I think
############################### SET WORKING DIRECTORY TO THE REPLICATION FOLDER:

setwd("")

###############################################################################
data <- read_dta("experimentdata.dta")
source("Functions_replication.R")
#Take out the abs file path...It's wrong...

#EACH MAIN SPECIFICATION FROM PAPER
m_1flex_schedule <- data[data$treatment_number==1,]#FLEX SCHEDULE
m_2flex_hours <- data[data$treatment_number==4,]#FLEX HOURS
m_3work_home <- data[data$treatment_number==3,]#WORK FROM HOME
m_4comb_flex <- data[data$treatment_number==5,]#COMBINED FLEX
m_5emp_disc <- data[data$treatment_number==2,]#EMPLOYER DISCRETION 
#This for the special alpha bootstrap.
full_main <- data[data$treatment_number %in% c(1, 2,3,4,5), ]

# test <- m_5emp_disc%>%
#   group_by(wagegap) %>%
#   summarize(
#     chose_position1 = mean(chose_position1)
#   )

dataset_names <- ls(pattern = "m_") 
dataset_names #Ordered names
my_datasets <- mget(dataset_names)
my_datasets[1] #Now this logic works

#0 is baseline, how they do it in paper

#Estimated alpha using the methods they use in the paper. Baseline
estimate_error(m_1flex_schedule)
estimate_error(m_2flex_hours)
estimate_error(m_3work_home)
estimate_error(m_4comb_flex)
estimate_error(m_5emp_disc)
estimate_error(full_main)

alphas <-c(0.15345323,.17849636,.23885947,.24274945,.12624598)#In order in paper so 1 4 3 5 2
set.seed(529)
replication_table<-table5(my_datasets,B = 500,alpha_mode = 0)
replication_table_latex <- xtable(replication_table)
print(replication_table_latex)

#Let's see the results with the confidence interval
# replication_table<-table5(my_datasets,B = 500,alpha_mode = 0,sd_or_conf = 1)
# replication_table_latex <- xtable(replication_table)
# print(replication_table_latex)

#EXTENSIONS:
####################################################################ALPHAS:
# Variations on a theme.

#No innatentiveness is in their appendix but it's a good starting point.
no_innatentive_table<-table5(my_datasets,B = 500,alpha_mode = 2)
no_innatentive_table_latex <- xtable(no_innatentive_table)
print(no_innatentive_table_latex)

#Alphas estimated using sample mean for wage gap <-2 at individual level.
table_5withmeans<-table5(my_datasets,B = 500,max_iter = 25,alpha_mode = 1)
table_5withmeans_table <- xtable(table_5withmeans)
print(table_5withmeans_table)

#Alphas estimated using sample mean for wage gap <-2 at individual level, 
#containing the pooled 5
table_5with_full<-table5(my_datasets,B=500,alpha_mode=3,max_iter=20,sd_or_conf=0,full_main)
table_5witt_full_latex <- xtable(table_5with_full)
print(table_5witt_full_latex)

#Same procedure but just with the employer discretion dataset!
table_5with_employee_discretion<-table5(my_datasets,B = 500,max_iter = 30,alpha_mode = 3,sd_or_conf = 0,m_5emp_disc)
table_5with_employee_discretion_latex <- xtable(table_5with_employee_discretion)
print(table_5with_employee_discretion_latex)


#alpha i:
alpha_i_dependent <- table5_mod(my_datasets,B = 500,max_iter = 30,max=0.24,min=0.13)
alpha_i_dependent_latex <- xtable(alpha_i_dependent)
print(alpha_i_dependent_latex)
#################################################################OBSERVATIONAL:


#GRAPH: 

graph(m_1flex_schedule,"Flexible schedule")
graph(m_2flex_hours,"Flexible hours")
graph(m_3work_home,"Work from home")
graph(m_4comb_flex,"Combined flexible")
graph(m_5emp_disc,"Employer discretion")





#Miscellaneous:
alpha_review(m_1flex_schedule,alpha_type)
alpha_review(m_2flex_hours)
alpha_review(m_3work_home)
alpha_review(m_4comb_flex)
alpha_review(m_5emp_disc) 
#Alphas with type 1
alpha_review(m_1flex_schedule,alpha_type=1)
alpha_review(m_2flex_hours,alpha_type = 1)
alpha_review(m_3work_home,alpha_type = 1)
alpha_review(m_4comb_flex,alpha_type = 1)
alpha_review(m_5emp_disc,alpha_type = 1) 
#For employee discretion I will have to reestimate.
#Sometimes neg because function does the estimation sometimes...Instead of mean.

