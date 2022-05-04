## https://www.bupar.net/getting_started.html

## https://www.researchgate.net/profile/Gert-Janssenswillen/publication/320056971_bupaR_Business_Process_Analysis_in_R/links/5a5de916aca272d4a3deb62f/bupaR-Business-Process-Analysis-in-R.pdf
## http://ceur-ws.org/Vol-2703/paperTD7.pdf

# install.packages("bupaR")
# install.packages("edeaR")
# install.packages("eventdataR")
# install.packages("processmapR")
# install.packages("processmonitR")
# install.packages("xesreadR")
# install.packages("petrinetR")
# install.packages("pm4py")

#devtools::install_github("bupaverse/pm4py")


#Process Mining is much more than using a specific tool. 
# Mostly, it is an iterative procedure involving asking the relevant business questions, 
# understanding the data, interpreting the data correctly (statistical significance vs. 
# practical relevance), and most importantly deriving measures for improving the 
# process under investigation.

# https://www.win.tue.nl/bpi/doku.php?id=2017:challenge

#library(pm4py)

#pm4py::install_pm4py()
library(bupaR)
library(edeaR)
library(eventdataR)
library(processmapR)
library(processmonitR)
library(xesreadR)
library(petrinetR)




## https://bupaverse.github.io/pm4py/ 
# As Inductive Miner of PM4PY is not life-cycle aware, keep only `complete` events:
# patients_completes <- patients[patients$registration_type == "complete", ]
# 
# # Discovery with Inductive Miner
# pn <- pm4py::discovery_inductive(patients_completes)
# 
# # This results in an auto-converted bupaR Petri net and markings
# str(pn)
# 
# # Render with bupaR
# pm4py::render_PN(pn$petrinet)
# 
# 
# # Render with  PM4PY and DiagrammeR
# library(DiagrammeR)
# viz <- reticulate::import("pm4py.visualization.petrinet")
# viz
# # Convert back to Python
# py_pn <- r_to_py(pn$petrinet)
# class(py_pn)
# 
# # Render to DOT with PMP4Y
# dot <- viz$factory$apply(py_pn)$source
# grViz(diagram = dot)
# 
# # Compute alignment
# alignment <- conformance_alignment(patients_completes, pn$petrinet, pn$initial_marking, pn$final_marking)
# 
# # # Alignment is returned in long format as data frame
# head(alignment)
# 
# # Evaluate model quality
# quality <- evaluation_all(patients_completes, pn$petrinet, pn$initial_marking, pn$final_marking)



## https://bupar.net/materials/20170904%20poster%20bupaR.pdf


# https://www.rpubs.com/wxyj2014/307543

## Building event frame  ####

t=Sys.time()
data <- data.frame(case = rep("A",5),
                   activity_id = c("A","B","C","D","E"),
                   activity_instance_id = 1:5,
                   lifecycle_id = rep("complete",5),
                   timestamp = c(t,t+1000,t+2000,t+3000,t+4000),
                   resource = rep("resource 1", 5))

data <- data.frame(case = rep("A",8),
                   activity_id = c("Outreached",
                                   "Intake Registration",
                                   "Post-Distribution Monitored",
                                   "Assessed as Eligible",
                                   "Assessed as Not Eligible",
                                   "Appealed",
                                   "Re-assessed",
                                   "Enrolled / prioritised",
                                   "Not-enrolled",
                                   "Notified",
                                   "Assisted"),
                   activity_instance_id = 1:8,
                   lifecycle_id = rep("complete",8),
                   timestamp = c(t,t+1000,t+2000,t+3000,t+4000,t+5000,t+6000,t+7000),
                   resource = rep("resource 1", 8))

data
first_log <- bupaR::eventlog(data,
                             case_id = "case",
                             activity_id = "activity_id",
                             activity_instance_id = "activity_instance_id",
                             lifecycle_id = "lifecycle_id",
                             timestamp = "timestamp",
                             resource_id = "resource")

data("patients")
#class(patients)
#head(patients)
mylog <-  patients

data("sepsis")
mylog2 <-  sepsis

## Activity Frequency  ####
first_log %>% 
  edeaR::activity_frequency(level = "activity")

mylog %>% 
  edeaR::activity_frequency(level = "activity")


## Plot ressources ### 
edeaR::activity_presence(first_log)
x <- edeaR::end_activities(first_log, level_of_analysis="resource")    
plot(x)


edeaR::activity_presence(mylog)
x <- edeaR::end_activities(mylog, level_of_analysis="resource")    
plot(x)


edeaR::activity_presence(mylog2)
x <- edeaR::end_activities(mylog2, level_of_analysis="resource")    
plot(x)


## Process Map #####
processmapR::process_map(first_log)

processmapR::process_map(patients)

processmapR::process_map(sepsis)


sepsis %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = .90) %>%    
  processmapR::process_map(render = F) #%>% 
# DiagrammeR::export_graph(file_name = here::here("data-raw","bupar_process map.png"),file_type = 'PNG')


sepsis %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = .30) %>%    
  processmapR::process_map(performance(mean, "mins"), render = T)

## Activity Follower matrix #####

precedence_matrix <- sepsis  %>%
  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = .80) %>%    
  processmapR::precedence_matrix() %>% 
  plot()
precedence_matrix


## Generate variant overview #####

trace_explorer <- sepsis %>%
  processmapR::trace_explorer(coverage = 0.5)
trace_explorer

## Show throughput time; In hours by Application Type  ###

sepsis %>%
  # show only the most frequent traces
  edeaR::filter_trace_frequency(percentage = .80) %>%    
  group_by(`diagnose`) %>% 
  # Metric: Throughput time of cases
  edeaR::throughput_time('log', units = 'hours')


## Resource Map#####

processmapR::resource_map(patients)

processmapR::resource_map(sepsis)

## Relative Duration in Hours #####

processmapR::dotted_chart(patients, x = "relative", 
                          y ="duration", 
                          color = NULL, units ="hours")

processmapR::dotted_chart(sepsis, x = "absolute", 
                          y ="duration", 
                          color = NULL, units ="hours")



## Relative Start in Hours ###

processmapR::dotted_chart(patients, x = "relative", 
                          y ="start",
                          color = NULL, units ="hours")

processmapR::dotted_chart(sepsis, x = "absolute", 
                          y ="start", 
                          color = NULL, units ="hours")

#The most useful tool may be the interactive plot: idotted_chart(patients) to be run on the console

mylog <- sepsis 
processmapR::idotted_chart(mylog) 

### process Monitoring #####
processmonitR::activity_dashboard(mylog)
processmonitR::resource_dashboard(mylog)
processmonitR::rework_dashboard(mylog)
processmonitR::performance_dashboard(mylog)

# https://s3.amazonaws.com/assets.datacamp.com/production/course_6004/slides/chapter1.pdf
#A first glimpse of the event log
#This information can be viewed by printing the summary of an event log

library(bupaR)
data(learning)
summary(learning)
n_cases(learning)
n_activities(learning)
n_events(learning)


#Exploring activities
activity_labels(learning)

# A frequency table of traces can be retrieved with the traces function
# They can be visualized using the trace_explorer function
traces(learning)
trace_explorer(learning)

#Create event log object
event_data %>%
  eventlog(case_id = "patient",
           activity_id = "handling",
           activity_instance_id = "handling_id",
           timestamp = "time",
           lifecycle_id = "registration_type",
           resource = "employee")


## https://medium.com/process-mining-and-analytics/process-mining-in-10-minutes-with-r-1ab28ed74e81
## https://rpubs.com/jwross83/624656