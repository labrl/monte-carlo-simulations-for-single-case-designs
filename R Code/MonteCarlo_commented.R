#Conducting Monte Carlo Simulations to Generate and Analyze Single-Case Graphs

#Import our functions
source('functions_commented.R')

#Set values of each characteristic to test
nb_pointsA_list = c(3,5)
nb_pointsB_list = c(5,10)
a_list = c(0, 0.2, 0.4)
tr_list = c(0, 15, 30)
smd_list = c(0,0,0,1,2,3)
ct = 10

#Generate AB_data

#Empty list to contain the entire data series
all_AB_data = c()

#Empty vector to contain the true values
true_values = c()

#Empty vector to contain trend values for each data series
trend_values = c()

#Loop for each variable
for (nb_pointsA in nb_pointsA_list){
  for (nb_pointsB in nb_pointsB_list){
   for (a in a_list){
     for (tr in tr_list){
       for (smd in smd_list){
         
         #Create times series
         time_series = create_time_series(nb_pointsA+nb_pointsB, a, ct)
         
         #Divide points in Phases A and B and add smd to Phase B
         AB_data = create_AB_data(time_series, nb_pointsA, nb_pointsB, smd)
         
         #Add trend (optional)
         AB_data = add_trend(AB_data, tr)
         
         #Add data series to list
         all_AB_data = c(all_AB_data, AB_data)
         
         #Add true value to vector
         if (smd == 0){
           true_values = c(true_values, 0)
         } else {
           true_values = c(true_values, 1)
         }
         
         #Add trend value to vector
         trend_values = c(trend_values, tr)
       }
     }
   }
  }
}

#Create graphs

#Create pdf file contaoning all graphs (ABgraphs.pdf)
pdf("ABgraphs.pdf", paper="USr", height = 300, width=400) 

#Loop skipping every second line
for (i in seq(1,length(all_AB_data), 2)){
  ABgraph(all_AB_data[i:(i+1)])
}

#Save and close the .pdf file
dev.off()

#Apply CDC to all data series

#Create vector to hold results
cdc_results = c()

#Repeat for each data series
for (i in seq(1,length(all_AB_data), 2)){
  
  #Apply CDC method and add to results vector
  cdc_results = c(cdc_results, CDC_method(all_AB_data[i:(i+1)]))
}


#Check validity of CDC method
#Overall agreement
agreement_CDC = sum(cdc_results == true_values)/length(true_values)

#Type I error rate
idx_0 = which(true_values == 0)
typeI_error_CDC = sum(cdc_results[idx_0])/length(idx_0)

#Power 
idx_1 = which(true_values == 1)
power_CDC = sum(cdc_results[idx_1])/length(idx_1)

#Check validity of visual inspection

#Import expert data
expert_data = read.csv('Expert_data.csv', header = FALSE)[1]

#Overall agreement
agreement_VI = sum(expert_data[1] == true_values)/length(true_values)

#Type I error rate
typeI_error_VI = sum(expert_data[idx_0,1])/length(idx_0)

#Power 
power_VI = sum(expert_data[idx_1,1])/length(idx_1)

#Examine Type I error based on trend for CDC method

#Create results vector
error_by_trend = c()

#Repeat for each trend value
for (tr in tr_list){
  
  #Identify graphs with the specific trend value and no true change
  idx = intersect(which(trend_values == tr), which(true_values == 0))
  
  #Compute error by trend
  error_by_trend = c(error_by_trend, sum(cdc_results[idx])/length(idx)) 
}

#Associate trend with each Type I error rate value
names(error_by_trend) = tr_list

#Examine power based on trend for CDC method

#Create results vector
power_by_trend = c()

#Repeat for each trend value
for (tr in tr_list){
  
  #Identify graphs with the specific trend value and a true change
  idx = intersect(which(trend_values == tr), which(true_values == 1))
  
  #Compute power by trend
  power_by_trend = c(power_by_trend, sum(cdc_results[idx])/length(idx)) 
}

#Associate trend with each power value
names(power_by_trend) = tr_list
