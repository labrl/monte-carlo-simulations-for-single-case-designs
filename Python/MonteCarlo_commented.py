# -*- coding: utf-8 -*-
"""
Created on Wed Aug 11 10:23:41 2021

@author: Marc Lanovaz
"""
#Conducting Monte Carlo Simulations to Generate and Analyze Single-Case Graphs

#Import packages
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages
import pandas as pd

#Import functions
from functions_commented import create_time_series, create_AB_data, add_trend,\
    ABgraph, CDC_method

#Set values for each characteristic of data series
nb_pointsA_list = [3,5]
nb_pointsB_list = [5,10]
a_list = [0, 0.2, 0.4]
tr_list = [0, 15, 30]
smd_list = [0,0,0,1,2,3]
ct = 10

#Empty list to contain all the input data
all_AB_data = []

#Empty vector to contain the true values
true_values = np.empty((0,))

#Empty vector to contain trend values for each data series
trend_values = np.empty((0,))

#Loop for each variable
for nb_pointsA in nb_pointsA_list:
    for nb_pointsB in nb_pointsB_list:
        for a in a_list:
            for tr in tr_list: 
                for smd in smd_list: 
                    
                    #Create time series
                    time_series = create_time_series(nb_pointsA+nb_pointsB, a,
                                                     ct)
                    
                    #Divide series in Phases A and B and add smd to Phase B
                    AB_data = create_AB_data(time_series, nb_pointsA, 
                                             nb_pointsB, smd)
                    
                    #Add trend (optional)
                    AB_data = add_trend(AB_data, tr)
                    
                    #Add data series to list
                    all_AB_data.append(AB_data)
                    
                    #Add true value to vector
                    if smd == 0:
                        true_values = np.hstack((true_values,0))
                    if smd > 0:
                        true_values = np.hstack((true_values,1))
                    
                    #Add trend value to vector
                    trend_values = np.hstack((trend_values, tr))

#Create AB graphs

#Create pdf
pp = PdfPages('ABgraphs.pdf')

#For each data series
for i in range(len(all_AB_data)):
    
    #Create graph
    ABgraph(all_AB_data[i])
    
    #Save graph to pdf
    pp.savefig()

#Close pdf
pp.close()

#Apply CDC to all graphs 

#Create list to hold results
cdc_results = []

#For each data series
for i in range(len(all_AB_data)):
    
    #Apply CDC and append result to vector
    cdc_results = np.hstack((cdc_results, CDC_method(all_AB_data[i])))

#Check validity of CDC method
#Overall agreement
agreement_CDC = np.sum(cdc_results == true_values)/len(true_values)

#Type I error rate
idx_0, = np.where(true_values == 0)
typeI_error_CDC = np.sum(cdc_results[idx_0])/len(idx_0)

#Power 
idx_1, = np.where(true_values == 1)
power_CDC = np.sum(cdc_results[idx_1])/len(idx_1)

#Check validity of visual inspeciton 

#Import expert data
expert_data = (pd.read_csv('Expert_data.csv', header = None)).values.flatten()

#Overall agreement
agreement_VI = np.sum(expert_data == true_values)/len(true_values)

#Type I error rate 
typeI_error_VI = np.sum(expert_data[idx_0])/len(idx_0)

#Power 
power_VI = np.sum(expert_data[idx_1])/len(idx_1)

#Examine Type I error based on trend for CDC method

#Create results data frame for Type I error by trend 
error_by_trend = pd.DataFrame(data = np.zeros((1,3)), columns = tr_list)

#Repeat for each trend value
for tr in tr_list: 
    
    #Identify graphs with specifc trend values and no true change
    idx = np.intersect1d(np.where(trend_values == tr)[0], 
                         np.where(true_values == 0)[0])
    
    #Compute Type I error by trend while adding to data frame
    error_by_trend[tr] = np.sum(cdc_results[idx])/len(idx)

#Create results data frame for power by trend
power_by_trend = pd.DataFrame(data = np.zeros((1,3)), columns = tr_list)

#Repeat for each trend value
for tr in tr_list: 
    
    #Identify graphs with specifc trend values and a true change
    idx = np.intersect1d(np.where(trend_values == tr)[0], 
                         np.where(true_values == 1)[0])
    
    #Compute Type I error by trend and add to data frame 
    power_by_trend[tr] = np.sum(cdc_results[idx])/len(idx)

