# -*- coding: utf-8 -*-
"""
Created on Wed Aug 11 09:24:12 2021

@author: Marc Lanovaz
"""
#Conducting Monte Carlo Simulations to Generate and Analyze Single-Case Graphs

#Import packages
import numpy as np
import math
import matplotlib.pyplot as plt

#This function creates a time series with n points, an autocorrelation of a,
#and a constant of ct

def create_time_series(n, a, ct):
    
    #Create an empty vector to hold values
    time_series = np.empty((0,))
    
    #Compute first point (no autocorrelation possible)
    point1 = np.random.normal(size = 1)

    #Add point1 to time series
    time_series = np.hstack((time_series, point1))
    
    #Repeat the process below for all subsequent points
    for i in range(1, n):
        
        #Compute autocorrelated point
        point = a*time_series[i-1]+np.random.normal(size = 1)
        
        #Add autcorrelated point to time series
        time_series = np.hstack((time_series, point))
    
    #Add constant to all points
    time_series = time_series + ct
    
    #Return the time series
    return(time_series)

#This function creates data for an alternating treatment graph with an 
#autocorrelation of a, a trend of tr (in degrees), a constant of ct, a minimum 
#of nb_points in each condition, and a standardized mean difference of smd. The 
#alternation scheme can be 'systematic', 'semi-random', or 'random' 

def create_AT_data(a, tr, ct, nb_points, smd, alternation):
    
    #If alternation is systematic (e.g., ABABABA)
    if alternation == 'systematic':
        
        #Create labels
        labels = np.array(['A', 'B'] * nb_points)
    
    #If altenration is semi-random (in blocks of two)
    if alternation == 'semi-random':
        
        #Empty vector for labels
        labels = np.empty((0,))
        
        #Repeat process for each pair of points
        for i in range(nb_points):
            
            #Randomly select the order of two conditions
            conditions = np.random.choice(['A', 'B'], 2, replace = False)
        
            #Add conditions to labels 
            labels = np.hstack((labels, conditions))
    
    #If the alternation is completely random 
    if alternation == 'random':
        
        #Empty array
        labels = np.empty((0,))
        
        #Run code until minimum number of points is reached for both phases
        while (np.sum(labels == 'A') < nb_points) | (np.sum(labels == 'B') < nb_points):
            
            #Randomly select one condition 
            condition = np.random.choice(['A', 'B'], 1)
            
            #Add condition to labels 
            labels = np.hstack((labels, condition))
        
    #Create times series
    time_series = create_time_series(len(labels), a, ct)
    
    #Indices for Phase B
    idxB, = np.where(labels == 'B')
    
    #Add smd to values of Phase B
    all_values = time_series.copy()
    all_values[idxB] = all_values[idxB] + smd
    
    #Identify middle point around which to pivot trend
    middle_point = np.median(range(len(all_values)))
    
    #Apply trend to all points
    for i in range(len(all_values)):
        
        #Compute distance to middle point for each point
        distance = i - middle_point
        
        #Add trend to each point using trigonometry (tangent of radians)
        all_values[i] = all_values[i] + distance*math.tan(tr*math.pi/180)
    
    #Combine labels and values in same list
    AT_data = [labels, all_values]
    
    #Return alternating-treatment data
    return(AT_data)

#Function to produce alternating-treatment graph

def ATgraph(AT_data):
    
    #Identify indices for Phases A and B
    A, = np.where(AT_data[0] == 'A')
    B, = np.where(AT_data[0] == 'B')
    
    #Extract values for Phases A and B
    valuesA = AT_data[1][A]
    valuesB = AT_data[1][B]   
   
    #Initialize figure
    fig = plt.figure()
    ax = fig.add_subplot(111)
    
    #Plot data
    plt.plot(A+1, valuesA, 'ks-', label = 'Phase A')
    plt.plot(B+1, valuesB, 'ko-', label = 'Phase B')
       
    #Add axes titles
    plt.xlabel('Measurement Times')
    plt.ylabel('Behavior')
    
    #Add legend to graph
    ax.legend(loc='lower right', frameon=False)
    
    #Adjust height of graph
    plt.ylim(0, np.max(AT_data[1]*1.2))
    
    #Remove labels for y axis 
    labels = [item.get_text() for item in ax.get_yticklabels()]
    empty_string_labels = ['']*len(labels)
    ax.set_yticklabels(empty_string_labels)
    
    #Remove right and top borders
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)

#To test function, remove the hashtags from the two lines below
AT_data = create_AT_data(0.1, 30, 10, 5, 10, 'semi-random')
ATgraph(AT_data)