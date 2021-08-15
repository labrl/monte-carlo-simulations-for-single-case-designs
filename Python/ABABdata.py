# -*- coding: utf-8 -*-
"""
Created on Thu Aug 12 10:43:00 2021

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
    
    #Repeat the process below for all subsequent point
    for i in range(1, n):
        
        #Compute autocorrelated point
        point = a*time_series[i-1]+np.random.normal(size = 1)
        
        #Add autocorrelated point to time series
        time_series = np.hstack((time_series, point))
    
    #Add constant to all points
    time_series = time_series + ct
    
    #Return the time series
    return(time_series)
    
#This function creates data for an ABAB graph with an autocorrelation of a, a 
#trend of tr (in degrees), a constant of ct, nb_pointsA1 and nbpointsA2 in the 
#first and second Phase A, nb_pointsB1 and nb_pointsB2 in the first and second 
#Phase B, and a standardized mean difference of smd 

def create_ABAB_data(a, tr, ct, nb_pointsA1, nb_pointsB1, nb_pointsA2, 
                     nb_pointsB2, smd):
    
    #Compute total number of points
    total_points = nb_pointsA1 + nb_pointsB1 + nb_pointsA2 + nb_pointsB2
    
    
    #Create time series
    time_series = create_time_series(total_points, a, ct)
    
    #Extract values for first Phase A
    PhaseA1 = time_series[0:nb_pointsA1].copy()
    
    #Extract values and add smd for first Phase B
    PhaseB1 = time_series[nb_pointsA1:(nb_pointsA1+nb_pointsB1)].copy() + smd 
    
    #Extract values for second Phase A
    PhaseA2 = time_series[(nb_pointsA1+nb_pointsB1):(nb_pointsA1+nb_pointsB1+
                                                       nb_pointsA2)].copy()
    
    #Extract values and add smd for second Phase B
    PhaseB2 = time_series[(nb_pointsA1+nb_pointsB1+nb_pointsA2):(nb_pointsA1+
                          nb_pointsB1+nb_pointsA2+nb_pointsB2)].copy() + smd 
    
    
    #Combine all values in a single vector
    all_values = np.hstack((PhaseA1, PhaseB1, PhaseA2, PhaseB2))
    
    
    #Identify middle point around which to pivot trend
    middle_point = np.median(range(len(all_values)))
    
    #Apply trend to all points
    for i in range(len(all_values)):
        
        #Compute distance to middle point for each point
        distance = i - middle_point
        
        #Add trend to each point using trigonometry (tangent of radians)
        all_values[i] = all_values[i] + distance*math.tan(tr*math.pi/180)
    
    
    #Create labels
    labels = np.array(['A1']*nb_pointsA1 + ['B1'] * nb_pointsB1 + ['A2'] *
                      nb_pointsA2 + ['B2'] * nb_pointsB2)
    
    #Combine labels and values in same list
    ABAB_data = [labels, all_values]
    
    #Return ABAB data
    return(ABAB_data)

#Function to produce ABAB graph

def ABABgraph(ABAB_data):
    
    #Identify indices for Phases A and B
    A1, = np.where(ABAB_data[0] == 'A1')
    B1, = np.where(ABAB_data[0] == 'B1')
    A2, = np.where(ABAB_data[0] == 'A2')
    B2, = np.where(ABAB_data[0] == 'B2')
    
    #Extract values for Phases A and B
    valuesA1 = ABAB_data[1][A1]
    valuesB1 = ABAB_data[1][B1]
    valuesA2 = ABAB_data[1][A2]
    valuesB2 = ABAB_data[1][B2]
    
    #Initialize figure
    fig = plt.figure()
    ax = fig.add_subplot(111)
    
    #Plot data
    plt.plot(A1+1, valuesA1, 'k', B1+1, valuesB1, 'k', A2+1, valuesA2, 'k', 
             B2+1, valuesB2, 'k', marker = 's', clip_on=False)
    
    #Add phase change lines
    plt.axvline(x=len(A1)+0.5, color = 'k', ls='dashed')
    plt.axvline(x=len(A1)+len(B1)+0.5, color = 'k', ls='dashed')
    plt.axvline(x=len(A1)+len(B1)+len(A2) + 0.5, color = 'k', ls='dashed')
           
    #Add labels
    plt.xlabel('Measurement Times')
    plt.ylabel('Behavior')
    
    #Adjust height of graph
    plt.ylim(0, np.max(ABAB_data[1]*1.2))
    
    #Add labels at the top of each Phase 
    plt.text((len(A1)+1)/2, np.max(ABAB_data[1]*1.2), 'Phase A', ha = 'center')
    plt.text((len(A1)*2+len(B1)+1)/2, np.max(ABAB_data[1]*1.2), 'Phase B', 
             ha = 'center')
    plt.text((len(A1)*2+len(B1)*2+len(A2)+1)/2, np.max(ABAB_data[1]*1.2), 
             'Phase A', ha = 'center')
    plt.text((len(A1)*2+len(B1)*2+len(A2)*2+len(B2)+1)/2, 
                np.max(ABAB_data[1]*1.2), 'Phase B', ha = 'center')   
    
    #Remove labels for y axis 
    labels = [item.get_text() for item in ax.get_yticklabels()]
    empty_string_labels = ['']*len(labels)
    ax.set_yticklabels(empty_string_labels)
    
    #Remove right and top borders
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)

#To test function, remove the hashtags from the two lines below
#ABAB_data = create_ABAB_data(0.2, 30, 10, 5, 6, 7, 8, 10)
#ABABgraph(ABAB_data)