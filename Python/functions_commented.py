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
from sklearn.linear_model import LinearRegression

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
        
        #Add autocorrelated point to time series
        time_series = np.hstack((time_series, point))
    
    #Add constant to all points
    time_series = time_series + ct
    
    #Return the time series
    return(time_series)

#This function creates data for an AB graph with nb_pointsA in Phase A, 
#nb_pointsB in Phase B, and a standardized mean difference of smd 

def create_AB_data(time_series, nb_pointsA, nb_pointsB, smd):
    
    #Compute total number of points
    total_points = nb_pointsA + nb_pointsB
    
    #Extract value for Phase A
    PhaseA = time_series[0:nb_pointsA].copy()
    
    #Extract and add smd to values of Phase B
    PhaseB = time_series[nb_pointsA:total_points].copy() + smd 
    
    #Create labels
    labels = np.array(['A']*nb_pointsA + ['B'] * nb_pointsB)
    
    #Combine labels and values in the same list
    AB_data = [labels, np.hstack((PhaseA, PhaseB))]
    
    #Return AB data
    return(AB_data)

#Function to add trend of tr degrees to AB series (optional)

def add_trend(AB_data, tr):
    
    #Identify middle point around which to pivot trend
    middle_point = np.median(range(len(AB_data[1])))
    
    #Apply trend to all points
    for i in range(len(AB_data[1])):
        
        #Compute distance to middle point for each point
        distance = i - middle_point
        
        #Add trend to each point using trigonometry (tangent of radians)
        AB_data[1][i] = AB_data[1][i] + distance*math.tan(tr*math.pi/180)
    
    #Return trended AB data series
    return (AB_data)

#Function to produce AB graph

def ABgraph(AB_data):
    
    #Identify indices for Phases A and B
    A, = np.where(AB_data[0] == 'A')
    B, = np.where(AB_data[0] == 'B')
    
    #Extract values for Phase A and B
    valuesA = AB_data[1][A]
    valuesB = AB_data[1][B]   
   
    #Initilizae figure
    fig = plt.figure()
    ax = fig.add_subplot(111)
    
    #Plot data
    plt.plot(A+1, valuesA, 'k', B+1, valuesB, 'k', marker = 's', 
             clip_on=False)
    
    #Add phase change line
    plt.axvline(x=len(A)+0.5, color = 'k', ls='dashed')
    
    #Add labels
    plt.xlabel('Measurement Times')
    plt.ylabel('Behavior')
    
    #Adjust height of graph
    plt.ylim(0, np.max(AB_data[1]*1.2))
    
    #Add labels to Phases 
    plt.text((len(A)+1)/2, np.max(AB_data[1]*1.2), 'Phase A', ha = 'center')
    plt.text((len(A)*2+len(B)+1)/2, np.max(AB_data[1]*1.2), 'Phase B', 
             ha = 'center')
    
    #Remove labels from y axis 
    labels = [item.get_text() for item in ax.get_yticklabels()]
    empty_string_labels = ['']*len(labels)
    ax.set_yticklabels(empty_string_labels)
    
    #Remove right and top borders
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)

#List of cutoff values from Fisher et al. (2003)
Fisheretal=[np.nan,np.nan,3,4,5,6,6,7,8,8,9,9,10,11,12,12,12,13,13,13,14,14,15]

#Function apply CDC method

def CDC_method(AB_data):
    
    #Identify indices for Phases A and B
    A, = np.where(AB_data[0] == 'A')
    B, = np.where(AB_data[0] == 'B')
    
    #Extract values for Phase A and B
    valuesA = AB_data[1][A]
    valuesB = AB_data[1][B]

    #Mean line increased by .25 standard deviations
    meanLine = np.mean(valuesA)+np.std(valuesA)*0.25
    
    #Trend line 
    #Organize data
    y = np.expand_dims(valuesA, axis=1)
    X = np.array([range(len(A))]).T
    
    #Run linear regression on Phase A
    lm = LinearRegression().fit(X, y)
    
    #Project trend line on Phase B and add .25 standard deviations
    trendLine = lm.coef_*np.array(range(len(A),(len(A)+len(B))))+lm.intercept_
    trendLine = np.round(trendLine, 3) + np.std(valuesA)*0.25
    
    #Number of points falling above both lines
    sigPoints = np.sum(np.logical_and(valuesB > meanLine, valuesB > trendLine))
    
    #Return 1 (effect) if equal to or greater than cutoff value
    if sigPoints >= Fisheretal[len(B)-1] :
        return 1
    
    #Return 0 (no effect) if lower than cutoff value
    else:
        return 0
