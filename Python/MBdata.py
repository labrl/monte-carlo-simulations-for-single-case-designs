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
    
#This function creates data for a multiple baseline graphs with an 
#autocorrelation of a, a trend of tr (in degrees), a constant of ct, a minimum 
#of nb_pointsA in Phase A, a mininum of nb_pointsB in Phase B, stagger each 
#tiers by stagger_points, nb_tiers number of tiers, and a standardized 
#mean difference of smd

def create_MB_data(a, tr, ct, nb_pointsA, nb_pointsB, stagger_points, 
                     nb_tiers, smd):
    
    #Create empty labels and values vectors
    labels = np.empty((0,))
    all_values = np.empty((0,))
    
    #Compute total number of points per tier
    total_points = nb_pointsA + (nb_tiers-1)*stagger_points + nb_pointsB
    
    #Repeat for each tier
    for tier in range(nb_tiers):
        
        #Create time series
        time_series = create_time_series(total_points, a, ct)
        
        #Number of points per phase for tier
        nb_pointsA_tier = nb_pointsA + (tier)*stagger_points
        nb_pointsB_tier = total_points - nb_pointsA_tier
        
        #Extract values for Phase A
        PhaseA = time_series[0:nb_pointsA_tier].copy()
    
        #Extract values and add smd for Phase B
        PhaseB = time_series[nb_pointsA_tier:(nb_pointsA_tier+
                                              nb_pointsB_tier)].copy() + smd 
        
        #Combine all values in a single vector for tier
        tier_values = np.hstack((PhaseA, PhaseB))
    
        #Identify middle point around which to pivot trend
        middle_point = np.median(range(len(tier_values)))
        
        #Apply trend to all points
        for i in range(len(tier_values)):
            
            #Compute distance to middle point for each point
            distance = i - middle_point
            
            #Add trend to each point using trigonometry (tangent of radians)
            tier_values[i] = tier_values[i] + distance*math.tan(tr*math.pi/180)
        
    
        #Create labels for tier
        tier_labels = np.array(['A' + str(tier + 1)]*nb_pointsA_tier 
                                + ['B' + str(tier +1)] * nb_pointsB_tier)
        
        #Add labels for tier to data series
        labels = np.hstack((labels, tier_labels))
        
        #Add values for tier to data series
        all_values = np.hstack((all_values, tier_values))
        
    #Combine labels and values in same list
    MB_data = [labels, all_values]
    
    #Return multiple baseline data
    return(MB_data)

#Function to produce multiple baseline graph

def MBgraph(MB_data):

    #Extract number of tiers from MB_data 
    nb_tiers = int((MB_data[0][len(MB_data[0])-1])[1])
    
    #Number of points per tier and x axis values
    total_points = int(len(MB_data[1])/nb_tiers)
    
    #Create graphs on top of each other
    fig, axs = plt.subplots(3)
    
    #Repeat for each tier
    for tier in range(nb_tiers):
        
        #Identify indices of values for Phases A and B for this tier
        idxA_tier, = np.where(MB_data[0] == ['A' + str(tier+1)])
        idxB_tier, = np.where(MB_data[0] == ['B' + str(tier+1)])
        
        #Graph data for Phases A and B in tier 
        axs[tier].plot(range(1,len(idxA_tier)+1), MB_data[1][idxA_tier], 'k', 
          range(len(idxA_tier)+1, total_points+1), 
          MB_data[1][idxB_tier], 'k', marker = 's', clip_on=False)
    
        #Add phase change line
        axs[tier].axvline(x=len(idxA_tier)+0.5, color = 'k', ls='dashed')
        
        #Adjust height of graph
        axs[tier].set_ylim(0, np.max(MB_data[1]*1.2))
      
        #Remove labels for y axis 
        labels = [item.get_text() for item in axs[tier].get_yticklabels()]
        empty_string_labels = ['']*len(labels)
        axs[tier].set_yticklabels(empty_string_labels)
                
        #Remove right and top borders
        axs[tier].spines['right'].set_visible(False)
        axs[tier].spines['top'].set_visible(False)
        
        #Remove labels for x ayis (except for lower tier)
        if tier != (nb_tiers - 1):
            labels = [item.get_text() for item in axs[tier].get_xticklabels()]
            empty_string_labels = ['']*len(labels)
            axs[tier].set_xticklabels(empty_string_labels)
    
    #Add axes titles        
    fig.text(0.5, 0.04, 'Measurement Times', ha='center', va='center')
    fig.text(0.06, 0.5, 'Behavior', ha='center', va='center', 
             rotation='vertical')
        
#To test function, remove the hashtags from the two lines below
#MB_data = create_MB_data(0.2, 15, 10, 5, 9, 3, 3, 5)
#MBgraph(MB_data)
