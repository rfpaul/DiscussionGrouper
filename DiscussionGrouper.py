## DiscussionGrouper
# This script takes in a file of students in a discussion section, and outputs 
# a file containing groups optimized for peer learning. Optimized groups are 
# determined by the Non-dominated Sorting Genetic Algorithm (NSGA-II) (see Deb, 
# et al. 2002).

# 2017-2018 Robert Paul and Elise Nishikawa
# Corresponding author: robert.f.paul [[at]] gmail [[dot]] com
# Licensed under GPL v3, see https://www.gnu.org/licenses/gpl.html

# Input data formatting (see examples in test_data):
# ID	  Score  Gender Ethnicity Leader
# A, A     35      1          0     1
# B, Q     30      1          4     0
# C, F     29      1          0     0
# D, V     26      1          4     0
# F, C     29      0          3     0
# ...
# CSV file
# Last name/NetID/hash value or other unique identifier as first column
# Gender coded as: male=0, female or other=1
# Ethnic background coded as an integer,
# 0=White, 1=Asian, 2=International, 3=Hispanic, 4=African American, 5=Other
# Raw math/test scores are converted to: middle 80% = 0, bottom or top 10% = 1
# Personality coded as binary, Leader = 1, other = 0

# Group composition:
# Formation of 5 groups with 3 or 4 members
# In smaller classes, 4 groups with 3 or 4 members

# Optimization criteria (roughly by order of decreasing importance):
# Per group:
# Sum of genders is 0, 3, or 4
# Sum of math score categories is 0 or 1
# If a minority is present in a group, at least 2 of the same minority are in
# the group
# Sum of personality types is 0 or 1

## Package imports
# Base packages, should load with no problem
import tkinter as tk
from tkinter import filedialog

# **NOTE!** These packages may need to be installed!
# Install through the command line shell with: pip install <package name here>
import numpy as np
import pandas as pd
from deap import algorithms, base, creator, tools

## Function definitions

# Encode scores based on percentile rank
def CatScores(scores):
    # Initialize everyone to category 0 (middle 80%)
    score_cats = np.repeat(0, len(scores))
    # Get the top and bottom 10% scores
    cutoffs = np.percentile(scores, [10, 90])
    # Everyone below the 10th and above the 90th percentiles is coded to 1
    # Everyone else (middle 80%) is coded to 0
    # 10th percentile or below cutoff
    score_cats[scores <= cutoffs[0]] = 1
    # 90th percentile or above cutoff
    score_cats[scores >= cutoffs[1]] = 1
  
    # Return vector of categorized scores
    return score_cats

# Evaluate "fitness" for gender compositions
# Sum the gender composition of each group
# +0.1 to fitness per group with a sum of 0, 3, or 4
# If all groups meet criteria, fitness set to +1

# Evaluate "fitness" for math percentile distributions
# Sum the score percentile category
# +0.1 to fitness per group with a sum of 1
# If all groups meet criteria, fitness set to +1

# Evaluate "fitness" for diversity

# length(unique(Ethnicity)) == 1
# +0.1 to fitness per group meeting criteria
# If all groups meet criteria, fitness set to +1

# Evaluate "fitness" for only including at most one "leader" personality
# Sum the leader category
# +0.1 to fitness per group with a sum of 1 or 0
# If all groups meet criteria, fitness set to +1

# Evaluate "fitness" for uniqueness versus previous arrangement solutions

## Main script run

# Read input
# Ask for file
root = tk.Tk()
root.withdraw()
filePath = filedialog.askopenfilename(title = "Open discussion class file")
# Open the file
classData = pd.read_csv(filePath)

# Process input
classData['Score_Cat'] = CatScores(classData['Score'])

nStudents = len(classData)
# If we have at least 15 students...
if nStudents >= 15:
    # Place them into 5 groups
    nGroups = 5
else: # But if we have 14 or fewer students...
    # Place them into 4 groups
    nGroups = 4

# Get the initial group arrangement; this is the "chromosome" that will be 
# permutated, recombined, and optimized
initArrang = np.tile(range(1, nGroups + 1), 5)[0:nStudents]

# Run genetic algorithm

# Extract and format Pareto-optimal solutions

# Output to file
# Format: CSV file, IDs with numeric group assignments over n columns of group
# arrangements
# ID   Arrangement_A   Arrangement_B   Arrangement_C   ... Arrangement_n
# A, A             1               1               2               
# B, Q             1               5               5               
# C, F             5               3               4               
# D, V             2               1               4               
# F, C             2               4               1               
# ...
filePath = filedialog.asksaveasfilename(title = "Save results", 
                                        defaultextension = ".csv")
result.to_csv(filePath)

## First attempt -- for reference
# StudentList$Race=as.character(StudentList$Race)
# if(sum(StudentList$Race == "African-American")==1){(StudentList$Race[StudentList$Race%in%"African-American"]<-"Solo")}
# if(sum(StudentList$Race == "Asian")==1){(StudentList$Race[StudentList$Race%in%"Asian"]<-"Solo")}
# if(sum(StudentList$Race == "Hispanic")==1){(StudentList$Race[StudentList$Race%in%"Hispanic"]<-"Solo")}
# 
# 
# #Number of groups to divide into
# A=5
# #The size of your groups
# B=4
# #Setting a starter value to what you want the sum to be
# D=0
# #While loop means that it will keep running until you meet the criteria
# #Each group that fulfills the rules will get a value of 6
# #If you want to allow one group to have violations of math or personality then you can change E from 0 to 1
# E=0
# while(D<((A*6)-E){
# #assign group number randomly to the student list
# StudentList$Group=(sample((rep(1:A, each=B)), replace=F))
# #a matrix holding the values of how well each group fills the rules
# C=matrix(nrow=A, ncol=1)
# #Loop through each group
#     for(i in 1:A){
#       #Check that there are not more than two males in a group
# gendercheck=ifelse((nrow(StudentList[StudentList$Gender==1&StudentList$Group==i,]))>2,0,2)
#   #Check that if there is an ethnic minority in the group, there is at least one other person that shares their ethnicity if possible
# #If there is only one student of a given ethnic group, we've changed their ethnicity to "Solo" already to not mess up this portion
# racecheck=  ifelse((sum(StudentList$Race[which(StudentList$Group==i)] == "African-American")==1), 0,
#       (ifelse((sum(StudentList$Race[which(StudentList$Group==i)]  == "Asian")==1),0,
#        (ifelse((sum(StudentList$Race[which(StudentList$Group==i)] == "Hispanic")==1),0,2)))))
# #Make sure that you either have one high scorer with the rest medium OR you have up to two low scorers with the rest medium
# mathcheck=ifelse((sum(as.numeric(as.character(StudentList$Math[which(StudentList$Group==i)]))))>2,0,1)
# #Make sure you don't have more than one "Leader" type
# personalitycheck=ifelse((nrow(StudentList[StudentList$Personality==1&StudentList$Group==i,]))>1,0,1)
# #Sum the values of whether or not the group in question met the rules 
#      C[i,1]=(gendercheck+racecheck+mathcheck+personalitycheck)}
# #Sum all the groups if they met the rules
# #If this is less than A*6, then the while loop will run over again
# #Once the while loop is fulfilled, then the dataframe "Student List" will have everyone assigned to a group that fills the rules
# D=sum(C)}
# 
