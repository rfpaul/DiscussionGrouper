## DiscussionGrouper
# This script takes in a file of students in a discussion section, and outputs a file
# containing groups optimized for peer learning. Optimized groups are determined by
# the Non-dominated Sorting Genetic Algorithm (NSGA-II) (see Deb, et al. 2002).

# 2017-2018 Elise Nishikawa and Robert Paul
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

# Optimization criteria (roughly by order of decreasing importance):
# Group composition:
# Formation of 5 groups with 3 or 4 members
# Per group:
# Sum of genders is 0, 3, or 4
# Sum of math score categories is 0 or 1
# If a minority is present in a group, at least 2 of the same minority are in the group
# Sum of personality types is 0 or 1
# For the generated sets of groups:
# Maximize uniqueness between grouping solutions

## Require these packages--install and load if they're not available
if (!require(mco)) {
  install.packages("mco")
  library(mco)
}

if (!require(foreach)) {
  install.packages("foreach")
  library(foreach)
}

## Function definitions

# Encode scores

# Split into groups

# Evaluate "fitness" for number and sizes of groups

# Evaluate "fitness" for gender compositions

# Evaluate "fitness" for math percentile distributions

# Evaluate "fitness" for diversity

# Evaluate "fitness" for only including at most one "leader" personality

# Evaluate "fitness" for uniqueness versus previous solutions

## Main script run

# Read input

# Process input

# Run genetic algorithm

# Extract and format Pareto-optimal solutions

# Output to file
# Format: IDs with numeric group assignments over n columns of groupings
# ID   Group_A   Group_B   Group_C   Group_D ... Group_n
# A, A       1         1         2         4
# B, Q       1         5         5         3
# C, F       5         3         4         2
# D, V       2         1         4         1
# F, C       2         4         1         5
# ...

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
