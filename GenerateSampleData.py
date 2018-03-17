# Generate sample discussion section data
# Requires Python version 3.6+
# (c) Robert Paul 2018, licensed under GPL

# Package imports
import os
import random
import csv
import datetime as dt

# Number of students in the class
classSize = 20

# List of capital letters
capLetters = ["A","B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
              "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V",
              "W", "X", "Y", "Z"]

# Two-letter "names" from A,A to Z,Z
nameIDs = [i + ',' + j for i in capLetters for j in capLetters]

# Class demographics
whiteProp = 0.488
asianProp = 0.387
latinxProp = 0.075
afrAmericanProp = 0.05

maleProp = 0.43
femaleProp = 0.57

# Demographic codes
demoCodes = [0, # White
             1, # Asian
             2, # Latinx
             3] # African American

# Gender codes / personality type codes
binaryCodes = [0, # Male or non-leader personality type
               1] # Female or leader personality type

# Score category codes
scoreCodes = [-1, # Low
              0, # Medium
              1] # High

# Demographic proportions
demoProps = [whiteProp, asianProp, latinxProp, afrAmericanProp]

genProps = [maleProp, femaleProp]

# Assume 20% of students are "leader" personality types
leaderProps = [0.8, 0.2]

# Score proportions; 8% low, 52% medium, 40% high
scoreProps = [0.08, 0.52, 0.4]

# Header for CSV file
header = ["ID", "Score_Cat", "Sex", "Ethnicity", "Personality"]

# First and last "names" (just <last initial>,<first initial> format)
ids = random.sample(nameIDs, k = classSize)
ids.sort()

# Generic random ACT scores
scoreSample = random.choices(scoreCodes, weights = scoreProps, k = classSize)

# Sample of students based on gender proportions
genSample = random.choices(binaryCodes, weights = genProps, k = classSize)

# Sample of students based on demographic proportions
demoSample = random.choices(demoCodes, weights = demoProps, k = classSize)

# Sample of personality types
perSample = random.choices(binaryCodes, weights = leaderProps, k = classSize)

# Zip lists together to make into rows
rows = zip(ids, scoreSample, genSample, demoSample, perSample)

# Get the current file path
currentPath = os.path.dirname(os.path.realpath(__file__))

# Create the test_data directory if it doesn't exist
try:
    os.mkdir(currentPath + "/test_data")
except:
    pass

# Generate a file name from the current UNIX epoch time
filename = str(int(dt.datetime.timestamp(dt.datetime.now()))) + "-students.csv"

# Open the file stream and write to the output file
with open(currentPath + "/test_data/" + filename, 'w') as f:
    csvwriter = csv.writer(f)
    # Write the header
    csvwriter.writerow(header)
    # Write the rows
    for row in rows:
        csvwriter.writerow(row)
