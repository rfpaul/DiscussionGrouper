# Generate sample discussion section data
# Uses Python 3.6+

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

# Fall 2017 Undergraduate demographic numbers
total = 33624
whiteProp = 15061 / total
asianProp = 6053 / total
internatProp = 5569 / total
latinxProp = 3748 / total
afrAmericanProp = 1973 / total
# Multi-race + (2x) Native American + Unknown
otherProp = (1017 + 16 + 23 + 164) / total

maleProp = 18345 / total
femaleProp = 15267 / total

# Demographic codes
demoCodes = [0, # White
             1, # Asian
             2, # Foreign
             3, # Hispanic
             4, # African American
             5] # Multi-race + Native American + Unknown

# Gender codes / personality type codes
binaryCodes = [0, # Male or non-leader personality type
               1] # Female or leader personality type

# Demographic proportions
demoProps = [whiteProp, asianProp, internatProp,
             latinxProp, afrAmericanProp, otherProp]

genProps = [maleProp, femaleProp]

# Assume 15% of students are "leader" personality types
leaderProps = [.85, .15]

# Header for CSV file
header = ["Last", "First", "Score", "Gender", "Ethnicity", "Leader"]

# First and last "names" (just letters)
lastNames = random.sample(capLetters, k = classSize)
lastNames.sort()
firstNames = random.choices(capLetters, k = classSize)

# Generic random ACT scores with a mean of 30, SD of 4.5, max of 36
scoreSample =   [n for n in 
                    [int(random.gauss(30, 4.5))
                     for i in range(0, classSize + 5)]
                if n <= 36][0:classSize]

# Sample of students based on gender proportions
genSample = random.choices(binaryCodes, weights = genProps, k = classSize)

# Sample of students based on demographic proportions
demoSample = random.choices(demoCodes, weights = demoProps, k = classSize)

# Sample of personality types
perSample = random.choices(binaryCodes, weights = leaderProps, k = classSize)

# Zip lists together to make into rows
rows = zip(lastNames, firstNames, scoreSample, genSample, demoSample, perSample)

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
