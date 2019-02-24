# This Python script processes the class list (in CSV format) to prepare it
# for the grouping algorithm. It encodes the student demographic data and
# splits the course list by section into separate files.
# (c) Robert Paul 2018, licensed under GPL

import numpy as np
import pandas as pd
import tkinter as tk
from tkinter import filedialog
from tkinter import messagebox

# What is the current semester?
semester = "SP19"

# Which columns are we pulling out?
useCols = ["ID", "Section", "Sex", "Ethnicity", "First_Gen", "EOP_Status",
    "Score_Cat", "Personality"]

# Initialize the dialog boxes
root = tk.Tk()
root.withdraw()

# Ask for the input file
filePath = filedialog.askopenfilename(
        title = "Select class demographics CSV file",
        filetypes = (("CSV files", "*.csv"),
                     ("Text files", "*.txt"),
                     ("All files", "*.*")))
root.update()

# Ask for the output directory
messagebox.showinfo("Information",
                    "Student demographic data will be encoded and split " + 
                    "into separate files by section. Select the directory " +
                    "for where to save the section files.")
outputPath = filedialog.askdirectory()
root.update()

# Read the CSV file into a Pandas dataframe
classData = pd.read_csv(filePath)

# Replace blank (NaN) values as 0
classData = classData.replace(np.nan, 0)
# Fix section name typo/autocorection
classData = classData.replace('AND', "ADN")
# Code male versus female
classData["Sex"] = classData["Sex"].replace({'F': 1, 'M': 0})
# Code score categories
classData["Score_Cat"] = classData["Score_Cat"].replace(
    {'Low': -1,
     'Medium': 0,
     'High': 1})
# Code ethnicity
classData["Ethnicity"] = classData["Ethnicity"].replace(
    {'^White$': 0, # This regex codes "White" if it's the only ethnicity listed
     'Asian': 1,
     'Hispanic or Latino': 2,
     'Black or African American': 3,
     'American Indian or Alaska Native': 4},
     regex = True)
# Code first generation
classData["First_Gen"] = classData["First_Gen"].replace('Y', 1)
# Code EOP
classData["EOP_Status"] = classData["EOP_Status"].replace('EOP', 1)
# Code personality
classData = classData.replace('leader', 1)
classData.loc[classData.Personality != 1, "Personality"] = 0

# Create ID column as Last name,First name
classData["ID"] = classData["Last name"] + ',' + classData["First name"]

# Collect data into a results dataframe
results = classData[useCols]
# Group by section
sectionGrouping = results.groupby("Section")

# Save each section as its own file
for name, group in sectionGrouping:
    # Format file name to save as
    outputFile = outputPath +  "/{}-{}.csv".format(name, semester)
    # Save the section file, overwriting the file if it exists
    group.to_csv(outputFile, index = False)
