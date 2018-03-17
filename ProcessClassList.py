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
semester = "SP18"

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
classData = classData.replace('F', 1)
classData = classData.replace('M', 0)
# Code score categories
classData = classData.replace('Low', -1)
classData = classData.replace('Medium', 0)
classData = classData.replace('High', 1)
# Code ethnicity
classData = classData.replace('Caucasian', 0)
classData = classData.replace('Asian', 1)
classData = classData.replace('Latina/o', 2)
classData = classData.replace('African American', 3)
# Code personality
classData = classData.replace('leader', 1)
classData.loc[classData.Personality != 1, "Personality"] = 0

# Create ID column as Last name,First name
classData["ID"] = classData["Last name"] + ',' + classData["First name"]

# Collect data into a results dataframe
results = classData[["ID", "Section", "Sex", "Ethnicity",
                     "ACT Math", "Personality"]]
# Rename the ACT Math score column to Score_Cat
results = results.rename(columns = {"ACT Math":"Score_Cat"})
# Group by section
sectionGrouping = results.groupby("Section")

# Save each section as its own file
for name, group in sectionGrouping:
    # Format file name to save as
    outputFile = outputPath +  "/{}-{}.csv".format(name, semester)
    # Save the section file
    group.to_csv(outputFile, index = False)
