# Merge class list
# Update the class list based on e-mail addresses, merging the demographic
# data with a current roster
# (c) Robert Paul 2018, licensed under GPL

import numpy as np
import pandas as pd
import tkinter as tk
from tkinter import filedialog
from tkinter import messagebox

# Initialize the dialog boxes
root = tk.Tk()
root.withdraw()

# Ask for the student demographics file
demPath = filedialog.askopenfilename(
        title = "Select class demographics CSV file",
        filetypes = (("CSV files", "*.csv"),
                     ("Text files", "*.txt"),
                     ("All files", "*.*")))
root.update()

# Ask for the updated roster file
rostPath = filedialog.askopenfilename(
        title = "Select updated roster CSV file",
        filetypes = (("CSV files", "*.csv"),
                     ("Text files", "*.txt"),
                     ("All files", "*.*")))
root.update()

# Ask for the save location of the new demographics file
savePath = filedialog.asksaveasfilename(
        title = "Save file name for new demographic data",
        defaultextension = ".csv")
root.update()

# Load files into data frames
demData = pd.read_csv(demPath)
roster = pd.read_csv(rostPath)

# Merge the lists by e-mail address
results = roster.merge(demData, on = "Email address", how="left")
results = results.rename(columns = {"First name_x": "First name",
                                    "Last name_x": "Last name",
                                    "UIN_x": "UIN"})
results = results[demData.columns]

# Warn about late adds with empty data
# To be implemented

# Save the file
results.to_csv(savePath, index = False)
