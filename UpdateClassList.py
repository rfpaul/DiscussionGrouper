# Merge class list
# Update the class list based on e-mail addresses, merging the demographic
# data with a current roster
# (c) Robert Paul 2018, licensed under GPL

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

# Information box: we will prompt for save location of the new demographics file
messagebox.showinfo("Information",
                    "Save file name for updated demographics data.")
root.update()

# Ask for the save name and location of the new demographics file
savePath = filedialog.asksaveasfilename(
        title = "Save file name for new demographic data",
        defaultextension = ".csv")
root.update()

# Load files into data frames
demData = pd.read_csv(demPath)
roster = pd.read_csv(rostPath)

# Columns to use to merge the data
mergeCols = ["First name", "Last name", "Email address"]
# Drop the columns except the ones we're merging on
roster = roster[mergeCols]

# Merge the lists by shared column
results = roster.merge(demData, on = mergeCols, how="left")
results = results[demData.columns]

# Warn about late adds with empty data
# Late adds will have null (NaN) section information
lateAdds = results["Section"].isnull().sum()
if lateAdds > 0:
    # Give a warning about late adds
    messagebox.showwarning(title="Late add warning",
                        message="Late adds found: {}. ".format(lateAdds) + 
                        "Manual entry of data for these students in the " +
                        "demographics file is required.")
root.update()

# Save the file
results.to_csv(savePath, index = False)
