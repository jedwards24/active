# Quick Notes

## Work so far

I have only properly worked with Wahoo files. Data from other sources does have some differences but the main parts are the same.

## TODO

* Fill in missing session data
* Retain file names as attributes. 
* Add file name/id to full session data  

Power-duration:

+ Save individual power-duration files to data_processed/power_duration
+ bad data handling
+ runtime tests
+ Consolidate all code
+ Try non log plot
+ `pwr_duration_plot()` to take session data directly (call `pwr_duration()` internally)?
+ Plot to compare single ride with best of rest (add id to session data?)
+ `power_bests()` to return a tibble with times column?
  
Elevation correction
Plot elevation with map
Coros data
Normalised power function

Basic shiny doc for interacting with activities (for exploration/debugging). 

## Processing notes

1. Read individual fit files --> fit_file object. Save `data_processed/fit_obs/`.
2. fit_file objects --> extract. This is a list for each file of five tibbles by message type. Save `data_processed/extracts/`.
3. Extracts --> one of each of the following: records (list), session (data frame), laps (list), events (list), acts (df). Includes data cleaning and any possible edits.

Additional steps:

* `log_session_PROCESS.R` joins my activ.ty log data and the session data.
* `power_duration.R` produces power-duration curves for any activities with power data.

## Misc 

The trackeR package only reads TCX, GCX, DB3 so I've not done anything with it. It have other useful functions which will work on the processed data.

Times in record and session data seem to be GMT (not adjusted for BST).
