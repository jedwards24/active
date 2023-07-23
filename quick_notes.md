# Quick Notes

## Work so far

I have only properly worked with Wahoo files. Data from other sources does have some differences but the main parts are the same.

## TODO

Power-duration:
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

## Processing notes

1. Read individual fit files --> fit_file object. Save `data_processed/fit_obs/`.
2. fit_file objects --> extract. This is a list for each file of five tibbles by message type. Save `data_processed/extracts/`.
3. Extracts --> master record list and session tibble. Includes data cleaning and any possible edits.

## Misc 

The trackeR package only reads TCX, GCX, DB3 so I've not done anything with it. It have other useful functions which will work on the processed data.

Times in record and session data seem to be GMT (not adjusted for BST).
