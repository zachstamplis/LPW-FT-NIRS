FT-NIRS Daily Age Modelling w/ Little Port Walter (LPW) & Gulf of Alaska (GOA) Individual Based Model (IBM) Pacific Cod otoliths

This Git is a hodgepodge of my M.S. thesis work at the University of Alaska - Fairbanks, College of Fisheries & Ocean Sciences
This thesis details using FT-NIRS scans of juvenile Pacific cod otoliths to develop multivariate models capable of predicting/estimating
daily age.  

Herein is analysis from two datasets:

LPW - lab-reared, wild-caught juvenile fish raised in captivity and sampled systematically to provide a known range of ages for 
YOY Pacific cod FT-NIRS daily aging efforts.

IBM - wild-caught specimens from throughout the GOA to additionally test FT-NIRS daily aging techniques, however not lab-reared 
and of unknown range of ages.  These fish were broadly sampled in two GOA regions, near Kodiak Island and around the Shumagin Islands.
Alongside the FT-NIRS scans, a subsample of 40 specimens (20 from each region, split evenly between different size classes) were selected
for ICP-MS trace element analysis of their otoliths.  This data is a bit messy but contains up to 4 ablation transects for each otoliths.
The transects are supposed to run across the core or central primordia, then out to the leading edge of transect thin-section otoliths, 
however some transects missed the core completely or were otherwise unusable.

Broadly this Git includes:
- details for importing FT-NIRS scans of otoliths from a Bruker MPA II and Opus software (.0 files)
- pre-processing methods for spectral data
- various multivariate modelling approachs (PCR via LM and GAM, PLS, Machine Learning [Random Forest & Boosted Regression Trees] )
- fairly complex and thorough 10-fold cross-validation of my small FT-NIRS dataset, repeated with different splits of specimens 500 times.
- comparisons of different model approaches performance across these 500 replications  (R2, RMSE, RPD, K-S test to see how well hatch is predicted)

Folder Information:
- Spectral data included in FT-NIRS_spectra folder
- ICP-MS laser ablation data contained in ICP-MS folder
- Various metadata from aging and/or field collections found in metadata folder
- pre-run model results stored in .RDS files in RDS_dataframes