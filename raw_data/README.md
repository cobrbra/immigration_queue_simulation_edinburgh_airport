# Raw Data folder

We have not provided raw data as part of this GitHub repository for two reasons: firstly, some of it is private (e.g. only available to Maxwell Institute researchers) and we might make this repository public at some point. Secondly, it's too large and would slow down working with GitHub.

We are, however, very willing to pass this on upon reasonable request and consultation with our industrial partners at Edinburgh Airport. We've made no changes at all to datasets beyond the processing laid out in our workflow (for example see "code/process_data.R").

The structure of this repository and how each of its contents are used can be determined by investigation our "_target.R" file, for example with  `targets::tar_visnetwork(targets_only = TRUE)` run from the home directory of this project. We'll lay it out here, however:

 * A folder "aircrafts". This contains publicly available data categorising aircraft types and capacities. It is used to cross-link our historical arrivals dataset.
 * A folder "airports". This contains publicly available data categorising world airports by country and city (amongst other things). 
 * A folder "observed_aircrafts_arrivals", containing raw data provided by Edinburgh Airport Noise Lab specifying historical arrivals schedules for passenger aircraft at Edinburgh Airport between the years 2019 and 2022.
 * An Excel spreadsheet "EAL International Arrivals Data.xlsx", provided by the SIAM-IMA modelling competition and specifying future arrivals schedules and modelling assumptions.