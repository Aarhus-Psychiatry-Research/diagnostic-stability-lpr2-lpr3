# Examining the stability of diagnostic coding across the transition from the DNPR2 to the DNPR3 for a publication
Preprint: [INSERT LINK HERE]

The repo is split into multiple directories:
1. doc/validation – where we check assumptions about the data which have been questioned during our analyses
2. rmd - where we perform data management and analyses
3. src - for functions used in the analyses
4. tests - testing of functions

Each of these folders are "self-contained" in the sense that all variables used in a file is either defined in the same file, or in a previous file from the same folder.

In general, code uses `lpr3` for the DNPR3 and `lpr2` for the DNPR2. This is to conform with the naming scheme in the Central Denmark Region Business Intelligence Office, which is where the data comes from.

Results are generated in `Render.rmd`. 

# Brief explanation of analyses:
1. individual_unique_diagnoses - within-quarter incident number of unique diagnoses. Calculated for two approaches to determining whether a sequence of visits is from the same treatment course:
    * `seq_responsibility_id`: Visits are considered part of the same treatment course if they share a sequence responsibility identifier (DNPR3 "forløbsansvar"), specified further in the print table S1
    * `same_clinic`: Visits are considered part of the same treatment course if they are from the same patient in the same clinic
2. diagnostic_stability_in_course - to which extent does the first diagnosis and the last diagnosis in a treatment course match?
    * seq_responsibility_id: Visits are considered part of the same treatment course if they share a sequence responsibility identifier (DNPR3 "forløbsansvar"), specified further in the print table S1
    * same_clinic: Visits are considered part of the same treatment course if they are from the same patient in the same clinic
3. subchapter_props - is the proportion of visits within each ICD-10 subchapter stable (e.g. the proportion of all visits within a quarter that are coded as F3)
    * seq_responsibility_id: Visits are considered part of the same treatment course if they share a sequence responsibility identifier (DNPR3 "forløbsansvar"), specified further in the print table S1
    * same_clinic: Visits are considered part of the same treatment course if they are from the same patient in the same clinic
