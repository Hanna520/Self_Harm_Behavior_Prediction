Objectives:

This study aims to extract information from medical and mental health progress notes using AI algorithms to make actionable predictions of suicidal and self-injurious events to improve the efficiency of triage for health care services and prevent suicidal and injurious events from happening at California’s Orange County Jails.

Data:

The data used in this study are the medical and mental health progress notes as well as structured data recorded in the Orange County Jail’s Correctional Health Services database. The data cannot be shared publicly due to potentially identifiable and sensitive patient information. 

About the code:

1. NLP_Model.ipynb:  Runs the Transformer Encoder model on notes data alone and on notes alongside structured data with and without under-sampling. 
2. NLP_Model_Data_Augmentation.ipynb: Runs the Transformer Encoder model on the notes data with data augmentation
3. Machine_Learning.ipynb:  Runs Machine Learning models on structured data alone and on structured data alongside predicted probabiliites from the Transformer Encoder model on ntoes data alone, with and without under-sampling.
4. notes_positive.R: Extracts the notes data (in XML format) for the positive cases
5. notes_negative.R: Extracts the notes data (in XML format) for the negative cases
6. structured_notes_combined.R: Stacks all the notes data for the negative cases, combines the negative cases with positive cases, and extracts structured data for both