# README

This repository contains the experimental data and analysis scripts to accompany the paper *Do syntactic and semantic similarity lead to interference effects? Evidence from self-paced reading and event-related potentials using German*.

The repository includes the stimuli used in the experiment (material), self-paced reading data, the pre-processed EEG data used in the analysis as well as the R scripts for the statistical analysis and the code for the computational modeling presented in the paper. The raw EEG data can be accessed via https://zenodo.org/records/10518106.


## Material
German sentences were presented word-by-word in a self-paced manner for the self-paced reading experiment and in an auto-paced manner for the EEG experiment. The critical word was always the verb preceded by two adverbs.

## Code & Data
The folder code_data contains all files (data and code) to reproduce the analyses presented in the paper, separately for the plausibility norming, the SPR and the EEG experiments. Additionally, it contains all files for the computational modeling presented in the paper.

### Plausibility Norming
This folder contains the data of both rating studies reported in the paper. 

1) The norming study that investigating whether the animate distractor was a suitable subject of the critical verb
The file 'norming_analysis.R' runs the complete analysis of this norming study presented in the paper. Its input is the file 'pandora_plausibility_rating_corrected.csv'.

2) The plausibility judgement study investigating whether there was a plausibility difference between the high and low semantic interference conditions.
The file 'analysis_ordinal.R' runs the complete analysis of this judgement study presentend in the paper. It reads in the file 'norming_data_HighvsLowSem.csv' and the file 'pandora_spr_774_precrit.csv'.

### Computational Modeling (LV05_modeling)
This folder contains all files to reproduce the computational modeling presented in the paper. 

All the r-scripts in this folder call the models in the **subfolder 'Models'**. The file 'Prior_predictions-all-models.R' generates the prior predictions for all models and creates Figure 12 shown in the paper. The r-scripts 'Simulate-data-one-cue-model.R', 'Simulate-data-two-cues-model.R' and 'Simulate-data-three-cues-model.R' generate prior predictions for all models under more priors and with more iterations than the script 'Prior_predictions-all-models.R'. These more extensive simulations are saved in the **subfolder 'Simulated-data'** and are used for the model comparison using Bayes factors, carried out in the script 'Compute-Bayes-factors.R'. 

### SPR
The **folder 'data'** contains several .csv-files. The three files 'pandora_web_data_allsessions_short.csv', 'pandora_repS1_data_cleaned_short.csv' and 'pandora_repS2_data_cleaned_short.csv' are the raw data from the self-paced reading experiment. The file 'pandora_web_data_allsessions_short.csv' contains the data of both sessions of experiment E1a. The file 'pandora_repS1_data_cleaned_short.csv' contains the data of experiment E1b with the items 1-60. The file 'pandora_repS2_data_cleaned_short.csv' contains the data of experiment E1b with the items 61-120. The files 'pandora_spr_774_precrit.csv', 'pandora_spr_774_crit.csv' and 'pandora_spr_774_spill.csv' contain the pooled data of all experiments for the pre-critical, critical and spill-over region, respectively.

Additionally, the **'data' folder** contains three .Rda-files ('code_data/SPR/data/BFs_spr_pooled_774_precrit.Rda', 'code_data/SPR/data/BFs_spr_pooled_774_crit.Rda', 'code_data/SPR/data/BFs_spr_pooled_774_spill.Rda') which include the region-wise Bayes Factors computed by the analysis scripts, which are used for plotting ('code_data/SPR/r-scripts/Plot_stats.R').

The **folder 'r-scripts'** contains several .R-files to reproduce the analyses and figures presented in the paper. All their input files are located in the folder 'code_data/SPR/data'. The file 'acc_analysis.R' runs the complete analysis of the comprehension question accuracy presented in the paper. Its input are the three files 'code_data/SPR/data/pandora_web_data_allsessions_short.csv', 'code_data/SPR/data/pandora_repS1_data_cleaned_short.csv' and 'code_data/SPR/data/pandora_repS2_data_cleaned_short.csv'.
These three .csv-files are also the input for the file 'Plot_ReadingTimes.R' to create figures (all sub-figures of figure 3 in the paper) of the reading times across the whole sentence and regions of special interest. 
The file 'create_regionwise_datasets.R' takes the files 'code_data/SPR/data/pandora_web_data_allsessions_short.csv', 'code_data/SPR/data/pandora_repS1_data_cleaned_short.csv' and 'code_data/SPR/data/pandora_repS2_data_cleaned_short.csv' as input and creates the files 'code_data/SPR/data/pandora_spr_774_precrit.csv', 'code_data/SPR/data/pandora_spr_774_crit.csv' and 'code_data/SPR/data/pandora_spr_774_spill.csv' as output. These three files ('code_data/SPR/data/pandora_spr_774_precrit.csv', 'code_data/SPR/data/pandora_spr_774_crit.csv' and 'code_data/SPR/data/pandora_spr_774_spill.csv') are the input for the respective analysis scripts ('analyses_precrit.R', 'analyses_crit.R', 'analyses_spill.R'). These analyses scripts run the Bayesian analyses presented in the paper. *Caution*: It will take very long to run these scripts. 
The file 'Plot_stats.R' takes model fits, which can be generated with the analyses scripts ('analyses_precrit.R', 'analyses_crit.R', 'analyses_spill.R'), and the Bayes Factors ('code_data/SPR/data/BFs_spr_pooled_774_precrit.Rda', 'code_data/SPR/data/BFs_spr_pooled_774_crit.Rda', 'code_data/SPR/data/BFs_spr_pooled_774_spill.Rda') to create the figures in the paper.

Furthermore, the **folder 'r-scripts'** contains the empty folders 'plots' and 'model_fits', in which files generated by the r-scripts will be saved.

### Previous Studies
The file 'PreviousFindings.Rmd' is used to compute the estimates and Bayes factors of the previous studies Van Dyke (2007) and Mertzen et al. (2023). It loads the estimates, 'M21_byregionsummaries_de.rda' and 'M21_byregionsummaries_en.rda', and the Bayes factors from 'code_data/SPR/r-scripts/BFs_schoknecht.rda' and 'BFs_mertzen23.Rda'. The latter was generated by the script 'compute_BFs.R' (this script loads the model fits from the folders 'model_fits_E' and 'model_fits_G' which were generated by the r scripts 'models_E.R' and 'models_G.R' which load the files 'dataRetroEN_processed.txt' and 'dataRetroDE_processed.txt', respectively.). 

The empty folders 'plots', 'model_fits_E' and 'model_fits_G' are used to save the outputs generated by the r-scripts.

### EEG

The **folder 'r_scripts'** contains two .R-files. The file 'acc_analysis.R' runs the complete analysis of the comprehension question accuracy presented in the paper. The file 'analyses.R' runs the complete analysis of the ERP data presented in the paper. Both .R-files read data from the **folder 'data'**. Additionally, this folder contains two empty sub-folders, 'plots' and 'model_fits', which will be used to save files generated by the r-scripts.

The **folder 'data'** contains two .csv-files as well as two sub-folders. The file 'Pandora_mean_200_0.csv' contains the single-trial pre-stimulus ERP data (-200 to 0 ms pre-stimulus onset) and the file 'Pandora_mean_300_500.csv' contains the single-trial data of the critical verb in the N400 time-window (300 to 500 ms). These .csv-files are the input for the r-script 'analyses.R' ('code_data/EEG/r_scripts/analyses.R'). The **sub-folder 'logs'** contains the log-files generated by OpenSesame which include the behavioral results (comprehension question responses). These log-files are the input for the r-script 'acc_analysis.R' ('code_data/EEG/r_scripts/acc_analysis.R'). The **sub-folder 'exclude'** contains three files. The files 'exclude_s1.txt' and 'exclude_s2.txt' contain the information which participants were excluded due to accuracy below 70%, in the respective experimental session (s1: session 1, s2: session 2). The file 'TooManyArtefacts.csv' contains the information which participants were excluded due to too many artefacts (less than 20 artefact-free trials per condition) in their EEG recording in either of the sessions. These three files are read into the script 'acc_analysis.R'.

The **folder 'MNE_scripts'** contains the .py-files used for EEG data preprocessing. These files require the raw EEG data which are stored on zenodo (https://zenodo.org/records/10518106) as input. After downloading, the raw EEG data should be saved in the **sub-folder 'raw'**. The sub-folder 'raw' currently only contains an empty sub-folder 'split_recording'. This sub-folder is the intended location for all files from the directory 'OSF Storage/split_eeg_recording'. Session 1 of participant 114 was recorded in two separate files due to a technical issue and therefore, needs special handling during preprocessing (see .py-files preproc_rename.py,  merge_epochs.py, merge_epochs_precrit.py, merge_epochs_distractor.py).

The workflow through the .py-files for the analyses of the critical verb presented in the paper is as follows:

- preproc_rename.py
- epoch_verb_item.py
- merge_sessions.py
- erps.py
- retrieve.py

To analyse the distractor or pre-critical words the corresponding epoch, merge_session and erps files should be run. The folder contains several empty folders (ica, processed, epochs_verb_item, epochs_precrit_item, epochs_distractor_item, rejections_item, merged_epochs, merged_epochs_precrit, merged_epochs_distractor, plots, plots_precrit, plots_distractor) which are the directories where files generated by the .py-scripts will be saved.

#### Preprocessing parameters
The EEG data preprocessing was done with the following parameters:

- Online reference: left mastoid
- Offline re-reference: average of both mastoids offline
-Filter: bandpass 0.1 – 30 Hz
- ICA:
  - horizontal EOG channels: HEOGli (left canthus), HEOGre (right canthus)
  - vertical EOG channels: VEOGlo (above left eye), VEOGlu (under left eye), VEOGro (above right eye), VEOGru (under right eye)
  - Artifact rejection: epochs (-200 - 1000 ms ) were automatically rejected, in which any EEG channels showed a difference of more than 150 µV
  
#### Markers

- 1-120 = Item markers at the first word of the sentence
- 201-222 = counts the words within each sentence starting at the second word (after the item marker)
- 225  = condition markers: Low syntactic interference / Low semantic interference 
- 235  = condition markers: Low syntactic interference / High semantic interference 
- 245  = condition markers: High syntactic interference / Low semantic interference 
- 255  = condition markers: High syntactic interference / High semantic interference

Due to a coding error, the condition markers (225, 235, 245, 255) were sent at the word after the critical verb. The script epoch.py takes care of this by epoching based on the marker which was sent prior to the condition marker.


- Additional Makers
  - 121 - 160 = Fillers (at the first word of the sentence)
  - 194 = fixation cross
  - 191 = comprehension question appears on the screen
  - 191 = response
  - 195 = timeout
  - 189 = end of practice block
  - 198 = break
  - 199 = end of experiment
