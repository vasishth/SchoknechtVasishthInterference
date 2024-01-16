import mne
import glob
import pandas as pd
import os.path as op
import numpy as np

# ensures that we can see interactive plots
%matplotlib qt

mne.set_log_level('WARNING')

#parameters for artefact detection and epoching
#tmin, tmax = -0.2, 1.0

# create a list to store our retrieved values in
#retrieved = []

# create a list for epoch dfs
epochs_for_export = list()
export_prestim = list()
export_n400 = list()


# create list of epochs files
epoch_files = glob.glob('epochs_verb_item/*.fif.gz')
#epoch_files = epoch_files[1:100]

for file in epoch_files:
    # extract participant number
    # note the use of split() from os.path (op) to do this
    subj_no = op.split(file)[1][0:15]
        
    print('processing participant ' + subj_no)

    epochs = mne.read_epochs(file,preload=True)
    
    print('Convert to data frame')
    df_epo = epochs.to_data_frame(long_format=True)
    df_epo['subj'] = subj_no
    new_epo1 = df_epo['condition'].str.split('/', expand=True)
    new_epo1.columns = ['cond','item']
    new_epo2 = df_epo['subj'].str.split('_', expand=True)
    new_epo2.columns = ['session','exp', 'subject']
    df2_epo = pd.concat([df_epo, new_epo1, new_epo2], axis=1)
    df2_epo.drop(columns = ['condition', 'subj', 'exp', 'epoch', 'ch_type'], inplace=True)
    df3_epo = df2_epo.round(decimals=3)
    
    prestim_upper = df3_epo[df3_epo['time'] <= 0]
    prestim = prestim_upper[prestim_upper['time'] >= -0.2]

    #prestim.to_csv('Pandora_verb_prestim.csv', sep=',', index=False)

    n400_upper = df3_epo[df3_epo['time'] <= 0.5]
    n400 = n400_upper[n400_upper['time'] >= 0.3]
    #n400.to_csv('Pandora_verb_n400.csv', sep=',', index=False)
    
    export_prestim.append(prestim)
    export_n400.append(n400)


# concatenate epochs for export   
#df_epochs=pd.concat(epochs_for_export, ignore_index=False)
export_prestim_all=pd.concat(export_prestim, ignore_index=False)
export_n400_all=pd.concat(export_n400, ignore_index=False)


#save 
export_prestim_all.to_csv('Pandora_prestim.csv',sep=',', index=False)
export_n400_all.to_csv('Pandora_300_500.csv',sep=',', index=False)

### tidy up columns of single trial df (df_epochs)
#columns = list(df_epochs)
#new_epo1 = df_epochs['condition'].str.split('/', expand=True)
#new_epo1.columns = ['cond','item']
#new_epo2 = df_epochs['subject'].str.split('_', expand=True)
#new_epo2.columns = ['session','exp', 'subj']
#df2_epo = pd.concat([df_epochs,new_epo1, new_epo2], axis=1)
#df2_epo.drop(columns = ['condition', 'subject', 'exp', 'epoch', 'ch_type'], inplace=True)
#df3_epo = df2_epo.round(decimals=4)
#df_epochs.to_csv('Pandora_verb_sampling.csv', sep=',', index=False)



# look at new triggers
#with np.printoptions(threshold=np.inf):
#    print(events)