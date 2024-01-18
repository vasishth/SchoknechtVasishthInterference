import mne
import glob
import os.path as op
import pandas as pd
import numpy as np

# ensures that we can see interactive plots
%matplotlib qt

from utils_v2 import compute_ica_correction, mark_bads

mne.set_log_level('WARNING')

# loop through epoch files to concatenate epochs from 1st and 2nd session
for i in range(1,151):
    print('processing participant ' + str(i).zfill(4))
    epoch_files = glob.glob('epochs_precrit_item/*' + str(i).zfill(4) + '-epo.fif.gz')
    if (len(epoch_files) == 0):
        print('no file available --> do nothing')
    elif (len(epoch_files) == 1):
        print('only one session available --> do nothing')
    else:
        epochs1 = mne.read_epochs(epoch_files[0],preload=True)
        epochs2 = mne.read_epochs(epoch_files[1],preload=True)
        merged = mne.concatenate_epochs([epochs1, epochs2])
        merged_files = 'merged_epochs_precrit/' + str(i).zfill(4) + '-epo.fif.gz'
        merged.save(merged_files, overwrite = True)
       
    
# subject 114 
epoch_files = glob.glob('epochs_precrit_item/*' + '114*' + '-epo.fif.gz')

epochs1 = mne.read_epochs(epoch_files[0],preload=True)
epochs2 = mne.read_epochs(epoch_files[1],preload=True)
epochs3 = mne.read_epochs(epoch_files[2],preload=True)
merged = mne.concatenate_epochs([epochs1, epochs2, epochs3])
merged_files = 'merged_epochs_precrit/' + '0114' + '-epo.fif.gz'
merged.save(merged_files, overwrite = True)
