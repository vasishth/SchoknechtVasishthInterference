import mne
import glob
import os.path as op
import pandas as pd
import numpy as np

# ensures that we can see interactive plots
%matplotlib qt

from utils_v2 import compute_ica_correction, mark_bads

mne.set_log_level('WARNING')

# construct a montage to assign to the data
montage = mne.channels.make_standard_montage('standard_1020')

# create list of raw files using glob and appropriate wildcard pattern
#raw_files = glob.glob('raw_low_accuracy/s*.vhdr')
raw_files = glob.glob('raw/*.vhdr')
#raw_files = raw_files[5:7]

# loop through the list of raw files for preprocessing
for raw_name in raw_files:
    
    # extract participant number
    # note the use of split() from os.path (op) to do this
    subj_no = op.split(raw_name)[1][0:15]
        
    print('processing participant ' + subj_no)
    raw = mne.io.read_raw_brainvision(raw_name, preload = True, eog = ['HEOGli', 'HEOGre', 'VEOGlo', 'VEOGlu',  'VEOGro', 'VEOGru'])
    
    # trim data for some subjects
    if (subj_no == 's2_pandora_0078'):
        raw = raw.crop(tmin=138)
    elif(subj_no == 's2_pandora_0034'):
        raw = raw.crop(tmin=83, tmax=2863)
    elif(subj_no == 's2_pandora_0070'):
        raw = raw.crop(tmin=91, tmax=2417)
    elif(subj_no == 's2_pandora_0075'):
        raw = raw.crop(tmin=52, tmax=2600)
    elif(subj_no == 's2_pandora_0081'):
        raw = raw.crop(tmax=2667)
    elif(subj_no == 's2_pandora_0090'):
        raw = raw.crop(tmin=150)
    elif(subj_no == 's2_pandora_0149'):
        raw = raw.crop(tmin=129, tmax=2516)
    elif(subj_no == 's1_pandora_0013'):
        raw = raw.crop(tmin=35, tmax=2784)
    elif(subj_no == 's1_pandora_0042'):
        raw = raw.crop(tmax=2613)
    elif(subj_no == 's1_pandora_0075'):
        raw = raw.crop(tmin=178, tmax=2663)
    elif(subj_no == 's1_pandora_0078'):
        raw = raw.crop(tmin=89, tmax=2526)
    
    # identify bad channels
    raw = mark_bads('bad_chans.txt',raw, subj_no)

    # re-reference to the mean of the left and right mastoids
    # special circumstances for BV data when the active reference is one of the mastoids
    # we need to first create an empty channel
    raw = mne.add_reference_channels(raw, ref_channels = ['M1'])
    raw.set_eeg_reference(ref_channels=['M1','TP9'])
    
    # set montage to add information about electrode positions
    raw.set_montage(montage)
    
    # set the reference channels to type misc so that they don't lead to rejections later
    raw.set_channel_types({'M1':'misc','TP9':'misc'})
    
    # recompute EOG channels as bipolar
    
    # for some subjects without left VEOG (due to noisy left VEOG channels)
    if (subj_no == 's1_pandora_0050' or subj_no == 's2_pandora_0078' or subj_no == 's1_pandora_0117' or subj_no == 's2_pandora_0066' or subj_no == 's2_pandora_0074' or subj_no == 's2_pandora_0130'):
        raw = mne.set_bipolar_reference(raw, ['HEOGli', 'VEOGro'], ['HEOGre', 'VEOGru'], ch_name=['EOGH','EOGVr'], ch_info=None, copy=True, verbose=None) 
    # for some subjects without right VEOG (due to noisy right VEOG channels)
    elif (subj_no == 's1_pandora_0030' or subj_no == 's1_pandora_0070' or subj_no == 's1_pandora_0123' or subj_no == 's1_pandora_0142' or subj_no == 's2_pandora_0070' or subj_no == 's2_pandora_0081' or subj_no == 's2_pandora_0090' or subj_no == 's2_pandora_0142'):
        raw = mne.set_bipolar_reference(raw, ['HEOGli','VEOGlo'], ['HEOGre','VEOGlu'], ch_name=['EOGH','EOGVl'], ch_info=None, copy=True, verbose=None)    
    else:
        raw = mne.set_bipolar_reference(raw, ['HEOGli','VEOGlo', 'VEOGro'], ['HEOGre','VEOGlu', 'VEOGru'], ch_name=['EOGH','EOGVl', 'EOGVr'], ch_info=None, copy=True, verbose=None)  
    
    # Interpolate bad channels
    raw.interpolate_bads(reset_bads=True)
    
    # compute ICA correction (does not work for some subjects)
    if (subj_no == 's1_pandora_0002' or subj_no == 's1_pandora_0013' or subj_no == 's1_pandora_0015' or subj_no == 's1_pandora_0033' or subj_no == 's1_pandora_0036' or subj_no == 's1_pandora_0042' or subj_no == 's1_pandora_0062' or subj_no == 's1_pandora_0074' or subj_no == 's1_pandora_0075' or subj_no == 's1_pandora_0081' or subj_no == 's1_pandora_0083' or subj_no == 's1_pandora_0086' or subj_no == 's1_pandora_0089' or subj_no == 's1_pandora_0113' or subj_no == 's1_pandora_0127' or subj_no == 's1_pandora_0132' or subj_no == 's1_pandora_0134' or subj_no == 's1_pandora_0135' or subj_no == 's1_pandora_0138' or subj_no == 's1_pandora_0141' or subj_no == 's1_pandora_0147' or subj_no == 's1_pandora_0148' or subj_no == 's2_pandora_0005' or subj_no == 's2_pandora_0020' or subj_no == 's2_pandora_0023' or subj_no == 's2_pandora_0034' or subj_no == 's2_pandora_0042' or subj_no == 's2_pandora_0050' or subj_no == 's2_pandora_0074' or subj_no == 's2_pandora_0075' or subj_no == 's2_pandora_0078' or subj_no == 's2_pandora_0083' or subj_no == 's2_pandora_0104' or subj_no == 's2_pandora_0127' or subj_no == 's2_pandora_0130' or subj_no == 's2_pandora_0148'):
        print('No ICA')
    else:
        raw = compute_ica_correction(raw,subj_no)
    

    # filter the data
    raw.filter(0.1, 30., l_trans_bandwidth='auto', h_trans_bandwidth='auto', filter_length='auto', method='fir', fir_window='hamming', phase='zero', n_jobs=1)
    
    # rename missnamed subjects
    if (subj_no == "s2_pandora_0200"):
        subj_no = "s2_pandora_0001"
    
    if (subj_no == "s2_04_pandora_0"):
        subj_no = "s2_pandora_0004"

    # save preprocessed data
    processed_file = 'processed/' + subj_no + 'raw.fif.gz'
    raw.save(processed_file,fmt='single',overwrite=True)


#### checks
# example attributes of a raw object: .info and .ch_names
#print(raw.info)
#print(raw.ch_names)

# example methods for a raw object: .plot() and .plot_psd()
#raw.plot()
#raw.plot_psd(fmin = 0, fmax = 30)
#raw.plot_psd()
    
    
    
# preprocess subject 114 (their session 1 recording was split due to technical error)
raw = mne.io.read_raw_brainvision('raw/split_recording/s1_pandora_0114.vhdr', preload = True, eog = ['HEOGli', 'HEOGre', 'VEOGlo', 'VEOGlu',  'VEOGro', 'VEOGru'])
subj_no = '1144A'
raw = mne.add_reference_channels(raw, ref_channels = ['M1'])
raw.set_eeg_reference(ref_channels=['M1','TP9'])
raw.set_montage(montage)
raw.set_channel_types({'M1':'misc','TP9':'misc'})
raw = mne.set_bipolar_reference(raw, ['HEOGli','VEOGlo', 'VEOGro'], ['HEOGre','VEOGlu', 'VEOGru'], ch_name=['EOGH','EOGVl', 'EOGVr'], ch_info=None, copy=True, verbose=None) 
raw = compute_ica_correction(raw,subj_no)
raw.filter(0.1, 30., l_trans_bandwidth='auto', h_trans_bandwidth='auto', filter_length='auto', method='fir', fir_window='hamming', phase='zero', n_jobs=1)
processed_file = 'processed/' + 's1_pandora_0114A' + 'raw.fif.gz'
raw.save(processed_file,fmt='single',overwrite=True)

raw = mne.io.read_raw_brainvision('raw/split_recording/s1_pandora_0114.1.vhdr', preload = True, eog = ['HEOGli', 'HEOGre', 'VEOGlo', 'VEOGlu',  'VEOGro', 'VEOGru'])
subj_no = '1144B'
raw = mne.add_reference_channels(raw, ref_channels = ['M1'])
raw.set_eeg_reference(ref_channels=['M1','TP9'])
raw.set_montage(montage)
raw.set_channel_types({'M1':'misc','TP9':'misc'})
raw = mne.set_bipolar_reference(raw, ['HEOGli','VEOGlo', 'VEOGro'], ['HEOGre','VEOGlu', 'VEOGru'], ch_name=['EOGH','EOGVl', 'EOGVr'], ch_info=None, copy=True, verbose=None) 
raw = compute_ica_correction(raw,subj_no)
raw.filter(0.1, 30., l_trans_bandwidth='auto', h_trans_bandwidth='auto', filter_length='auto', method='fir', fir_window='hamming', phase='zero', n_jobs=1)
processed_file = 'processed/' + 's1_pandora_0114B' + 'raw.fif.gz'
raw.save(processed_file,fmt='single',overwrite=True)


