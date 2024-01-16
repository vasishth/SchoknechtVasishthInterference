import mne
import csv
import os.path as op

from mne.preprocessing import ICA
from mne.preprocessing import create_eog_epochs

import matplotlib.pyplot as plt


def compute_ica_correction(raw, subj_no, reject_ica = dict(eeg=250e-6), n_jobs = 2, ica_path = 'ica'):
    """Compute ICA-based correction of EOG channels
    
    Arguments:
        raw -- mne raw object
        subj_no -- subject number
        reject_ica -- dictionary with rejection parameters (default: eeg=250e-6)
        n_jobs -- number of jobs to use for filtering (default: 2)
        ica_path -- path for saving ICA plots and csv file with component info (default: ./ica)
        
    Returns:
        mne raw object
    """
    
    raw_copy = raw.copy()
    raw_copy.filter(1., 40., n_jobs = n_jobs, fir_design = 'firwin')

    ica_picks = mne.pick_types(raw_copy.info, eeg=True, eog=False, misc=False, stim=False, exclude='bads')

    #set ICA parameters
    #method = 'fastica'
    method = 'picard'
    decim = 3
    random_state = 23
    #max_iter = 2000
    #tol=0.001
    ica = ICA(n_components=None, method=method, random_state=random_state)

    #Fit ICA
    ica.fit(raw_copy, picks=ica_picks, decim=decim, reject=reject_ica)

    #Find EOG artefacts
    eog_average = create_eog_epochs(raw_copy, reject=reject_ica, picks=ica_picks).average()
    eog_epochs = create_eog_epochs(raw_copy, reject=reject_ica)
    eog_inds, scores = ica.find_bads_eog(eog_epochs)

    #Plot components identified as corresponding to EOG artefacts and overlays
    ica_plot_base = op.join(ica_path, subj_no + '_ica_plot')
    ica_overlay_name = op.join(ica_path, subj_no + '_ica_overlay')

    ica.plot_overlay(eog_average, exclude=eog_inds, show=False).savefig(ica_overlay_name)

    for comp in range(len(eog_inds)):
        index = str(comp+1)
        ica_plot_name = ica_plot_base + index
        ica.plot_properties(eog_epochs, picks=eog_inds, psd_args={'fmax':35.}, image_args={'sigma':1.}, show=False)[comp].savefig(ica_plot_name)

    #Apply ICA (to unfiltered data)
    ica.exclude.extend(eog_inds)
    ica.apply(raw)

    #Record components
    comps = ica.labels_
    ica_comp_filename = op.join(ica_path, subj_no + '_ica_comps.csv')
    w = csv.writer(open(ica_comp_filename, "w"))
    for key, val in comps.items():
        w.writerow([key, val])

    plt.close('all')

    return raw

def mark_bads(bad_chan_file, raw, subjnr):
    """Mark bad electrodes from info on file
    
    Arguments:
        bad_chan_file -- text file (tab separated) with bad channel info
        raw -- mne raw object
        subjnr -- subject number
        
    Returns:
        mne raw object
    """
    if op.isfile(bad_chan_file):
        reader = csv.DictReader(open(bad_chan_file),dialect=csv.excel_tab)
        raw.info['bads'] = [r['Channel'] for r in reader if r['Subject'] == subjnr]
        print("The following channels were marked as bad: "+str(raw.info['bads']))
    else:
        print(bad_chan_file + ": no such file")
    return raw
