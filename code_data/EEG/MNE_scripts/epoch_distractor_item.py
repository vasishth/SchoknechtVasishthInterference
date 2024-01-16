import mne
import glob
import pandas as pd
import os.path as op
import numpy as np

from philistine.mne import retrieve, abs_threshold

mne.set_log_level('WARNING')

#parameters for artefact detection and epoching
tmin, tmax = -0.2, 1.0
baseline = None
reject = dict(eeg=150e-6)
flat = dict(eeg=5e-6)

# define critical events
# distractors
event_id = {'x':210,
            'y':211}

# conditions
event_id_LoSyn = {'a':225,
            'b':235}

event_id_HiSyn = {'c':245,
            'd':255}

#########################################
# create to retrieve single-trial data
# define time windows of interest
windows = {"prestim":(-200, 0),
           "n400" : (300, 500),
            "late_pos": (600,900)}

# create a list to store our retrieved values in
retrieved = []

#  create a list for epoch dfs
epochs_for_export = list()
##########################################

# create list of preprocessed raw files
raw_files = glob.glob('processed/*raw.fif.gz')
#raw_files = raw_files[93:]

# loop through the list of preprocessed files for epoching and single-trial data export
for raw_name in raw_files:
    
    # extract participant number
    subj_no = op.split(raw_name)[1][0:15]
        
    print('processing participant ' + subj_no)
    raw = mne.io.read_raw_fif(raw_name, preload = True)
        
    # extract events
    events = mne.events_from_annotations(raw)[0]
    
    # adapt events so that the condition trigger (225, 235, 245, 255) includes item info as well  
    # to do so, loop through the events array
    for x in range(0,len(events)):
        # get trigger info from column 3 of events
        trigger = events[x,2]
        # check if trigger is a condition trigger 
        # due to different word order, we need to do this separately for HiSyn and LoSyn conditions
        if trigger in event_id_HiSyn.values():
            # go back 17 triggers to get item info
            item = events[x-17,2]
            # create composite trigger
            new_trigger = (trigger)*1000 + item
            # insert new trigger at old location
            events[x,2] = new_trigger
        if trigger in event_id_LoSyn.values():
            # go back 16 triggers to get item info
            item = events[x-16,2]
            # create composite trigger
            # see slides for rationale
            new_trigger = (trigger)*1000 + item
            # insert new trigger at old location
            events[x,2] = new_trigger
            # the new triggers look like this e.g., 255049 (255 = condition, 049 = item)
    
    # adapt events to include item and condition info at distractor position (210 or 211)
    # to do so, loop through the events array
    for x in range(0,len(events)):
        # get trigger info from column 3 of events
        trigger = events[x,2] # to check x = 21 (hisyn) or 68 (lowsyn)
        # check if trigger is a distractor event
        if trigger == 211:
            # go down 6 triggers to get verb trigger with item and condition info
            verbtr = events[x+6,2]
            # create composite trigger
            # only if verb trigger has length=6 (=critical) and if cond = HiSyn
            if len(str(verbtr)) == 6 and int(str(verbtr)[0:3]) in event_id_HiSyn.values():
                comb_trigger = (trigger)*1000000 + verbtr
                # insert new trigger at old location
                events[x,2] = comb_trigger
        if trigger == 210:
            # go down 6 triggers to get verb trigger with item and condition info
            verbtr = events[x+6,2]
             # create composite trigger
             # only if verb trigger has length=6 (=critical) and if cond = LoSyn
            if len(str(verbtr)) == 6 and int(str(verbtr)[0:3]) in event_id_LoSyn.values():
                comb_trigger = (trigger)*1000000 + verbtr
                # insert new trigger at old location
                events[x,2] = comb_trigger

    # create new event-id dict with cond + item combinations
    conditions = [225, 235, 245, 255]
    event_id_new = {}
    for cond in event_id:
        for item in range(1,121):
                for c in conditions:
                    event_key = str(c) +'/' + str(item)
                    event_val = (event_id[cond])*1000000 + (c*1000) + item
                    # only add to new dict if actually present for the current participant
                    if event_val in events[:,2]:
                        event_id_new[event_key] = event_val
    
    # pick types of channels to epoch
    picks = mne.pick_types(raw.info, eeg=True, eog=True, stim=False, misc=False)

    # create epochs
    epochs = mne.Epochs(raw, events, event_id=event_id_new, tmin=tmin, tmax=tmax, proj=False, picks=picks, baseline=None, detrend=0, reject_by_annotation=False, flat=flat, reject=reject, preload=True)
    
    # apply absolute threshold criterion
    bad_epoch_mask = abs_threshold(epochs, 75e-6)
    epochs.drop(bad_epoch_mask,reason="absolute threshold")

    # save epochs
    epochs_file = 'epochs_distractor_item/' + subj_no + '-epo.fif.gz'
    epochs.save(epochs_file, overwrite = True)

    # plot rejections
    print("Plot rejections")
    # note the use of op.join() here to create a path + filename combination
    r_name = op.join('rejections_item/', subj_no + '_epo.png')
    rejections = epochs.plot_drop_log(show=False)
    rejections.savefig(r_name)
    
   