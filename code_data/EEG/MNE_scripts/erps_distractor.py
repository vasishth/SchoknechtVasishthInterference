import mne
import glob
import pandas as pd
import numpy as np
import os.path as op
from philistine.mne import retrieve
import matplotlib.pyplot as plt
from mne import read_evokeds
mne.set_log_level('WARNING')

# define critical events
event_id = {'225':225,
           '235': 235,
            '245':245,
           '255': 255}

avs_by_cond = {cond: [] for cond in event_id}

# create list of epoch files using glob and appropriate wildcard pattern
epoch_files = glob.glob('merged_epochs_distractor/*-epo.fif.gz')
#epoch_files = epoch_files[1:4]

# print the list to inspect it
print(epoch_files)
N_subj = str(len(epoch_files))

# loop through epoch files to load for averaging and creating a grand average
for epoch_name in epoch_files:
    
    # extract participant number
    subj_no = epoch_name[7:22]

    epochs = mne.read_epochs(epoch_name,preload=True)
    
    # to compare different conditions, we need to calculate by-condition averages
    evokeds_a = epochs['225'].average()
    evokeds_b = epochs['235'].average()
    evokeds_c = epochs['245'].average()
    evokeds_d = epochs['255'].average()
    
    # create single subject plots
    title  = 'subj' + subj_no + ' (Pz)'
    plot_pz = mne.viz.plot_compare_evokeds({'225':evokeds_a, '235':evokeds_b, '245':evokeds_c, '255':evokeds_d}, picks=['Pz'], 
         #colors={'Anomaly': 'red', 'Congruent': 'black',},
         invert_y=True,   
         show=False, title=title)
    
    figure_name_pz = 'plots_distractor/' + 'subj' +  subj_no + 'conds_Pz.png'
    plot_pz[0].savefig(figure_name_pz)
    
    
    for cond in event_id:
        evokeds = epochs[cond].average()
        avs_by_cond[cond].append(evokeds)
        
    
# create grand averages from our dictionary of lists
# we again use a dictionary comprehension to do this
grands = {cond: mne.grand_average(avs_by_cond[cond]) for cond in event_id}

grand_a = grands['225']
grand_b = grands['235']
grand_c = grands['245']
grand_d = grands['255']
        
grand_a.plot(time_unit="ms")

grand_a.plot_joint(times = [0.4, 0.8])
grand_c.plot_topomap(times = [0.5, 0.9])


diff = mne.combine_evoked((grand_a, -grand_b), weights='equal')
diff.plot_joint(times = [0.5, 0.9] )

rois = mne.channels.make_1020_channel_selections(diff.info, midline="z")
diff.plot_image(group_by=rois,show=False, show_names='all')

# create figure title
title  = 'Pz'
plot_pz = mne.viz.plot_compare_evokeds(grands,
                                       picks=['Pz'], 
                                       invert_y=True, 
                                        show=False, title=title)
    
figure_name_pz = 'plots_distractor/' +  'N_' + N_subj + '_Pz.png'
plot_pz[0].savefig(figure_name_pz)
    
title  = 'Cz'
plot_cz = mne.viz.plot_compare_evokeds(grands,
                                       picks=['Cz'], 
                                       invert_y=True, 
                                       show=False, title=title)
    
figure_name_cz = 'plots_distractor/' +  'N_' + N_subj + '_Cz.png'
plot_cz[0].savefig(figure_name_cz)
    
title  = 'Fz'
plot_fz = mne.viz.plot_compare_evokeds(grands,
                                       picks=['Fz'], 
                                       invert_y=True, 
                                       show=False, title=title)
    
figure_name_fz = 'plots_distractor/' +  'N_' + N_subj + '_Fz.png'
plot_fz[0].savefig(figure_name_fz)
title  = 'all electrodes'
plot_all = mne.viz.plot_compare_evokeds(grands, picks='eeg', #colors={'Congruent': 'black', 'Anomaly': 'red'}, 
                                        invert_y=True,show=False, title=title, axes='topo', show_sensors=True, legend='upper right')
    
figure_name_all = 'plots_distractor/' +  'N_' + N_subj + '_all_elec' +'.png'
plot_all[0].savefig(figure_name_all)

title  = 'some electrodes'
picks=['F3', 'Fz', 'F4','C3', 'Cz', 'C4', 'P3', 'Pz']
plot_some = mne.viz.plot_compare_evokeds(grands, picks=picks, invert_y=True,show=False, title=title, axes='topo', show_sensors=True, legend='upper right')
    
figure_name_some = 'plots_distractor/' +  'N_' + N_subj + '_some_elec_old' + '.png'
plot_some[0].savefig(figure_name_some)





# plot all 4 conditions
grands['hi sem / hi syn'] = grands['255']
grands['hi sem / lo syn'] = grands['235']
grands['lo sem / hi syn'] = grands['245']
grands['lo sem / lo syn'] = grands['225']


del grands['225']
del grands['235']
del grands['245']
del grands['255']


plot_cz = mne.viz.plot_compare_evokeds(grands,
                                       picks=['Cz'], 
                                       colors={'hi sem / hi syn': 'C0', 'hi sem / lo syn': 'C0', 'lo sem / hi syn': 'C1', 'lo sem / lo syn': 'C1' ,},
                                       linestyles={'hi sem / hi syn': 'dashed', 'hi sem / lo syn': 'solid', 'lo sem / hi syn': 'dashed','lo sem / lo syn': 'solid'},
                                       legend='upper right',
                                       invert_y=True, #vlines=list((-0.9,-0.6,-0.4, 0)),
                                       show=False, title='Cz')
    
figure_name_cz = 'plots_distractor/' +  'N_' + N_subj + '_Cz.png'
plot_cz[0].savefig(figure_name_cz)


plot_all = mne.viz.plot_compare_evokeds(grands,
                                       picks='eeg',
                                       axes='topo',
                                        colors={'hi sem / hi syn': 'C0', 'hi sem / lo syn': 'C0', 'lo sem / hi syn': 'C1', 'lo sem / lo syn': 'C1' ,},
                                       linestyles={'hi sem / hi syn': 'dashed', 'hi sem / lo syn': 'solid', 'lo sem / hi syn': 'dashed','lo sem / lo syn': 'solid'},
                                       invert_y=True, #vlines=list((-0.9,-0.6,-0.4, 0)),
                                       show=False)
    
figure_name_all = 'plots_distractor/' +  'N_' + N_subj + '_all_elec.png'
plot_all[0].savefig(figure_name_all)

# some electrodes
picks=['F3', 'Fz', 'F4','C3', 'Cz', 'C4', 'P3', 'Pz']
plot_some = mne.viz.plot_compare_evokeds(grands,
                                       picks=picks,
                                       axes='topo',
                                        colors={'hi sem / hi syn': 'C0', 'hi sem / lo syn': 'C0', 'lo sem / hi syn': 'C1', 'lo sem / lo syn': 'C1' ,},
                                       linestyles={'hi sem / hi syn': 'dashed', 'hi sem / lo syn': 'solid', 'lo sem / hi syn': 'dashed','lo sem / lo syn': 'solid'},
                                       invert_y=True, #vlines=list((-0.9,-0.6,-0.4, 0)),
                                       show=False, time_unit='ms')
    
figure_name_some = 'plots_distractor/' +  'N_' + N_subj + '_some_elec.png'
plot_some[0].savefig(figure_name_some)
plot_some[0].set_size_inches(11, 10)
plot_some[0].savefig('plots_distractor/' +  'N_' + N_subj + '_some_elec.png', dpi=1200, bbox_inches='tight')
