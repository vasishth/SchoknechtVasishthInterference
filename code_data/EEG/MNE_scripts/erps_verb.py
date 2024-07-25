import mne
import glob
import pandas as pd
import numpy as np
import os.path as op
import philistine.mne
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
#epoch_files = glob.glob('epochs_verb_item/*-epo.fif.gz')
epoch_files = glob.glob('merged_epochs/*-epo.fif.gz')

#epoch_files = epoch_files[1:2]
# number of files
N_subj = str(len(epoch_files))

# loop through epoch files to load for averaging and creating a grand average
for epoch_name in epoch_files:
    
    # extract participant number
    #subj_no = epoch_name[28:32]
    subj_no = epoch_name[14:18]

    epochs = mne.read_epochs(epoch_name,preload=True)
    
    # to compare different conditions, we need to calculate by-condition averages
    evokeds_a = epochs['225'].average()
    evokeds_b = epochs['235'].average()
    evokeds_c = epochs['245'].average()
    evokeds_d = epochs['255'].average()
    
    # create single subject plots
    title  = 'subj' + subj_no + ' (Cz)'
    plot_cz = mne.viz.plot_compare_evokeds({'225':evokeds_a, '235':evokeds_b, '245':evokeds_c, '255':evokeds_d}, picks=['Cz'],  invert_y=True,  show=False, title=title)
    
    figure_name_cz = 'plots/' + 'subj' +  subj_no + '_Cz.png'
    plot_cz[0].savefig(figure_name_cz)
    
    
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
       
# animate - inanimate
grand_anim = mne.combine_evoked((grand_d, grand_b), weights='equal')
grand_inan = mne.combine_evoked((grand_c, grand_a), weights='equal')

plot_animacy_cz = mne.viz.plot_compare_evokeds(dict(high_sem=grand_anim, low_sem=grand_inan),picks=['Cz'],linestyles=['solid', 'dashed'], time_unit='ms',legend='upper right',show_sensors=False, invert_y=True)
plot_animacy_cz[0].set_size_inches(9, 7)
plt.rcParams.update({'font.size': 18})
plot_animacy_cz[0].savefig('plots/' +  'ERP_animacy_Cz' + '.png', dpi=1200, bbox_inches='tight')

picks=['F3', 'Fz', 'F4','C3', 'Cz', 'C4', 'P3', 'Pz']
plot_animacy_some = mne.viz.plot_compare_evokeds(dict(animate=grand_anim, inanimate=grand_inan), picks=picks, invert_y=True,show=False, axes='topo', show_sensors=True, legend='upper right')
plot_animacy_some[0].savefig('plots/' +  'ERP_animacy_some' + '.png', dpi=600, bbox_inches='tight')

plot_animacy_all = mne.viz.plot_compare_evokeds(dict(animate=grand_anim, inanimate=grand_inan), picks='eeg', invert_y=True,show=False, axes='topo', show_sensors=True, legend='upper right')
plot_animacy_all[0].savefig('plots/' +  'ERP_animacy_all' + '.png', dpi=600, bbox_inches='tight')


# difference waves
# hisem - lowsem
diff_anim = mne.combine_evoked((grand_d, grand_b, -grand_a, -grand_c), weights='equal')

diff_anim.plot_joint(times = [0.3, 0.325, 0.35 , 0.375, 0.4, 0.425, 0.45, 0.475, 0.5, 0.8],title='Difference waves (animate-inanimate) at all electrodes\n').savefig('plots/' +  'animate-inanimate_all'+ '.png', dpi=200, bbox_inches='tight')
diff_anim.plot_topomap(times=[0.3, 0.325, 0.35 , 0.375, 0.4, 0.425, 0.45, 0.475, 0.5, 0.525, 0.5, 0.55, 0.575, 0.6, 0.625, 0.65, 0.675, 0.7, 0.75, 0.8,0.825, 0.85, 0.875,0.9], show_names=False, time_unit='ms', vlim=(-0.3,0.3)).savefig('plots/' +  'animate-inanimate_timecourse'+ '.png', dpi=600, bbox_inches='tight')

cm = 1/2.54 
plt.subplots(figsize=(12*cm, 12*cm))
fig = diff_anim.plot_topomap(times=[0.4], average=0.204, show_names=False, time_unit='ms', vlim=(-0.3,0.3))
fig.suptitle('[hisem] - [losem]', fontsize = 12, x=0.3)
fig.savefig('plots/' +  'topo_animate-inanimate_300_500'+ '.png', dpi=600, bbox_inches='tight')

cm = 1/2.54 
plt.subplots(figsize=(12*cm, 12*cm))
fig = diff_anim.plot_topomap(times=[0.75], average=0.304, show_names=False, time_unit='ms', vlim=(-0.3,0.3), size=2)
fig.suptitle('[hisem] - [losem]', fontsize = 20, x=0.3)
fig.savefig('plots/' +  'topo_animate-inanimate_600_900'+ '.png', dpi=600, bbox_inches='tight')

######################################################################
# hisyn - lowsyn
grand_subj = mne.combine_evoked((grand_c, grand_d), weights='equal')
grand_nsubj = mne.combine_evoked((grand_a, grand_b), weights='equal')

plot_subj_cz = mne.viz.plot_compare_evokeds(dict(high_syn=grand_subj, low_syn=grand_nsubj),picks=['Cz'],linestyles=['solid', 'dashed'], time_unit='ms',legend='upper right',show_sensors=False, invert_y=True)
plot_subj_cz[0].set_size_inches(9, 7)
plt.rcParams.update({'font.size': 18})
plot_subj_cz[0].savefig('plots/' +  'ERP_subj_Cz' + '.png', dpi=1200, bbox_inches='tight')

picks=['F3', 'Fz', 'F4', 'C3', 'Cz', 'C4', 'P3', 'Pz', 'P4']
plot_subj_some = mne.viz.plot_compare_evokeds(dict(subj=grand_subj, nsubj=grand_nsubj), picks=picks, invert_y=True,show=False, axes='topo', show_sensors=True, legend='upper right')
plot_subj_some[0].savefig('plots/' +  'ERP_subj_some' + '.png', dpi=600)

plot_subj_all = mne.viz.plot_compare_evokeds(dict(subj=grand_subj, nsubj=grand_nsubj), picks='eeg', invert_y=True,show=False, axes='topo', show_sensors=True, legend='upper right')
plot_subj_all[0].savefig('plots/' +  'ERP_subj_all' + '.png', dpi=600, bbox_inches='tight')

# difference waves
diff_subj = mne.combine_evoked((grand_d, grand_c, -grand_a, -grand_b), weights='equal')

diff_subj.plot_joint(times = [0.3, 0.325, 0.35 , 0.375, 0.4, 0.425, 0.45, 0.475, 0.5, 0.8],title='Difference waves (hisyn-losyn) at all electrodes\n').savefig('plots/' +  'subj-nsubj_all'+ '.png', dpi=200, bbox_inches='tight')
diff_subj.plot_topomap(times=[0.3, 0.325, 0.35 , 0.375, 0.4, 0.425, 0.45, 0.475, 0.5, 0.525, 0.5, 0.55, 0.575, 0.6, 0.625, 0.65, 0.675, 0.7, 0.75, 0.8,0.825, 0.85, 0.875,0.9], show_names=False, time_unit='ms', vlim=(-0.3,0.3)).savefig('plots/' +  'subj-nsubj_timecourse'+ '.png', dpi=600, bbox_inches='tight')

cm = 1/2.54 
plt.subplots(figsize=(18*cm, 18*cm))
fig = diff_subj.plot_topomap(times=[0.4], average=0.204, show_names=False, time_unit='ms', vlim=(-0.3,0.3))
fig.suptitle('[hisyn] - [losyn]', fontsize = 12, x=0.3)
fig.savefig('plots/' +  'topo_subj-nsubj_300_500'+ '.png', dpi=600, bbox_inches='tight')

cm = 1/2.54 
plt.subplots(figsize=(12*cm, 12*cm))
fig = diff_subj.plot_topomap(times=[0.75], average=0.304, show_names=False, time_unit='ms', vlim=(-0.3,0.3), size=2)
fig.suptitle('[hisyn] - [losyn]', fontsize = 20, x=0.3)
fig.savefig('plots/' +  'topo_subj-nsubj_600_900'+ '.png', dpi=600, bbox_inches='tight')


# plot all 4 conditions
grands['lo sem / lo syn'] = grands['225']
grands['lo sem / hi syn'] = grands['245']
grands['hi sem / lo syn'] = grands['235']
grands['hi sem / hi syn'] = grands['255']

del grands['225']
del grands['235']
del grands['245']
del grands['255']


plot_cz = mne.viz.plot_compare_evokeds(grands,
                                       picks=['Cz'], 
                                       colors={'hi sem / lo syn': 'C0', 'lo sem / lo syn': 'C1' , 'hi sem / hi syn': 'C0', 'lo sem / hi syn': 'C1'},
                                       linestyles={'hi sem / lo syn': 'solid', 'lo sem / lo syn': 'solid' , 'hi sem / hi syn': 'dashed', 'lo sem / hi syn': 'dashed'},
                                       legend='lower center',
                                       invert_y=True, #vlines=list((-0.9,-0.6,-0.4, 0)),
                                       show=False, title='Cz')
    
figure_name_cz = 'plots/' +  'N_' + N_subj + '_Cz.png'
plot_cz[0].savefig(figure_name_cz)


plot_all = mne.viz.plot_compare_evokeds(grands,
                                       picks='eeg',
                                       axes='topo',
                                     colors={'hi sem / lo syn': 'C0', 'lo sem / lo syn': 'C1' , 'hi sem / hi syn': 'C0', 'lo sem / hi syn': 'C1'},
                                       linestyles={'hi sem / lo syn': 'solid', 'lo sem / lo syn': 'solid' , 'hi sem / hi syn': 'dashed', 'lo sem / hi syn': 'dashed'},
                                       invert_y=True, #vlines=list((-0.9,-0.6,-0.4, 0)),
                                       show=False)
    
figure_name_all = 'plots/' +  'N_' + N_subj + '_all_elec.png'
plot_all[0].savefig(figure_name_all)

# some electrodes
picks=['F3', 'Fz', 'F4','C3', 'Cz', 'C4', 'P3', 'Pz']
plot_some = mne.viz.plot_compare_evokeds(grands,
                                       picks=picks,
                                       axes='topo',
                                      colors={'hi sem / lo syn': 'C0', 'lo sem / lo syn': 'C1' , 'hi sem / hi syn': 'C0', 'lo sem / hi syn': 'C1'},
                                       linestyles={'hi sem / lo syn': 'solid', 'lo sem / lo syn': 'solid' , 'hi sem / hi syn': 'dashed', 'lo sem / hi syn': 'dashed'},
                                       invert_y=True, #vlines=list((-0.9,-0.6,-0.4, 0)),
                                       show=False)
    
figure_name_some = 'plots/' +  'N_' + N_subj + '_some_elec.png'
plot_some[0].savefig(figure_name_some)