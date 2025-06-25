from nilearn.glm.first_level import FirstLevelModel
from nilearn.interfaces.fmriprep import load_confounds
import pandas as pd
import os
import numpy as np
import glob
from nilearn import image
from nilearn.glm import compute_contrast
from nilearn.glm.contrasts import compute_fixed_effects
import sys

main_dir = '/burg/psych/users/gl2910'
data_dir = '/burg/psych/users/gjf2118/who-said-what'
TR = 1.0
num_runs = 4
runs = [str(i + 1) for i in range(num_runs)]

OUTPUT_TYPE = 'z_score'  # OUTPUT_TYPE = 'effect_size'
WSW_STAGE = 'encoding'

# correct_within... the DM does not even have trials where between error occurs
# correct_within_bt... the DM includes ALL trials,
# but we still want to take the contrast only for correct>within
DM_TYPE = 'correct_within_bt2'

sub = sys.argv[1]
print(f"****** STARTING SUB-{sub} ******\n")

events = []
fmri_img_paths = []
confounds = []
summary_stats_per_run = {}

# Initialize list to store design matrices
design_matrices = []

for run in runs:
    behavioral_df_path = f'{data_dir}/data/behavioral_data/WSW/{WSW_STAGE}_{DM_TYPE}/sub-{sub}_{WSW_STAGE}_{DM_TYPE}_run-{run}.csv'
    behavioral_df = pd.read_csv(behavioral_df_path)
    behavioral_df = behavioral_df[['trial_type', 'onset', 'duration']]
    events.append(behavioral_df)
    
    fmri_img_path = f'{data_dir}/data/fmriprep_output/sub-{sub}/ses-FreemanWSW/func'
    fmri_imgs = sorted(glob.glob(f'{fmri_img_path}/*{WSW_STAGE}_*preproc_bold*.nii.gz'))
    fmri_img_paths.append(fmri_imgs[0])
    
    confound, _ = load_confounds(
                fmri_imgs[0],
                strategy=["motion", "wm_csf", "global_signal"],
                motion="full",
                wm_csf="basic",
                global_signal="basic")        
    confounds.append(confound)


assert len(events) == len(fmri_img_paths) == len(confounds) == num_runs, "Mismatch in events, fMRI, or confounds."

mask_img_path = f'{data_dir}/data/brain_masks/intersection_all_mask.nii.gz'
glm = FirstLevelModel(t_r=TR, mask_img=mask_img_path)

print('Fitting the GLM...')
for i, run in enumerate(runs):
    print(f"Fitting GLM for run {i}...")
    
    try:
        glm.fit(fmri_img_paths[i], events=events[i], confounds=confounds[i])
        # Store the design matrix after fitting
        design_matrices.append(glm.design_matrices_[0]) 
    except Exception as e:
        print(f"Error fitting GLM for run {i}: {e}")
        continue  # Skip this run if there's an error

    # Loop through each condition (trial type)
    conditions = events[i].trial_type.unique()
    for condition in conditions:
        # Get the design matrix for the current run
        design_matrix = design_matrices[i]

        # Check if condition exists in the design matrix columns
        if condition in design_matrix.columns:
            # Create a contrast for the condition
            contrast_val = np.zeros(len(design_matrix.columns))
            contrast_val[design_matrix.columns.get_loc(condition)] = 1

            # Compute summary statistics for each condition
            stat = glm.compute_contrast(contrast_val, output_type=OUTPUT_TYPE)

            # Store summary statistics for fixed effects later
            if condition not in summary_stats_per_run:
                summary_stats_per_run[condition] = {"effect_sizes": [], "variances": []}

            summary_stats_per_run[condition]["effect_sizes"].append(stat["effect_size"])
            summary_stats_per_run[condition]["variances"].append(stat["effect_variance"])
        else:
            print(f"Condition '{condition}' not found in the design matrix for run {run}. Skipping this condition.")        

# Compute fixed effects across runs for each condition
print("Computing fixed effects...")
fixed_effects_dir = f'{main_dir}/output/encoding_correct_within_bt2/zmaps'
os.makedirs(fixed_effects_dir, exist_ok=True)


for condition, stats in summary_stats_per_run.items():
    contrast_imgs = stats["effect_sizes"]
    variance_imgs = stats["variances"]
    
    # Assuming a method to compute fixed effects; you can adjust this depending on your implementation
    # If you need to use a predefined method like nilearn's fixed_effect computation, adjust here
    fixed_fx_contrast, fixed_fx_variance, fixed_fx_stat = compute_fixed_effects(
        contrast_imgs,
        variance_imgs,
        mask_img_path,
    )

  # Save fixed effects maps (z-map comes from fixed_fx_stat)
    zmap_save_dir = f"{fixed_effects_dir}"
    os.makedirs(zmap_save_dir, exist_ok=True)

    #zmap_save_dir = f"{fixed_effects_dir}/zmaps"
    #os.makedirs(zmap_save_dir, exist_ok=True)

    fixed_fx_contrast.to_filename(f"{zmap_save_dir}/sub-{sub}_{WSW_STAGE}_{DM_TYPE}_{condition}_fixed_effects_contrast.nii.gz")
    fixed_fx_variance.to_filename(f"{zmap_save_dir}/sub-{sub}_{WSW_STAGE}_{DM_TYPE}_{condition}_fixed_effects_variance.nii.gz")
    fixed_fx_stat.to_filename(f"{zmap_save_dir}/sub-{sub}_{WSW_STAGE}_{DM_TYPE}_{condition}_z-map.nii.gz") # Save the final z-map here

print(f"Results saved for sub-{sub}!")
