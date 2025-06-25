import numpy as np
import pandas as pd
import nibabel as nib
import glob
import os
import sys

from nilearn.glm.first_level import FirstLevelModel
from nilearn.interfaces.fmriprep import load_confounds
from nilearn.reporting import make_glm_report

############################################################
############ SET PATHS/DIRS AND HYPERPARAMETERS ############
############################################################

# main_dir = '/burg/psych_new/users/gjf2118/who-said-what'
main_dir = '/burg/psych/users/gl2910'
data_dir = '/burg/psych/users/gjf2118/who-said-what'

TR = 1.0
FWHM = 6 #smoothing during GLM

OUTPUT_TYPE = 'z_score'  # OUTPUT_TYPE = 'effect_size'
WSW_STAGE = 'encoding'

# correct_within... the DM does not even have trials where between error occurs
# correct_within_bt... the DM includes ALL trials,
# but we still want to take the contrast only for correct>within
DM_TYPE = 'correct_within_bt2'

# get the subject number through sys.argv...
# it looks like this: python <your_script.py> <subject_number>
# sys.argv[0] is the script name, sys.argv[1] is whatever comes after script name (subject number in our case)
sub = '11'
# already 1 = 001, ..., 10 = 010, etc...
print("sub: " + sub)

runs = [i for i in range(1, 5)] # 4 WSW runs

intersection_all_mask = nib.load(f'{data_dir}/data/brain_masks/intersection_all_mask.nii.gz')


################################################
############ LOAD BEHAVIORAL DATA ############
################################################
events = []

# could/should use glob...
for run in runs:
    temp = pd.read_csv(f'{data_dir}/data/behavioral_data/WSW/{WSW_STAGE}_{DM_TYPE}/sub-{sub}_{WSW_STAGE}_{DM_TYPE}_run-{run}.csv')
    events.append(temp)


###############################################
############ LOAD FMRI IMAGES ############
################################################
path = f'{data_dir}/data/fmriprep_output/sub-{sub}/ses-FreemanWSW/func'

fmri_imgs = sorted(glob.glob(f'{path}/*{WSW_STAGE}_*preproc_bold*.nii.gz'))
# make sure there are the right amount of images
assert len(fmri_imgs) == 4, f"wrong number of fmri images, supposed to be 4, there are only {len(fmri_imgs)}"

# get the confounds dfs
confounds = []

for file in fmri_imgs:
    confound, _ = load_confounds(file,
                                # strategy=['motion', 'wm_csf'],
                                strategy=["motion", "wm_csf", "global_signal"],
                                motion="full",
                                wm_csf="basic",
                                global_signal="basic"
                                )
    confounds.append(confound)

################################################
############ GLM ############
################################################

# instantiate the glm
glm = FirstLevelModel(
    t_r=TR,
    mask_img=intersection_all_mask,
    high_pass=0.008,
    smoothing_fwhm=FWHM
    )

# create save_dir path if doesnt exist
glm_save_dir = f"{main_dir}/output/{WSW_STAGE}_{DM_TYPE}/zmaps"
if not os.path.exists(glm_save_dir):
    os.makedirs(glm_save_dir)

report_save_dir = f"{main_dir}/output/{WSW_STAGE}_{DM_TYPE}/reports"
if not os.path.exists(report_save_dir):
    os.makedirs(report_save_dir)

# fit the GLM
glm.fit(fmri_imgs, events=events, confounds=confounds)

# for grace analysis
# don't take any subtraction or contrasts
contrast = "within_error_face_sentence"  
z_map = glm.compute_contrast(contrast, output_type=OUTPUT_TYPE)
z_map.to_filename(f"{glm_save_dir}/sub-{sub}_{WSW_STAGE}_{DM_TYPE}_within_error_zmap.nii.gz")

contrast = "between_error_face_sentence"  
z_map = glm.compute_contrast(contrast, output_type=OUTPUT_TYPE)
z_map.to_filename(f"{glm_save_dir}/sub-{sub}_{WSW_STAGE}_{DM_TYPE}_between_error_zmap.nii.gz")

contrast = "correct_face_sentence"
z_map = glm.compute_contrast(contrast, output_type=OUTPUT_TYPE)
z_map.to_filename(f"{glm_save_dir}/sub-{sub}_{WSW_STAGE}_{DM_TYPE}_correct_zmap.nii.gz")

print(f"****** CONTRASTS FOR SUB-{sub} DONE ******")



# generate and save reports
report = make_glm_report(
        glm,
        contrasts=['correct_face_sentence', 'within_error_face_sentence', 'between_error_face_sentence']
        )
report.save_as_html(f'{report_save_dir}/sub-{sub}_report.html')


