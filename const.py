#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""#########################################################
############################################################
### Define constants used by multiple source files.      ###
###                                                      ###
### Author: Daniel Dantas                                ###
### Last edited: Mar 2022                                ###
############################################################
#########################################################"""

import os

EXT_TSV = ".tsv"
EXT_LIN = "_linear.tsv"
EXT_NEAR  = "_nearest.tsv"
EXT_INFERRED_RR = "_inferred_rr.tsv"
EXT_INFERRED_NN = "_inferred_nn.tsv"
EXT_FLOW  = "_flow.tsv"
EXT_FILT  = "_flow_filter.tsv"
EXT_SUBS  = "_flow_subs.tsv"

## Used mostly by Polar.py, Data.py and Plot.py
TYPE_RR  = "R"
TYPE_ECG = "E"
TYPE_SPK = "S"

## Used mostly by WinOp.py
FOLDER_DATA = "data"
FOLDER_DEBUG = os.path.join(FOLDER_DATA, "DEBUG")

FILENAME_ROUTINE  = "routine.txt"
FILENAME_LOG      = "log.txt"

FILENAME_ECG      = "subj%d_ecg.tsv"
FILENAME_ECG_S1   = "subj1_ecg.tsv"
FILENAME_ECG_S2   = "subj2_ecg.tsv"

FILENAME_RR       = "subj%d_rr.tsv"
FILENAME_RR_S1    = "subj1_rr.tsv"
FILENAME_RR_S2    = "subj2_rr.tsv"

FILENAME_VIDEO    = "subj%d.mp4"
FILENAME_VIDEO_S1 = "subj1.mp4"
FILENAME_VIDEO_S2 = "subj2.mp4"


## Used mostly by 01_sync/sync.py
FOLDER_SYNC = "01_sync"
FILENAME_VIDEO_SYNC    = "subj%d_sync.mp4"
FILENAME_VIDEO_SYNC_S1 = "subj1_sync.mp4"
FILENAME_VIDEO_SYNC_S2 = "subj2_sync.mp4"

## Used mostly by 02_preprocess/preprocess.py
FOLDER_PREP = "02_preprocess"
FOLDER_OUTPUT = "output"
FILENAME_DATASET = ["dataset_jf.tsv", "dataset_dd.tsv"]
# FILENAME_DATASET = "dataset_dd.tsv"
FILENAME_ANNOTATION = ["annotation_jf.eaf", "annotation_dd.eaf"]
# FILENAME_ANNOTATION = "annotation_dd.eaf"
DATASET_HEADERS = ['folder', 'annotator', 'block', 'label', 'time', 'IsImit', 'IsSync', 'Imitator', 'Model',
                   'hr_subj1_linear',     'hr_subj2_linear',     'hr_subj1_nearest',     'hr_subj2_nearest', \
                   'hr_subj1_ecg_linear', 'hr_subj2_ecg_linear', 'hr_subj1_ecg_nearest', 'hr_subj2_ecg_nearest', \
                   'rr_subj1_linear',     'rr_subj2_linear',     'rr_subj1_nearest',     'rr_subj2_nearest', \
                   'rr_subj1_ecg_linear', 'rr_subj2_ecg_linear', 'rr_subj1_ecg_nearest', 'rr_subj2_ecg_nearest', \
                   'nn_subj1_ecg_linear', 'nn_subj2_ecg_linear', 'nn_subj1_ecg_nearest', 'nn_subj2_ecg_nearest', \
                   'msg1', 'msg2']

# "subj%d_rr_linear.tsv"
FILENAME_RR_LIN          = FILENAME_RR.replace(EXT_TSV, EXT_LIN)
FILENAME_RR_LIN_S1       = FILENAME_RR_S1.replace(EXT_TSV, EXT_LIN)
FILENAME_RR_LIN_S2       = FILENAME_RR_S2.replace(EXT_TSV, EXT_LIN)

# "subj%d_rr_nearest.tsv"
FILENAME_RR_NEAR           = FILENAME_RR.replace(EXT_TSV, EXT_NEAR)
FILENAME_RR_NEAR_S1        = FILENAME_RR_S1.replace(EXT_TSV, EXT_NEAR)
FILENAME_RR_NEAR_S2        = FILENAME_RR_S2.replace(EXT_TSV, EXT_NEAR)

# "subj%d_ecg_inferred_rr.tsv"
FILENAME_ECG_RR       = FILENAME_ECG.replace(EXT_TSV, EXT_INFERRED_RR)
FILENAME_ECG_RR_S1    = FILENAME_ECG_S1.replace(EXT_TSV, EXT_INFERRED_RR)
FILENAME_ECG_RR_S2    = FILENAME_ECG_S2.replace(EXT_TSV, EXT_INFERRED_RR)

# "subj%d_ecg_inferred_rr_linear.tsv"
FILENAME_ECG_RR_LIN      = FILENAME_ECG_RR.replace(EXT_TSV, EXT_LIN)
FILENAME_ECG_RR_LIN_S1   = FILENAME_ECG_RR_S1.replace(EXT_TSV, EXT_LIN)
FILENAME_ECG_RR_LIN_S2   = FILENAME_ECG_RR_S2.replace(EXT_TSV, EXT_LIN)

# "subj%d_ecg_inferred_rr_nearest.tsv"
FILENAME_ECG_RR_NEAR       = FILENAME_ECG_RR.replace(EXT_TSV, EXT_NEAR)
FILENAME_ECG_RR_NEAR_S1    = FILENAME_ECG_RR_S1.replace(EXT_TSV, EXT_NEAR)
FILENAME_ECG_RR_NEAR_S2    = FILENAME_ECG_RR_S2.replace(EXT_TSV, EXT_NEAR)

# "subj%d_ecg_inferred_nn.tsv"
FILENAME_ECG_NN       = FILENAME_ECG.replace(EXT_TSV, EXT_INFERRED_NN)
FILENAME_ECG_NN_S1    = FILENAME_ECG_S1.replace(EXT_TSV, EXT_INFERRED_NN)
FILENAME_ECG_NN_S2    = FILENAME_ECG_S2.replace(EXT_TSV, EXT_INFERRED_NN)

# "subj%d_ecg_inferred_nn_linear.tsv"
FILENAME_ECG_NN_LIN      = FILENAME_ECG_NN.replace(EXT_TSV, EXT_LIN)
FILENAME_ECG_NN_LIN_S1   = FILENAME_ECG_NN_S1.replace(EXT_TSV, EXT_LIN)
FILENAME_ECG_NN_LIN_S2   = FILENAME_ECG_NN_S2.replace(EXT_TSV, EXT_LIN)

# "subj%d_ecg_inferred_nn_nearest.tsv"
FILENAME_ECG_NN_NEAR       = FILENAME_ECG_NN.replace(EXT_TSV, EXT_NEAR)
FILENAME_ECG_NN_NEAR_S1    = FILENAME_ECG_NN_S1.replace(EXT_TSV, EXT_NEAR)
FILENAME_ECG_NN_NEAR_S2    = FILENAME_ECG_NN_S2.replace(EXT_TSV, EXT_NEAR)

# "subj%d_spk.tsv"
FILENAME_RR_SPK       = "subj%d_rr_spk.tsv"
FILENAME_RR_SPK_S1    = "subj1_rr_spk.tsv"
FILENAME_RR_SPK_S2    = "subj2_rr_spk.tsv"

## Used mostly by 03_optical_flow/optical_flow.py
FOLDER_OPTI = "03_optical_flow"
LEN_PREF_VIDEO = 5

## Used mostly by 04_optical_flow_filter/optical_flow_filter.py
FOLDER_FILT = "04_optical_flow_filter"

