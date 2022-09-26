#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""#########################################################
############################################################
### Filter detected hand movements data.                 ###
###                                                      ###
### Author: Daniel Dantas                                ###
### Last edited: AUG 2022                                ###
############################################################
#########################################################"""

import os
import sys
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Import module from parent folder
filepath = os.path.dirname(__file__)
modpathrel = os.path.join(filepath, "..")
modpathabs = os.path.abspath(modpathrel)
sys.path.append(modpathabs)

#import Data
import const as k
#import utils

DISPLAY = False

def df_concat(df, flow_l_npix_filt, flow_r_npix_filt):
  arr = []
  arr.append(pd.Series(df["flow_l_cx"], dtype=float))
  arr.append(pd.Series(df["flow_l_cy"], dtype=float))
  arr.append(pd.Series(df["flow_l_vx"], dtype=float))
  arr.append(pd.Series(df["flow_l_vy"], dtype=float))
  arr.append(pd.Series(df["flow_l_npix"], dtype=float))
  arr.append(pd.Series(flow_l_npix_filt, name="flow_l_npix_filt", dtype=int))
  arr.append(pd.Series(df["flow_r_cx"], dtype=float))
  arr.append(pd.Series(df["flow_r_cy"], dtype=float))
  arr.append(pd.Series(df["flow_r_vx"], dtype=float))
  arr.append(pd.Series(df["flow_r_vy"], dtype=float))
  arr.append(pd.Series(df["flow_r_npix"], dtype=float))
  arr.append(pd.Series(flow_r_npix_filt, name="flow_r_npix_filt", dtype=int))
  
  df_out = pd.concat(arr, axis=1)
  return df_out

def create_data_file(df, filename_output):
  df.to_csv(filename_output, sep = '\t', index=False, mode = "w", header = True)


def search_tf(arr, s):
  for i in range(s, len(arr) - 1):
    if (arr[i] == True):
      if (arr[i+1] == False):
        return i
  return -1

def search_ft(arr, s):
  for i in range(s, len(arr) - 1):
    if (arr[i] == False):
      if (arr[i+1] == True):
        return i+1
  return -1

def interpolate(df, a, b, col = 0):
  if (a+1 >= b):
    print("Error: interpolate: index a + 1 = %d == %d = b" % (a, b))
  delta = float(b - a)
  va = df.iloc[a, col]
  vb = df.iloc[b, col]
  for i in range(a+1, b):
    wa = (b - i) / delta
    wb = (i - a) / delta
    df.iloc[i, col] = va * wa + vb * wb

def filter_column(df, col, npix_filt):
  a = 0
  if (npix_filt[0] == 0):
    b = search_ft(npix_filt, a)
    if (b > 0):
      df.iloc[a:b, col] = df.iloc[b + 1, col]
  while (True):
    b = search_tf(npix_filt, a)
    if (b > 0):
      c = search_ft(npix_filt, b)
      a = c
      if (c > 0):
        print("%d..%d\n" % (b, c))
        interpolate(df, b, c, col)
      else:
        print("%d..end\n" % (b))
        df.iloc[b+1:, col] = df.iloc[b, col]
        break
    else:
      break

def erode_column(df, col_npix, th = 400, w_filt = 1):
  npix_filt = df.iloc[:, col_npix] > th
  for i in range(len(npix_filt) - 2 - w_filt):
    if (npix_filt[i] == False) and (npix_filt[i + w_filt + 1] == False):
      npix_filt[i+1 : i+1+w_filt] = False
  return npix_filt

def extrapolate(df):
  while (len(df) < 19210):
    df = df.append(df.iloc[-1], ignore_index=True)
  return df

def write_to_dataset_filter(filename_input, filename_output, filename_subsamp):
  df = pd.read_csv(filename_input, sep="\t")
  mask_blur = [.25, .5, .25]
  th = 400
  w_filt = 2
  conv_nsteps = 3

  col_npix = 4
  flow_l_npix_bool = df.iloc[:, col_npix] > th
  flow_l_npix_filt = erode_column(df, col_npix, th, w_filt)
  for col in range(0, 4):
    filter_column(df, col, flow_l_npix_filt)
    for i in range(conv_nsteps):
      df.iloc[:, col] = np.convolve(df.iloc[:, col], mask_blur, mode="same")

  col_npix = 9
  flow_r_npix_bool = df.iloc[:, col_npix] > th
  flow_r_npix_filt = erode_column(df, col_npix, th, w_filt)
  for col in range(5, 9):
    filter_column(df, col, flow_r_npix_filt)
    for i in range(conv_nsteps):
      df.iloc[:, col] = np.convolve(df.iloc[:, col], mask_blur, mode="same")

  if (DISPLAY):
    plt.plot(df["flow_l_cx"])
    plt.plot(df["flow_l_cy"])
    plt.plot(flow_l_npix_filt * 100)
    plt.plot(flow_l_npix_bool * 50)

    plt.plot(-df["flow_r_cx"])
    plt.plot(-df["flow_r_cy"])
    plt.plot(flow_r_npix_filt * -100)
    plt.plot(flow_r_npix_bool * -50)
    plt.show()

  df_out = df_concat(df, flow_l_npix_filt, flow_r_npix_filt)
  create_data_file(df_out, filename_output)
  df_subsamp = pd.DataFrame(df_out, index=range(0, len(df), 30))

  if (DISPLAY):
    plt.plot(df_subsamp["flow_l_cx"])
    plt.plot(df_subsamp["flow_l_cy"])
    plt.plot(flow_l_npix_filt * 100)
    plt.plot(flow_l_npix_bool * 50)

    plt.plot(-df_subsamp["flow_r_cx"])
    plt.plot(-df_subsamp["flow_r_cy"])
    plt.plot(flow_r_npix_filt * -100)
    plt.plot(flow_r_npix_bool * -50)
    plt.show()

  create_data_file(df_subsamp, filename_subsamp)


"""#########################################################
############################################################
### Main function                                        ###
############################################################
#########################################################"""

def main(path_input, path_output):

  #for d in dir_list:
    #path_input = d

    path_filt = os.path.join(path_output, k.FOLDER_FILT)
    if not os.path.exists(path_filt):
      os.mkdir(path_filt)

    files = os.listdir(path_input)
    files.sort()
    print(files)
    for filename_flow in files:
      
      filename_dataset = filename_flow[0:k.LEN_PREF_VIDEO] + k.EXT_FILT
      filename_subsamp = filename_flow[0:k.LEN_PREF_VIDEO] + k.EXT_SUBS
      filename_input = os.path.join(path_input, filename_flow)
      filename_output = os.path.join(path_filt, filename_dataset)
      filename_subsamp = os.path.join(path_filt, filename_subsamp)

      print("path_input: " + path_input)
      print("path_output: " + path_output)
      print("path_filt: " + path_filt)
      print("filename_flow: " + filename_flow)
      print("filename_dataset: " + filename_dataset)      
      print("filename_input: " + filename_input)
      print("filename_output: " + filename_output)
      write_to_dataset_filter(filename_input, filename_output, filename_subsamp)
      #input("Press Enter...")

      
      if filename_flow in files:
        pass
        #filename_video = os.path.join(path_input, k.FOLDER_OPTI, filename_ds)
        #filename_dataset = os.path.join(path_input, k.FOLDER_OPTI, filename_ds)
        #print(filename_dataset)
        #write_to_dataset(path_input, path_output, filename_video, filename_dataset)

      else:
        if filename_annot not in files:
          print(f"Could not find {filename_annot}... Skipping")


if __name__ == "__main__":

  # data/output
  print(sys.argv)
  if len(sys.argv) < 2:
    print(f"Usage: python3 optical_flow_filter.py <dir_input> <dir_output>") 
    exit(1)

  dir_input = sys.argv[1]
  dir_output = sys.argv[2]

  print(dir_input)
  print(dir_output)

  #path_dataset = os.path.join(k.FOLDER_DATA, k.FOLDER_OUTPUT)
  #if not os.path.exists(path_dataset):
  #  os.mkdir(path_dataset)
  #print("path_dataset = %s" % path_dataset)

  main(dir_input, dir_output)
