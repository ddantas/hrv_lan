#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""#########################################################
############################################################
### Plot hand movements data.                            ###
###                                                      ###
### Author: Daniel Dantas                                ###
### Last edited: JAN 2023                                ###
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

import const as k


"""#########################################################
############################################################
### Main function                                        ###
############################################################
#########################################################"""

def main(filename_input):

  df = pd.read_csv(filename_input, sep="\t")
  dfr = df.rolling(10).mean()
  dx = df["flow_l_cx"] - dfr["flow_l_cx"]
  dy = df["flow_l_cy"] - dfr["flow_l_cy"]
  dlxy = np.sqrt(dx * dx + dy * dy)

  plt.plot(df["flow_l_cx"])
  plt.plot(df["flow_l_cy"])
  plt.plot(df["flow_l_npix_filt"] * 100)
  plt.plot(df["flow_l_cx"] - dfr["flow_l_cx"] + 200)
  plt.plot(df["flow_l_cy"] - dfr["flow_l_cy"] + 200)
  plt.plot(dlxy + 200)
  #plt.plot(df["flow_l_npix_bool"] * 50)

  plt.plot(-df["flow_r_cx"])
  plt.plot(-df["flow_r_cy"])
  plt.plot(df["flow_r_npix_filt"] * -100)
  #plt.plot(df["flow_r_npix_bool"] * -50)

  plt.legend(["l_cx",
              "l_cy",
              "l_npix_filt",
              "l_cx - mav",
              "l_cy - mav",
              "dlxy",
              "r_cx",
              "r_cy",
              "r_npix_filt"],
             bbox_to_anchor=(1.0, 1.0), loc='upper left')
  plt.tight_layout()
  plt.show()



if __name__ == "__main__":

  # data/output
  print(sys.argv)
  if len(sys.argv) < 2:
    print(f"Usage: python3 optical_flow_plot.py <file_input>")
    path_input = "/home/ddantas/script/python_venv/hrv_lan/data/b012/04_optical_flow_filter"
    #file_input = "subj1_flow_subs.tsv"
    #file_input = "subj1_flow_filter.tsv"
    file_input = "subj2_flow_subs.tsv"
    #file_input = "subj2_flow_filter.tsv"
    full_input = os.path.join(path_input, file_input)
    sys.argv.append(full_input)
    #exit(1)

  full_input = sys.argv[1]
  print(full_input)
  main(full_input)
