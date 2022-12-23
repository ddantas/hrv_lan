import sys
from biosppy.signals import ecg
import numpy as np
import os

filepath = os.path.dirname(__file__)
modpathrel = os.path.join(filepath, "..")
modpathabs = os.path.abspath(modpathrel)
sys.path.append(modpathabs)
import Data
import const as k
import utils

def spike(filename_input, filename_output, t0, duration):

  data = Data.Data.load_raw_data(filename_input)

  if (data.datatype != k.TYPE_RR):
    print("Error: rr_spike.py: infer_spike_from_rr: File \"%s\" is not of type RR." % filename_input)
    print("Please load a file with RR data, i.e., columns [time, heart_rate, rr_interval].")
    raise ValueError("Invalid datatype in Data constructor")
    return

  spike = infer_spike_from_rr(data, t0, duration)
  data_spike = Data.Data(k.TYPE_SPK)
  data_spike.time = spike
  data_spike.save_raw_data(filename_output)

def infer_spike_from_rr(data, t0, duration):

  spike = []

  i = 0
  j = 0
  s = 0.0
  while (data.time[i] < t0):
    i += 1

  # This quotient converts the rr_interval to seconds
  s = (data.rr_interval[i] / 1024.0) / 2.0
  print("len = %d" % len(data.rr_interval))
  while (s < duration and i < len(data.rr_interval) - 1):
    print(i)
    spike.append(s)
    i += 1
    # This quotient converts the rr_interval to seconds
    s += data.rr_interval[i] / 1024.0 

  print(spike)  
  return spike


if __name__ == '__main__':

  if len(sys.argv) < 3:
    print("Usage: `python rr_spike.py <data_file.tsv>`")
    print("\tYou can pass in one or more files.")
  else:
  
    filename_input  = sys.argv[1]
    filename_output = sys.argv[2]
    if 'rr' in filename_input:
      print("Inferring SPIKES from RR intervals of the file {filename_input}.")
      spike(filename_input, filename_output)
    else:
      print(f'All files should contain RR intervals, ignoring file {f}.')


