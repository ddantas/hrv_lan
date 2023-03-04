import sys
from biosppy.signals import ecg
import matplotlib.pyplot as plt
import pyhrv
import hrvanalysis
import numpy as np
import os

filepath = os.path.dirname(__file__)
modpathrel = os.path.join(filepath, "..")
modpathabs = os.path.abspath(modpathrel)
sys.path.append(modpathabs)
import Data
import const as k
import utils


def process_ecg_signal(data):
	ECG_SAMPLING_FREQ = 130
	if data.datatype != k.TYPE_ECG:
		return None
	signal = data.ecg
	#rate = 130.0
	#rate = 140.0
	rate = data.find_ecg_sampling_rate()
	#rate = (rate - ECG_SAMPLING_FREQ) * 2.0 + ECG_SAMPLING_FREQ
	out = ecg.ecg(signal=signal, sampling_rate=rate, show=False)
	return out


def infer_rr_intervals_from_ecg(data):
	new_data = Data.Data(k.TYPE_RR)
	signal = data.ecg
	file_t0 = data.time[0]

	out = process_ecg_signal(data)

	time_intervals = out[0]
	rpeaks = out[2]
	heart_rate = out[-1]
	last_peak = rpeaks[0]

	for i in range(1, len(heart_rate)):

		peak_index = rpeaks[i]
		time = time_intervals[peak_index] + file_t0
		last_time = time_intervals[last_peak] + file_t0
		hr = heart_rate[i]
		rr = (float(time) - float(last_time))*1024
		time = float(time)

		new_data.time.append(time)
		new_data.heart_rate.append(hr)
		new_data.rr_interval.append(rr)

		last_peak = peak_index

	return(new_data)

def infer_nn_intervals_from_ecg(data):
	new_data = Data.Data(k.TYPE_RR)
	signal = data.ecg
	file_t0 = data.time[0]

	out = process_ecg_signal(data)

	#time_intervals = out[0]
	rpeaks = out[2]
	#heart_rate = out[-1]
	#last_peak = rpeaks[0]

	rate = data.find_ecg_sampling_rate()
	rpeaks_s = rpeaks / rate
	rpeaks_ms = rpeaks_s * 1000
	nni = pyhrv.tools.nn_intervals(rpeaks_ms)
	hr = pyhrv.tools.heart_rate(nni)
	time = rpeaks_s + file_t0
	time = time[0:-1]

	if True:
		nni2_ms = hrvanalysis.remove_outliers(rr_intervals=nni,
							low_rri=300,
							high_rri=2000)
		nni3_ms = hrvanalysis.interpolate_nan_values(rr_intervals=nni2_ms,
							interpolation_method="linear")
		nni3 = np.array(nni3_ms) / 1000.0 * rate
		nni3 = np.insert(nni3, 0, rpeaks[0])
		rpeaks3 = np.cumsum(nni3)
		signal_filtered = out[1]
		plt.plot(signal)
		plt.plot(signal_filtered)
		plt.vlines(x=rpeaks, ymin=-500, ymax=500, color="yellow")
		plt.vlines(x=rpeaks3, ymin=-1000, ymax=1000, color="red")
		plt.show()

	new_data.time = time
	new_data.heart_rate = hr
	new_data.rr_interval = nni

	return(new_data)


def infer_nn_intervals_from_ecg1(data):
	new_data = Data.Data(k.TYPE_RR)
	signal = data.ecg
	file_t0 = data.time[0]

	#out = process_ecg_signal(data)

	#time_intervals = out[0]
	#rpeaks = out[2]
	#heart_rate = out[-1]
	#last_peak = rpeaks[0]
	signal_filtered = out[1]

	rate = data.find_ecg_sampling_rate()
	nni_filtered = pyhrv.tools.nn_intervals(signal=signal_filtered)

	rpeaks_s = rpeaks / rate
	rpeaks_ms = rpeaks_s * 1000
	nni = pyhrv.tools.nn_intervals(rpeaks_ms)
	hr = pyhrv.tools.heart_rate(nni)
	time = rpeaks_s + file_t0
	time = time[0:-1]

	print(len(time))
	print(len(hr))
	print(len(nni))

	new_data.time = time
	new_data.heart_rate = hr
	new_data.rr_interval = nni

	return(new_data)


def save_rr_intervals_from_ecg(filename_input, filename_output):
	data = Data.Data.load_raw_data(filename_input)
	overwrite = 1

	new_data = infer_rr_intervals_from_ecg(data)
	new_data.save_raw_data(filename_output, overwrite)

def save_nn_intervals_from_ecg(filename_input, filename_output):
	data = Data.Data.load_raw_data(filename_input)
	overwrite = 1

	new_data_nn = infer_nn_intervals_from_ecg(data)
	new_data_nn.save_raw_data(filename_output, overwrite)

if __name__ == '__main__':

	if len(sys.argv) < 2:
		print("Usage: `python rr_inference.py <data_file.tsv>`")
		print("\tYou can pass in one or more files.")

	files = sys.argv[1:]

	for f in sys.argv[1:]:

		if 'ecg' in f:
			print(f"Inferring RR intervals from ECG values of the file {f}.")
			path = os.path.dirname(f)
			filename_output = os.path.join(path, "output_rr_inferred_from_ecg.tsv")
			save_rr_intervals_from_ecg(f, filename_output)

		else:
			print(f'All files should contain ECG values, ignoring file {f}.')
