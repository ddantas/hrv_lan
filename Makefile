

dox:
	doxygen hrv_lan.dox

runop:
	python3 WinOp.py

runsub:
	python3 WinSub.py

# Synchronization of videos of both subjects.
# Force both videos to have the same length.
runsync:
	python3 01_sync/sync.py data/b042
	python3 01_sync/sync.py data/b044

runprep:
	#python3 02_preprocess/preprocess.py data/b*
	python3 02_preprocess/preprocess.py data/b001
	python3 02_preprocess/preprocess.py data/b002
	python3 02_preprocess/preprocess.py data/b003
	python3 02_preprocess/preprocess.py data/b004
	python3 02_preprocess/preprocess.py data/b009
	python3 02_preprocess/preprocess.py data/b01*
	python3 02_preprocess/preprocess.py data/b02*
	python3 02_preprocess/preprocess.py data/b03*
	python3 02_preprocess/preprocess.py data/b04*

runopti:
	python3 03_optical_flow/optical_flow.py data/b002/01_sync data/b002
	python3 03_optical_flow/optical_flow.py data/b004/01_sync data/b004
	python3 03_optical_flow/optical_flow.py data/b006/01_sync data/b006
	python3 03_optical_flow/optical_flow.py data/b008/01_sync data/b008
	python3 03_optical_flow/optical_flow.py data/b010/01_sync data/b010
	python3 03_optical_flow/optical_flow.py data/b012/01_sync data/b012
	python3 03_optical_flow/optical_flow.py data/b014/01_sync data/b014
	python3 03_optical_flow/optical_flow.py data/b016/01_sync data/b016
	python3 03_optical_flow/optical_flow.py data/b018/01_sync data/b018
	python3 03_optical_flow/optical_flow.py data/b020/01_sync data/b020
	python3 03_optical_flow/optical_flow.py data/b022/01_sync data/b022
	python3 03_optical_flow/optical_flow.py data/b024/01_sync data/b024
	python3 03_optical_flow/optical_flow.py data/b026/01_sync data/b026
	python3 03_optical_flow/optical_flow.py data/b028/01_sync data/b028
	python3 03_optical_flow/optical_flow.py data/b030/01_sync data/b030
	python3 03_optical_flow/optical_flow.py data/b032/01_sync data/b032
	python3 03_optical_flow/optical_flow.py data/b034/01_sync data/b034
	python3 03_optical_flow/optical_flow.py data/b036/01_sync data/b036
	python3 03_optical_flow/optical_flow.py data/b038/01_sync data/b038
	python3 03_optical_flow/optical_flow.py data/b040/01_sync data/b040
	python3 03_optical_flow/optical_flow.py data/b042/01_sync data/b042
	python3 03_optical_flow/optical_flow.py data/b044/01_sync data/b044
	python3 03_optical_flow/optical_flow.py data/b046/01_sync data/b046
	python3 03_optical_flow/optical_flow.py data/b048/01_sync data/b048

runfilt:
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b002
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b004
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b006
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b008
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b010
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b012
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b014
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b016
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b018
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b020
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b022
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b024
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b026
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b028
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b030
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b032
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b034
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b036
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b038
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b040
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b042
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b044
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b046
	python3 04_optical_flow_filter/optical_flow_filter.py data/b002/03_optical_flow data/b048


# Profiling of WinOp.py
profop:
	yappi -b -f pstat -o data/yappi_profop WinOp.py
	snakeviz data/yappi_profop

# Profiling of WinSub.py
profsub:
	yappi -b -f pstat -o data/yappi_profsub WinSub.py
	snakeviz data/yappi_profsub



