echo "========================================================================"
echo "[PIPELINE] Starting pipeline"
echo "========================================================================"
echo "[PIPELINE] Building data"
Rscript outliers.R -d
Rscript cibm.R -c -g
Rscript distance.R 
python3 main.py -c 4 -d
python3 main.py -c 6 -d
python3 main.py -c 8 -d
echo "[PIPELINE] Data building finished"
sleep 5
echo "[PIPELINE] Running analysis"
python3 mstknn_analysis.py -k 2,3,4,5 --method=all --function=both -e
python3 mstknn_analysis.py -k 2,3,4,5 --method=all --function=both -c
python3 mstknn_analysis.py -k 4,6,8 --method=kmeans --function=both -c