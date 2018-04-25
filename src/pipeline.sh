echo "[PIPELINE] Starting pipeline"
echo "[PIPELINE] Building data"
#Rscript outliers.R -d
series=$1
./flip.sh
Rscript cibm.R -c -g -s $series
Rscript distance.R -s $series
python3 distance.py --series-type=$series
python3 main.py -c 4 -d --series-type=$series
python3 main.py -c 6 -d --series-type=$series
python3 main.py -c 8 -d --series-type=$series
echo "[PIPELINE] Data building finished"
sleep 5
echo "[PIPELINE] Running analysis"
python3 mstknn_analysis.py --series-type=$series -k 2,3,4,5 --method=all --function=both -e
python3 mstknn_analysis.py --series-type=$series -k 4,6,8 --method=kmeans --function=both -e
python3 mstknn_analysis.py --series-type=$series -k 2,3,4,5 --method=all --function=both -c
python3 mstknn_analysis.py --series-type=$series -k 4,6,8 --method=kmeans --function=both -c
./flip.sh