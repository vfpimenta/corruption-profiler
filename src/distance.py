#!/usr/bin/python3

from sklearn.metrics import pairwise
from profiler import Profiler
import pandas as pd

def main():
	profiler = Profiler(light=True)
	series = profiler.read_congressman_json(54)
	series_list = [x[-1] for x in series.values()]

	distances = pairwise.euclidean_distances(series_list, series_list)
	df = pd.DataFrame(distances)
	df.columns = series.keys()
	df.index = series.keys()

	df.to_csv("distance-kmeans.csv")

if __name__ == '__main__':
	main()