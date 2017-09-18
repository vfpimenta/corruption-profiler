from profiler import Profiler
from sklearn.cluster import KMeans
from scipy.spatial import distance
import matplotlib.pyplot as plt
import numpy as np
import itertools

def main():
    profiler = Profiler()
    series = profiler.get_general(opt='state')
    kmeans = KMeans(n_clusters=3).fit(list(series.values()))

    for i in range(len(series.keys())):
        if kmeans.labels_[i] == 0:
            plt.plot(list(series.values())[i], color='mistyrose')
        elif kmeans.labels_[i] == 1:
            plt.plot(list(series.values())[i], color='skyblue')
        elif kmeans.labels_[i] == 2:
            plt.plot(list(series.values())[i], color='palegreen')

    plt.plot(kmeans.cluster_centers_[0], color='red', linewidth='2')
    plt.plot(kmeans.cluster_centers_[1], color='blue', linewidth='2')
    plt.plot(kmeans.cluster_centers_[2], color='green', linewidth='2')
    plt.show()

    '''
    idist = []
    for i in range(len(series.keys())):
        idist.append(distance.euclidean(list(series.values())[i],
            kmeans.cluster_centers_[kmeans.labels_[i]]))

    odist = list(map(lambda t: distance.euclidean(t[0],t[1]), 
        itertools.combinations(kmeans.cluster_centers_, 2)))

    print('Inner-cluster distance: {}'.format(np.mean(idist)))
    print('Outer-cluster distance: {}'.format(np.mean(odist)))
    '''

if __name__ == '__main__':
    main()