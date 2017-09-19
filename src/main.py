from profiler import Profiler
from sklearn.cluster import KMeans
from scipy.spatial import distance
import matplotlib.pyplot as plt
import numpy as np
import itertools

def plot_cluster(n_clusters, series, kmeans):
    deep_colors = ['red','blue','green','purple','olive','darkslategray','saddlebrown','deeppink']
    light_colors = ['mistyrose','skyblue','palegreen','mediumpurple','khaki','aquamarine','tan','pink']

    if n_clusters > 8 or n_clusters < 2:
        raise Exception('Unable to plot {} clusters!'.format(n_clusters))

    for key in series.keys():
        plt.plot(series[key][0], color=light_colors[series[key][2]])    

    for i in range(len(kmeans.cluster_centers_)):
        plt.plot(kmeans.cluster_centers_[i], 
            color=deep_colors[i], linewidth='2') 

    plt.show()

def iodistance(series_list, kmeans):
    idist = []
    for i in range(len(series_list)):
        idist.append(distance.euclidean(series_list[i],
            kmeans.cluster_centers_[kmeans.labels_[i]]))

    odist = list(map(lambda t: distance.euclidean(t[0],t[1]), 
        itertools.combinations(kmeans.cluster_centers_, 2)))

    print('Inner-cluster distance: {}'.format(np.mean(idist)))
    print('Outer-cluster distance: {}'.format(np.mean(odist)))

def main(n_clusters, threshold, _plot=False, _dist=False):
    profiler = Profiler()
    series = profiler.get_general(opt='state')
    series_list = list(series.values())
    kmeans = KMeans(n_clusters=n_clusters).fit(series_list)
    series_dist = kmeans.transform(series_list)

    for i in range(len(series.keys())):
        key = list(series.keys())[i]
        series[key] = (series[key], series_dist[i], kmeans.labels_[i])

    # Plot the cluster curves
    if _plot:
        plot_cluster(n_clusters, series, kmeans)

    
    # Calculate inner/outer cluster distance to select best partition
    if _dist:
        iodistance(series_list, kmeans)

    # Find outliers
    outliers = []
    for key in series.keys():
        cluster_distances = list(map(lambda v: v[1][v[2]], 
            list(filter(lambda el: el[2] == series[key][2], 
                series.values()))))
        mu = np.mean(cluster_distances)
        sd = np.std(cluster_distances)
        d = series[key][1][series[key][2]]

        if  d > (mu+threshold*sd) or d < (mu-threshold*sd):
            outliers.append(key)

    print('Outliers found: {}'.format(outliers))

if __name__ == '__main__':
    main(n_clusters=5, threshold=2.32)