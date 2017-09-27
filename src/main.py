from collections import OrderedDict
from profiler import Profiler
from sklearn.cluster import KMeans
from scipy.spatial import distance
from optparse import OptionParser
import matplotlib.pyplot as plt
import numpy as np
import itertools
import time
import os.path

parser = OptionParser()
parser.add_option('-c', '--clusters', dest='opt_n_clusters', type='int',
    help='Number of clusters (2 to 8).', metavar='NUMBER')
parser.add_option('-p', '--plot', dest='opt_plot', action='store_true',
    help='Plot the graph for the clusters and its members.', metavar='BOOLEAN')
parser.add_option('-f', '--fit', dest='opt_fit', action='store_true',
    help='Claculate the inner/outer distance to clusters.', metavar='BOOLEAN')

(options, args) = parser.parse_args()

def plot_cluster(n_clusters, series, kmeans):
    deep_colors = ['red','blue','green','purple','olive','darkslategray','saddlebrown','deeppink']
    light_colors = ['mistyrose','skyblue','palegreen','mediumpurple','khaki','aquamarine','tan','pink']

    if n_clusters > 8 or n_clusters < 2:
        raise Exception('Unable to plot {} clusters!'.format(n_clusters))

    #for key in series.keys():
        #plt.plot(series[key][0][2], color=light_colors[series[key][2]]) 

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

def main(n_clusters, threshold, _plot=False, _fit=False):
    profiler = Profiler()
    series = profiler.read_congressman_json()
    series_list = [x[2] for x in series.values()]
    kmeans = KMeans(n_clusters=n_clusters).fit(series_list)
    series_dist = kmeans.transform(series_list)

    for i in range(len(series.keys())):
        key = list(series.keys())[i]
        series[key] = (series[key], series_dist[i], kmeans.labels_[i])

    # Plot the cluster curves
    if _plot:
        plot_cluster(n_clusters, series, kmeans)

    
    # Calculate inner/outer cluster distance to select best partition fit
    if _fit:
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

    filename = '../data/outliers-{}.log'.format(time.strftime('%Y-%m-%d-%H:%M:%S'))
    with open(filename,'w') as logfile:
        logfile.write('Outliers found for {} clusters and threshold {}\n'.format(n_clusters, threshold))
        logfile.write('UF\tName\n')
        for outlier in outliers:
            logfile.write('{}\t{}\n'.format(series[outlier][0][1], series[outlier][0][0]))

if __name__ == '__main__':
    main(n_clusters=options.opt_n_clusters, threshold=2.32, 
        _plot=options.opt_plot, _fit=options.opt_fit)