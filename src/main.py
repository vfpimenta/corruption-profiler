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
    help='Number of clusters for kmeans.', metavar='NUMBER')
parser.add_option('-p', '--plot', dest='opt_plot', action='store_true',
    help='Plot the graph for the clusters and its members. Number of clusters to plot are limited to 8 to avoid visual clutter.', metavar='BOOLEAN')
parser.add_option('-f', '--fit', dest='opt_fit', action='store_true',
    help='Calculate the inner/outer distance to clusters.', metavar='BOOLEAN')
parser.add_option('-l', '--log', dest='opt_log', action='store_true',
    help='Print outlier log file.', metavar='BOOLEAN')

(options, args) = parser.parse_args()

def plot_cluster(n_clusters, series, kmeans):
    colors = dict(
        deep=['red','blue','green','purple','olive','darkslategray','saddlebrown','deeppink'],
        light=['mistyrose','skyblue','palegreen','mediumpurple','khaki','aquamarine','tan','pink']
    )

    if n_clusters > 8 or n_clusters < 2:
        raise Exception('Unable to plot {} clusters!'.format(n_clusters))

    for key in series.keys():
        plt.plot(series[key][0][2], color=colors['light'][series[key][2]]) 

    for i in range(len(kmeans.cluster_centers_)):
        plt.plot(kmeans.cluster_centers_[i], 
            color=colors['deep'][i], linewidth='2') 

    plt.show()

def in_out_distance(series_list, kmeans):
    idist = []
    for i in range(len(series_list)):
        idist.append(distance.euclidean(series_list[i],
            kmeans.cluster_centers_[kmeans.labels_[i]]))

    odist = list(map(lambda t: distance.euclidean(t[0],t[1]), 
        itertools.combinations(kmeans.cluster_centers_, 2)))

    print('Inner-cluster distance: {}'.format(np.mean(idist)))
    print('Outer-cluster distance: {}'.format(np.mean(odist)))

def print_outlier_log(n_clusters, threshold, series, outliers):
    filename = '../log/outliers-{}.log'.format(time.strftime('%Y-%m-%d-%H:%M:%S'))
    with open(filename,'w') as logfile:
        logfile.write('Outliers found for {} clusters and threshold {}\n'.format(n_clusters, threshold))
        logfile.write('UF\tName\n')
        for outlier in outliers:
            logfile.write('{}\t{}\n'.format(series[outlier][0][1], series[outlier][0][0]))

def main(n_clusters, threshold=2.32, _plot=False, _fit=False, _log=False):
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
        in_out_distance(series_list, kmeans)

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

    # Log results
    if _log:
        print_outlier_log(n_clusters,threshold,series,outliers)

if __name__ == '__main__':
    if options.opt_n_clusters == None or options.opt_n_clusters < 2:
        raise Exception('Number of clusters not provided!')
    main(n_clusters=options.opt_n_clusters, _plot=options.opt_plot, 
        _fit=options.opt_fit, _log=options.opt_log)