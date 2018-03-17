#!/usr/bin/python3

from collections import OrderedDict
from profiler import Profiler
from sklearn.cluster import KMeans
from scipy.spatial import distance
from optparse import OptionParser
from itertools import compress
import matplotlib.pyplot as plt
import numpy as np
import itertools
import time
import json
import os

parser = OptionParser()
parser.add_option('-c', '--clusters', dest='opt_n_clusters', type='int',
    help='Number of clusters for kmeans.', metavar='NUMBER')
parser.add_option('-p', '--plot', dest='opt_plot', action='store_true',
    help='Plot the graph for the clusters and its members. Number of clusters to plot are limited to 8 to avoid visual clutter.', metavar='BOOLEAN')
parser.add_option('-f', '--fit', dest='opt_fit', action='store_true',
    help='Calculate the inner/outer distance to clusters.', metavar='BOOLEAN')
parser.add_option('-l', '--log', dest='opt_log', action='store_true',
    help='Print outlier log file.', metavar='BOOLEAN')
parser.add_option('-t', '--threshold', dest='opt_threshold', type='float',
    help='Threshold for outlier detection.', metavar='NUMBER')
parser.add_option('-d', '--dump', dest='opt_dump', action='store_true',
    help='Dump clusters to data/ folder.', metavar='BOOLEAN')

(options, args) = parser.parse_args()

def plot_cluster(n_clusters, legislature, series, kmeans):
    colors = dict(
        deep=['red','blue','green','purple','olive','darkslategray','saddlebrown','deeppink'],
        light=['mistyrose','skyblue','palegreen','mediumpurple','khaki','aquamarine','tan','pink']
    )

    if n_clusters > 8 or n_clusters < 2:
        raise Exception('Unable to plot {} clusters!'.format(n_clusters))

    fig, ax = plt.subplots( nrows=1, ncols=1 )

    for key in series.keys():
        ax.plot(series[key][0][-1], color=colors['light'][series[key][-1]]) 

    for i in range(len(kmeans.cluster_centers_)):
        ax.plot(kmeans.cluster_centers_[i], 
            color=colors['deep'][i], linewidth='2') 

    #plt.show()
    fig.savefig('../img/cluster/{}_clusters_{}leg-{}.png'.format(n_clusters,legislature,time.strftime('%Y-%m-%d-%H:%M:%S')), bbox_inches='tight')
    plt.close(fig)

def in_out_distance(series_list, kmeans):
    idist = []
    for i in range(len(series_list)):
        idist.append(distance.euclidean(series_list[i],
            kmeans.cluster_centers_[kmeans.labels_[i]]))

    odist = list(map(lambda t: distance.euclidean(t[0],t[1]), 
        itertools.combinations(kmeans.cluster_centers_, 2)))

    print('Inner-cluster distance: {}'.format(np.mean(idist)))
    print('Outer-cluster distance: {}'.format(np.mean(odist)))

def print_outlier_log(n_clusters, threshold, series, outliers, legislature):
    filename = '../log/outliers-{}.log'.format(time.strftime('%Y-%m-%d-%H:%M:%S'))
    with open(filename,'w') as logfile:
        logfile.write('Outliers found for {} clusters and threshold {}\n'.format(n_clusters, threshold))
        logfile.write('Considering congressman for legislature #{}\n'.format(legislature))
        logfile.write('UF\tName\n')
        for outlier in outliers:
            logfile.write('{}\t{}\n'.format(series[outlier][0][1], series[outlier][0][0]))

def dump(congressman_ids, congressman_labels, n_clusters, legislature):
    clusters = list()
    for label in set(congressman_labels):
        clusters.append(list(compress(congressman_ids, list(congressman_labels == label))))
    directory = "../data/default/dump/kmeans/k-{}".format(n_clusters)
    path = "{}/dump-clusters-{}.json".format(directory, legislature)

    if not os.path.exists(directory):
        os.makedirs(directory)

    with open(path,'w') as jsonfile:
        json.dump(clusters, jsonfile)

def main(n_clusters, legislatures, threshold=2.32, _plot=False, _fit=False, _log=False, _dump=False):
    if not threshold:
        threshold = 2.32

    for legislature in legislatures:
        print('Running profiler for legislature {}...'.format(legislature))
        profiler = Profiler()
        series = profiler.read_congressman_json(legislature=legislature)
        if _dump:
            with open('../data/JSON/congressman_{}_outliers.json'.format(legislature)) as jsonfile:    
                file_outliers = json.load(jsonfile)
            series_list = list()
            keys_list = list()
            for congressman_id in series.keys():
                congressman = series.get(congressman_id)
                if congressman_id not in file_outliers:
                    keys_list.append(congressman_id)
                    series_list.append(congressman[-1])
        else:
            series_list = [x[-1] for x in series.values()]

        kmeans = KMeans(n_clusters=n_clusters).fit(series_list)
        series_dist = kmeans.transform(series_list)

        if _plot or _log:
            for key in range(len(series_list)):
                key = list(series.keys())[i]
                series[key] = (series[key], series_dist[i], kmeans.labels_[i])

        # Plot the cluster curves
        if _plot:
            plot_cluster(n_clusters, legislature, series, kmeans)

        
        # Calculate inner/outer cluster distance to select best partition fit
        if _fit:
            in_out_distance(series_list, kmeans)

        # Dump clusters for evaluation
        if _dump:
            dump(keys_list, kmeans.labels_, n_clusters, legislature)

        # Log results
        if _log:
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

            print_outlier_log(n_clusters,threshold,series,outliers,legislature)

if __name__ == '__main__':
    if options.opt_n_clusters == None or options.opt_n_clusters < 2:
        raise Exception('Number of clusters not provided!')
    main(n_clusters=options.opt_n_clusters, legislatures=[53,54,55],
        threshold=options.opt_threshold, _plot=options.opt_plot, 
        _fit=options.opt_fit, _log=options.opt_log, _dump= options.opt_dump)