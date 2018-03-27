from sklearn.metrics import silhouette_samples, silhouette_score
from scipy.cluster.hierarchy import linkage, dendrogram
from scipy.spatial.distance import pdist, squareform

import matplotlib.pyplot as plt
import matplotlib.cm as cm
import numpy as np

import csv
import os

def get_cluster_colors(cluster_labels):
  colors = list()
  color_map = {
    0:'red',
    1:'orange',
    2:'yellow',
    3:'green',
    4:'blue',
    5:'indigo',
    6:'purple',
    7:'black'
  }
  for i in cluster_labels:
    colors.append(color_map[i])

  return colors

def get_cluster_labels(header, clusters):
  labels = list()
  for congressman_id in header:
    for i in range(len(clusters)):
      if congressman_id in clusters[i]:
        labels.append(i)
        break

  return np.array(labels)

def distance_matrix(method):
  distance = list()
  header = None

  with open('distance-{}.csv'.format(method), 'r') as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
      if not header:
        header = row[1:]
      else:
        distance.append(row[1:])

  return distance, header

def _silhouette(clusters, method, k, series_type):
  fig, ax = plt.subplots()

  matrix, header = distance_matrix(method)
  cluster_labels = get_cluster_labels(header, clusters)

  # The silhouette_score gives the average value for all the samples.
  # This gives a perspective into the density and separation of the formed
  # clusters
  silhouette_avg = silhouette_score(matrix, cluster_labels, metric='precomputed')

  # Compute the silhouette scores for each sample
  sample_silhouette_values = silhouette_samples(matrix, cluster_labels, metric='precomputed')

  y_lower = 10
  n_clusters = len(clusters)
  for i in range(n_clusters):
    # The 1st subplot is the silhouette plot
    # The silhouette coefficient can range from -1, 1 but in this example all
    # lie within [-0.1, 1]
    ax.set_xlim([-0.1, 1])
    # The (n_clusters+1)*10 is for inserting blank space between silhouette
    # plots of individual clusters, to demarcate them clearly.
    ax.set_ylim([0, len(matrix) + (n_clusters + 1) * 10])

    # Aggregate the silhouette scores for samples belonging to
    # cluster i, and sort them
    ith_cluster_silhouette_values =  \
      sample_silhouette_values[cluster_labels == i]

    ith_cluster_silhouette_values.sort()

    size_cluster_i = ith_cluster_silhouette_values.shape[0]
    y_upper = y_lower + size_cluster_i

    color = cm.spectral(float(i) / n_clusters)
    ax.fill_betweenx(np.arange(y_lower, y_upper),
                      0, ith_cluster_silhouette_values,
                      facecolor=color, edgecolor=color, alpha=0.7)

    # Label the silhouette plots with their cluster numbers at the middle
    ax.text(-0.05, y_lower + 0.5 * size_cluster_i, str(i))

    # Compute the new y_lower for next plot
    y_lower = y_upper + 10  # 10 for the 0 samples

  ax.set_title("The silhouette plot for the various clusters.")
  ax.set_xlabel("The silhouette coefficient values")
  ax.set_ylabel("Cluster label")

  # The vertical line for average silhouette score of all the values
  ax.axvline(x=silhouette_avg, color="red", linestyle="--")

  ax.set_yticks([])  # Clear the yaxis labels / ticks
  ax.set_xticks([-0.1, 0, 0.2, 0.4, 0.6, 0.8, 1])

  directory = '../img/{}/silhouette/'.format(series_type)
  if not os.path.exists(directory):
    os.makedirs(directory)

  fig.savefig('../img/{}/silhouette/{}_k{}.png'.format(series_type, method, k), bbox_inches='tight')
  plt.close(fig)

def _dendrogram(clusters, method, k, series_type):
  fig, ax = plt.subplots()

  matrix, header = distance_matrix()
  cluster_labels = get_cluster_labels(header, clusters)

  matrix = np.array(matrix)
  float_matrix = matrix.astype(np.float)
  linkage_matrix = linkage(squareform(float_matrix))

  dg = dendrogram(linkage_matrix, no_plot=True)
  dg_labels = list()
  for idx in dg['leaves']:
    dg_labels.append(cluster_labels[idx])

  dg = dendrogram(linkage_matrix, leaf_font_size=8,
    truncate_mode='lastp', p=10,
    show_contracted=True)

  directory = '../img/{}/dendrogram/'.format(series_type)
  if not os.path.exists(directory):
    os.makedirs(directory)

  fig.savefig('../img/{}/dendrogram/{}.png'.format(series_type, method), bbox_inches='tight')
  plt.close(fig)