from sklearn.metrics import silhouette_samples, silhouette_score
from scipy.cluster.hierarchy import linkage, dendrogram
from scipy.spatial.distance import pdist, squareform

import matplotlib.pyplot as plt
import matplotlib.cm as cm
import numpy as np

import csv

def get_cluster_labels(header, clusters):
  labels = list()
  for congressman_id in header:
    for i in range(len(clusters)):
      if congressman_id in clusters[i]:
        labels.append(i)
        break

  return np.array(labels)

def distance_matrix():
  distance = list()
  header = None

  with open('distance.csv', 'r') as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
      if not header:
        header = row[1:]
      else:
        distance.append(row[1:])

  return distance, header

def _silhouette(clusters, method, k):
  fig, ax = plt.subplots()

  matrix, header = distance_matrix()
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

  fig.savefig('../img/default/silhouette/{}_k{}.png'.format(method, k), bbox_inches='tight')
  plt.close(fig)

def _dendrogram(clusters, method, k):
  #fig, ax = plt.subplots()

  matrix, header = distance_matrix()
  cluster_labels = get_cluster_labels(header, clusters)

  linkage_matrix = linkage(squareform(matrix))

  dg = dendrogram(linkage_matrix, labels=cluster_labels)

  plt.plot(dg)
  plt.show()