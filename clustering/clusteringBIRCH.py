# affinity propagation clustering
from pandas import read_csv
from numpy import unique
from numpy import where
from numpy import log
from sklearn.cluster import Birch
from matplotlib import pyplot

df = read_csv("artist_clustering.csv",index_col=0)

df["followers"] = df["followers"].apply(lambda x: log(x+1))
df = df.iloc[:,2:]

df["popularity"] = df["popularity"]/100
df["followers"] = df["followers"]/df["followers"].max()

# define the model
n = 7
model = Birch(threshold=0.01, n_clusters=n)
# fit the model
model.fit(df)
# assign a cluster to each example
yhat = model.predict(df)
# retrieve unique clusters
clusters = unique(yhat)

df["Yhat"] = yhat
# create scatter plot for samples from each cluster
c = len(clusters)
for cluster in clusters:
	# create scatter of these samples
    pyplot.scatter(df[df["Yhat"] == cluster]["popularity"], df[df["Yhat"] == cluster]["followers"])
# show the plot
pyplot.title(f"BIRCH, n={n},c={c}")
pyplot.show()


### FINAL ###
final_df = read_csv("additional_artists.csv")
final_df = final_df.iloc[:,:4]
final_df["category"] = yhat

final_df.to_csv("artist_BIRCH.csv",index=False)
