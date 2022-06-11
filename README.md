# Authors:

* [Andrea Cioffi](https://www.linkedin.com/in/andrea-cioffi-9799ba206/)
* [Michele Di Sabato](https://www.linkedin.com/in/michele-di-sabato/)
* [Francesco Pascuzzi](https://www.linkedin.com/in/fr-pscz/)
* [Chiara Schembri](https://www.linkedin.com/in/chiara-schembri-06398a223/)


# Statistics Behind Spotify - a nonparametric approach to music
![spotify_logo](readme_images/spotify_logo.png)
<p align="center">
  <img src="readme_images/spotify_logo.png">
</p>

## :dart: Framework and goals:
Spotify is one of the most widely used audio streaming and media services provider. On top of many songs and podcasts, Spotify also provides access to user and song related data. These data are accessible through the [Spotify API](https://developer.spotify.com/console/) and comprise many technical features that characterize each song, such as its tempo, its popularity, duration, loudness or even its mood. Based on these features, we asked ourselves: is it possible to predict the popularity of a song based on its technical features? Which aspect of a song should an artist focus on to get more visibility? Could record labels or a new up-and-coming artist benefit from these data-driven and data-informed decisions? Answering these questions required a lot of work, both in terms of data collection, preparation, processing, cleaning, exploration and in terms of analysis.

## :wrench: Statistical tools:
To answer these questions, we used a fully nonparametric approach. The statistical tools we applied include: Generalized Additive Models (GAMs), permutational tests, Robust Statistics, Mixed Effect Models, permutational ANOVA and MANOVA, Clustering (knn and k-means), Principal Component Analysis (PCA), Bootstrap and Conformal Prediction.

The files [report.pdf](/report.pdf) and [slides.pdf](slides.pdf) contain a brief summary of the project's results.
