import json
import pandas as pd
import numpy as np


# -------------------------
# Input filenames
# -------------------------

f_od  = input("Filename for original dataset     >> ")
f_as  = input("Filename for artists by song      >> ")
f_pa  = input("Filename for popularity by artist >> ")

f_out = input("Filename for saving new dataset   >> ")


# -------------------------
# Load data
# -------------------------

original_dataset = pd.read_csv(f_od)

with open(f_as, "r") as f:
    artists_by_song = json.loads(json.load(f))
    f.close()

with open(f_pa, "r") as f:
    popularity_by_artist = json.loads(json.load(f))
    f.close()


# -------------------------
# Multiple artists?
# -------------------------

def combine(disc_list,mode):
    # mode can be either "top" or "add"
    if mode == "top":
        max_songs = 0
        for disc in disc_list:
            if disc["total"] == 0:
                disc["popularity"].append(0)
                disc["total"] = 1
            if disc["total"] > max_songs:
                discography = disc
                max_songs = disc["total"]
            elif disc["total"] == max_songs:
                discography = combine([discography, disc], "add")
        return discography
    elif mode == "add":
        discography = {"popularity":[],"total":0}
        for disc in disc_list:
            if disc["total"] == 0:
                disc["popularity"].append(0)
                disc["total"] = 1
            discography["popularity"] += disc["popularity"]
            discography["total"] += disc["total"]
        return discography
    else:
        print("[EE] Mode \"" + mode + "\" unknown. Try new mode.")
        mode = input(">> ")
        return combine(disc_list,mode)


# -------------------------
# Normalization function
# -------------------------

def stndrd(popularity,discography):
    p = popularity - np.mean(discography["popularity"])
    if discography["total"] > 1:
        sd = np.std(discography["popularity"])
        if sd > 0:
            p = p/sd
    return p

def minmax(popularity,discography):
    M = max(discography["popularity"])
    m = min(discography["popularity"])
    if M-m > 0:
        p = (popularity-m)/(M-m)
    else:
        p = 0.5
    return p

def pctile(popularity,discography):
    v = [x < popularity for x in discography["popularity"]]
    p = sum(v)/len(v)
    return p


# -------------------------
# Get discography
# -------------------------

def get_discography(track_uri):
    disc_list = []
    artists = artists_by_song[track_uri]
    for artist in artists:
        disc_list.append(popularity_by_artist[artist])

    return combine(disc_list, "add")


# -------------------------
# Apply normalization
# -------------------------

for index in range(original_dataset.shape[0]):
    print(" -")
    title = original_dataset.loc[index,"title"]
    t_uri = original_dataset.loc[index,"uri"]
    print(f"Song: {title}")
    print("Artists:")
    artists = artists_by_song[t_uri]
    for artist in artists:
        a_name = artists[artist]
        print(f"  {a_name} ({artist})")
    original_dataset.loc[index,"popularity"] = minmax(
            original_dataset.loc[index,"popularity"],
            get_discography(original_dataset.loc[index,"uri"])
            )


# -------------------------
# Save dataset
# -------------------------

original_dataset.to_csv(f_out, index = False)
