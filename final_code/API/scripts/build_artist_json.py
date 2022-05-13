import json

# generate list of unique artists

with open("text/song_artists.json","r") as f:
    songs = json.loads(json.load(f))

output_json = {}

for t_uri in songs:
    for a_uri in songs[t_uri]:
        output_json[a_uri] = songs[t_uri][a_uri]

with open("text/artist_db.json","w") as f:
    json.dump(json.dumps(output_json),f)
