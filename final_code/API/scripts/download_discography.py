from spotifyhelper.utils import discography_to_json
import json

# Download list of popularities for artist discography

OAuth = input('Input Spotify token   >> ')
ifile = input('Input file (artists)  >> ')
ofile = input('Output file (pop)     >> ')

with open(ifile, 'r') as infile:
    artists = json.loads(json.load(infile))

out_json = discography_to_json(artists,OAuth)

with open(ofile, 'w') as outfile:
    json.dump(out_json, outfile)
