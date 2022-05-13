from spotifyhelper.utils import file_to_artlist
import json

# build database mapping songs to authors

OAuth = input('Input Spotify token   >> ')
ifile = input('Input file (songs)    >> ')
ofile = input('Output file (artists) >> ')
jsonf = input('Output file (json)    >> ')

out_json = file_to_artlist_alt(ifile,ofile,OAuth)

with open(jsonf, 'w') as outfile:
    json.dump(out_json, outfile)
