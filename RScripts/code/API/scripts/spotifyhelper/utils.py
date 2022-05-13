import json
import requests
from math import ceil
import urllib.parse
from spotifyhelper.getters import resilient_response

def file_to_artlist(ifile, ofile, token_OAuth):
    output_json = {}
    # API url for non-audio features
    noaudio_endpoint = "https://api.spotify.com/v1/tracks/"

    with open(ifile,'r') as input_file:
        with open(ofile,'a') as output_file:
            for line in input_file:
                track_uri = line[:-1]

                artlist = {}

                response = resilient_response(noaudio_endpoint + track_uri,token_OAuth)
                for artist in response["artists"]:
                    art_name = artist["name"]
                    art_uri = artist["id"]
                    output_file.write(f"{art_uri}\n")
                    artlist[art_uri] = art_name
                output_json[track_uri] = artlist
                
            output_file.close()
        input_file.close()
    output_json = json.dumps(output_json)
    return output_json

def discography_to_json(idict, token_OAuth):
    output_json = {}

    # API url for searching artists
    search_endpoint = "https://api.spotify.com/v1/search?q=artist%3A"
    search_suffix   = "&type=track&limit=50&offset="

    for uri in idict:
        songs = []

        initial_r = resilient_response(search_endpoint + urllib.parse.quote(idict[uri]) + search_suffix + "0", token_OAuth)
        if initial_r == "give up":
            break
        n_songs   = initial_r["tracks"]["total"]

        for song in initial_r["tracks"]["items"]:
            songs.append(song["popularity"])

        n_reqs = ceil(min(n_songs,1000)/50) - 1

        response = 0
        for i in range(n_reqs):
            offset = str((i+1)*50)
            response = resilient_response(search_endpoint + urllib.parse.quote(idict[uri]) + search_suffix + offset, token_OAuth)
            
            if response == "give up":
                break

            for song in response["tracks"]["items"]:
                songs.append(song["popularity"])

        if response == "give up":
            break

        artist = {"popularity" : songs, "total" : n_songs}

        output_json[uri] = artist

    output_json = json.dumps(output_json)
    return output_json
