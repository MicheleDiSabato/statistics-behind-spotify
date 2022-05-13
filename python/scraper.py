import requests


store_names = "titles.py"
store_data  = "spotify.csv"

playlist_id = "4EdmaJCUXvXsXJKXZGvj2r"
token_OAuth = 'BQDkmNgWnAlEmKEiqxjYUro66daJIoYCQ4KKiitLS-jACLJhX2hbZB8CTUMeUi658qqFlGolfIbsHgf6sczJ2-KIoQ89H6I_xloxq3Wn1ieAEKGj6bU8RQEIbgVx7aYTLAXwSj83mg' 

#-----------------------------------------
# PLAYLIST
#-----------------------------------------

def get_playlist(playlist_id, token_OAuth, offset = 0):
    playlist_endpoint = "https://api.spotify.com/v1/playlists/"

    spotify_headers = {'Accept': 'application/json', 
            'Content-Type': 'application/json',
            'Authorization': 'Bearer ' + token_OAuth}
    if offset == 0:
        response = requests.get(playlist_endpoint + playlist_id + '/tracks', headers = spotify_headers)
    else:
        response = requests.get(playlist_endpoint + playlist_id + '/tracks?offset=' + str(offset), headers = spotify_headers)
    playlist = response.json()
    return playlist

#-----------------------------------------
# TRACK (popularity, name)
#-----------------------------------------

def get_pop_name(track_id, token_OAuth, file_names, file_data, ID):
    track_endpoint = "https://api.spotify.com/v1/tracks/"

    spotify_headers = {'Accept': 'application/json', 
            'Content-Type': 'application/json',
            'Authorization': 'Bearer ' + token_OAuth}

    response = requests.get(track_endpoint + track_id, headers = spotify_headers)
    song = response.json()

    file_names.write("        " + str(ID) + ": \"" + song['name'] + "\",\n")
    file_data.write(str(ID) + "," + str(song['popularity']) + ",")

#-----------------------------------------
# TRACK (audio features)
#-----------------------------------------

def get_audio(track_id, token_OAuth, file_data):
    track_endpoint = "https://api.spotify.com/v1/audio-features/"

    spotify_headers = {'Accept': 'application/json', 
            'Content-Type': 'application/json',
            'Authorization': 'Bearer ' + token_OAuth}

    response = requests.get(track_endpoint + track_id, headers = spotify_headers)
    song = response.json()

    file_data.write(str(song['danceability']) + "," + str(song['energy']) + "," + str(song['key']) + "," + str(song['loudness']) + "," + str(song['mode']) + "," + str(song['speechiness']) + "," + str(song['acousticness']) + "," + str(song['instrumentalness']) + "," + str(song['liveness']) + "," + str(song['valence']) + "," + str(song['tempo']) + "," + str(song['duration_ms']) + "\n")

#------------------------------------------
# BREAK SPOTIFY RATE LIMIT TO CHECK WHAT IT IS
#------------------------------------------
# Apparently it's good enough

#start = time()


#while response.statuscode == 200:
#    response = requests.get("https://api.spotify.com/v1/tracks/" + track_id, headers = spotify_headers)

#end = time()
#print(end-start)

#------------------------------------------
# MAIN
#------------------------------------------


# Initialize files
file_names = open(store_names, "w")
file_names.write("tracks = {\n")
file_data  = open(store_data, "w")
file_data.write('ID,popularity,danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,duration_ms\n')

ID = 0

def write_data(playlist):
    global ID
    for track_obj in playlist['items']:

        ID += 1
        
        uri = track_obj['track']['uri']
        track_id = uri[14:] 

        get_pop_name(track_id, token_OAuth, file_names, file_data, ID)
        get_audio(track_id, token_OAuth, file_data)
        print("Song " + str(ID) + " succesfully saved.", end = "\r")

for offset in [0, 100, 200, 300, 400]:
    playlist = get_playlist(playlist_id, token_OAuth, offset)
    write_data(playlist)


file_names.write("        }\n")
file_names.close()
file_data.close()
print()
