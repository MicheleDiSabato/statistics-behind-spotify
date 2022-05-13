import requests
from time import sleep

# Spotify's API often blocks the connection for no reason
#------------------------------------------------
def resilient_response(url, token_OAuth):
    # Headers to allow connection
    spotify_headers = {'Accept': 'application/json', 
            'Content-Type': 'application/json',
            'Authorization': 'Bearer ' + token_OAuth}
    
    # Perform up to 5 calls with 2 seconds cooldown
    for _ in range(0,5):
        success = True
        try:
            response = requests.get(url, headers = spotify_headers)
        except Exception:
            success = False

        if not success or not response.ok:
            success = False
            print("\n//EXCEPTION_LOG// Retrying in 2 seconds //")
            sleep(2)
        else:
            success = True
            break

    # Extract json
    if success:
        response = response.json()
        return response
    else:
        print("\n//EXCEPTION_MANAGER// Try getting new token //")
        new_token = input(">> ")
        if new_token == "give up":
            return "give up"
        if new_token == "show response":
            print(response.json())
            new_token = input(">> ")
        if new_token == "show url":
            print("//URL// " + url)
            new_token = input(">> ")
        response.close()
        return resilient_response(url, new_token)
