[![Spotify](https://europe-west1-drive-api-281722.cloudfunctions.net/nowPlaying)](https://open.spotify.com/user/garcilaso13)

Display a svg with the track you are currently playing. If you aren't playing any song, a recently played track will be shown. 

Base on [this](https://github.com/novatorem/novatorem) repo but using Scala and deployed in Cloud Functions.

## Run locally

```shell
./mvnw function:run -Drun.functionTarget=<class-package-path>  -Drun.port=<port>
```