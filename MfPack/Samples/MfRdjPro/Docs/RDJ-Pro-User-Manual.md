# RDJ Pro GUI guide

This guide explains what the controls in RDJ Pro do. It avoids technical details and uses the names shown on screen.

> Tip: Move the mouse over a control. Many controls show a short hint.

## Main window

| Control | What it does |
|---|---|
| Clock | Shows the local time. |
| Setup | Opens the application settings. |
| Playlist | Opens the Playlist Editor. |
| Effects | Shows or hides the master effects window. |
| Broadcast | Opens or closes Media Server Control. |
| Notes | Shows or hides the notes area. |
| Minimize | Sends RDJ Pro to the taskbar. |
| Maximize/Restore | Switches between a full-size and normal window. |
| Close | Closes RDJ Pro. |
| Notes box | A place for temporary notes. It is not sent to listeners. |
| Clear Notes | Empties the notes box. |
| DJ Name | The name shown to listeners. |
| Showtitle | The current programme or show name. |
| Event | A name for a live event or loopback source. |
| Title | A description of the current live activity. |
| Set DJ and Showtitle | Publishes the entered names, event information, and selected logo. |
| Logo | Shows the current broadcast image. Double-click it to choose another image. |

When a normal audio track is playing, its artist and title are shown to listeners. During loopback capture, **Event** and **Title** are used when the captured programme has no song information.

## Setup

Use the three buttons on the left to switch between **General**, **Broadcast**, and **Audio Recorder** settings. Choose **OK** to save changes or **Cancel** to leave without saving.

### General

| Control | What it does |
|---|---|
| Channel decks | Chooses how many normal player decks RDJ Pro creates. |
| Loopback decks | Chooses how many live computer-audio decks RDJ Pro creates. |
| Audio buffer duration | Changes how much audio RDJ Pro keeps ready. A larger value is safer; a smaller value reacts faster. |
| Main output (MASTER) | Chooses the speakers or output used for the main mix. |
| Enable Headphones output (PFL/CUE) | Turns the separate headphone preview output on or off. |
| Cue output (PHONES) | Chooses the headphone or preview device. |
| Enable Microphone input | Shows and enables the microphone deck. |
| Microphone input | Chooses the microphone or input device. |
| Audio Recordings | Chooses where audio recordings are saved. |
| Database | Chooses where the RDJ Pro music database is stored. |
| Artwork | Shows the compatibility artwork location. Caddy Artwork is the public browser location. |
| Video Recordings | Chooses where video recordings are saved. |
| `...` buttons | Browse for the folder beside the button. |
| Override System Sleep | Tries to keep the computer awake while RDJ Pro is running. |

### Broadcast

| Control | What it does |
|---|---|
| Root Path | Chooses the main Caddy website folder. |
| Config Path | Chooses the Caddy configuration file. |
| JSON Path | Chooses the `nowplaying.json` file used by the browser. |
| Caddy Command | Shows or sets the command used to run Caddy. |
| Artwork Path | Chooses where browser artwork is stored. |
| Camera | Chooses the camera used for video broadcasting. |
| Refresh Cameras | Searches again for connected cameras. |
| Video Path | Chooses where live video files are published. |
| Content Type URL | Sets the video type sent to the browser. Normally this can stay unchanged. |
| MP4 Segment size | Changes the length of each small live-video part. Normally this can stay unchanged. |
| Local network list | Shows this computer's available local IPv4 addresses. |
| Refresh | Searches again for local network addresses. |
| Use address | Adds the selected local address to the managed Caddy configuration. |
| Remove LAN | Removes the managed local address from the Caddy configuration. |

### Audio Recorder

| Control | What it does |
|---|---|
| Output format | Chooses the recording file format. |
| Enable stream switch detection | Lets the recorder react when Windows changes an audio device. |
| PCM audio output format | Uses the normal uncompressed Windows audio format. |
| Disable MMCSS | Disables Windows multimedia priority. Leave this off unless troubleshooting. |
| Don't overwrite existing files | Protects recordings that already have the same filename. |
| Latency | Changes the recorder response delay. The default is suitable for most users. |
| Capture buffer size | Changes how much incoming audio is kept ready for recording. |

## Master deck

| Control | What it does |
|---|---|
| Left and Right faders | Set the main output volume for each side. |
| Lock | Moves the left and right faders together. |
| Balance | Moves the sound towards the left or right speaker. Double-click to return to the centre. |
| PFL volume | Sets the headphone preview volume. |
| PFL Mute | Silences the headphone preview output. |
| Filename | Sets the name of a new audio recording. RDJ Pro adds the file extension. |
| Device | Chooses the source used by the endpoint recorder. |
| Pre-FX | Records the clean mix before master effects. |
| Post-FX | Records the mix after master effects. |
| Start | Starts or stops the audio recording. |
| Recording and time indicators | Show whether recording is active and how long it has run. |

## Channel deck

Each channel deck plays an audio file or playlist.

| Control | What it does |
|---|---|
| Load | Selects an audio file. |
| Play/Pause | Starts playback or pauses it. |
| Stop | Stops playback and returns to the start. |
| Progress bar | Shows the playback position. Click or drag it to move within the track. |
| Volume | Sets this deck's level in the main mix. |
| Balance | Moves this deck towards the left or right speaker. |
| Pitch | Makes playback slower or faster. Double-click to return to normal speed. |
| Input Gain | Adjusts the level before the deck's effects and volume control. |
| PFL | Sends this deck to the headphone preview. |
| Mute | Silences this deck in the main mix. |
| Auto Cue | Starts playback automatically when the volume fader is moved up. |
| X Fade | Links this deck with one other selected deck. Raising one linked fader lowers the other. |
| Playlist | Opens the playlist choice and starts playlist mode. |
| Previous / Next | Moves to the previous or next playlist item. |
| Loop | Repeats playback. In playlist mode it repeats the playlist. |
| Shuffle | Plays playlist items in a mixed order. |
| EQ | Turns the deck's tone adjustment on or off. |
| EQ Gain | Raises or lowers the selected tone area. |
| EQ Frequency | Chooses the centre of the tone area. |
| EQ Q | Makes the adjusted tone area wider or narrower. |
| BPM and beat light | Show the detected tempo and beat. |
| Status, Played, and Duration | Show the loaded item and playback times. |

## Microphone deck

| Control | What it does |
|---|---|
| On | Starts the microphone. |
| Off | Stops the microphone. |
| Volume | Sets the microphone level in the main mix. |
| Balance | Moves the microphone towards the left or right speaker. |
| Input Gain | Adjusts the microphone level before effects and volume. |
| Mute | Silences the microphone in the main mix. |
| PFL | Sends the microphone to the headphone preview. |
| X Fade | Lets the microphone share linked fader movement with another selected deck. |
| PEQ Gain / Frequency / Q | Adjust the microphone tone area, its centre, and its width. |
| Noise Gate | Turns automatic background-noise reduction on or off. |
| Compressor | Turns automatic control of loud microphone peaks on or off. |
| Echo | Turns the microphone echo effect on or off. |
| Compressor / Echo / Noise Gate tabs | Open the controls for that effect. |
| Signal and clip lights | Show microphone activity and warn when the input is too loud. |

### Noise Gate controls

| Control | What it does |
|---|---|
| Threshold | Sets how loud the microphone must be before it opens. |
| Attack | Sets how quickly it opens. |
| Hold | Keeps it open briefly after speech. |
| Release | Sets how smoothly it closes. |
| Floor | Sets how quiet the microphone becomes while the gate is closed. |

### Compressor controls

| Control | What it does |
|---|---|
| Threshold | Sets when loud-sound control starts. |
| Ratio | Sets how strongly loud sound is reduced. |
| Attack | Sets how quickly the compressor reacts. |
| Release | Sets how quickly it lets go. |
| Makeup | Restores volume after compression. |
| Knee | Makes the start of compression harder or smoother. |

### Echo controls

| Control | What it does |
|---|---|
| Mix | Sets how much echo is heard. |
| Delay | Sets the time between the voice and its echo. |
| Feedback | Sets how many times the echo repeats. |
| Tone | Makes the echo darker or brighter. |
| Spring | Adds a spring-like echo character. |
| Wow Depth | Sets the amount of slow pitch movement. |
| Wow Rate | Sets the speed of that movement. |

## Loopback deck

A loopback deck captures sound from another application running on the same computer.

| Control | What it does |
|---|---|
| Source | Opens the process picker to choose the application to capture. |
| Process name and PID | Show the selected application and its process number. |
| Play/Stop | Starts or stops capturing the selected application. |
| Volume | Sets the captured sound level in the main mix. |
| Balance | Moves the captured sound towards the left or right speaker. |
| Pitch | Makes the captured sound slower or faster. |
| Input Gain | Adjusts the level before effects and volume. |
| PFL | Sends the captured sound to the headphone preview. |
| Mute | Silences the captured sound in the main mix. |
| X Fade | Lets this deck share linked fader movement with another selected deck. |
| PEQ Gain / Frequency / Q | Adjust the captured sound's tone. |
| BPM and beat light | Show the detected tempo and beat. |
| LIVE, format, played time, and status | Show whether capture is active and what it is doing. |

## Process picker

| Control | What it does |
|---|---|
| Search | Filters the application list by name. |
| Active only | Shows applications that are currently producing audio. |
| Audio only | Hides applications without an audio session. |
| System processes | Also shows Windows system processes. |
| Refresh | Updates the process list. |
| Main applications | Lists the main applications that can be captured. |
| Child processes / sessions | Lists separate parts or audio sessions of the selected application. |
| Whole app tree | Captures the selected application and its related processes. |
| Selected Only | Captures only the selected process or session. |
| OK | Uses the selected source. |
| Cancel | Closes the picker without changing the source. |

## Playlist Editor

| Control | What it does |
|---|---|
| Library list | Shows tracks stored in the RDJ Pro library. Double-click a track to add it. |
| Playlist list | Shows the tracks in the selected playlist. |
| Search | Finds library tracks using the entered text. |
| Clear | Removes the search filter. |
| Playlist choice | Chooses the playlist to view or edit. |
| New | Creates a playlist. |
| Save | Saves the current playlist and its order. |
| Delete | Deletes the selected playlist. |
| Add | Adds the selected library track to the playlist. |
| Remove | Removes the selected playlist entry. The audio file is not deleted. |
| Move Up / Move Down | Changes the selected track's place in the playlist. |
| File | Chooses one audio file to add. |
| Scan Folder | Adds supported audio files from a folder to the library. |
| Cancel Scan | Stops a folder scan. |
| Clear Library | Removes all entries from the library database. It does not delete audio files. |
| Clear Missing Tracks | Removes library entries whose files can no longer be found. |
| Tag editor | Opens the selected track's tag information. |
| Playlist Duration and Status | Show the total playing time and current action. |

## Tag Editor

| Control | What it does |
|---|---|
| Path | Shows the audio file being edited. |
| Artist | Sets the performer name. |
| Title | Sets the track title. |
| Album | Sets the album name. |
| Album/Artist | Sets the main artist for the complete album. |
| Genre | Sets the music style. |
| Composer | Sets the composer name. |
| Comment | Stores a free-text note in the file tags. |
| Year | Sets the release year. |
| Track No | Sets the track number. |
| Disc No | Sets the disc number. |
| BPM | Sets the track tempo. |
| Key | Sets the musical key. |
| Gain dB | Stores a playback gain value. |
| OK | Saves the changed tags. |
| Cancel | Closes without saving changes. |

## Master Effects

Use **3-Band EQ**, **Compressor Limiter**, and **Flanger / Echo** to switch between effect pages.

### 3-Band EQ

| Control | What it does |
|---|---|
| Enable | Turns the master tone control on or off. |
| Low Gain | Changes bass. |
| Mid Gain | Changes middle tones. |
| High Gain | Changes treble. |

### Compressor and limiter

| Control | What it does |
|---|---|
| Compressor Enable | Turns automatic control of loud parts on or off. |
| Threshold | Sets when compression begins. |
| Ratio | Sets how strongly loud sound is reduced. |
| Attack / Release | Set how quickly compression starts and stops. |
| Makeup | Restores volume after compression. |
| Auto Makeup | Lets RDJ Pro set the restored volume. |
| Knee | Makes compression begin harder or smoother. |
| RMS Detector | Uses the average sound level instead of short peaks. |
| Limiter Enable | Stops the master output from becoming too loud. |
| Ceiling | Sets the highest allowed output level. |
| Lookahead | Lets the limiter prepare for a peak slightly early. |
| Limiter Release | Sets how quickly limiting stops after a peak. |
| True Peak Enable | Adds extra protection against hidden output peaks. |
| TP Ceiling | Sets the highest allowed true-peak level. |
| Oversample | Sets how carefully true peaks are checked. |

### Flanger / Echo

| Control | What it does |
|---|---|
| Enable | Turns the effect on or off. |
| Delay | Sets the basic delay time. |
| Depth | Sets the amount of movement in the delay. |
| Rate | Sets the speed of that movement. |
| Feedback | Sets how much affected sound is sent through the effect again. |
| Wet | Sets how much affected sound is mixed in. |
| Preset Echo | Loads ready-made echo settings. |
| Preset Flanger | Loads ready-made flanger settings. |
| Set Defaults | Returns the master effects to their standard settings. |

## Media Server Control

### Server page

| Control | What it does |
|---|---|
| Broadcast | Starts or stops the live browser broadcast. |
| Camera | Uses the camera selected in Setup. |
| Static Image | Uses a still image instead of the camera. |
| Preview | Shows the selected video source. |
| Log | Shows broadcast messages and errors. |
| ON AIR | Shows whether a live broadcast is running. |
| LOCKED / UNLOCKED | Shows whether another broadcaster owns the broadcast lock. |
| HEALTH | Shows the general broadcast condition. |
| Broadcast status | Shows whether the browser stream is online. |

### Recorder page

| Control | What it does |
|---|---|
| Filename | Sets the name of the MP4 recording. |
| Video Only | Records video without the programme audio. |
| Start | Starts or stops MP4 recording. |
| REC and recorded time | Show whether recording is active and how long it has run. |

### Cast

| Control | What it does |
|---|---|
| Cast device list | Chooses a discovered casting device. |
| Discover | Searches the local network for casting devices. |
| Cast live | Sends the live RDJ Pro stream to the selected device. |
| Stop | Stops casting. |
| Volume | Sets the casting device volume. |
| Mute | Silences the casting device. |
| Cast status | Shows the connection state. |

## Audio Device chooser

| Control | What it does |
|---|---|
| Device list | Shows the available Windows audio devices. |
| Refresh | Updates the device list. |
| OK | Uses the selected device. |
| Cancel | Closes without changing the device. |

## File browser

| Control | What it does |
|---|---|
| Location | Chooses a known local or network location. |
| IPv4 address | Shows the computer address for the selected local or network location. The text can be copied. |
| Path | Accepts a folder or network path. |
| Find | Opens the entered path. |
| File filter | Chooses which file types are shown. |
| Folder list | Shows folders. Double-click to open one. |
| File list | Shows files in the current folder. |
| Preview | Shows a preview when the selected file supports it. |
| Selected file and Duration | Show the current choice and its playing time. |
| OK | Uses the selected file. |
| Cancel | Closes without selecting a file. |
