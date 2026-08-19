# RDJ User Manual

This guide explains what the controls in RDJ do. It avoids technical details and uses the names shown on screen.

> Tip: Move the mouse over a control. Many controls show a short hint.

## Getting started

On its first run, RDJ creates its settings file and opens **RDJ Setup**. Before playing audio, select a **Main output (MASTER)**. If you use headphones for previewing, enable **Headphones output (PFL / CUE)** and select a different output device.

A typical session is:

1. Choose **Setup** and check the audio devices.
2. Use **Load** on a channel deck to select a track.
3. Raise the channel and master volume controls.
4. Choose **Play**.
5. Use **PFL** to preview a source through headphones without sending it to the main mix.

## Main window

| Control | What it does |
|---|---|
| Clock | Shows the local time. |
| Setup | Opens the application settings. |
| Playlist | Opens the Playlist Editor. |
| Effects | Shows or hides the master effects window. |
| Broadcast | Opens or closes Media Server Control. |
| Notes | Shows or hides the notes area. |
| Minimize | Sends RDJ to the taskbar. |
| Maximize/Restore | Switches between a full-size and normal window. |
| Close | Closes RDJ. RDJ stops its background work before exiting. |
| Notes box | A place for temporary notes. It is not sent to listeners. |
| Clear Notes | Empties the notes box. |
| DJ Name | The DJ name published with the broadcast information. |
| Showtitle | The current programme or show name. |
| Set DJ and Showtitle | Publishes the entered DJ name, show title, and selected logo. |
| Logo | Shows the current broadcast image. Double-click it to choose another image. |

When a normal audio track is playing, RDJ can publish its artist and title as now-playing information. The DJ name and show title describe the programme itself.

## Setup

Use the two buttons on the left to switch between **General** and **Broadcast** settings. Audio-recorder settings are included on the General page. Choose **OK** to save changes or **Cancel** to leave without saving.

Changing audio devices or deck counts causes RDJ to rebuild its audio layout.

### General

| Control | What it does |
|---|---|
| Channel decks | Chooses how many normal player decks RDJ creates. |
| Loopback decks | Chooses how many live computer-audio decks RDJ creates. |
| Capture buffersize | Changes how much audio RDJ keeps ready. A larger value is safer; a smaller value reacts faster. |
| Main output (MASTER) | Chooses the speakers or output used for the main mix. |
| Enable Headphones output (PFL / CUE) | Turns the separate headphone-preview output on or off. |
| Cue output (PHONES) | Chooses the headphone or preview device. |
| Enable Microphone input | Shows and enables the microphone deck. |
| Microphone input (MICROPHONE) | Chooses the microphone or input device. |
| Recordings | Chooses the folder where audio recordings are saved. |
| Database | Chooses the folder containing the RDJ music database. |
| Covers | Chooses the folder used for local cover images. |
| `...` buttons | Browse for the folder beside the button. |

### Audio recorder

| Control | What it does |
|---|---|
| Output format | Chooses the recording file format, such as WAV or FLAC. |
| Capture buffersize | Changes how much incoming audio is kept ready for recording. |
| Latency | Changes the recorder response delay. The default is suitable for most users. |
| Enable stream switch detection | Lets the recorder react when Windows changes an audio device. |
| Default PCM audio output format | Uses the normal uncompressed Windows audio format. |
| Disable MMCSS | Disables Windows multimedia priority. Leave this off unless troubleshooting. |
| Don't overwrite existing files | Protects a recording that already has the same filename. |

### Broadcast

The Broadcast page contains three groups: the Icecast stream connection, the Icecast server manager, and the Caddy/JSON paths.

#### Icecast settings

| Control | What it does |
|---|---|
| Host | Sets the computer or server running Icecast. Use `127.0.0.1` when it runs on this computer. |
| Port | Sets the Icecast connection port. The usual value is `8000`. |
| Mount | Sets the broadcast mount point, for example `/live` or `/live.aac`. |
| User name | Sets the Icecast source user name. |
| Password | Sets the Icecast source password. It must match the server configuration. |
| Broadcast name | Sets the station or stream name shown to listeners. |
| Description | Sets a short description of the broadcast. |
| Genre | Sets the music or programme category. |
| Broadcast URL | Sets the public website address associated with the stream. |
| Public Stream | Allows Icecast to advertise the stream publicly when the server permits it. |

#### Icecast server manager

| Control | What it does |
|---|---|
| Host | Sets the Icecast server address used for status checks. |
| Port | Sets the Icecast server port used for status checks. |
| EXE Path | Selects the local Icecast executable. Leave local launch settings unused when Icecast runs on another computer. |
| Config Path | Selects the Icecast XML configuration file. |
| HTTP Path | Sets the server HTTP path. This is normally `/`. |
| Working Dir | Selects the folder from which Icecast is started. |
| Startup Delay | Sets how long RDJ waits for Icecast to become ready after starting it. |
| Auto Restart | Lets RDJ restart the locally managed Icecast server after an unexpected stop. |
| `...` buttons | Browse for the file or folder beside the button. |

#### Caddy / JSON settings

| Control | What it does |
|---|---|
| Caddy Path | Selects the main Caddy folder. |
| Caddy Config Path | Selects the Caddy configuration file. |
| Caddy JSON Path | Selects the `nowplaying.json` file updated by RDJ. |
| Caddy Command | Sets the command used to start Caddy. Leave it unchanged unless you manage a custom installation. |
| Caddy Covers Path | Selects the folder in which cover images are published. |
| `...` buttons | Browse for the file or folder beside the button. |

If Icecast and Caddy run on a separate server, local executable and command paths may be left unused. The host, port, mount, credentials, and shared JSON/cover locations must still match that installation.

## Master deck

| Control | What it does |
|---|---|
| Left and Right faders | Set the main output volume for each side. |
| Lock | Moves the left and right faders together. |
| Balance | Moves the sound towards the left or right speaker. Double-click to return to the centre. |
| PFL volume | Sets the headphone-preview volume. |
| PFL Mute | Silences the headphone-preview output. |
| Filename | Sets the name of a new audio recording. RDJ adds the file extension. |
| Device | Chooses an audio endpoint as the recorder source. |
| Pre-FX | Records the clean mix before master effects. |
| Post-FX | Records the mix after master effects. |
| Start | Starts or stops audio recording. |
| Recording and time indicators | Show whether recording is active and how long it has run. |
| Level displays | Show the main and PFL output levels. |

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
| EQ | Turns the deck's parametric tone adjustment on or off. |
| EQ Gain | Raises or lowers the selected tone area. |
| EQ Frequency | Chooses the centre of the tone area. |
| EQ Q | Makes the adjusted tone area wider or narrower. |
| BPM and beat light | Show the detected tempo and beat. |
| Status, Played, and Duration | Show the loaded item and playback times. |

## Microphone deck

The microphone deck is shown when **Enable Microphone input** is selected in Setup.

| Control | What it does |
|---|---|
| On | Starts the microphone. |
| Off | Stops the microphone. |
| Volume | Sets the microphone level in the main mix. |
| Balance | Moves the microphone towards the left or right speaker. |
| Input Gain | Adjusts the microphone level before effects and volume. |
| Mute | Silences the microphone in the main mix. |
| PFL | Sends the microphone to the headphone preview. |
| PEQ Gain / Frequency / Q | Adjust the microphone tone area, its centre, and its width. |
| Noise Gate | Turns automatic background-noise reduction on or off. |
| Compressor | Turns automatic control of loud microphone peaks on or off. |
| Echo | Turns the microphone echo effect on or off. |
| Compressor / Echo / Noise Gate buttons | Open the controls for that effect. |
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
| Select source | Opens the process picker to choose the application to capture. |
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
| Active only | Shows applications that are currently active. |
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
| Library list | Shows tracks stored in the RDJ library. Double-click a track to add it. |
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
| Cancel Scan | Requests cancellation of a folder scan. RDJ waits for the current operation to finish safely. |
| Clear Library | Removes all entries from the library database. It does not delete audio files. |
| Clear Missing Tracks | Removes library entries whose files can no longer be found. |
| Tag editor | Opens the selected track's tag information. |
| Playlist Duration and Status | Show the total playing time and current action. |

## Tag Editor

RDJ currently uses the Tag Editor for supported tagged audio files, including MP3 files.

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
| Gain dB | Stores a playback-gain value. |
| OK | Saves the changed tags. |
| Cancel | Closes without saving changes. |

## Master Effects

Use **3-Band EQ**, **Compressor Limiter**, and **Flanger / Echo** to switch between effect pages.

### 3-Band EQ

| Control | What it does |
|---|---|
| EQ | Turns the master tone control on or off. |
| Low Gain | Changes bass. |
| Mid Gain | Changes middle tones. |
| High Gain | Changes treble. |

### Compressor and limiter

| Control | What it does |
|---|---|
| Compressor | Turns automatic control of loud parts on or off. |
| Threshold | Sets when compression begins. |
| Ratio | Sets how strongly loud sound is reduced. |
| Attack / Release | Set how quickly compression starts and stops. |
| Makeup | Restores volume after compression. |
| Auto Makeup | Lets RDJ set the restored volume. |
| Knee | Makes compression begin harder or smoother. |
| RMS Detector | Uses the average sound level instead of short peaks. |
| Limiter | Stops the master output from becoming too loud. |
| Ceiling | Sets the highest allowed output level. |
| Lookahead | Lets the limiter prepare for a peak slightly early. |
| Limiter Release | Sets how quickly limiting stops after a peak. |
| True Peak Guard | Adds extra protection against hidden output peaks. |
| TP Ceiling | Sets the highest allowed true-peak level. |
| Oversample | Sets how carefully true peaks are checked. |

### Flanger / Echo

| Control | What it does |
|---|---|
| Flanger / Echo | Turns the effect on or off. |
| Delay | Sets the basic delay time. |
| Depth | Sets the amount of movement in the delay. |
| Rate | Sets the speed of that movement. |
| Feedback | Sets how much affected sound is sent through the effect again. |
| Wet | Sets how much affected sound is mixed in. |
| Preset Echo | Loads ready-made echo settings. |
| Preset Flanger | Loads ready-made flanger settings. |
| Set Defaults | Returns the master effects to their standard settings. |

## Media Server Control

Media Server Control starts or stops locally managed Icecast/Caddy services and connects RDJ's audio stream to Icecast. If the services run on another computer, start them there and use **Broadcast** only to connect the RDJ source stream.

| Control | What it does |
|---|---|
| Start / Stop | Starts or stops the locally configured Icecast and Caddy services. |
| Broadcast | Connects or disconnects RDJ's audio broadcast. |
| Auto restart | Restarts the locally managed server after an unexpected disconnect when enabled. |
| Log | Shows server, connection, and broadcast messages. |
| Server status | Shows whether the server is stopped, starting, running, or stopping. |
| Broadcast status | Shows whether RDJ is offline, connecting, live, reconnecting, or in an error state. |
| ON AIR | Shows whether RDJ is currently broadcasting. |

Start the server services before choosing **Broadcast** when Icecast/Caddy are installed locally. Stopping the services also disconnects the broadcast.

## Audio Device chooser

| Control | What it does |
|---|---|
| Device list | Shows the available Windows audio endpoint devices. |
| Refresh | Updates the device list. |
| OK | Uses the selected device. |
| Cancel | Closes without changing the device. |

## File browser

| Control | What it does |
|---|---|
| Location | Chooses a known local or network location. |
| Path | Accepts a folder or network path. |
| Find | Opens the entered path. |
| File filter | Chooses which file types are shown. |
| Folder list | Shows folders. Double-click to open one. |
| File list | Shows files in the current folder. |
| Preview | Shows a preview when the selected file supports it. |
| Selected file and Duration | Show the current choice and its playing time. |
| OK | Uses the selected file. |
| Cancel | Closes without selecting a file. |

The browser searches for network stations in the background. A network path can also be entered directly, for example `\\MyServer\Music`.

## Closing RDJ

Use the main window's **Close** button. RDJ disconnects broadcasting, closes its child windows, stops background library and network scans, and then releases its audio devices. A scan may take a moment to reach a safe stopping point.
