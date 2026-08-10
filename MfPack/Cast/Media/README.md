# MfPack Cast media-conversion support

These units are the media conversion and subtitle dependency closure extracted
from the proven MfPlayer X2 Chromecast implementation. They contain no form or
player-class dependency and are used by `MfCastTranscode` when a client creates
the facade with conversion enabled.

The current MKV route performs Media Foundation conversion to fragmented MP4.
A future media planner should distinguish codec-compatible MKV files that need
only remuxing from files that require video or audio transcoding.
