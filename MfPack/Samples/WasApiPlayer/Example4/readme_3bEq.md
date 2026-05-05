# High/Mid/Low EQ (v4): Mid Notch + Shelf Bandwidth

This update adds two *professional* controls often found on mixers/engineer EQs:

1. **Mid Mode**
* **Peaking (bell)**: boost/cut around center frequency
* **Notch (band-stop)**: removes a narrow band (feedback control)

  * Mid gain is ignored in notch mode (depth is inherent to a notch)
2. **Shelf bandwidth / slope**
* Low/High shelves now expose RBJ shelf parameter **S** (slope)
* Lower S = gentler transition, higher S = steeper



Defaults:

* Low shelf: 100 Hz, S=1.0
* Mid: 1000 Hz, Q=1.0, Mode=Peaking
* High shelf: 10 kHz, S=1.0



Ranges (clamped inside the MFT):

* Gain: -24..+24 dB
* Low Hz: 20..400
* Mid Hz: 200..6000
* High Hz: 2000..20000
* Mid Q: 0.3..6.0
* Shelf S: 0.1..4.0

