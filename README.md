# cl-fourier-animation
### Orm Finnendahl

Animation of complex inverse fourier transformation inspired by the
YouTube videos of 3b1b.

## License

GPL 2.0 or later

## Installation

Using Quicklisp:

Put this folder into ~/quicklisp/local-projects/

Then start up emacs/slime and issue:

`(ql:quickload "cl-fourier-animation")`

`(cl-fourier-animation:main)`

This is a first sketch. There are different display modes and other
shapes can be added by extracting the points from an svg file (the
parser and the fft analysis code is contained, although not thoroughly
documented).

The advantage of this code is that being interactive zooming and
animations for different display modes could easily get implemented.

TODO:

- Animation for different display modes

- Keboard shortcuts for number of partials, change of shape to draw,
  speed, etc.
  
- freq labeling of circles

- Maybe changing to cl-sdl2 and doing the animation in 3D using opengl
  (with time as 3rd axis). This would be very helpful for teaching
  properties of dsp processing (using waveforms).
