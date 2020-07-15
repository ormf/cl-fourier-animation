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

On the bottom of cl-fourier-animation.lisp are a couple of commands
commented out. You change between different shapes with `(set-shape
*achtel-512*)`, `(set-shape *hessen-512*)` or `(set-shape
*violinschluessel-512*)`. You can change the speed, and the maximum
number of partials to be used and choose between differnet
displaymodes of the rotating arrows.

Other shapes can be added by extracting the points from an svg file
(see file "data.lisp").

The advantage of this code is that being interactive zooming and
animations for different display modes could easily get implemented.

TODO:

- Animation for different display modes

- Keyboard shortcuts for number of partials, change of shape to draw,
  speed, etc.
  
- Labeling of circles with freq index.

- Maybe changing to cl-sdl2 and doing the animation in 3D using opengl
  (with time as 3rd axis). This could be very helpful for teaching
  properties of dsp processing (using waveforms rather than 2D
  shapes).
