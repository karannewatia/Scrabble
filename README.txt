CS 3110 Final Project:

S C R A B B L E
-------------------------------------------------------------------------------
Made by:
Arman Vaziri (adv28)
Connor McHugh (cam462)
Karan Newatia (kn348)
Hyuckin David Lim (hdl39)

Dependencies:
- XQuartz
- OCaml
- OCaml Graphics

Installation:
- If you are running the course-provided Virtual Box disk image, you can stop
  reading now! It has all necessary dependencies pre-installed.
- XQuartz : download and run the .dmg installer from https://www.xquartz.org
- OCaml Graphics
  - type 'ocamlfind query graphics' into your terminal
    - if a path is returned, you're good to go!
    - if you receive a 'Package "graphics" not found' error, that means you need
      to keep reading this setup document.
  - to install the OCaml Graphics module, ensure you have Homebrew on your Mac.
  - Run 'brew install ocaml'
  - If the above test still fails, try $ opam switch reinstall 4.05.0
   (as the OCaml version might have been updated).
  - At this point, you will have successfully installed the Graphics module!
Play Game:
Simply type 'make play' to start up the Scrabble GUI.

Enjoy!
