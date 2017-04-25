# GUI programming

### Prerequisites [WARNING: Ugly-ish Haskell code]
Before we start you should know how the environment is set up, even if there's a bash script that will do all the work for you.
```bash
sudo apt-get install libgtk3-dev, build-essential, libgl1-mesa-dev
# the ones for graphics

sudo apt-get install haskell-platform
# this for the Haskell platform (I know, it was obvious from the name)
# it will install the ghc haskell compiler,
# the cabal package manager (think of Python's pip) and some other tools

# when the repo is on your machine and the above dependencies installed, run
cd MIDPS-labs/\[lab2\]
cabal update
cabal install gtk2hs-buildtools
cabal install gtk3
# note that the order does matter, and it will take some time, btw.
```

To run the code, use `cabal run src/Main.hs` or just run the executable `HCalculator`. (**Note**: I am not sure that the executable file will run on your machine, so I provide the alternative to run everything from the ground up)

## Intro
For this laboratory work we will do the following:
- Make a simple GUI Calculator (We'll use GTK+)
- Default operations are: +,-,*,/,power,sqrt,SignInversion(+/-),operation with decimal floating point.
- Splitting codebase into two modules: User Interface and Business Logic.

---

## Act 1:


## Act ... :


## Act n:

---

## Known Issues
- The square rooting and sign inversion operations look ugly as hell. Sorry.
- The application window is small. When resized, its buttons remain the same size. The size doesn't matter anyway.
- No more known issues.

## Conclusion
First of all, Haskell is incredible! Second, Java is not Haskell :smile: . Now the stuff related to the Laboratory work itself.
In order to build GUI applications using Haskell, one should choose either to write it using a wrapper for a known GUI library, like wxWidgets, GTK+, or QT; or to use Functional Reactive Programming. The second approach is a more Haskell-ish one, but it has its drawbacks.
For example, one approach would be to continuously refresh the View, which sometimes is expensive or unnecessary. The wrappers are meant to bring the Event Driven Paradigm in Haskell. Listening for events instead of ever refreshing the application in a hope for some events to happen.
Also, using a wrapper is easier. We chose GTK+ because the wrapper for it is the most frequently maintained one by the Haskell community. For more information see [this](http://keera.co.uk/blog/2014/05/23/state-gui-programming-haskell/) and [this](https://wiki.haskell.org/Applications_and_libraries/GUI_libraries).

## Bibliography
- greatly inspired by [this](https://www.stackbuilders.com/tutorials/haskell/gui-application/) 
- and by the O'Reily book _Real World Haskell_ by Bryan O'Sullivan (ISBN 978-0-596-51498-3)
