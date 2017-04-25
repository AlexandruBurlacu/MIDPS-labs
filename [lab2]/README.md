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

Ah, yeah, we've used Visual Studio Code editor/IDE.

## Intro
For this laboratory work we will do the following:
- Make a simple GUI Calculator (We'll use GTK+)
- Default operations are: +,-,*,/.
- Splitting codebase into two modules: User Interface and Business Logic.

**Beside that:**
- We provide support for the following operations in our application: power, sqrt, SignInversion(+/-).
- Provide support for operations with decimal floating point numbers.
- Your program contains two modules, the GUI module and Business Logic module.
---

## Act 1: Why Haskell?
Haskell is a known lazy[1] pure[2] functional language, with strict (doesn't allow implicit conversions) static typing system. What is really awesome about it is the [Hindley–Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) which allows Haskell to infer almost any type, so no expressions like `type var_name = new type();`. Also, as mentioned in [1] it is lazy, which means that it can perform computations on demand, rather than on definitions. This property, however, implies some "space" issues, when a piece of code is declared but never used.
[2] means that all functions defined in Haskell are referentially transparent - for any given input, the function has a well-defined, deterministic output, or in other words, it can be harmless swapped with the output value of that function.
For side effects (interactions with the outer world) Haskell uses Monads.
But the main reason why we chose Haskell is because of our GREAT love for the functional paradigm.

## Act 2: The structure
Haskell programs naturally are divided into 'pure' and 'unpure'/'unsafe'/'effectfull'/'#giveYourOwnDefinition' code.

The pure code is the one that takes a parameter and returns a value, simple, isn't it? But this definition is crucial to understand. The unpure code is the one that is non-deterministic, for example, interacts, one way or another with the external world.
For such code, we use Monads, a Category Theory marvel which provides us a way to compose and deal with non-deterministic and imperative computations. For side effects, we use the IO Monad. Btw, another example of Monads are Maybe/Optional type or Either. The main program is the entry point, and it is of IO () type.

So it is natural that we divided the program into pure part (Business Logic) which is called _Logic.hs_ and unpure part, where the Main function resides with all it's View related code is called _Main.hs_.

## Act 3: GTK+ the Haskell way
To make a calculator using `gtk2hs` we first require to initialize the GUI and to draw a window using `windowNew` function, then we set the parameters of the window instance and then we bind buttons and the display to the window instance. Also, each button has it's own handler. That's basically it.

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
