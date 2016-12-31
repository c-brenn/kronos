# Kronos

Reversible interpreter for a simple imperative language. An assignment for CS4012 - Topics in Functional Programming.

# Design

`Interpreter.hs` is loosely based on `Interpreter8.hs` provided in the lecture notes. It runs a basic REPL.
The available commands are:
- `Next` => Runs the next instruction
- `Back` => Undoes the most recent instruction, stepping back through the program
- `Inspect variable` => Prints the value of `variable` (and all of its previous values). The current value is the head of the list.

After thinking about how to implement a reversible history, I settled on translating the `Statement` DSL provided into a pseudo-assembly.
The `Statement` DSL is recursive in structure, so navigating up and down the tree would require some kind of Zipper - which seemed overly complicated. The pseudo-assembly is a list of instructions, so navigation only requires keeping a program counter.
The recursive structure also makes undoing control flow (`If` and `While`) impossible (in my mind). Consider the following statement:
`if x = 1 then x = 1 else x = 1`. This type of statement is allowed in the provided DSL, and makes it impossible to reverse in one go. You can't tell which branch you took originally.
I added simple `GoTo` and conditional `GoToFalse` instructions (which allow `if` and `while` to be implemented). This is easily reversed, as `GoTo offset` becomes `GoTo -offset`.

The state of the interpreter is a record containing:

- the environment mapping variable names to values
- the program counter
- a list of executed instructions

The list of executed instructions is used for going back. To execute `Back`, the most recently executed instruction is taken from the head of the list. It is then undone.

Executing and undoing instructions is quite simple. `executeInstruction` and `undoInstruction` update the environment if necessary and return the offset that should be added to the program counter.

The environment was changed from `Map.Map String -> Value` to `Map.Map String -> [Value]` - the current value of a variable is the head of the list. Undoing an assignment is then simply popping the head of the list.


## Static Analysis

I didn't spend much time on the static analysis -  I implemented a simple check for unused variables. It gets the set of assigned variables and the set of used variables and returns the difference of the two sets.

# Setting up

```bash
git clone https://github.com/c-brenn/kronos.git && cd kronos
stack setup
stack build
```

# Running


There is some issue with stack and readline on OSX (at leas on my machine) - typing backspace for example results in the escape code rather than deleting a character.
After building, you can bypass stack by running the executable directly (so you can use backspace). Or you can use `stack exec kronos` and type very carefully


The executable takes the path to a program to run as a command line argument.
```bash
stack exec kronos test-program.conor
# or
./.stack-work/path/to/executable/kronos test-program.conor
```
