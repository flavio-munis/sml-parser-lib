# **SML Parser Lib**
Implements a parser combinator library to create powerful parsers using few a functions and operators. To run the project you can use `CM.Make "make.cm"` from a SML/NJ REPL, or you can add it to your project and import the necessary structure or signatures.

## 🧩 **Interfaces**
The `interface.sig` file define haskell-like abstract signatures that can be used by any valid _type t_.

## 🧪 **Tests**
Run `CM.make "run-tests.cm"` in tests folder to automatically run all unit tests.

## 💻 **Implementation**
### **Json Parser**
Implements a fully functional Json Parser based on [ECMA-404 (The JSON Data
Interchange Syntax)](https://ecma-international.org/wp-content/uploads/ECMA-404.pdf). 

## 🌟 **Inspirations**
This repo is heavily inspired by [this video](https://www.youtube.com/watch?v=N9RUqGYuGfw) from [tsoding](https://github.com/tsoding).

## 📝 **To Do**
- [ ] Add/Remove/Update Values from Json Tree
- [x] Prettify/Minify Json
- [x] Parser for Double Values
- [x] Write Json To File
- [x] Parser for Escape Characters
- [x] Error Tracking
