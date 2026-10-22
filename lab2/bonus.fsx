let SHOW x = printf "%A\n" x

// replace the following Symbol by the valid implementation
type Symbol = Int

// ### Homework 5.1
//Complete all the function below again but witout recursion ussage - so all the recursion code should be tail recursive
// ##Implement `computeonp` function (AiSD or [Wiki](https://pl.wikipedia.org/wiki/Odwrotna_notacja_polska)). Hint: `::` is "right-associative"

// #### --------------- Your code goes below ---------------
let computeonp (stack: int list) (symbols: Symbol list): int option = None

// test the function, e.g. `computeonpTR [] [NumSymbol 4; NumSymbol 2; OpSymbol Multiply]`
// Important!!!! Replace the None with commented out assignment o computeonp
let ``Homework 5.1``: int option = None //computeonpTR [] [NumSymbol 4; NumSymbol 2; OpSymbol Multiply]

// #### Value of ``exercise 5.1``
SHOW ``Homework 5.1``

// ### Homework 5.2
// ##Using `parseSymbols` and `compute`, write `onp` function

// #### --------------- Your code goes below ---------------
let onpTR (expression: string): int option = None

let ``Homework 5.2`` = onpTR "2 7 + 3 / 14 3 - 4 * + 3 +"

// #### Value of ``exercise 5.2``
SHOW ``Homework 5.2``

// ### Homework 5.3
// ##Implement `conv2onpTR` function (AiSD or (https://pl.wikipedia.org/wiki/Odwrotna_notacja_polska)).

// #### --------------- Your code goes below ---------------
let conv2onpTR (expression: string): Symbol list option = None

let ``Homework 5.3`` =
    conv2onpTR "( 2 + 5 ) * 3 - 4 * ( 16 + 5 )"

// #### Value of ``exercise 5.3``

SHOW ``Homework 5.3``

// ### Homework 5.4

// #### --------------- Your code goes below ---------------
let computeTR (expression: string): int option = None

let ``Homework 5.4`` =
    computeTR "( 2 + 5 ) * 3 - 4 * ( ( 16 - 1 ) * 2 + 5 )"

// #### Value of ``exercise 5.4``
SHOW ``Homework 5.4``

//////////////////////////////////////////////////////////////
/// Indeks:
/// Imię:
/// Nazwisko:
/// 
/// Podsumowanie zalizowanych zadan: