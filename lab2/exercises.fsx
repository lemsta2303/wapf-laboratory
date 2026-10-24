let SHOW x = printf "%A\n" x
// ### Exercise 2.1
// ## Implement `parseNumber` function.
// ## You might find following functions useful:
// ## `ToCharArray()` (String member), `Array.forall`, `System.Char.IsDigit`, `System.Int32.Parse`.

// #### --------------- Your code goes below ---------------
let parseNumber (value: string): int option =
    if value <> "" && value |> String.forall System.Char.IsDigit
    then Some (int value)
    else None


let ``exercise 2.1`` = parseNumber "23"

// #### Value of ``exercise 2.1``

SHOW ``exercise 2.1``

// ### Exercise 2.2
// ## Declare `splitBy` function - a wrapper function arround `Split` method from `String` object.
// ## Hints: Use `Split` method from `String` and `Array.toList` function to convert array to list type.
// #### --------------- Your code goes below ---------------
let splitBy (separator: char) (str: string): string list = 
    str.Split separator
    |> Array.toList

splitBy ',' "Abc,def,ghj"


let ``exercise 2.2`` = "1,3,5,8,10" |> splitBy ','

// #### Value of ``exercise 2.2``
SHOW ``exercise 2.2``

// ### Exercise 3.1
// ##Define `Operator` and `Symbol` Discriminated Union Types.
// ##
// ##`Symbol` should use `Operator` as field in one case
// ##

// #### --------------- Your code goes below ---------------
// `Int` is used here only so that the code compiles.
// Remove it and instead define proper Discriminated Union cases:
// Operator might be one of the following: Plus, Minus, Multiply or Divide
type Operator = 
    | Plus
    | Minus
    | Multiply 
    | Divide 

// Same as above:
// Symbol might be either a NumSymbol (with int) or OpSymbol (with Operator)

type Symbol = 
    | NumSymbol of myNum: int
    | OpSymbol of myOperator: Operator

// ### Exercise 3.2
// # With help of pattern matching, implement `apply` function.

// #### --------------- Your code goes below ---------------
let apply (operator: Operator) (left: int) (right: int): int = 
    match operator with
    | Plus -> left + right
    | Minus -> left - right
    | Multiply -> left * right
    | Divide -> left / right

// test the function, e.g. `apply Divide 15 4`
let ``exercise 3.2`` = apply Divide 10 2

// #### Value of ``exercise 3.2``
SHOW ``exercise 3.2``

// ### Exercise 3.3
// ##Implement `parseSymbol` - try parse all operators first, and then in nested `match` expression use `parseNumber` function

// #### --------------- Your code goes below ---------------
let parseSymbol (token: string): Symbol option = 
    match token with 
    | "+" -> Some (OpSymbol Plus)
    | "-" -> Some (OpSymbol Minus)
    | "*" -> Some (OpSymbol Multiply)
    | "/" -> Some (OpSymbol Divide)
    | _ -> 
        match parseNumber token with
            | Some n -> Some (NumSymbol n)
            | None -> None
    

let ``exercise 3.3`` =
    List.map parseSymbol [ "+"; "/"; "12"; "11230" ]
// #### Value of ``exercise 3.3``
SHOW ``exercise 3.3``

// ### Helper function "sequenceOpts"
// ##if all elements are Some values, return Some of those values
// ##otherwise if there's at least one None, return None

let rec sequenceOpts (optionals: 'a option list): 'a list option =
    match optionals with
    | [] -> Some []
    | None :: _ -> None
    | Some h :: t -> sequenceOpts t |> Option.map (fun t -> h :: t)

sequenceOpts ``exercise 3.3``

// ### Exercise 3.4
// ##Implement `parseSymbols`. Useful functions: `List.map`, `sequenceOpts` as well as `splitBy` and `parseSymbol`

// #### --------------- Your code goes below ---------------
let parseSymbols (expression: string): Symbol list option = 
    splitBy ' ' expression 
    |> List.map parseSymbol
    |> sequenceOpts
    

let ``exercise 3.4`` = "1 2 / +" |> parseSymbols

// #### Value of ``exercise 3.4``
SHOW ``exercise 3.4``

// ### Homework 4.1
// ##Implement `computeonp` function (AiSD or [Wiki](https://pl.wikipedia.org/wiki/Odwrotna_notacja_polska)). Hint: `::` is "right-associative"

// #### --------------- Your code goes below ---------------
let rec computeonp (stack: int list) (symbols: Symbol list): int option = 
    match stack, symbols with 
    | [wynik], [] -> Some wynik
    // jesli ostatni symbol w liscie to liczba, to wywolaj dodajac do stosu
    | _, NumSymbol n :: rest -> computeonp (n :: stack) rest
    // jesli ostatni symbol w liscie to opeartor, zdejdmij dwie liczby ze stosu, wykonaj dzialanie, odloz na stos
    | b :: a :: s, OpSymbol op :: rest -> computeonp (apply op a b :: s)  rest
    | _ -> None

// test the function, e.g. `computeonp [] [NumSymbol 4; NumSymbol 2; OpSymbol Multiply]`
// Important!!!! Replace the None with commented out assignment o computeonp
let ``homework 4.1``: int option = computeonp [] [NumSymbol 4; NumSymbol 2; OpSymbol Divide]

// #### Value of ``exercise 4.1``
SHOW ``homework 4.1``

// ### Homework 4.2
// ##Using `parseSymbols` and `compute`, write `onp` function

// #### --------------- Your code goes below ---------------
let onp (expression: string): int option = 
    match parseSymbols expression with 
    | Some symbols -> 
        match computeonp [] symbols with 
        | Some result -> Some result
        | None -> None
    | None -> None


let ``homework 4.2`` = onp "2 7 + 3 / 14 3 - 4 * + 3 +"

// #### Value of ``exercise 4.2``
SHOW ``homework 4.2``

// ### Homework 4.3
// ##Implement `conv2onp` function (AiSD or (https://pl.wikipedia.org/wiki/Odwrotna_notacja_polska)).

// #### --------------- Your code goes below ---------------
type StackItem =
    | OpItem of Operator
    | LeftParen

let priority (operator: Operator): int = 
    match operator with
    | Plus | Minus -> 1
    | Divide | Multiply -> 2

let rec convert (tokens: string list) (stack: StackItem list) (output: Symbol list): Symbol list option = 
    match tokens, stack with
    | [], OpItem op :: s -> convert [] s (OpSymbol op :: output)
    | [], LeftParen :: _ -> None
    | [], [] -> Some ( List.rev output )
    | "(" :: rest, _ -> convert rest (LeftParen :: stack) output
    | ")" :: _, OpItem op :: s -> convert tokens s (OpSymbol op :: output)
    | ")" :: rest, LeftParen :: s -> convert rest s output
    | ")" :: _, [] -> None
    | token :: rest, _ -> 
        match parseSymbol token with
        | None -> None
        | Some (NumSymbol n) -> convert rest stack (NumSymbol n :: output)
        | Some (OpSymbol op ) -> 
            match stack with 
            | OpItem top :: s when priority top >= priority op -> convert tokens s (OpSymbol top :: output)
            | _ -> convert rest (OpItem op :: stack) output

let conv2onp (expression: string): Symbol list option = 
    convert (splitBy ' ' expression) [] []

let ``homework 4.3`` =
    conv2onp "( 2 + 5 ) * 3 - 4 * ( 16 + 5 )"

// #### Value of ``exercise 4.3``

SHOW ``homework 4.3``

// ### Homework 4.4

// #### --------------- Your code goes below ---------------
let compute (expression: string): int option = 
    match conv2onp expression with
    | Some symbols -> computeonp [] symbols
    | None -> None

let ``homework 4.4`` =
    compute "( 2 + 5 ) * ( 3 - 1 )"

// #### Value of ``exercise 4.4``
SHOW ``homework 4.4``



//////////////////////////////////////////////////////////////
/// Indeks: 193333
/// Imię: Stanisław
/// Nazwisko: Lemański
/// 
/// Podsumowanie zalizowanych zadan:
/// Zostały wykonane wszystkie zadania do 4.4 włącznie. Zadań dodtakowych nie robiłem. Wszystkie funkcje testowałem na przykładowych danych i dawały poprawne wyniki.