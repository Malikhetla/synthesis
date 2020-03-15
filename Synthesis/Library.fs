module Synthesis

let abelar a = 
  a > 12 && a < 3097 &&  a % 12 = 0
    //failwith "Not implemented"
    

let area b h =  
  match b < 0.0 || h < 0.0 with 
  | true -> failwith "Not implemented"
  | _ -> 0.5 * b * h
       
let zollo c =
    //failwith "Not implemented"
    match c < 0 with 
    |true -> -1 * c
    |_ -> c * 2

let min a b =
    //failwith "Not implemented"
    match a < b with 
    |true -> a
    |_ -> b

let max a b =
    //failwith "Not implemented"
    match a > b with
    |true -> a 
    |_ -> b

let ofTime a b c = a * 3600 + b * 60 + c
    //failwith "Not implemented"
    
let toTime a =
    //failwith "Not implemented"
    let Hours = a /  3600
    let Minutes = a%3600/60 
    let seconds = a%3600%60
    match a < 0  with 
    |true -> (0, 0, 0)
    |_ -> (Hours, Minutes, seconds)



let digits a =
   let rec Digits b acc =
     //| 0 -> 0
     match b < 10 && b >=0 || b > -10 && b <= 0 with 
     | true -> acc + 1
     | false -> Digits(b / 10) (acc + 1)
     //| _ -> failwith "Not implemented"
   Digits a 0

let minmax input =
   let a, b, c, d = input 
   (min(min a b ) (min c d )), (max (max a b) (max c d))
    //failwith "Not implemented"

let isLeap input =
    match input < 1582 with 
    |true -> failwith "Not implemented"
    | _ -> (input % 4 = 0) && not(input % 100 = 0) || (input % 4 = 0) && (input % 100 = 0) && (input % 400 = 0)


let month m =
   match m with 
   | 1-> "January", 31
   | 2 ->"February", 28
   | 3 ->"March", 31
   | 4 ->"April", 30
   | 5 ->"May", 31
   | 6 ->"June", 30
   | 7 ->"July", 31
   | 8 ->"August", 31
   | 9 ->"September", 30
   | 10 ->"October", 31
   | 11->"November", 30
   | 12 ->"December", 31
   |_ -> failwith "Not implemented"


let toBinary m =
  let rec calculate c acc=
    match m < 0 with 
    | true -> failwith "Not implemented"
    | false -> 
        match c = 0 with 
        | true -> acc
        | false ->
            let p = c%  2      //calculate (c /2) + c%2
            match  p with 
            |0 -> calculate (c/2) ("0" + acc)
            |1 -> calculate (c/2) ("1" + acc)
  match m = 0 with 
  |true -> "0"
  |false -> calculate m ""
 // calculate m ""

let bizFuzz x =
  let rec CheckDivision n (a, b, c) = 
   match n < 0 with 
    | true -> (a, b, c)
    |false -> 
        match n = 0 with 
        | true -> (a, b, c)
        | false -> 
           match n % 3 = 0 && n % 5 = 0 with
           | true ->  CheckDivision (n-1)(a+1, b+1, c+1)
           | false ->
              match n % 5 = 0 with 
              | true ->  CheckDivision (n-1)(a, b+1, c)
              | false ->
                match n % 3 = 0 with
                | true ->  CheckDivision (n-1)(a+1, b, c)
                | false -> CheckDivision (n-1) (a, b, c)
  CheckDivision x (0, 0, 0)
    //failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ = 
     failwith "Not implemented"
     //!!!!!!!!!!!!!Please uncomment out the code because it is incomplete it causes errors to others!!!!!!!!!!!!!!!!!
     //let x1, y1 = a
     //let x2, y2 = b
     //let  x =  (x1 - x2) * (x1 - x2)
     //let y =  (y1 - y2) * (y1 - y2)
     //let sum =  x + y
    
     //let sqrt n = 
       //   let rec calculate guess i = 
         //   match i = guess with 
           // | true -> guess
           // | _ -> 
              //let g = (guess + n/guess) / 2.0
             // calculate g (i + 1.0)
          //match n <= 0.0 with 
         // | true -> failwith "Impossibru!"
          //| _ -> calculate (n/2.0) 0.0
     //sqrt sum

     