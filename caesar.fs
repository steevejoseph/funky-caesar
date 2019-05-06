open System

let inline let2nat c = int c - 97

let nat2let x = 
    int x + 97|> char

let shift x a =
    if Char.IsLower(a) then nat2let (((let2nat a) + x) % 26)
    else a

let encode (x : int) (xs : string) =
    xs |> String.map (fun letter -> shift x letter)

let decode (x : int) (xs : string) = 
    xs |> String.map (fun letter -> shift (26 - x) letter)  

let table = [|8.2; 1.5; 2.8; 4.3; 12.7; 2.2; 2.0; 6.1; 7.0; 0.2; 0.8; 4.0; 2.4;
         6.7; 7.5; 1.9; 0.1;  6.0; 6.3; 9.1; 2.8; 1.0; 2.4; 0.2; 2.0; 0.1|]

let lowers xs = 
       xs 
       |> Seq.filter (fun xs -> Char.IsLower(xs))
       |> Seq.length

let count x xs =
    Seq.length(Seq.filter (fun x' -> x' = x) xs)

let percent x y = (float x / float y) * float 100

let abc = [|'a'..'z'|]

let freqs xs = 
    let lowC = lowers xs
    let note = abc |> Array.map (fun c -> (count c xs))
    let po = note |> Array.map (fun c -> (percent c lowC))
    po  

let rotate (n : int) (xs : string) = 
    let arr = xs.ToCharArray()
    let l = arr.Length
    let q = l - n
    let arr2 = Array.permute (fun index -> (index + q) % l) arr
    System.String.Join ("", arr2)
    
let chisqr os es : float =
  let arraypairs = Array.zip os es
  let cspairs = arraypairs |> Array.map (fun (a,b) -> (((a - b) * (a - b)) / (b)))
  Array.sum cspairs

     
let position n arr =
    List.findIndex (fun x -> x = n) arr


let alphabetnum = [|0..25|]

let crack (m : string) = 
  let marray = m.ToCharArray()
  let freqs m = alphabetnum |> Array.map (fun c -> chisqr (freqs (decode c m)) table)
  decode (position (Array.min (freqs m)) (freqs m)) m