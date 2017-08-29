open System.IO
let numbers = File.ReadLines "013LargeSum/013LargeSum.txt"
let toString a = a.ToString()

numbers 
|> Seq.sumBy bigint.Parse
|> toString 
|> (fun (res:string) -> res.[0..9])
