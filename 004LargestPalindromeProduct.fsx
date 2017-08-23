let isPalindrome n = 
    let nToString = n.ToString()
    [0..nToString.Length/2] |> List.fold(fun isPalindrome i->
        isPalindrome && nToString.[i] = nToString.[nToString.Length - 1 - i ]    
    ) true


let pairs s e = seq {
    for i in s..e do
        for j in s..e do
            yield (i, j)
} 

pairs 100 999 |> Seq.fold (fun largestPalindrome (i, j) -> 
    let product = i*j
    if isPalindrome product && largestPalindrome < product
    then product
    else largestPalindrome
) 1001