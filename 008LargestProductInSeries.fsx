(*
    Find the thirteen adjacent digits in the 1000-digit number 
    that have the greatest product. What is the value of this product?
*)

namespace LargestProductInSeries
    module M = 
    let series = @"7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

    let numbers = Seq.map (fun ch -> int64 ch - int64 '0') series |> Seq.toArray
    let product s = Seq.reduce (*) s

    let largestProductInSeries subseriesLength (numbers: int64 []) = 
        Seq.fold (fun (maxProduct, firstMemberIndex) i ->
            let firstMember = numbers.[firstMemberIndex]
            if firstMember = 0L then (maxProduct, firstMemberIndex + 1)
            else
                let currentProduct = 
                    product numbers.[firstMemberIndex..firstMemberIndex + subseriesLength - 1]
                let newProduct = (currentProduct / firstMember) * numbers.[i]
                let newMaxProduct = max newProduct maxProduct           
                
                (newMaxProduct, firstMemberIndex + 1)
        ) (product numbers.[0..subseriesLength - 1], 0) [subseriesLength..numbers.Length - 1]

    largestProductInSeries 13 numbers


    let largestProductInSeries2 subseriesCount (numbers: int64 [])  = 
        Seq.fold (fun maxProduct i ->
            let currentProduct = product numbers.[i..i + subseriesCount - 1]
            let newProduct = product numbers.[i + 1..i + subseriesCount]
            let newMaxProduct = max newProduct maxProduct           
            
            newMaxProduct
        ) (product numbers.[0..subseriesCount - 1]) [0..numbers.Length - subseriesCount - 1]

    largestProductInSeries2 13 numbers


    largestProductInSeries 3 [|2L;1L;3L;4L|] 
    largestProductInSeries 2 [|6L;3L;3L;4L|] 
    largestProductInSeries2 3 [|2L;1L;3L;4L|] 
    largestProductInSeries2 2 [|6L;3L;3L;4L|] 

    largestProductInSeries 33 numbers 
    largestProductInSeries2 33 numbers