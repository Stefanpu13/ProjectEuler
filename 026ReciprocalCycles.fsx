(*
    A unit fraction contains 1 in the numerator. 
    The decimal representation of the unit fractions with denominators 2 to 10 are given:

    1/2	= 	0.5
    1/3	= 	0.(3)
    1/4	= 	0.25
    1/5	= 	0.2
    1/6	= 	0.1(6)
    1/7	= 	0.(142857)
    1/8	= 	0.125
    1/9	= 	0.(1)
    1/10	= 	0.1
    Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. 
    It can be seen that 1/7 has a 6-digit recurring cycle.

    Find the value of d < 1000 for which 1/d contains the longest 
    recurring cycle in its decimal fraction part.
*)


(*
    division is finished when:
    remainder is zero
    a cycle is reached
*)

let rec getDivisionCycleLength divisionResults divident divisor =         
    match divident, divident < divisor with
    | 0, _ -> 0
    | r, true -> getDivisionCycleLength divisionResults (r * 10) divisor
    | r, false ->
        if Set.contains r divisionResults
        then Set.count divisionResults
        else getDivisionCycleLength (Set.add r divisionResults) (r % divisor)  divisor
    
// #time
[1..1000] 
|> List.maxBy (fun n -> getDivisionCycleLength Set.empty 1 n)
