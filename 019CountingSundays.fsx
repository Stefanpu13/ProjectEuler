(*
    You are given the following information, but you may prefer to do some research for yourself.

    1 Jan 1900 was a Monday.
    Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
    How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
*)

(* 

starting from 1 Jan 1900 Mon, use tuple with 4 elems - year, month, dayof week, day
days are from 1 to 31 ; 1 to 30 ; 1 to 28/29
dayOfWeek 0-6
month 0-11
year 1900-2000

*)

let getMonthDaysCount month year = 
    match month with
    |4 | 6 |9 |11 -> 30    
    |2 ->
        match year with 
        | leap when  year % 400 = 0 || year % 4 = 0 -> 29
        | _ -> 28
    | _ -> 31

let getNext maxCount current = if current >= maxCount then 1 else current + 1
let getNextDay = getNext 
let getNextDayOfWeek = getNext 7 
let getNextMonth = getNext 12
let getDatesFrom1900To2000 () = 
    Seq.unfold (fun (year, month, dayOfWeek, day) ->
        match (year, month, dayOfWeek, day) with
        |(2000, 12, _, 31) -> None
        | _ -> 
            let daysInMonth = getMonthDaysCount month year
            let nextElemDay = getNextDay daysInMonth day
            let nextElemDayOfWeek = getNextDayOfWeek dayOfWeek
            let nextElemMonth = if day > nextElemDay then getNextMonth month else month
            let nextElemYear = if month > nextElemMonth then year + 1 else year
            let nextElem = (nextElemYear, nextElemMonth, nextElemDayOfWeek, nextElemDay)

            Some (nextElem, nextElem)
    ) (1900, 1, 1, 1)

getDatesFrom1900To2000 () 
|> Seq.filter (fun date ->
    match date with 
    | (y, month, 7, 1) when y > 1900 && y < 2001 -> true
    | _ -> false
) |> Seq.length

// shorter solution
// #time
let monthDaysCount = [|31;28;31;30;31;30;31;31;30;31;30;31|]
let leapYearFebDaysCount y = (if y % 400 = 0 || (y % 100 <> 0 && y % 4 = 0) then 29 else 28)

[1901..2000] 
|> List.fold(fun (daysCounter, searchedSundaysCounter) year ->  
    monthDaysCount.[1] <-leapYearFebDaysCount year
    monthDaysCount 
    |> Array.fold (fun (currentFirstDayOfMonth, sundaysCounter) monthDaysCount ->
        let nextFirstDayOfMonth = currentFirstDayOfMonth + monthDaysCount
        if nextFirstDayOfMonth % 7 = 0 
        then nextFirstDayOfMonth, sundaysCounter + 1
        else nextFirstDayOfMonth, sundaysCounter
    ) (daysCounter, searchedSundaysCounter)
) (366, 0) 
|> snd