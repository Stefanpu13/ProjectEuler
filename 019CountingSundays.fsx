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

For 1900 to 2000 -> convert to year
year - array of days
for 1 to 12

use last day from prev month to generate current month
for 1 to "get month days count"
create

variant 2 :
starting from 1 Jan 1900 Mon, use tuple with 4 elems - year, month, dayof week, day
days are from 1 to 31 ; 1 to 30 ; 1 to 28/29
dayOfWeek 0-6
month 0-11
year 1900-2000

*)

type WeekDay = Mon = 0|Tues = 1|Wed = 2|Thur = 3|Fri = 4|Sat = 5|Sun = 6
type Month = Jan = 1|Feb = 2|Mar = 3|Apr = 4|May = 5|Jun = 6|Jul = 7|Aug = 8|Sep = 9|Oct = 10|Nov = 11|Dec= 12

let (|Leap|_|) year = 
    match year % 400 = 0 || year % 4 = 0 with
    | true -> Some year
    | false -> None

let getMonthDaysCount month year = 
    match month with
    |Month.Sep | Month.Apr |Month.Jun |Month.Nov -> 30    
    |Month.Feb ->
        match year with 
        | Leap _ -> 29
        | _ -> 28
    | _ -> 31

let getNext maxCount currentVal = if currentVal >= maxCount then 1 else currentVal + 1
let getNextDay = getNext 
let getNextDayOfWeek = getNext 7 
let getNextMonth = getNext 12
let cal = 
    Seq.unfold (fun (year, month, dayOfWeek, day) ->
        match (year, month, dayOfWeek, day) with
        |(2000, 12, _, 31) -> None
        | _ -> 
            let daysInMonth = getMonthDaysCount (enum<Month>month) year
            let nextElemDay = getNextDay daysInMonth day
            let nextElemDayOfWeek = getNextDayOfWeek dayOfWeek
            let nextElemMonth = if day > nextElemDay then getNextMonth month else month
            let nextElemYear = if month > nextElemMonth then year + 1 else year
            let nextElem = (nextElemYear, nextElemMonth, nextElemDayOfWeek, nextElemDay)

            Some (nextElem, nextElem)
    ) (1900, 1, 1, 1)

getMonthDaysCount Month.Feb 2000
let (|Twentieth |_|) year = 
    if year >= 1901 && year <= 2000 then Some year else None

cal 
|> List.ofSeq 
|> List.filter (fun date ->
    match date with 
    | (Twentieth y, month, 7, 1) -> true
    | _ -> false
) |> List.length
