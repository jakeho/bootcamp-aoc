open Belt

// Tom softy: Let's begin.
let boardingPasses = Year2020Day5Input.y2020d5 |> Js.String.trim |> Js.String.split("\n")

//boardingPasses->Js.log

let getRowDirections = (str: string) => {
  //  (str |> Js.String.split(""))->Array.slice(~offset=0, ~len=7) // ??
  str->Js.String2.split("")->Array.slice(~offset=0, ~len=7)
}

let getSeatDirections = (str: string) => {
  str->Js.String2.split("")->Array.sliceToEnd(7)
}

let searchWithin = (directions: array<string>) => {
  let min = ref(0)
  let max = ref(Int.fromFloat(2.0 ** Float.fromInt(directions->Array.length) -. 1.0))
  directions->Array.forEach(d => {
    if d === "F" || d === "L" {
      max := max.contents - Js.Math.ceil_int(Float.fromInt(max.contents - min.contents) /. 2.0)
    } else if d === "B" || d === "R" {
      min := min.contents + Js.Math.ceil_int(Float.fromInt(max.contents - min.contents) /. 2.0)
    }
  })

  min.contents === max.contents ? max.contents : 0
}

//"BBFFBFFRRR" |> getRowDirections |> Js.log // [ 'B', 'B', 'F','F', 'B', 'F','F' ]
//"BBFFBFFRRR"->getSeatDirections->Js.log // [ 'R', 'R', 'R' ]

//["F", "B", "F", "B", "B", "F", "F"]->searchWithin->Js.log // 44
//["F", "F", "F", "F", "F", "F", "F"]->searchWithin->Js.log // 0
//["B", "B", "B", "B", "B", "B", "B"]->searchWithin->Js.log // 127

let seatIds: array<int> =
  boardingPasses
  ->Array.map(pass => {
    let row = pass->getRowDirections->searchWithin
    let col = pass->getSeatDirections->searchWithin
    row * 8 + col
  })
  ->Belt.SortArray.Int.stableSort

// Part #1
seatIds->Array.get(boardingPasses->Array.length - 1)->Js.log2("is the highest ID from the list")

// Part #2
// There must be the seat ID of -1/+1 from mine on the list.
let findMySeat = (ids: array<int>): int => {
  let prevSeatIdx = ids->Js.Array2.findIndex(id => !(ids->Js.Array2.includes(id + 1)))

  if prevSeatIdx >= 0 && prevSeatIdx < ids->Js.Array2.length {
    let mySeatIdx = ids->Array.get(prevSeatIdx)->Option.getWithDefault(-1)
    mySeatIdx > 0 ? mySeatIdx + 1 : -1
  } else {
    -1
  }
}

seatIds->findMySeat->Js.log2("is my seat")
//[2, 3, 4, 6, 7, 8]->findMySeat->Js.log2("is my seat") // 5
