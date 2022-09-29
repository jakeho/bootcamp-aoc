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

// Part #1
boardingPasses
->Array.map(pass => {
  let row = pass->getRowDirections->searchWithin
  let col = pass->getSeatDirections->searchWithin
  row * 8 + col
})
->Belt.SortArray.Int.stableSort
->Array.get(boardingPasses->Array.length - 1)
->Js.log

// Part #2
//boardingPasses
//->Array.reduce([], (arr, pass) => {
//  let row = pass->getRowDirections->searchWithin
//  let col = pass->getSeatDirections->searchWithin
//
//  arr[pass] = 0
//})
//->Js.log
