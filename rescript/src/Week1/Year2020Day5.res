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

// 꼬리재귀 tail recursion
let rec search = (directions: array<string>, left: float, right: float): int => {
  if left === right {
    left->Int.fromFloat
  } else {
    let direction = directions->Array.get(0)
    let diff = right -. left
    let _ = directions->Js.Array2.shift

    let (l, r) = switch direction {
    | Some("F" | "L") => (left, right -. Js.Math.ceil_float(diff /. 2.0))
    | Some("B" | "R") => (left +. Js.Math.ceil_float(diff /. 2.0), right)
    | _ => (-1., -1.)
    }

    search(directions, l, r)
  }
}

let searchWithin2 = (directions: array<string>) => {
  let left = 0.
  let right = 2. ** Float.fromInt(directions->Array.length) -. 1.
  search(directions, left, right)
}

//"BBFFBFFRRR" |> getRowDirections |> Js.log // [ 'B', 'B', 'F','F', 'B', 'F','F' ]
//"BBFFBFFRRR"->getSeatDirections->Js.log // [ 'R', 'R', 'R' ]

//["F", "B", "F", "B", "B", "F", "F"]->searchWithin->Js.log // 44
//["F", "F", "F", "F", "F", "F", "F"]->searchWithin->Js.log // 0
//["B", "B", "B", "B", "B", "B", "B"]->searchWithin->Js.log // 127

let seatIds: array<int> =
  boardingPasses
  ->Array.map(pass => {
    let row = pass->getRowDirections->searchWithin2
    let col = pass->getSeatDirections->searchWithin2
    row * 8 + col
  })
  ->Belt.SortArray.Int.stableSort

// Part #1
seatIds->Array.get(boardingPasses->Array.length - 1)->Js.log2("is the highest ID from the list")

// Part #2
// There must be the seat ID of -1/+1 from mine on the list.
let findMySeat = (ids: array<int>): int => {
  // index 사용하지 않고 풀 수 있을까?
  let prevSeatIdx = ids->Js.Array2.findIndex(id => !(ids->Js.Array2.includes(id + 1)))

  if prevSeatIdx >= 0 {
    // Option.getWithDefault 없이 개선해보기, pattern matching으로 해보기
    let mySeatId = ids->Array.get(prevSeatIdx)->Option.getWithDefault(-1)
    mySeatId > 0 ? mySeatId + 1 : -1
  } else {
    -1
  }
}

let findPrevSeat = (ids: array<int>): option<int> => {
  ids->Js.Array2.find(id => !(ids->Js.Array2.includes(id + 1)))
}

// let findEmptySeat = (prevSeat: option<int>): int => {
//   switch prevSeat {
//   | Some(prevSeat) => prevSeat + 1
//   | None => -1
//   }
// }
let findEmptySeat = (prevSeat: option<int>): option<int> => prevSeat->Option.map(s => s + 1)

let print = a =>
  switch a {
  | Some(a) => Js.log(`${a} is my seat`)
  | None => Js.log(`can't find my seat`)
  }

// seatIds->findPrevSeat->findEmptySeat->Js.log2("is my seat")
// seatIds->findPrevSeat->findEmptySeat->print
//[2, 3, 4, 6, 7, 8]->findMySeat->Js.log2("is my seat") // 5

// Option 타입과 Array
// type option<int> = Some(int) | None = undefined
// e.g. Array.get(n): option<'a>
// fn: Promise
// 펑터 Functor, 모나드 Monad

// Array, List idx
// arr->Array.map(mapFn)->Array.keep(checkFn)->Array.reduce(fn) AVL tree
// x:option<int>->Option.map(mapFn)->Option.getWithDefault(y)
// optional chaining a?.b?.c

// O(n)

// type color = Red | Yellow | Blue
