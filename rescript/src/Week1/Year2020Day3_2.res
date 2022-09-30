open Belt

let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day3.txt")

let parser = (str: string) => {
  str->Js.String2.trim->Js.String2.split("\n")
}

let contains = (str: string, index: int, char: string) => {
  if str->Js.String2.length === 0 || index < 0 || index > str->Js.String2.length || char === "" {
    false
  } else if str->Js.String2.charAt(index) === char {
    true
  } else {
    false
  }
}

// x: shift amount to the right
// y: shift amount to the bottom
let executor = (arr: array<string>, (x, y): (int, int)) => {
  arr->Array.reduceWithIndex(0.0, (matchCnt, item, index) => {
    if (
      index > 0 &&
      mod(index, y) === 0 &&
      item->contains(
        mod(x * (index / y)->Float.fromInt->Js.Math.floor_int, item->Js.String2.length),
        "#",
      )
    ) {
      matchCnt +. 1.0
    } else {
      matchCnt
    }
  })
}

let parsedInput = input->parser

// Testing
//"#..##..#...##............#..##."->contains(3, "#")->Js.log // true
//"#..##..#...##............#..##."->contains(mod(31, 31), "#")->Js.log // true
//"#..##..#...##............#..##."->contains(mod(34, 31), "#")->Js.log // true
//"#..##..#...##............#..##."->contains(mod(35, 31), "#")->Js.log // true
//"#..##..#...##............#..##."->contains(mod(36, 31), "#")->Js.log // false

//parsedInput->Array.get(1)->Js.log // #.#...#...#....#.........#..#..
//parsedInput->Array.get(322)->Js.log // .....#......#..#...#.#.....#...

// Part #1
parsedInput->executor((3, 1))->Js.log // 162

// Part #2
//parsedInput->executor((1, 1))->Js.log // 80
//parsedInput->executor((3, 1))->Js.log // 162
//parsedInput->executor((5, 1))->Js.log // 77
//parsedInput->executor((7, 1))->Js.log // 83
//parsedInput->executor((1, 2))->Js.log // 37
// all *. => 3064612320

let coords = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

let multiplyResults = (input: array<string>, coordTuples: array<(int, int)>): float => {
  coordTuples->Array.reduce(1., (sum, coord: (int, int)) => {
    sum *. input->executor(coord)
  })
}
//getResult(parsedInput)(coords)->Js.log
parsedInput->multiplyResults(coords)->Js.log
