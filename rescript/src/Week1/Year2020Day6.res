open Belt

let sample = `
abc

a
b
c

ab
ac

a
a
a
a

b
`

// split the answers by groups
// split the answers by person
// count the answers -> Set?
let flatten = (arr: array<array<'a>>): array<'b> => arr->Array.concatMany
/*
let flatten = (arr: array<option<array<'a>>>): array<'b> =>
  arr->Array.map(arr => arr->Option.map(arr => arr->Array.concatMany))
*/

// identityFn
let id = x => x

let trimAndSplit = (str: string, regExp: Js_re.t): array<string> =>
  str->Js.String2.trim->Js.String2.splitByRe(regExp)->Array.keepMap(id)

let splitGroup = (str: string): array<string> => str->trimAndSplit(%re("/\\n\\n/g"))

let splitLine = (str: string): array<string> => str->trimAndSplit(%re("/\\n/g"))

let splitChar = (str: string): array<string> => str->trimAndSplit(%re("/\\B/g"))

let splitGroupAndLine = (str: string, lineSplitter: string => array<string>): array<
  array<string>,
> => {
  str->splitGroup->Array.map(lineSplitter)
}

let dedupe = (strArr: array<string>) => {
  strArr->Set.String.fromArray->Set.String.toArray
}

let intersect = (strArrX: array<string>, strArrY: array<string>) => {
  let setX = strArrX->Set.String.fromArray
  let setY = strArrY->Set.String.fromArray

  setX->Set.String.intersect(setY)->Set.String.toArray
}

// Part 1
// Union
Js.log("Part 1")
let getAnswerPart1 = (str: string) => {
  str
  ->splitGroupAndLine(splitLine)
  ->Array.map(arr => arr->Array.map(answers => answers->splitChar)->flatten->dedupe->Array.length)
  ->Array.reduce(0, (sum, n) => sum + n)
}

sample->getAnswerPart1->Js.log // 11
Year2020Day6Input.input->getAnswerPart1->Js.log // 6726

// Part 2
// Intersect
Js.log("\n\nPart 2")
let getAnswerPart2 = (str: string) => {
  str
  ->splitGroupAndLine(splitLine) // [ [abc], [a,b,c], ... ] array<array<string>>
  ->Array.map(arr => {
    let setArr = arr->Array.map(answers => {
      answers->splitChar->Set.String.fromArray
    })

    let compareSet = setArr->Array.get(0)

    compareSet
    ->Option.mapWithDefault(Set.String.empty, compareSet =>
      setArr->Array.reduce(compareSet, Set.String.intersect)
    )
    ->Set.String.size
    //    switch compareSet {
    //    | Some(cs) => setArr->Array.reduce(cs, Set.String.intersect)
    //    | None => assert false
    //    }->Set.String.size
  })
  ->Array.reduce(0, (sum, n) => sum + n)
}

sample->getAnswerPart2->Js.log // 6
Year2020Day6Input.input->getAnswerPart2->Js.log // 3316
