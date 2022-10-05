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

let trimAndSplit = (str: string, regExp: Js_re.t): array<option<string>> =>
  str->Js.String2.trim->Js.String2.splitByRe(regExp)

let splitGroup = (str: string): array<option<string>> => str->trimAndSplit(%re("/\\n\\n/g"))

let splitLine = (str: string): array<option<string>> => str->trimAndSplit(%re("/\\n/g"))

let splitChar = (str: string): array<option<string>> => str->trimAndSplit(%re("/\\B/g"))

let splitGroupAndLine = (str: string, lineSplitter: 'a => array<option<'a>>) => {
  str
  ->splitGroup
  ->Array.map(arr => {
    arr->Option.map(opt => opt->lineSplitter)
  })
}

let dedupe = (strArr: array<option<string>>) => {
  strArr->Array.map(str => str->Option.getWithDefault(""))->Set.String.fromArray->Set.String.toArray
}

// Part 1
let getAnswerPart1 = (str: string) => {
  str
  ->splitGroupAndLine(splitLine)
  ->Array.map(arr => {
    // [ 'abc' ] [ 'a', 'b', 'c' ] [ 'ab', 'ac' ] [ 'a', 'a', 'a', 'a' ] [ 'b' ]
    arr->Option.map(optArr => {
      optArr
      ->Array.map(answers => answers->Option.getWithDefault("")->splitChar)
      ->flatten
      ->dedupe
      ->Array.length
    })
  })
  ->Array.reduce(0, (sum, n) => sum + n->Option.getWithDefault(0))
}

sample->getAnswerPart1->Js.log // 11
Year2020Day6Input.input->getAnswerPart1->Js.log // 6726

/*
let byQuestion = (arr: array<option<string>>): array<string> =>
  arr->Array.map(opt => opt->Option.getWithDefault("")->Js.String2.split(""))->flatten

let dedupe = (arr: array<string>): array<string> => {
  let newArr: array<string> = Array.make(-1, "")
  let _ = arr->Array.forEach(el => {
    let _ = switch newArr->Js.Array2.includes(el) {
    | true => -1
    | false => newArr->Js.Array2.push(el)
    }
  })
  newArr
}

//let sumDedupeWeight = (arr: array<array<option<'a>>>): int => {
//  arr->Js.Array2.reduce((sum, answers) => {
//    sum + answers->byQuestion->dedupe->Array.length
//  }, 0)
//}

//sample->byGroup->byPerson->flatten->byQuestion->dedupe->Js.log
sample->byGroup->byPerson->Array.map(arr => arr->byQuestion)->Js.log

// Part 1
//sample->byGroup->byPerson->sumDedupeWeight->Js.log // 11
//Year2020Day6Input.input->byGroup->byPerson->sumDedupeWeight->Js.log // 6726

// Part 2
//sample->byGroup->byPerson->sumIntersectWeight->Js.log
*/
