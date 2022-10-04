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
// Need to separate the lines for each person? Seems not...

let byGroup = (str: string): array<option<string>> =>
  str->Js.String2.trim->Js.String2.splitByRe(%re("/\\n\\n/g"))

let byPerson = (groups: array<option<string>>): array<array<option<string>>> =>
  groups->Array.map(group => group->Option.getWithDefault("")->Js.String2.splitByRe(%re("/\\n/g")))

let flatten = (arr: array<array<'a>>): array<'b> => arr->Array.concatMany

let byQuestion = (arr: array<option<'a>>) => {
  let newArr: array<string> = Array.make(-1, "")
  let _ = arr->Array.forEach(el => {
    let _ = switch el {
    | Some(el) => el->Js.String2.split("")->Js.Array.pushMany(newArr)
    | None => -1
    }
  })
  newArr
}

let dedupe = (arr: array<string>) => {
  let newArr: array<string> = Array.make(-1, "")
  let _ = arr->Array.forEach(el => {
    let _ = switch newArr->Js.Array2.includes(el) {
    | true => -1
    | false => newArr->Js.Array2.push(el)
    }
  })
  newArr
}

let sumWeight = (arr: array<array<option<'a>>>): int => {
  arr->Js.Array2.reduce((sum, answers) => {
    sum + answers->byQuestion->dedupe->Array.length
  }, 0)
}

//sample->byGroup->byPerson->flatten->byQuestion->dedupe->Js.log
sample->byGroup->byPerson->Js.log
sample->byGroup->byPerson->sumWeight->Js.log // 11

Year2020Day6Input.input->byGroup->byPerson->sumWeight->Js.log // 6726
