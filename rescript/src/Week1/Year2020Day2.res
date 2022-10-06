open Belt

// n-n x: xxxxxxx => min-max key: text
let parser = (input: string): array<(int, int, string, string)> => {
  // array<(int, int, string, string)>
  let regExp = %re("/^(\d+)-(\d+)\s(\w+):\s(\w+)$/g")
  input
  ->Js.String2.trim
  ->Js.String2.splitByRe(%re("/\\n/g"))
  ->Array.map(line => {
    line->Option.mapWithDefault((0, 0, "", ""), someLine => {
      let min =
        someLine->Js.String2.replaceByRe(regExp, "$1")->Int.fromString->Option.getWithDefault(0)
      let max =
        someLine->Js.String2.replaceByRe(regExp, "$2")->Int.fromString->Option.getWithDefault(0)
      let key = someLine->Js.String2.replaceByRe(regExp, "$3")
      let passwd = someLine->Js.String2.replaceByRe(regExp, "$4")

      (min, max, key, passwd)
    })
  })
}

let validator = ((min, max, key, passwd): (int, int, string, string)): bool => {
  let keyCount =
    passwd
    ->Js.String2.splitByRe(%re("/\B/g"))
    ->Array.reduce(0, (cnt, char) => {
      char->Option.mapWithDefault(0, char => {
        switch key === char {
        | true => cnt + 1
        | false => cnt
        }
      })
    })

  keyCount >= min && keyCount <= max
}

let validCounter = (cnt: int, state: bool): int => state ? cnt + 1 : cnt

Year2020Day2Input.sample
->parser
->Array.map(validator)
->Array.reduce(0, validCounter)
->Js.log2("valid password(s) found.")

Year2020Day2Input.input
->parser
->Array.map(validator)
->Array.reduce(0, validCounter)
->Js.log2("valid password(s) found.")
