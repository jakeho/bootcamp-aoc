open Belt

/*
Abbreviations

byr (Birth Year)
iyr (Issue Year)
eyr (Expiration Year)
hgt (Height)
hcl (Hair Color)
ecl (Eye Color)
pid (Passport ID)
cid (Country ID)
*/
type eyeColor = AMB | BLU | BRN | GRY | GRN | HZL | OTH
type year = Year(int)
/*
All fields should be placed except cid. cid treated as an optional field.
*/
type passport = {
  byr: year,
  iyr: year,
  eyr: year,
  hgt: string,
  hcl: string,
  ecl: eyeColor,
  pid: string,
  //  cid?: string, // optional field in Record is available from v10
  cid: option<string>,
}

let yearParser = (year: string, min: int, max: int): option<year> => {
  switch Js.Re.test_(%re("/^\\d{4}$/g"), year) {
  | true => year->Js.String2.trim->Int.fromString->Option.getWithDefault(0)->Year->Some
  | false => None
  }
}

let requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
let hasRequiredFields = (passport: Map.String.t<string>): bool => {
  let keysInPassport = passport->Map.String.keysToArray
  requiredFields->Array.every(field => keysInPassport->Js.Array2.includes(field))
}

let validCounter = (cnt: int, state: bool): int => state ? cnt + 1 : cnt

// Parse, don't validate
let parser = (input: string) => {
  let keyValueRegexp = %re("/^(\w+):([\w\d#]+)/g")
  input
  ->Js.String2.trim
  ->Js.String2.split("\n\n")
  ->Array.map(incomplete => {
    // ['ecl:gry','pid:860033327','eyr:2020','hcl:#fffffd','byr:1937','iyr:2017','cid:147','hgt:183cm'] <- split by " "
    incomplete
    ->Js.String2.replaceByRe(%re("/\\n/g"), " ")
    ->Js.String2.split(" ")
    ->Array.reduce(Map.String.empty, (passport, fieldAndValue) => {
      let key = fieldAndValue->Js.String2.replaceByRe(keyValueRegexp, "$1")
      let value = fieldAndValue->Js.String2.replaceByRe(keyValueRegexp, "$2")

      passport->Map.String.set(key, value)
    })
  })
}

/**
Requirements for the Part 2
byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.

Having separate validators for each field? How to store and applying them?
If they are declared as functions, you should know which function is a proper validator among the fields.
Variant? Pattern matching? 🤔
Let me start with the easiest way.
*/
let byrValidator = (year: int) => year >= 1920 && year <= 2002
let iyrValidator = (year: int) => year >= 2010 && year <= 2020
let eyrValidator = (year: int) => year >= 2020 && year <= 2030
let hgtValidator = (height: string) => {
  switch height |> Js.Re.exec_(%re("/^\d+(cm|in)$/g")) {
  | Some(_) => true // Need to check the number according to the unit.
  | None => false
  }
}
let hclValidator = (color: string) => {
  switch color |> Js.Re.exec_(%re("/^#[a-f\d]{6}$/g")) {
  | Some(_) => true
  | None => false
  }
}

let eclValidator = (eyeColor: eyeColor) => {
  switch eyeColor {
  | AMB | BLU | BRN | GRY | GRN | HZL | OTH => true
  }
} // How to map the string value with the types?
let pidValidator = (pid: string) => {
  switch pid |> Js.Re.exec_(%re("/^[0-9]{9}$/g")) {
  | Some(_) => true
  | None => false
  }
}

let hasValidValues = (passport: Map.String.t<string>): bool => {
  //  passport
  //  ->Map.String.getWithDefault("byr", "")
  //  ->Int.fromString
  //  ->Option.getWithDefault(0)
  //  ->byrValidator
  //  ->Js.log
  false
}

// Part1
Js.log("Part 1")
Year2020Day4Input.sample
->parser
->Array.map(hasRequiredFields)
->Array.reduce(0, validCounter)
->Js.log2("passport(s) are valid.") // 2

Year2020Day4Input.input
->parser
->Array.map(hasRequiredFields)
->Array.reduce(0, validCounter)
->Js.log2("passport(s) are valid.") // 260

// Part2
Js.log("\n\nPart 2")
let _ = Year2020Day4Input.sample->parser->Array.map(hasValidValues)
