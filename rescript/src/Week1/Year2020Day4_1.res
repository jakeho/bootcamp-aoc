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
/*
All fields should be placed except cid. cid treated as an optional field.
*/
//type passport = {
//  byr: string,
//  iyr: string,
//  eyr: string,
//  hgt: string,
//  hcl: string,
//  ecl: string,
//  pid: string,
//  cid: string, // optional field in Record is available from v10
//}

// split by line, split by space and then split by `:`.
//let passportTemplate = {
//  byr: "",
//  iyr: "",
//  eyr: "",
//  hgt: "",
//  hcl: "",
//  ecl: "",
//  pid: "",
//  cid: "",
//}
// Map? Record? Dict?
let parser = (input: string): array<Map.String.t<string>> => {
  input
  ->Js.String2.trim
  ->Js.String2.split("\n\n")
  ->Array.map(incomplete => {
    // ['ecl:gry','pid:860033327','eyr:2020','hcl:#fffffd','byr:1937','iyr:2017','cid:147','hgt:183cm'] <- split by " "
    incomplete
    ->Js.String2.replaceByRe(%re("/\\n/g"), " ")
    ->Js.String2.split(" ")
    ->Array.reduce(Map.String.empty, (passport, fieldAndValue) => {
      let key = fieldAndValue->Js.String2.replaceByRe(%re("/^(\w+):([\w\d#]+)/g"), "$1")
      let value = fieldAndValue->Js.String2.replaceByRe(%re("/^(\w+):([\w\d#]+)/g"), "$2")

      passport->Map.String.set(key, value)
    })
  })
}

let requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
let validate = (passport: Map.String.t<string>): bool => {
  let keysInPassport = passport->Map.String.keysToArray
  requiredFields->Array.every(field => keysInPassport->Js.Array2.includes(field))
}

let validCounter = (cnt: int, state: bool): int => state ? cnt + 1 : cnt

// Part1
Js.log("Part 1")
Year2020Day4Input.sample
->parser
->Array.map(validate)
->Array.reduce(0, validCounter)
->Js.log2("passport(s) are valid.") // 2

Year2020Day4Input.input
->parser
->Array.map(validate)
->Array.reduce(0, validCounter)
->Js.log2("passport(s) are valid.") // 260
