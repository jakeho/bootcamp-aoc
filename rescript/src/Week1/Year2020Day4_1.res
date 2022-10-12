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
type byr = BYR(int)
type iyr = IYR(int)
type eyr = EYR(int)
type hgt = CM(int) | INCH(int)
// type hcl = HEX(string)
type hcl = string
type ecl = AMB | BLU | BRN | GRY | GRN | HZL | OTH
type pid = string
type yearTypes = BYR(int) | IYR(int) | EYR(int) | None

let hgtVal = CM(100)
let hgtVal2 = INCH(100)

let _ = switch hgtVal {
| CM(hgtVal') => hgtVal'->Js.log2("is the type of CM")
| INCH(hgtVal') => hgtVal'->Js.log2("is the type of INCH")
| _ => Js.log("Not the type")
}

/*
All fields should be placed except cid. cid treated as an optional field.
*/
type passport = {
  byr: byr,
  iyr: iyr,
  eyr: eyr,
  hgt: hgt,
  hcl: hcl,
  ecl: ecl,
  pid: pid,
  //  cid?: string, // optional field in Record is available from v10
  cid: option<string>,
}

let requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
let hasRequiredFields = (passport: Map.String.t<string>): option<Map.String.t<string>> => {
  let keysInPassport = passport->Map.String.keysToArray
  requiredFields->Array.every(field => keysInPassport->Js.Array2.includes(field))
    ? Some(passport)
    : None
}

let validCounter = (cnt: int, state: bool): int => state ? cnt + 1 : cnt

// Parse, don't validate
let parser = (input: string): array<Map.String.t<string>> => {
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

// input:string -> Map.String.t -> array<option<passport>> -> array<passport>
// ~~phantom type: 유령타입~~
// type parameter
type invalid
type valid
type pp<'a> = {name: string, age: int}
let p1: pp<invalid> = {name: "toss", age: 3}
let p2: pp<valid> = {name: "greenlabs", age: 3}
// input:string -> Map.String.t -> array<passport<invalid>> -> array<passport<valid>>

/*
Having separate validators for each field? How to store and applying them?
If they are declared as functions, you should know which function is a proper validator among the fields.
Variant? Pattern matching? 🤔
Let me start with the easiest way.
*/

let optStringToOptInt = (str: option<string>) =>
  str->Option.getWithDefault("")->Js.String2.trim->Int.fromString

// Checks the string is int and in the given range.
let isYearBetween = (year: option<string>, min: int, max: int): bool => {
  switch Js.Re.test_(%re("/^\\d{4}$/g"), year->Option.getWithDefault("")) {
  | true =>
    switch year->optStringToOptInt->Option.getWithDefault(0) {
    | y => y >= min && y <= max
    }
  | false => false
  }
}
let isByr = (year: option<string>) => year->isYearBetween(1920, 2002)
let isIyr = (year: option<string>) => year->isYearBetween(2010, 2020)
let isEyr = (year: option<string>) => year->isYearBetween(2020, 2030)
let isHcl = (color: string) => {
  switch Js.Re.exec_(%re("/^#[a-f\d]{6}$/g"), color) {
  | Some(_) => true
  | None => false
  }
}

let eclParser = (color: string): option<ecl> => {
  switch color {
  | "amb" => Some(AMB)
  | "blu" => Some(BLU)
  | "brn" => Some(BRN)
  | "gry" => Some(GRY)
  | "grn" => Some(GRN)
  | "hzl" => Some(HZL)
  | "oth" => Some(OTH)
  | _ => None
  }
}

let hgtValidatorCm = (value: int) => {
  value >= 150 && value <= 193
}

let hgtValidatorInch = (value: int) => {
  value >= 59 && value <= 76
}

let hgtParser = (hgt: string): option<hgt> => {
  let regExp = %re("/^(\\d+)(cm|in){1}$/g")
  let size = hgt->Js_string2.replaceByRe(regExp, "$1")
  let unit = hgt->Js_string2.replaceByRe(regExp, "$2")
  let matched = Js_re.exec_(regExp, hgt)->Option.isSome
  switch matched && size !== "" && unit !== "" {
  | true =>
    let intSize = size->Int.fromString->Option.getExn
    switch unit {
    | "cm" => hgtValidatorCm(intSize) ? Some(CM(intSize)) : None
    | "in" => hgtValidatorInch(intSize) ? Some(INCH(intSize)) : None
    | _ => None
    }
  | false => None
  }
}

let isPid = (pid: string) => {
  switch Js.Re.exec_(%re("/^[0-9]{9}$/g"), pid) {
  | Some(_) => true
  | None => false
  }
}

let parser2 = (input: array<Map.String.t<string>>): array<passport> => {
  input
  ->Array.keepMap(hasRequiredFields)
  ->Array.keepMap(candidate => {
    let byr = candidate->Map.String.get("byr")
    let iyr = candidate->Map.String.get("iyr")
    let eyr = candidate->Map.String.get("eyr")
    let hgt = candidate->Map.String.get("hgt")->Option.getWithDefault("")->hgtParser
    let hcl = candidate->Map.String.get("hcl")
    let ecl = candidate->Map.String.get("ecl")->Option.getWithDefault("")->eclParser
    let pid = candidate->Map.String.get("pid")
    let cid = candidate->Map.String.get("cid")
    // It might be reasonable and affordable to check all fields...
    // But... is there any other way better than this?

    switch byr->isByr &&
    iyr->isIyr &&
    eyr->isEyr &&
    hgt->Option.isSome &&
    hcl->Option.getWithDefault("")->isHcl &&
    ecl->Option.isSome &&
    pid->Option.getWithDefault("")->isPid {
    | true =>
      Some({
        byr: BYR(byr->optStringToOptInt->Option.getWithDefault(0)),
        iyr: IYR(iyr->optStringToOptInt->Option.getWithDefault(0)),
        eyr: EYR(eyr->optStringToOptInt->Option.getWithDefault(0)),
        hgt: hgt->Option.getExn,
        hcl: hcl->Option.getWithDefault(""),
        ecl: ecl->Option.getExn,
        pid: pid->Option.getWithDefault(""),
        cid: cid,
      })
    | false => None
    }
  })
}

// Part1
Js.log("Part 1")
Year2020Day4Input.sample
->parser
->Array.keepMap(hasRequiredFields)
->Array.length
->Js.log2("passport(s) are valid.") // 2

Year2020Day4Input.input
->parser
->Array.keepMap(hasRequiredFields)
->Array.length
->Js.log2("passport(s) are valid.") // 260

// Part2
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
*/
Js.log("\n\nPart 2")
Year2020Day4Input.sample->parser->parser2->Array.length->Js.log // 2
Year2020Day4Input.input->parser->parser2->Array.length->Js.log // 153

// parser generator
// parser: input -> data
// parser generator: pred -> parser

// module Passport = {
//   type t = {name: string, age: int}
//   let isEcl = ...
//   let isHcl = ...
// }

// module Parser = {
//   type t
//   type make: (regExp, array<string => bool>) => t
//   type parse: (t, string) => array<option<Passport.t>>
// }

// input->Parser.make(...,[Passport.isEcl, Passport.isHcl])->Parser.parse

// Passport_Korea.res <- 2) x
// Passport.res
// module Korea = Passport_Korea <- 1) 먼저 여기로

// application
// Passport.Korea.x
