#!/usr/bin/env node
import { getEvaluator } from 'boon-js';
import { writeFileSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

// These are parser test cases from boon-js
// The assumption is that they at least represent something 
// somewhat tricky or error-prone.
const tests = [
  "first AND second OR third",
  "first AND second AND third",
  "first XOR second AND third OR fourth",
  "first OR second AND third",
  "first AND second XOR third OR fourth",
  "first OR second AND third XOR fourth",
  "first OR second XOR third AND fourth",
  "first OR NOT second XOR third AND NOT fourth",
  "(first OR second) AND third",
  "first XOR (second AND third)",
  "first XOR (second AND third) OR fourth",
  "NOT ((first OR second) AND NOT third) XOR (NOT fourth AND fifth)",
  "NOT (first AND second AND NOT third) XOR (NOT fourth XOR fifth)"
]

const values = {
  first: Math.random() > .5,
  second: Math.random() > .5,
  third: Math.random() > .5,
  fourth: Math.random() > .5,
  fifth: Math.random() > .5,
}

const caseTuples = tests.map(expr => {
  return {
    expr: expr,
    result: getEvaluator(expr)(values)
  }
}).map(testCase => {
  return `("${testCase.expr}", ${testCase.result ? "True" : "False"})`    
}).join('\n    , ');

const valueTuples = Object.entries(values).map(([key, value]) => {
    return `("${key}", ${value ? "True" : "False"})`;
});

const elmFile = join(dirname(fileURLToPath(import.meta.url)), '../tests/Compat.elm')
const moduleText = 
`
module Compat exposing (..)

import Dict

values = 
  Dict.fromList([${valueTuples}])

cases = 
    [ ${caseTuples}
    ]
`.trim();

writeFileSync(elmFile, moduleText);
