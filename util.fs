module Util

let inline implicitConv (x: ^a): ^b = ((^a or ^b): (static member op_Implicit: ^a -> ^b) x)
