open Core
open Commands

let command = Command.group ~summary:"main " [ ("test", Ctest.test) ]

let () = Command.run command
