#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.Testing.Common
nuget Fake.DotNet.Testing.Expecto
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"

#load ".fake/build.fsx/intellisense.fsx"
#load "./expecto.fsx"

open Expecto
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open System

Target.initEnvironment ()

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    |> Shell.cleanDirs 
)

Target.create "Build" (fun _ ->
    !! "src/**/*.*proj"
    ++ "tests/**/*.*proj"
    |> Seq.iter (DotNet.build id)
)

Target.create "Expecto" (fun _ ->
    !! "tests/xpect/bin/Release/**/xpect.exe"
    |> runExpecto id)

Target.create "All" ignore

"Clean"
  ==> "Build"
  ==> "Expecto"
  ==> "All"

Target.runOrDefault "All"
