#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.Testing.Common
nuget Fake.DotNet.Testing.Expecto
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"

#load ".fake/build.fsx/intellisense.fsx"
#load "./expecto.fsx"


open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Expecto
open System

let codeBinObjDirs = !! "src/**/bin" ++ "src/**/obj"
let codeProjects   = !! "src/**/*.*proj"

let testBinObjDirs = !! "tests/**/bin" ++ "tests/**/obj"
let testProjects   = !! "tests/**/*.*proj"

let expectoBins    = !! "tests/xpect/bin/Release/**/xpect.exe"

//Target.initEnvironment ()

let args = Target.getArguments()
let configuration = 
    match args with
    | Some args ->
        args
        |> Array.contains "-Release"
        |> function
        | true  -> DotNet.BuildConfiguration.Release
        | false -> 
            args
            |> Array.contains "-Debug"
            |> function
            | true  -> DotNet.BuildConfiguration.Debug
            | false -> DotNet.BuildConfiguration.Release
    | None ->
        DotNet.BuildConfiguration.Release

// Trace.trace <| sprintf "Configuration=%O" configuration
let bldWithConfiguration = DotNet.build (fun o -> { o with Configuration = configuration })

//
// Code
//

Target.create "Clean Code" (fun _ ->
    Trace.trace " --- Cleaning Code Bin & Obj Folders --- "

    codeBinObjDirs
    |> Shell.cleanDirs )

Target.create "Build Code" (fun _ ->
    Trace.trace " --- Building Code Projects --- "

    codeProjects
    |> Seq.iter bldWithConfiguration )

Target.create "BC" ignore

"Clean Code" ==> "Build Code" ==> "BC"

//
// Tests only
//

Target.create "Clean Tests" (fun _ ->
    Trace.trace " --- Cleaning Tests Bin & Obj Folders --- "

    testBinObjDirs
    |> Shell.cleanDirs )

Target.create "Build Tests" (fun _ ->
    Trace.trace " --- Building Tests Projects --- "

    testProjects
    |> Seq.iter bldWithConfiguration )

Target.create "Tests Expecto" (fun _ ->
    Trace.trace " --- Running Tests --- "

    expectoBins
    |> runExpecto id )


Target.create "BT" ignore

"Clean Tests" ==> "Build Tests" ==> "Tests Expecto" ==> "BT"

//
// All
//

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "tests/**/bin"
    ++ "tests/**/obj"
    |> Shell.cleanDirs 
)

Target.create "Build" (fun p ->

    Trace.trace <| sprintf " --- Building --- "
    Trace.trace <| sprintf "args=%O" args
    Trace.trace <| sprintf "params=%O" p.Context.Arguments

    !! "src/**/*.*proj"
    ++ "tests/**/*.*proj"
    |> Seq.iter bldWithConfiguration
)

Target.create "Expecto" (fun _ ->
    !! "tests/xpect/bin/Release/**/xpect.exe"
    |> runExpecto id)

Target.create "All" ignore

"Clean"
    ==> "Build"
    ==> "Expecto"
    ==> "All"

Target.runOrDefaultWithArguments "All"
