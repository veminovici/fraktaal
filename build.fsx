#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.Testing.Common
nuget Fake.DotNet.Testing.Coverlet
nuget Fake.DotNet.Testing.Expecto
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"

#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.DotNet.Testing
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
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
let pckWithConfiguration = DotNet.pack  (fun o -> { o with Configuration = configuration })

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
    |> Expecto.run id )


Target.create "BT" ignore

"Clean Tests" ==> "Build Tests" ==> "Tests Expecto" ==> "BT"

//
// All
//

Target.create "Clean" (fun _ ->

    Trace.trace "Cleaning lib and tests bin/obj folders"

    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "tests/**/bin"
    ++ "tests/**/obj"
    |> Shell.cleanDirs )

Target.create "Build" (fun _ ->

    Trace.trace "Building lib and tests projects"

    !! "src/**/*.*proj"
    ++ "tests/**/*.*proj"
    |> Seq.iter bldWithConfiguration )

Target.create "Pack" (fun _ ->

    Trace.trace "Packing lib projects"

    !! "src/**/*.*proj"
    |> Seq.iter pckWithConfiguration )

Target.create "Expecto" (fun _ ->
    Trace.trace "Running Expecto tests"

    !! "tests/xpect/bin/Release/**/xpect.exe"
    |> Expecto.run id )

Target.create "All" ignore

"Clean"
    ==> "Build"
    ==> "Expecto"
    ==> "Pack"
    ==> "All"

Target.runOrDefaultWithArguments "All"
