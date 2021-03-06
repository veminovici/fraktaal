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

let root = __SOURCE_DIRECTORY__

let codeBinObjDirs = !! "src/**/bin" ++ "src/**/obj"
let codeProjects   = !! "src/**/*.*proj"

let testBinObjDirs = !! "tests/**/bin" ++ "tests/**/obj"
let testProjects   = !! "tests/**/*.*proj"

let expectoBins    = !! "tests/xpect/bin/Release/**/xpect.exe"

let traceTitle ttl = 
    Trace.trace ""
    Trace.trace <| sprintf " --- %s --- " ttl
    Trace.trace ""


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

Trace.trace <| ""
Trace.trace <| "<<< Simplee.Fraktal Project >>>"
Trace.trace <| sprintf "Configuration=%O" configuration
Trace.trace <| ""

let bldWithConfiguration = DotNet.build (fun o -> { o with Configuration = configuration })
let pckWithConfiguration = DotNet.pack  (fun o -> { o with Configuration = configuration })
let tstWithConfiguration = DotNet.test  (fun o -> 
        { o with Configuration = configuration }
        |> Coverlet.withDotNetTestOptions (fun p -> 
            { p with
                OutputFormat = Coverlet.OutputFormat.OpenCover
                Output = "../../.coverage/coverage.xml"
                UseSourceLink = true} ))

//
// Code
//

Target.create "Lib.Clean" (fun _ ->
    traceTitle "Cleaning Code Bin & Obj Folders"

    codeBinObjDirs
    |> Shell.cleanDirs )

Target.create "Lib.Build" (fun _ ->
    traceTitle "Building Code Projects"

    codeProjects
    |> Seq.iter bldWithConfiguration )

Target.create "BC" ignore

//
// Tests only
//

Target.create "Tst.Clean" (fun _ ->
    traceTitle "Cleaning Tests Bin & Obj Folders"

    testBinObjDirs
    |> Shell.cleanDirs )

Target.create "Tst.Build" (fun _ ->
    traceTitle "Building Tests Projects"

    testProjects
    |> Seq.iter bldWithConfiguration )

Target.create "Tst.Expecto" (fun _ ->
    traceTitle "Running Expecto Tests"

    expectoBins
    |> Expecto.run id )

Target.create "Tst.Coverage" (fun _ ->
    traceTitle "Running Tests & Code Coverage"
    tstWithConfiguration "." )

Target.create "BT" ignore

//
// Release
//

Target.create "Rel.Pack" (fun _ ->
    traceTitle "Packing lib projects"

    !! "src/**/*.*proj"
    |> Seq.iter pckWithConfiguration )

Target.create "Release" ignore

"Lib.Clean"
==> "Lib.Build" 
==> "BC"
==> "Tst.Clean" 
==> "Tst.Build" 
//==> "Tst.Expecto"
==> "Tst.Coverage"
==> "BT"
==> "Rel.Pack"
==> "Release"

Target.runOrDefaultWithArguments "Release"
